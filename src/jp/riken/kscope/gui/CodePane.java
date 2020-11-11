/*
 * K-scope
 * Copyright 2012-2013 RIKEN, Japan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package jp.riken.kscope.gui;

import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.util.ArrayList;
import java.util.List;
import javax.swing.JTextPane;
import javax.swing.SizeRequirements;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.plaf.basic.BasicTextPaneUI;
import javax.swing.text.AbstractDocument;
import javax.swing.text.BadLocationException;
import javax.swing.text.BoxView;
import javax.swing.text.ComponentView;
import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.IconView;
import javax.swing.text.LabelView;
import javax.swing.text.ParagraphView;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;
import javax.swing.text.StyledEditorKit;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Text pine for displaying source code
 *
 * @author RIKEN
 */
public class CodePane extends JTextPane implements ITabComponent, FocusListener {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Automatic line feed character position (0 = no automatic line feed) */
  private int wordwrap;

  // (2012/4/18) changed by teraim
  private Font sourceFont; // Source font
  private Color activeRowColor; // Selected line background color

  /** Parent component */
  private ITabComponent parentComponent = null;

  /** Line background color list */
  private List<LinesBackground> listLinesColor;

  /**
   * Multi-line range background color information class
   *
   * @author RIKEN
   */
  public class LinesBackground {
    /** Start line number (1 ~) */
    public int start;
    /** End line number (1 ~) */
    public int end;
    /** Background color */
    public Color color;

    /**
     * Constructor
     *
     * @param start Start line
     * @param end End line
     * @param color Background color
     */
    public LinesBackground(int start, int end, Color color) {
      this.start = start;
      this.end = end;
      this.color = color;
    }
  }

  /** Constructor */
  public CodePane() {
    super();
    initGUI();
  }

  /**
   * Constructor
   *
   * @param doc Styled documentation
   */
  public CodePane(StyledDocument doc) {
    super(doc);
    initGUI();
  }

  /** Initialize the GUI */
  private void initGUI() {

    // (2012/4/18) added by teraim
    try {
      // Set the background color of the active line
      this.activeRowColor = new jp.riken.kscope.properties.SourceProperties().getActiverowColor();
      // Source view font settings
      this.sourceFont = new jp.riken.kscope.properties.SourceProperties().getFont();
      this.setFont(sourceFont);
    } catch (Exception e) {
      e.printStackTrace();
    }

    this.setEditorKit(new WordWrapEditorKit());
    this.setUI(new LineHighlightTextPaneUI());

    this.addFocusListener(this);
  }

  /**
   * Get the wrapping position (pixel) from the number of wrapping character strings.
   *
   * @return Return position (pixels)
   */
  private int getWordWrapWidth() {
    if (wordwrap <= 0) return 0;
    FontMetrics fontMetrics = getFontMetrics(getFont());
    int width = fontMetrics.charWidth('0') * wordwrap;
    return width;
  }

  /**
   * Get the automatic newline character position.
   *
   * @return wordwrap Automatic newline character position
   */
  public int getWordwrap() {
    return wordwrap;
  }

  /**
   * Set the automatic newline character position.
   *
   * @param wordwrap Automatic newline character position
   */
  public void setWordwrap(int wordwrap) {
    this.wordwrap = wordwrap;
    this.repaint();
  }

  /**
   * Line wrap paragraph view class
   *
   * @author RIKEN
   */
  private class WordWrapParagraphView extends ParagraphView {
    /**
     * Constructor
     *
     * @param elem Elements handled by this view
     */
    public WordWrapParagraphView(Element elem) {
      super(elem);
    }

    /**
     * Calculate the row width size requirement. <br>
     * Set the wrapping size for one line.
     *
     * @param axis Line position
     * @param r Component size and position object
     * @return Component size and position object
     */
    @Override
    protected SizeRequirements calculateMinorAxisRequirements(int axis, SizeRequirements r) {
      SizeRequirements req = super.calculateMinorAxisRequirements(axis, r);
      int wrapPixel = getWordWrapWidth();
      if (wrapPixel <= 0) {
        req.minimum = req.preferred;
      } else {
        req.minimum = wrapPixel;
        req.preferred = wrapPixel;
      }
      return req;
    }

    /**
     * Fetches the constraint span that flows against the specified child index.
     *
     * @param index Index of the queried view
     * @return View constraint span
     */
    @Override
    public int getFlowSpan(int index) {
      int wrapPixel = getWordWrapWidth();
      if (wrapPixel <= 0) {
        return Integer.MAX_VALUE;
      } else {
        return wrapPixel;
      }
    }
    /*
    public void layout(int width, int height) {
      super.layout(500, height);
    }
       */

  }

  /**
   * View creation class
   *
   * @author RIKEN
   */
  private class WordWrapViewFactory implements ViewFactory {
    /**
     * Create a view based on the element.
     *
     * @param elem Element to be created
     * @return view
     */
    @Override
    public View create(Element elem) {
      String kind = elem.getName();
      if (kind != null) {
        if (kind.equals(AbstractDocument.ContentElementName)) {
          return new LabelView(elem);
        } else if (kind.equals(AbstractDocument.ParagraphElementName)) {
          return new WordWrapParagraphView(elem);
        } else if (kind.equals(AbstractDocument.SectionElementName)) {
          return new BoxView(elem, View.Y_AXIS);
        } else if (kind.equals(StyleConstants.ComponentElementName)) {
          return new ComponentView(elem);
        } else if (kind.equals(StyleConstants.IconElementName)) {
          return new IconView(elem);
        }
      }
      return new LabelView(elem);
    }
  }

  /**
   * Set the background color for the current line.
   *
   * @author RIKEN
   */
  public class LineHighlightTextPaneUI extends BasicTextPaneUI {
    /** Constructor */
    public LineHighlightTextPaneUI() {
      CodePane.this.addCaretListener(
          new CaretListener() {
            @Override
            public void caretUpdate(CaretEvent e) {
              CodePane.this.repaint();
            }
          });
    }

    /**
     * Draw the background color.
     *
     * @param g drawing graphic
     */
    @Override
    public void paintBackground(Graphics g) {
      super.paintBackground(g);
      try {

        // Set the line background color
        if (CodePane.this.listLinesColor != null) {

          int startline = CodePane.this.getViewStartLine();
          int endline = CodePane.this.getViewEndLine();
          for (LinesBackground line : CodePane.this.listLinesColor) {
            // Is it within the display range?
            if (startline > line.end) continue;
            if (endline < line.start) continue;

            // Offset position of range line number
            int start = CodePane.this.getLineStartOffset(line.start);
            int end = CodePane.this.getLineEndOffset(line.end);

            // Get the drawing range of the start and end lines
            Rectangle startRect = modelToView(CodePane.this, start);
            Rectangle endRect = modelToView(CodePane.this, end);

            // Get the range to draw
            Rectangle drawRect = new Rectangle(startRect);
            drawRect.height = endRect.y + endRect.height - startRect.y;
            if (startRect.x > endRect.x) {
              drawRect.x = endRect.x;
            }
            int startEndPos = startRect.x + startRect.width;
            int endEndPos = endRect.x + endRect.width;
            drawRect.width =
                (startEndPos > endEndPos) ? startEndPos - drawRect.x : endEndPos - drawRect.x;

            // Draw background color
            g.setColor(line.color);
            g.fillRect(0, drawRect.y, CodePane.this.getWidth(), drawRect.height);
          }
        }

        // Highlight the current caret position
        if (activeRowColor != null) {
          Rectangle rect = modelToView(CodePane.this, CodePane.this.getCaretPosition());
          int y = rect.y;
          int h = rect.height;
          g.setColor(activeRowColor);
          g.fillRect(0, y, CodePane.this.getWidth(), h);
        }

      } catch (BadLocationException ex) {
        ex.printStackTrace();
      }
    }
  }

  /**
   * Formatted text style
   *
   * @author RIKEN
   */
  private class WordWrapEditorKit extends StyledEditorKit {
    /** Serial number */
    private static final long serialVersionUID = 1L;

    /**
     * Get the view creation class.
     *
     * @return View creation class
     */
    @Override
    public ViewFactory getViewFactory() {
      return new WordWrapViewFactory();
    }
  }

  /**
   * Get the parent component.
   *
   * @return Parent component
   */
  @Override
  public ITabComponent getParentComponent() {
    return this.parentComponent;
  }

  /**
   * Set the parent component.
   *
   * @param component Parent component
   */
  @Override
  public void setParentComponent(ITabComponent component) {
    this.parentComponent = component;
  }

  /**
   * Set focus listener
   *
   * @param listener Focus listener
   */
  @Override
  public void addTabFocusListener(TabFocusListener listener) {
    this.addFocusListener(listener);
  }

  /** Close tab */
  @Override
  public void closeTabComponent() {
    // Close the tab with the parent tab pine.
    if (this.parentComponent != null) {
      this.parentComponent.closeTabComponent();
    }
  }

  /**
   * Focus acquisition event. <br>
   * Since the caret is hidden when the focus is moved, the caret display is reset when the focus is
   * acquired.
   *
   * @param event Event information
   */
  @Override
  public void focusGained(FocusEvent event) {
    this.getCaret().setVisible(true);
  }

  /**
   * Loss of focus event
   *
   * @param event Event information
   */
  @Override
  public void focusLost(FocusEvent event) {}

  /**
   * Set the background color of the selected line.
   *
   * @param color Selected line background color
   */
  public void setActiveRowColor(Color color) {
    this.activeRowColor = color;
    this.repaint();
  }

  /**
   * Get the background color of the selected line.
   *
   * @return Selected line background color
   */
  public Color getActiveRowColor() {
    return this.activeRowColor;
  }

  /**
   * Get the offset (0 or more) from the beginning of the document at the beginning of the display.
   *
   * @return Offset from the beginning of the document (0 or more)
   */
  public int getViewStartOffset() {
    if (this.parentComponent == null) return 0;
    if (!(this.parentComponent instanceof ScrollCodePane)) return 0;

    ScrollCodePane scroll = (ScrollCodePane) this.parentComponent;
    int start = this.viewToModel(scroll.getViewport().getViewPosition());

    return start;
  }

  /**
   * Get the offset (0 or more) from the beginning of the document at the end of the display.
   *
   * @return Offset from the beginning of the document (0 or more)
   */
  public int getViewEndOffset() {
    if (this.parentComponent == null) return 0;
    if (!(this.parentComponent instanceof ScrollCodePane)) return 0;

    ScrollCodePane scroll = (ScrollCodePane) this.parentComponent;
    Rectangle rect = scroll.getViewport().getVisibleRect();
    int end =
        this.viewToModel(
            new Point(
                scroll.getViewport().getViewPosition().x + this.getWidth(),
                scroll.getViewport().getViewPosition().y + rect.height));

    return end;
  }

  /**
   * Get the line number at the beginning of the display.
   *
   * @return First line number (1 ~)
   */
  public int getViewStartLine() {
    // Display start position
    int start = getViewStartOffset();
    if (start < 0) return 0;

    Document doc = this.getDocument();
    int startline = doc.getDefaultRootElement().getElementIndex(start);

    return startline + 1;
  }

  /**
   * Get the line number at the end of the display.
   *
   * @return Last line number (1 ~)
   */
  public int getViewEndLine() {
    // Display end position
    int end = getViewEndOffset();
    if (end <= 0) return 0;

    Document doc = this.getDocument();
    int endline = doc.getDefaultRootElement().getElementIndex(end);

    return endline + 1;
  }

  /**
   * Get the number of lines in the displayed document.
   *
   * @return Number of document lines
   */
  public int getEndLine() {
    Document doc = this.getDocument();
    int endline = doc.getDefaultRootElement().getElementCount();
    return endline;
  }

  /**
   * Get the offset (0 or more) from the beginning of the document at the beginning of the specified
   * line number.
   *
   * @param line Specified line number (1-)
   * @return Offset from the beginning of the document (0 or more)
   */
  public int getLineStartOffset(int line) {
    if (line <= 0) return 0;

    // Start and end caret positions
    Document doc = this.getDocument();
    if (doc == null) return 0;
    Element root = doc.getDefaultRootElement();
    if (root == null) return 0;
    if (root.getElement(line - 1) == null) return 0;
    int startOffset = root.getElement(line - 1).getStartOffset();

    return startOffset;
  }

  /**
   * Get the offset (0 or more) from the beginning of the document at the end of the specified line
   * number.
   *
   * @param line Specified line number (1-)
   * @return Offset from the beginning of the document (0 or more)
   */
  public int getLineEndOffset(int line) {
    if (line <= 0) return 0;
    // Start and end caret positions
    Document doc = this.getDocument();
    if (doc == null) return 0;
    Element root = doc.getDefaultRootElement();
    if (root == null) return 0;
    if (root.getElement(line - 1) == null) return 0;
    int endOffset = root.getElement(line - 1).getEndOffset() - 1;

    return endOffset;
  }

  /**
   * Get the line number from the offset (0 or greater) from the beginning of the document
   *
   * @param pos Offset from the beginning of the document (0 or greater)
   * @return line number
   */
  public int getRow(int pos) {

    if (this.parentComponent == null) {
      // Search from the beginning of the document
      return SwingUtils.getRow(this, pos);
    }
    if (!(this.parentComponent instanceof ScrollCodePane)) {
      // Search from the beginning of the document
      return SwingUtils.getRow(this, pos);
    }
    int startOffset = getViewStartOffset();
    int endOffset = getViewEndOffset();
    int startLine = getViewStartLine();
    int endLine = getViewEndLine();

    if (pos < startOffset) {
      // Search from the beginning of the document
      return SwingUtils.getRow(this, pos);
    }
    if (pos >= endOffset) {
      return endLine;
    }

    // Check if it exists in the line
    for (int line = startLine; line <= endLine; line++) {
      int start = getLineStartOffset(line);
      int end = getLineEndOffset(line);
      if (start <= pos && pos <= end) {
        return line;
      }
    }

    return 0;
  }

  /**
   * Get column number
   *
   * @param pos Offset from the beginning of the document (0 or greater)
   * @return Column number
   */
  public int getColumn(int pos) {
    return 0;
  }

  /**
   * Get the row background color list
   *
   * @return line background color list
   */
  public List<LinesBackground> getListLinesColor() {
    return listLinesColor;
  }

  /**
   * Set line background color list
   *
   * @param list Line background color list
   */
  public void setListLinesColor(List<LinesBackground> list) {
    this.listLinesColor = list;
  }

  /**
   * Add line background color
   *
   * @param line Line background color
   */
  public void addListLinesColor(LinesBackground line) {
    if (this.listLinesColor == null) {
      this.listLinesColor = new ArrayList<LinesBackground>();
    }
    this.listLinesColor.add(line);
  }

  /** Clear the row background color list */
  public void clearListLinesColor() {
    this.listLinesColor = null;
  }

  /**
   * Add line background color
   *
   * @param start Start line
   * @param end End line
   * @param color Background color
   */
  public void addLinesBackground(int start, int end, Color color) {
    LinesBackground line = new LinesBackground(start, end, color);
    addListLinesColor(line);
  }
}
