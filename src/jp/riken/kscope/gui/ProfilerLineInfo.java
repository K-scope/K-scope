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
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.MatteBorder;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;
import javax.swing.text.StyleConstants;
import javax.swing.text.Utilities;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Profile bar graph display class
 *
 * @author RIKEN
 */
public class ProfilerLineInfo extends JPanel
    implements CaretListener, DocumentListener, PropertyChangeListener {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Line number placement: Left */
  public static final float LEFT = 0.0f;
  /** Line number placement: Center */
  public static final float CENTER = 0.5f;
  /** Line number placement: Right */
  public static final float RIGHT = 1.0f;
  /** Border setting for line number display area */
  @SuppressWarnings("unused")
  private static final Border OUTER = new MatteBorder(0, 0, 0, 2, Color.GRAY);

  private static final Border LEFT_BORDER = new MatteBorder(0, 2, 0, 0, Color.GRAY);
  /** Line number display area height */
  private static final int HEIGHT = Integer.MAX_VALUE - 1000000;
  /** Bar graph default color */
  private final Color DEFAULT_BARCLOR = Color.RED;
  /** Maximum bar graph width */
  private final int BARGRAPH_MAXWIDTH = 60;
  /** Text component to display line number */
  private JTextComponent component;

  /** Font change flag */
  private boolean updateFont;
  /** Margins to the left and right of line numbers */
  private int borderGap;
  /** From the font of the current line of the line number- */
  private Color currentLineForeground;
  /** Line number placement */
  private float digitAlignment;
  /** Minimum number of digits in line number */
  private int minimumDisplayDigits;

  /** Current line number Number of digits */
  private int lastDigits;
  /** Current line number area height */
  private int lastHeight;
  /** Current line number */
  private int lastLine;
  /** Bar graph data */
  private List<ISourceBargraph> listData;
  /** Font table */
  private HashMap<String, FontMetrics> fonts;
  /** Text color */
  private Color BAR_TEXTCOLOR = Color.BLACK;
  /** Maximum cost value display font size */
  private final int BAR_TEXT_FONTSIZE = 13;

  /**
   * Constructor
   *
   * @param component Line number The text component to be displayed
   */
  public ProfilerLineInfo(JTextComponent component) {
    this(component, 3);
  }

  /**
   * Constructor
   *
   * @param component Line number The text component to be displayed
   * @param minimumDisplayDigits Line number Number of digits
   */
  public ProfilerLineInfo(JTextComponent component, int minimumDisplayDigits) {
    this.component = component;

    setFont(component.getFont());

    setBorderGap(5);
    setCurrentLineForeground(Color.RED);
    setDigitAlignment(RIGHT);
    setMinimumDisplayDigits(minimumDisplayDigits);
    setBackground(Color.WHITE);

    component.getDocument().addDocumentListener(this);
    component.addCaretListener(this);
    component.addPropertyChangeListener("font", this);

    listData = new ArrayList<ISourceBargraph>();
  }

  /**
   * Get the font update flag.
   *
   * @return Font update flag
   */
  public boolean getUpdateFont() {
    return updateFont;
  }

  /**
   * Set the font update flag.
   *
   * @param updateFont Font update flag (true = font update)
   */
  public void setUpdateFont(boolean updateFont) {
    this.updateFont = updateFont;
  }

  /**
   * Get the left and right margins of the line number.
   *
   * @return Left and right margins for line numbers
   */
  public int getBorderGap() {
    return borderGap;
  }

  /**
   * Set the left and right margins for line numbers. <br>
   * Default margin = 5px
   *
   * @param borderGap Left and right margins for line numbers
   */
  public void setBorderGap(int borderGap) {
    this.borderGap = borderGap;
    Border inner = new EmptyBorder(0, borderGap, 0, borderGap);
    setBorder(new CompoundBorder(LEFT_BORDER, inner));
    lastDigits = 0;
    setPreferredWidth();
  }

  /**
   * Get the font color of the current line of the line number
   *
   * @return Font color of the current line of the line number
   */
  public Color getCurrentLineForeground() {
    return currentLineForeground == null ? getForeground() : currentLineForeground;
  }

  /**
   * Set the font color of the current line of the line number. <br>
   * Default color = RED
   *
   * @param currentLineForeground Font color of the current line of the line number
   */
  public void setCurrentLineForeground(Color currentLineForeground) {
    this.currentLineForeground = currentLineForeground;
  }

  /**
   * Get the line number placement (LEFT, CENTER, RIGHT).
   *
   * @return Line number placement (LEFT, CENTER, RIGHT)
   */
  public float getDigitAlignment() {
    return digitAlignment;
  }

  /**
   * Set the line number placement (LEFT, CENTER, RIGHT). <br>
   *
   * <ul>
   *   <li>TextLineNumber.LEFT
   *   <li>TextLineNumber.CENTER
   *   <li>TextLineNumber.RIGHT (default) </ ul>
   *
   * @param digitAlignment Line number alignment (LEFT, CENTER, RIGHT)
   */
  public void setDigitAlignment(float digitAlignment) {
    this.digitAlignment =
        digitAlignment > 1.0f ? 1.0f : digitAlignment < 0.0f ? -1.0f : digitAlignment;
  }

  /**
   * Get the minimum number of digits in a line number.
   *
   * @return Minimum number of digits in line number
   */
  public int getMinimumDisplayDigits() {
    return minimumDisplayDigits;
  }

  /**
   * Set the minimum number of digits in the line number.
   *
   * @param minimumDisplayDigits Minimum number of digits in line number
   */
  public void setMinimumDisplayDigits(int minimumDisplayDigits) {
    this.minimumDisplayDigits = minimumDisplayDigits;
    setPreferredWidth();
  }

  /** Set the line number display width. */
  private void setPreferredWidth() {
    Element root = component.getDocument().getDefaultRootElement();
    int lines = root.getElementCount();
    int digits = Math.max(String.valueOf(lines).length(), minimumDisplayDigits);

    // Update sizes when number of digits in the line number changes

    if (lastDigits != digits) {
      lastDigits = digits;
      FontMetrics fontMetrics = getFontMetrics(getFont());
      int width = fontMetrics.charWidth('0') * digits;
      Insets insets = getInsets();
      int preferredWidth = insets.left + insets.right + width;
      if (preferredWidth < BARGRAPH_MAXWIDTH) {
        preferredWidth = BARGRAPH_MAXWIDTH;
      }
      Dimension d = getPreferredSize();
      d.setSize(preferredWidth, HEIGHT);
      setPreferredSize(d);
      setSize(d);
    }
  }

  /** Draw a profiler graph. */
  @Override
  public void paintComponent(Graphics g) {
    super.paintComponent(g);

    // Determine the width of the space available to draw the line number
    FontMetrics fontMetrics = component.getFontMetrics(component.getFont());
    Font defaultFont = SwingUtils.getDefaultFont();
    int fontsize = BAR_TEXT_FONTSIZE;
    if (fontsize > component.getFont().getSize()) {
      fontsize = component.getFont().getSize();
    }
    Font textFont = defaultFont.deriveFont((float) fontsize);
    FontMetrics textMetrics = this.getFontMetrics(textFont);

    Insets insets = getInsets();
    int availableWidth = getSize().width - insets.left - insets.right;

    // Determine the rows to draw within the clipped bounds.
    Rectangle clip = g.getClipBounds();
    int rowStartOffset = component.viewToModel(new Point(0, clip.y));
    int endOffset = component.viewToModel(new Point(0, clip.y + clip.height));

    try {
      while (rowStartOffset <= endOffset) {
        if (isCurrentLine(rowStartOffset)) g.setColor(getCurrentLineForeground());
        else g.setColor(getForeground());

        // Get the line number as a string and then determine the
        // "X" and "Y" offsets for drawing the string.

        ISourceBargraph data = getLineData(rowStartOffset);
        if (data != null) {
          Color barColor = data.getBarColor();
          if (barColor == null) {
            barColor = DEFAULT_BARCLOR;
          }
          String valueText = data.getBarText();
          int width = (int) (data.getBarValue() * availableWidth);
          if (width == 0) width = 1;
          int stringWidth = textMetrics.stringWidth(valueText);
          int lineHeight = fontMetrics.getHeight();
          float ascent = fontMetrics.getAscent();
          float margin = (lineHeight - ascent) / 2;
          int x = getOffsetX(availableWidth, stringWidth) + insets.left;
          int y = getOffsetY(rowStartOffset, fontMetrics);
          // Bar chart
          g.setColor(barColor);
          g.fillRoundRect(this.borderGap, y - (int) (ascent - margin), width, (int) ascent, 0, 0);
          // value
          g.setFont(textFont);
          g.setColor(BAR_TEXTCOLOR);
          g.drawString(valueText, x, y);
        }

        // Move to the next row
        rowStartOffset = Utilities.getRowEnd(component, rowStartOffset) + 1;
      }
    } catch (Exception e) {
    }
  }

  /**
   * Determine if it is the current line.
   *
   * @return true = current line
   */
  private boolean isCurrentLine(int rowStartOffset) {
    int caretPosition = component.getCaretPosition();
    Element root = component.getDocument().getDefaultRootElement();

    if (root.getElementIndex(rowStartOffset) == root.getElementIndex(caretPosition)) return true;
    else return false;
  }

  /**
   * Get line number drawing X position
   *
   * @param availableWidth Display width
   * @param stringWidth Line number string width
   */
  private int getOffsetX(int availableWidth, int stringWidth) {
    return (int) ((availableWidth - stringWidth) * digitAlignment);
  }

  /**
   * Get line number drawing Y position
   *
   * @param rowStartOffset Display start point height
   * @param fontMetrics Line number character font
   */
  private int getOffsetY(int rowStartOffset, FontMetrics fontMetrics) throws BadLocationException {
    // Get the bounding rectangle of the row

    Rectangle r = component.modelToView(rowStartOffset);
    int lineHeight = fontMetrics.getHeight();
    int y = r.y + r.height;
    int descent = 0;

    // The text needs to be positioned above the bottom of the bounding
    // rectangle based on the descent of the font(s) contained on the row.

    if (r.height == lineHeight) // default font is being used
    {
      descent = fontMetrics.getDescent();
    } else // We need to check all the attributes for font changes
    {
      if (fonts == null) fonts = new HashMap<String, FontMetrics>();

      Element root = component.getDocument().getDefaultRootElement();
      int index = root.getElementIndex(rowStartOffset);
      Element line = root.getElement(index);

      for (int i = 0; i < line.getElementCount(); i++) {
        Element child = line.getElement(i);
        AttributeSet as = child.getAttributes();
        String fontFamily = (String) as.getAttribute(StyleConstants.FontFamily);
        Integer fontSize = (Integer) as.getAttribute(StyleConstants.FontSize);
        String key = fontFamily + fontSize;

        FontMetrics fm = fonts.get(key);

        if (fm == null) {
          Font font = new Font(fontFamily, Font.PLAIN, fontSize);
          fm = component.getFontMetrics(font);
          fonts.put(key, fm);
        }

        descent = Math.max(descent, fm.getDescent());
      }
    }

    return y - descent;
  }

  /**
   * Text caret position update event
   *
   * @param e Text caret position update event
   */
  @Override
  public void caretUpdate(CaretEvent e) {
    // Get the line the caret is positioned on

    int caretPosition = component.getCaretPosition();
    Element root = component.getDocument().getDefaultRootElement();
    int currentLine = root.getElementIndex(caretPosition);

    // Need to repaint so the correct line number can be highlighted

    if (lastLine != currentLine) {
      repaint();
      lastLine = currentLine;
    }
  }

  /**
   * Document change event
   *
   * @param e Document change event
   */
  @Override
  public void changedUpdate(DocumentEvent e) {
    documentChanged();
  }

  /**
   * Additional document event
   *
   * @param e Document change event
   */
  @Override
  public void insertUpdate(DocumentEvent e) {
    documentChanged();
  }

  /**
   * Document deletion event
   *
   * @param e Document change event
   */
  @Override
  public void removeUpdate(DocumentEvent e) {
    documentChanged();
  }

  /** Document change event */
  private void documentChanged() {
    // Preferred size of the component has not been updated at the time
    // the DocumentEvent is fired

    SwingUtilities.invokeLater(
        new Runnable() {
          @Override
          public void run() {
            int preferredHeight = component.getPreferredSize().height;

            // Document change has caused a change in the number of lines.
            // Repaint to reflect the new line numbers

            if (lastHeight != preferredHeight) {
              setPreferredWidth();
              repaint();
              lastHeight = preferredHeight;
            }
          }
        });
  }

  /**
   * Property change event
   *
   * @param evt Property change event
   */
  @Override
  public void propertyChange(PropertyChangeEvent evt) {
    if (evt.getNewValue() instanceof Font) {
      if (updateFont) {
        Font newFont = (Font) evt.getNewValue();
        setFont(newFont);
        lastDigits = 0;
        setPreferredWidth();
      } else {
        repaint();
      }
    }
  }

  /** Redraw. */
  public void update() {
    documentChanged();
  }

  /**
   * Set bar graph data
   *
   * @param list Bar graph data
   */
  public void setBargraphData(List<ISourceBargraph> list) {
    if (this.listData == null) {
      this.listData = new ArrayList<ISourceBargraph>();
    }
    this.listData.addAll(list);
  }

  /**
   * Add bar graph data
   *
   * @param value Bar graph data
   */
  public void addBargraphData(ISourceBargraph value) {
    if (this.listData == null) {
      this.listData = new ArrayList<ISourceBargraph>();
    }
    this.listData.add(value);
    return;
  }

  /**
   * Add bar graph data
   *
   * @param list Bar graph data
   */
  public void addBargraphData(List<ISourceBargraph> list) {
    if (this.listData == null) {
      this.listData = new ArrayList<ISourceBargraph>();
    }
    this.listData.addAll(list);
    return;
  }

  /** Clear the line data. */
  public void clearBargraphData() {
    if (this.listData == null) {
      this.listData = new ArrayList<ISourceBargraph>();
    }
    this.listData.clear();
  }

  /**
   * Get line data.
   *
   * @param rowStartOffset Row offset
   * @return line data
   */
  private ISourceBargraph getLineData(int rowStartOffset) {
    if (this.listData == null) return null;

    Element root = component.getDocument().getDefaultRootElement();
    int index = root.getElementIndex(rowStartOffset);
    Element line = root.getElement(index);

    if (line.getStartOffset() == rowStartOffset) {
      for (ISourceBargraph data : this.listData) {
        CodeLine code = data.getCodeLine();
        if (code.getStartLine() == index + 1) {
          return data;
        }
      }
    }
    return null;
  }
}
