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
import java.awt.Rectangle;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.List;
import java.util.Observable;
import java.util.Observer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;
import javax.swing.text.Position;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleContext;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.KEYWORD_TYPE;
import jp.riken.kscope.component.FrameScrollPane;
import jp.riken.kscope.data.BatchDocument;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.Keyword;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.data.VariableMemory;
import jp.riken.kscope.menu.SourcePanelPopupMenu;
import jp.riken.kscope.model.SourceCodeModel;
import jp.riken.kscope.properties.KeywordProperties;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.properties.VariableMemoryProperties;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Source code display pine. <br>
 * Provides a text pane that displays the source code. <br>
 *
 * @author RIKEN
 */
public class ScrollCodePane extends FrameScrollPane
    implements ITabComponent, ChangeListener, CaretListener, Observer {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Minimum number of columns in line number */
  private final int MINIMUMDISPLAYDIGITS = 5;
  /** Minimum number of digits in profiler cost data display area (= display width) */
  private final int PROFILER_BARGPRAPH_DIGITS = 7;

  /** Code display text pine */
  private CodePane sourcePane;

  /** Line number display header */
  private TextLineNumber lineHeader;
  /** Profiler display footer */
  private ProfilerLineInfo profilerFooter;

  /** Display source model */
  private SourceCodeModel model;

  /** Parent component */
  private ITabComponent parentCompornent = null;

  /** Tab size */
  private final int TAB_SIZE = 4;

  /**
   * Keyword properties. <br>
   * Apply keyword highlighting only to the area where the text pine is displayed. <br>
   * Save it for reapplying when scrolling.
   */
  private KeywordProperties propertiesKeyword;

  /** Selection code line information */
  private CodeLine selectedline;

  /** Current display start line number */
  private int currentStartLine;
  /** Current display end line number */
  private int currentEndLine;

  /** Constructor */
  public ScrollCodePane() {
    super();
    initGUI();
  }

  /**
   * Initialize. <br>
   * Place the source code tab.
   */
  private void initGUI() {
    try {
      sourcePane = new CodePane();
      // sourcePane.setUI(new LineHighlightTextPaneUI(sourcePane));

      // Make it read-only.
      sourcePane.setEditable(false);
      sourcePane.getCaret().setVisible(true); // Show the caret
      sourcePane.setParentComponent(this);

      // Set the tab size.
      SwingUtils.setTabSize(sourcePane, TAB_SIZE);

      this.setViewportView(sourcePane);

      // Line number display
      this.lineHeader = new TextLineNumber(sourcePane, MINIMUMDISPLAYDIGITS);
      this.lineHeader.setBackground(
          new jp.riken.kscope.properties.SourceProperties().getLineNumberColor());
      this.setRowHeaderView(this.lineHeader);

      // Profiler data display footer generation
      this.profilerFooter = new ProfilerLineInfo(sourcePane, PROFILER_BARGPRAPH_DIGITS);

      // Scroll event
      this.getViewport().addChangeListener(this);

      // CaretListener
      sourcePane.addCaretListener(this);

    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * Display profiler data display footer
   *
   * @param visible true = display
   */
  public void setVisibleBargraph(boolean visible) {
    if (visible) {
      this.setRowFooterView(this.profilerFooter);
    } else {
      this.setRowFooterView(null);
    }
  }

  /**
   * Add a string at the end.
   *
   * @param str Additional string
   * @throws BadLocationException String addition error
   */
  public void appendString(String str) throws BadLocationException {
    Document doc = this.sourcePane.getDocument();
    Position endPos = doc.getEndPosition();

    SimpleAttributeSet attr = new SimpleAttributeSet();
    doc.insertString(endPos.getOffset(), str, attr);

    // Set the tab size.
    SwingUtils.setTabSize(sourcePane, TAB_SIZE);
  }

  /** Clear the displayed document. */
  public void clearDocument() {
    StyleContext sc = new StyleContext();
    Document doc = new DefaultStyledDocument(sc);
    this.sourcePane.setDocument(doc);
  }

  /**
   * Get the display source file path (absolute path).
   *
   * @return filePath Display source file path (absolute path)
   */
  public String getFilePath() {
    if (this.model == null) return null;
    return this.model.getFilePath();
  }

  /**
   * Get text pine.
   *
   * @return text pine
   */
  public CodePane getSourcePane() {
    return this.sourcePane;
  }

  /**
   * Get the parent component.
   *
   * @return Parent component
   */
  @Override
  public ITabComponent getParentComponent() {
    return this.parentCompornent;
  }

  /**
   * Set the parent component.
   *
   * @param component Parent component
   */
  @Override
  public void setParentComponent(ITabComponent component) {
    this.parentCompornent = component;
  }

  /**
   * Set focus listener
   *
   * @param listener Focus listener
   */
  @Override
  public void addTabFocusListener(TabFocusListener listener) {
    this.addFocusListener(listener);
    if (this.sourcePane != null) {
      this.sourcePane.addTabFocusListener(listener);
    }
  }

  /** Close tab */
  @Override
  public void closeTabComponent() {
    // Close the tab with the parent tab pine.
    if (this.parentCompornent != null) {
      this.parentCompornent.closeTabComponent();
    }
  }

  /**
   * Set source view properties
   *
   * @param properties Source view properties
   */
  public void setSourceProperties(SourceProperties properties) {
    // Font
    sourcePane.setFont(properties.getFont());
    // Font color
    sourcePane.setForeground(properties.getFontColor());
    // Background color
    sourcePane.setBackground(properties.getBackgroundColor());
    // Selected line background color
    sourcePane.setActiveRowColor(properties.getActiverowColor());
    // Wrap position
    sourcePane.setWordwrap(properties.getWordwrap());
    if (this.model != null) {
      // Highlight range background color
      this.model.setColorHighlightArea(properties.getAreaColor());
      // Selection background color
      this.model.setColorSelectedBlock(properties.getBlockColor());

      // Update background color settings
      this.setLinesBackground();

      // Character color of search string
      this.model.setColorSearchFont(properties.getSearchFontColor());
      // Background color of search string
      this.model.setColorSearchBackground(properties.getSearchBackgroundColor());
    }

    // Redraw the line number header.
    this.lineHeader.setBackground(properties.getLineNumberColor());
    this.lineHeader.update();

    // Apply keyword properties, search / trace keywords.
    applyKeyword();
  }

  /**
   * Set keyword properties
   *
   * @param properties Keyword properties
   */
  public void setKeywordProperties(KeywordProperties properties) {
    this.propertiesKeyword = properties;

    // Apply keyword properties, search / trace keywords.
    applyKeyword();
  }

  /**
   * Set search / trace keywords
   *
   * @param keywords Search / trace keywords
   */
  public void setSearchWords(Keyword[] keywords) {
    if (this.model == null) return;
    if (keywords == null) return;

    // Determine if the keyword applies to this source
    List<Keyword> list = new ArrayList<Keyword>();
    for (Keyword word : keywords) {
      CodeLine searchline = word.getSearchLine();
      if (searchline == null || searchline.getSourceFile() == null) {
        list.add(word);
      } else {
        SourceFile searchFile = searchline.getSourceFile();
        SourceFile srcFile = this.model.getSourceFile();
        if (srcFile.equals(searchFile)) {
          list.add(word);
        }
      }
    }
    this.model.setSearchWords(list);

    // Apply keyword properties, search / trace keywords.
    applyKeyword();
  }

  /** Clear search / trace keywords. */
  public void clearSearchWords() {
    if (this.model == null) return;
    this.model.clearSearchWords();

    // Apply keyword properties, search / trace keywords.
    applyKeyword();
  }

  /** Apply keyword properties, search / trace keywords. */
  public void applyKeyword() {
    if (this.model == null) return;

    if (!(sourcePane.getDocument() instanceof BatchDocument)) {
      return;
    }
    BatchDocument doc = (BatchDocument) sourcePane.getDocument();

    // Get display start and end line numbers from display start and end caret positions
    int startline = this.sourcePane.getViewStartLine();
    int endline = this.sourcePane.getViewEndLine();
    if (startline <= 0 || endline <= 0) return;

    // Clear keyword highlights
    doc.clearKeywordAttributes(startline, endline);

    // Highlight setting after reservation of display position
    doc.applyHighlighting(this.propertiesKeyword, startline, endline);

    int startOffset = this.sourcePane.getLineStartOffset(startline);
    int endOffset = this.sourcePane.getLineEndOffset(endline);
    // Search / trace keyword highlighting
    List<Keyword> list = this.model.getSearchWords();
    if (list != null) {
      for (Keyword word : list) {
        try {
          // Search character code line information
          CodeLine searchline = word.getSearchLine();
          if (searchline == null) {
            // Apply highlight settings
            doc.applyKeyword(word, startOffset, endOffset);
          } else {
            // Check if the applicable line is included
            int startLineOffset = this.sourcePane.getLineStartOffset(searchline.getStartLine());
            int endLineOffset = this.sourcePane.getLineEndOffset(searchline.getEndLine());
            if (endOffset < startLineOffset) continue;
            if (endLineOffset < startOffset) continue;

            // Apply highlight settings
            doc.applyKeyword(word, startLineOffset, endLineOffset);
          }
        } catch (Exception e) {
          e.printStackTrace();
        }
      }
    }

    // Variable access destination memory keyword highlight setting
    List<VariableMemory> listVar = this.model.getVariableMemories();
    if (listVar != null) {
      for (Keyword word : listVar) {
        try {
          CodeLine searchline = word.getSearchLine();
          if (searchline == null) continue;
          // Check if the applicable line is included
          int startLineOffset = this.sourcePane.getLineStartOffset(searchline.getStartLine());
          int endLineOffset = this.sourcePane.getLineEndOffset(searchline.getEndLine());
          if (endOffset < startLineOffset) continue;
          if (endLineOffset < startOffset) continue;

          // Apply highlight settings
          doc.applyKeyword(word, startLineOffset, endLineOffset);
        } catch (Exception e) {
          e.printStackTrace();
        }
      }
    }
  }

  /**
   * Scroll change event
   *
   * @param event Event information
   */
  @Override
  public void stateChanged(ChangeEvent event) {

    // Display start line number
    int startLine = this.sourcePane.getViewStartLine();
    // Current display end line number
    int endLine = this.sourcePane.getViewEndLine();

    // If there is no change in the display row due to the change in the view, it will not be
    // applied.
    // Measures to mitigate applied calls due to frequent events
    if (this.currentStartLine == startLine && this.currentEndLine == endLine) {
      return;
    }

    // Apply keyword properties, search / trace keywords.
    applyKeyword();

    this.currentStartLine = startLine;
    this.currentEndLine = endLine;
  }

  /**
   * Read the source file
   *
   * @param source source file
   * @throws Exception Source file read error
   */
  public void readFile(SourceFile source) throws Exception {
    this.model = new SourceCodeModel(source);
    this.model.readFile();

    sourcePane.setDocument(this.model.getDocument());

    // Set the tab size.
    SwingUtils.setTabSize(sourcePane, TAB_SIZE);

    // Caret position first
    this.sourcePane.setCaretPosition(0);
    this.sourcePane.getCaret().setVisible(true); // Show the caret

    // Selected row information
    SourceFile f = model.getSourceFile();
    String fn = null;
    if (f != null) {
      fn = f.getPath();
    }
    this.selectedline = new CodeLine(f, null, 1, fn);

    // Set the observer.
    model.addObserver(this);
  }

  /**
   * Caret position update event
   *
   * @param event Event information
   */
  @Override
  public void caretUpdate(CaretEvent event) {
    if (this.model == null) return;
    // Selected row information
    this.selectedline = null;

    if (sourcePane.getDocument() == null) return;
    if (sourcePane.getDocument().getLength() <= 0) {
      Application.status.setMessageLocation(1, 1, null);
      return;
    }

    // Caret position
    int dot = event.getDot();
    // Returns the position on the opposite side of the logical selection. Same as dot if there is
    // no selection
    int mark = event.getMark();

    // Line number of caret position
    // int row = SwingUtils.getRow((JTextComponent)event.getSource(), event.getDot());
    int row = sourcePane.getRow(event.getDot());
    // Column number of caret position
    int col = SwingUtils.getColumn((JTextComponent) event.getSource(), event.getDot());

    // Clear selected string
    String selectword = null;
    try {
      BatchDocument document = (BatchDocument) sourcePane.getDocument();

      // Number of selected strings
      int len = dot < mark ? mark - dot : dot - mark;

      if (len > 0) {
        // Selected string
        selectword = document.getText(dot < mark ? dot : mark, len);
        if (selectword != null) {
          selectword = selectword.trim();
          // In case of selection, up to line feed position
          int crpos = selectword.indexOf('\n');
          if (crpos > 0) {
            selectword = selectword.substring(0, crpos);
          }
        }
      } else if (row > 0 && col > 0) {
        selectword = document.getCaretWord(row - 1, col - 1);
        //                System.out.println(document.getCaretWord(dot));
      }
      if (selectword != null) selectword = selectword.trim();

      // Check if the selected string is recognizable as a variable.
      if (!isVariableWord(selectword)) {
        // Not a variable name.
        selectword = null;
      }

      // Selected row information
      SourceFile f = model.getSourceFile();
      String fn = null;
      if (f != null) {
        fn = f.getPath();
      }
      selectedline = new CodeLine(f, selectword, row, fn);
      // Show in status bar
      Application.status.setMessageLocation(row, col, selectword);

    } catch (BadLocationException ble) {
      System.err.println(
          Message.getString("scrollcodepane.errout.processfaild")); // Failed to read the document.
    }
  }

  /**
   * Check if it can be recognized as a variable string. <br>
   * Check if the string contains the letters a-z.
   *
   * @param word variable string
   * @return true = Can be recognized as a variable string
   */
  private boolean isVariableWord(String word) {
    if (word == null) return false;

    String regex = Message.getString("scrollcodepane.variableword"); // [a-zA-Z]
    Pattern p = Pattern.compile(regex);
    Matcher m = p.matcher(word);

    return m.find();
  }

  /**
   * Add a selection of code line information
   *
   * @param lines Code line information
   */
  public void setSelectedBlock(CodeLine[] lines) {
    if (this.model == null) return;
    if (lines == null || lines.length <= 0) {
      clearSelectedBlock();
      return;
    }

    // Check if the source file information matches
    List<CodeLine> list = new ArrayList<CodeLine>();
    for (int i = 0; i < lines.length; i++) {
      if (lines[i].getSourceFile() != null) {
        if (lines[i].getSourceFile().equals(this.model.getSourceFile())) {
          // Source file match
          list.add(lines[i]);
        }
      } else {
        // Since there is no source file information, add it as it is
        list.add(lines[i]);
      }
    }

    // Add a selection of code line information
    this.model.setSelectedBlock(list);

    // Update background color settings
    this.setLinesBackground();
  }

  /**
   * Add a selection of code line information
   *
   * @param line Code line information
   */
  public void addSelectedBlock(CodeLine line) {
    if (line == null) return;
    if (this.model == null) return;

    // Check if the source file information matches
    // If there is no source file information, add it as it is
    //        if (line.getSourceFile() != null) {
    //             if (!line.getSourceFile().equals(this.model.getSourceFile())) {
    // // Source file mismatch
    //                 return;
    //             }
    //        }

    // Add a selection of code line information
    this.model.addSelectedBlock(line);

    // Update background color settings
    this.setLinesBackground();

    setLinePosition(line);
  }

  /** Clear the selection of code line information. */
  public void clearSelectedBlock() {
    if (this.model == null) return;
    this.model.clearSelectedBlock();

    // Update background color settings
    this.setLinesBackground();

    this.repaint();
    this.updateUI();
  }

  /** Set line highlight information in the source code pine */
  private void setLinesBackground() {
    if (this.model == null) return;
    // Clear line highlight settings
    this.sourcePane.clearListLinesColor();

    // Highlight range
    if (this.model.getColorHighlightArea() != null && this.model.getHighlightArea() != null) {
      List<CodeLine> lines = this.model.getHighlightArea();
      Color background = this.model.getColorHighlightArea();
      for (CodeLine line : lines) {
        this.sourcePane.addLinesBackground(line.getStartLine(), line.getEndLine(), background);
      }
    }
    // Select block
    if (this.model.getColorSelectedBlock() != null && this.model.getSelectedBlock() != null) {
      List<CodeLine> lines = this.model.getSelectedBlock();
      Color background = this.model.getColorSelectedBlock();
      for (CodeLine line : lines) {
        this.sourcePane.addLinesBackground(line.getStartLine(), line.getEndLine(), background);
      }
    }

    return;
  }

  /**
   * Display the specified line number position in the display area.
   *
   * @param line Display line number
   */
  public void setLinePosition(CodeLine line) {
    if (line == null) return;

    // Start line number
    int start = line.getStartLine();
    // Display the specified line number position in the display area.
    setLinePosition(start);
  }

  /**
   * Display the specified line number position in the display area.
   *
   * @param line Display line number
   */
  public void setLinePosition(int start) {

    if (start <= 0) start = 1;

    // Leave two lines at the top
    int viewLine = start - 2;
    if (viewLine <= 0) viewLine = 1;

    // Get the caret index of the line number
    int pos = this.sourcePane.getLineStartOffset(start);
    this.sourcePane.setCaretPosition(pos);

    try {
      Document doc = this.sourcePane.getDocument();
      Element root = doc.getDefaultRootElement();
      Element elem = root.getElement(viewLine - 1);
      if (elem == null) return;
      Rectangle rect = this.sourcePane.modelToView(elem.getStartOffset());
      if (rect == null) return;
      Rectangle viewRect = this.getViewport().getViewRect();
      rect.setSize(10, viewRect.height);
      this.sourcePane.scrollRectToVisible(rect);
    } catch (BadLocationException ble) {
      java.awt.Toolkit.getDefaultToolkit().beep();
    }
  }

  /**
   * Get the source code model
   *
   * @return Source code model
   */
  public SourceCodeModel getModel() {
    return this.model;
  }

  /**
   * Set the source file panel context menu
   *
   * @param menuSourcePanel Source File Panel Context Menu
   */
  public void setSourcePanelPopupMenu(SourcePanelPopupMenu menuSourcePanel) {
    this.sourcePane.setComponentPopupMenu(menuSourcePanel);

    // Added for reverse lookup function to Filtered-AST (2014/4/8 ohichi)
    this.sourcePane.addMouseListener((MouseListener) menuSourcePanel.getAction());
  }

  /**
   * Get selected line and selected character information
   *
   * @return Selected line information
   */
  public CodeLine getSelectedCodeLine() {
    // Get the selected string
    String selectText = sourcePane.getSelectedText();
    if (selectedline == null) return null;

    // Set the actually selected string
    selectedline.setStatement(selectText);
    return selectedline;
  }

  /**
   * Get the selected row range
   *
   * @return Selection line code information
   */
  public CodeLine getSelectedArea() {
    // Get the selected string
    String selectText = sourcePane.getSelectedText();
    int startpos = sourcePane.getSelectionStart();
    int endpos = sourcePane.getSelectionEnd();
    int startrow = sourcePane.getRow(startpos);
    int endrow = sourcePane.getRow(endpos);
    SourceFile f = model.getSourceFile();
    String fn = null;
    if (f != null) {
      fn = f.getPath();
    }
    CodeLine code = new CodeLine(f, selectText, startrow, endrow, fn);
    return code;
  }

  /**
   * Get the source file
   *
   * @return source file
   */
  public SourceFile getSelectedSourceFile() {
    if (this.model == null) return null;
    SourceFile file = this.model.getSourceFile();
    return file;
  }

  /** Copy to clipboard */
  public void copyClipboard() {
    this.sourcePane.copy();
  }

  /**
   * Clear search / trace keywords
   *
   * @param type Clear keyword type
   */
  public void clearSearchWords(KEYWORD_TYPE type) {
    if (this.model == null) return;
    this.model.clearSearchWords(type);

    // Apply keyword properties, search / trace keywords.
    applyKeyword();
  }

  /**
   * Set bar graph data
   *
   * @param bardata Bar graph data
   */
  public void setBargraphData(List<ISourceBargraph> bardata) {
    this.profilerFooter.setBargraphData(bardata);
  }

  /** Clear the bar graph data. */
  public void clearBargraphData() {
    this.profilerFooter.clearBargraphData();

    this.repaint();
    this.updateUI();
  }

  /**
   * Source view model change notification event
   *
   * @param o Notification source
   * @param arg Notification item
   */
  @Override
  public void update(Observable o, Object arg) {
    clearBargraphData();
    List<ISourceBargraph> list = this.model.getListBarData();
    if (list == null) {
      return;
    }

    // Set bar graph data
    setBargraphData(list);

    this.repaint();
    this.updateUI();
  }

  /**
   * Set variable access destination memory property
   *
   * @param properties Variable access destination memory property
   */
  public void setVariableMemoryProperties(VariableMemoryProperties properties) {

    if (this.model == null) return;
    if (properties == null) return;

    // Determine if variable access memory applies to this source
    List<VariableMemory> varmems = properties.getListVariableMemory();
    List<VariableMemory> list = new ArrayList<VariableMemory>();
    for (VariableMemory var : varmems) {
      CodeLine varline = var.getSearchLine();
      if (varline == null || varline.getSourceFile() == null) {
        continue;
      }
      SourceFile searchFile = varline.getSourceFile();
      SourceFile srcFile = this.model.getSourceFile();
      if (srcFile.equals(searchFile)) {
        list.add(var);
      }
    }
    this.model.setVariableMemories(list);

    // Apply keyword properties, search / trace keywords.
    applyKeyword();
  }

  public void addSelectedBlockNoCaret(CodeLine line) {

    if (line == null) return;
    if (this.model == null) return;
    // Add a selection of code line information
    this.model.addSelectedBlock(line);

    // Update background color settings
    this.setLinesBackground();

    clearSelectionPos();
  }

  /** Clear the selection. */
  public void clearSelectionPos() {
    int start = this.sourcePane.getSelectionStart();
    int end = this.sourcePane.getSelectionEnd();
    this.sourcePane.setSelectionEnd(start);
  }
}
