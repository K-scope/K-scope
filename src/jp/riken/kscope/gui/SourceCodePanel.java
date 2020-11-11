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

import java.awt.BorderLayout;
import java.util.Observable;
import java.util.Observer;
import javax.swing.JPanel;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import jp.riken.kscope.common.KEYWORD_TYPE;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.Keyword;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.menu.SourcePanelPopupMenu;
import jp.riken.kscope.model.SourceCodeModel;
import jp.riken.kscope.properties.KeywordProperties;
import jp.riken.kscope.properties.ProfilerProperties;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.properties.VariableMemoryProperties;

/**
 * Source code display pine with ruler. <br>
 * Display the profiler preview panel on the right side.
 *
 * @author RIKEN
 */
public class SourceCodePanel extends JPanel
    implements ITabComponent, ChangeListener, CaretListener, Observer {

  /** Serial number */
  private static final long serialVersionUID = 1L;
  /** Source Code Panel */
  private ScrollCodePane panelCode;
  /** Profile Laura Panel */
  private ProfilerRulerPanel rulerProfiler;
  /** Parent component */
  private ITabComponent parentCompornent = null;

  /** Constructor */
  public SourceCodePanel() {
    super();
    initGUI();
  }

  /**
   * Initialize. <br>
   * Place the source code panel.
   */
  private void initGUI() {
    try {
      BorderLayout thisLayout = new BorderLayout();
      this.setLayout(thisLayout);

      // Source code display pine
      {
        this.panelCode = new ScrollCodePane();
        this.add(this.panelCode, BorderLayout.CENTER);
      }
      // Profile profiling panel
      {
        this.rulerProfiler = new ProfilerRulerPanel(this.panelCode);
        this.add(this.rulerProfiler, BorderLayout.EAST);
      }

    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * Source view model change notification event
   *
   * @param o Notification source
   * @param arg Notification item
   */
  @Override
  public void update(Observable o, Object arg) {
    this.panelCode.update(o, arg);
    this.rulerProfiler.update(o, arg);
  }

  /**
   * Caret position update event
   *
   * @param event Event information
   */
  @Override
  public void caretUpdate(CaretEvent event) {
    this.panelCode.caretUpdate(event);
  }

  /**
   * Scroll change event
   *
   * @param event Event information
   */
  @Override
  public void stateChanged(ChangeEvent event) {
    this.panelCode.stateChanged(event);
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
    if (this.panelCode != null) {
      this.panelCode.addTabFocusListener(listener);
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
   * Read the source file
   *
   * @param source source file
   * @throws Exception Source file read error
   */
  public void readFile(SourceFile source) throws Exception {
    this.panelCode.readFile(source);
    this.rulerProfiler.addObserver();
  }

  /**
   * Set the source file panel context menu
   *
   * @param menuSourcePanel Source File Panel Context Menu
   */
  public void setSourcePanelPopupMenu(SourcePanelPopupMenu menuSourcePanel) {
    this.panelCode.setSourcePanelPopupMenu(menuSourcePanel);
  }

  /**
   * Get text pine.
   *
   * @return text pine
   */
  public CodePane getSourcePane() {
    return this.panelCode.getSourcePane();
  }

  /**
   * Set source view properties
   *
   * @param properties Source view properties
   */
  public void setSourceProperties(SourceProperties properties) {
    this.panelCode.setSourceProperties(properties);
  }

  /**
   * Set keyword properties
   *
   * @param properties Keyword properties
   */
  public void setKeywordProperties(KeywordProperties properties) {
    this.panelCode.setKeywordProperties(properties);
  }

  /**
   * Get the source code model
   *
   * @return Source code model
   */
  public SourceCodeModel getModel() {
    return this.panelCode.getModel();
  }

  /**
   * Get the display source file path (absolute path).
   *
   * @return filePath Display source file path (absolute path)
   */
  public String getFilePath() {
    return this.panelCode.getFilePath();
  }

  /** Clear the selection of code line information. */
  public void clearSelectedBlock() {
    this.panelCode.clearSelectedBlock();
  }

  /**
   * Add a selection of code line information
   *
   * @param line Code line information
   */
  public void addSelectedBlock(CodeLine line) {
    this.panelCode.addSelectedBlock(line);
  }

  /**
   * Display the specified line number position in the display area.
   *
   * @param line Display line number
   */
  public void setLinePosition(CodeLine line) {
    this.panelCode.setLinePosition(line);
  }

  /**
   * Display the specified line number position in the display area.
   *
   * @param start Display line number
   */
  public void setLinePosition(int start) {
    this.panelCode.setLinePosition(start);
  }

  /**
   * Get selected line and selected character information
   *
   * @return Selected line information
   */
  public CodeLine getSelectedCodeLine() {
    return this.panelCode.getSelectedCodeLine();
  }

  /**
   * Get the selected row range
   *
   * @return Selection line code information
   */
  public CodeLine getSelectedArea() {
    return this.panelCode.getSelectedArea();
  }

  /**
   * Get the source file
   *
   * @return source file
   */
  public SourceFile getSelectedSourceFile() {
    return this.panelCode.getSelectedSourceFile();
  }

  /**
   * Set search / trace keywords
   *
   * @param keywords Search / trace keywords
   */
  public void setSearchWords(Keyword[] keywords) {
    this.panelCode.setSearchWords(keywords);
  }

  /** Clear search / trace keywords. */
  public void clearSearchWords() {
    this.panelCode.clearSearchWords();
  }

  /**
   * Clear search / trace keywords
   *
   * @param type Clear keyword type
   */
  public void clearSearchWords(KEYWORD_TYPE type) {
    this.panelCode.clearSearchWords(type);
  }

  /** Clear the bar graph data. */
  public void clearBargraphData() {
    this.panelCode.clearBargraphData();
    this.rulerProfiler.clearProfilerData();
  }

  /** Copy the currently selected text to the clipboard */
  public void copyClipboard() {
    this.panelCode.copyClipboard();
  }

  /**
   * Display profiler data display footer
   *
   * @param visible true = display
   */
  public void setVisibleBargraph(boolean visible) {
    this.panelCode.setVisibleBargraph(visible);
  }

  /**
   * Show profiler data display ruler
   *
   * @param visible true = display
   */
  public void setVisibleRuler(boolean visible) {
    this.rulerProfiler.setVisible(visible);
  }

  /**
   * Set profiler properties
   *
   * @param properties Profiler properties
   */
  public void setProfilerProperties(ProfilerProperties properties) {
    // Cost bar graph display switching
    boolean visibleBargraph = properties.isVisibleBargraph();
    // Switch the display and redraw.
    setVisibleBargraph(visibleBargraph);
    // Cost ruler display switching
    boolean visibleRuler = properties.isVisibleRuler();
    setVisibleRuler(visibleRuler);
    // Set profiler properties
    this.rulerProfiler.setProfilerProperties(properties);
  }

  /**
   * Set variable access destination memory property
   *
   * @param properties Variable access destination memory property
   */
  public void setVariableMemoryProperties(VariableMemoryProperties properties) {
    this.panelCode.setVariableMemoryProperties(properties);
  }

  /**
   * Add a selected line. <br>
   * The caret is not moved.
   *
   * @param line Code line information
   */
  public void addSelectedBlockNoCaret(CodeLine line) {
    this.panelCode.addSelectedBlockNoCaret(line);
  }

  /** Apply keyword highlights. */
  public void applyKeyword() {
    this.panelCode.applyKeyword();
  }
}
