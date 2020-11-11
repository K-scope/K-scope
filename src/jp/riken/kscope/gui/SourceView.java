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

import java.awt.Component;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import javax.swing.JOptionPane;
import javax.swing.event.ChangeEvent;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.common.KEYWORD_TYPE;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.Keyword;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.menu.SourcePanelPopupMenu;
import jp.riken.kscope.model.SourceCodeModel;
import jp.riken.kscope.properties.KeywordProperties;
import jp.riken.kscope.properties.ProfilerProperties;
import jp.riken.kscope.properties.RequiredBFProperties;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.properties.VariableMemoryProperties;
import jp.riken.kscope.utils.FileUtils;

/**
 * Source code view class. <br>
 * Place the source code tab.
 *
 * @author RIKEN
 */
public class SourceView extends ClosableTabbedPane implements PropertyChangeListener {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Source File Panel Context Menu */
  private SourcePanelPopupMenu menuSourcePanel;

  /** Project folder */
  private File projectFolder;

  /** Constructor */
  public SourceView() {
    super(FRAME_VIEW.SOURCE_VIEW);
    initGUI();
  }

  /**
   * Initialize. <br>
   * Place the source code tab.
   */
  private void initGUI() {
    try {
      // (2012/4/17) changed by tomiyama and teraim
      // SourceCodePanel scrollPane = new SourceCodePanel();
      // this.addTab("blank:", scrollPane);

    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * Display the source file in a tab.
   *
   * @param filename File name
   * @throws Exception Source file read error
   */
  public void viewSource(String filename) throws Exception {
    File file = new File(filename);
    if (file.exists()) {
      if (file.isFile()) {
        viewSource(file);
      } else {
        String message =
            filename + " " + Message.getString("sourceview.errdialog.notfile"); // is not a file.
        JOptionPane.showMessageDialog(
            this.getParent(),
            message,
            Message.getString("dialog.common.error"), // error
            JOptionPane.ERROR_MESSAGE);
        throw new Exception(message);
      }
    } else {
      String message =
          filename
              + " "
              + Message.getString("sourceview.errdialog.filenotexist"); // does not exist.
      JOptionPane.showMessageDialog(
          this.getParent(),
          message,
          Message.getString("dialog.common.error"), // error
          JOptionPane.ERROR_MESSAGE);
      throw new Exception(message);
    }
  }

  /**
   * Display the source file in a tab.
   *
   * @param file file
   * @throws Exception Source file read error
   */
  public void viewSource(File file) throws Exception {
    if (!file.exists()) {
      String message =
          file.getPath()
              + " "
              + Message.getString("sourceview.errdialog.filenotexist"); // does not exist.
      JOptionPane.showMessageDialog(
          this.getParent(),
          message,
          Message.getString("dialog.common.error"), // error
          JOptionPane.ERROR_MESSAGE);
      throw new Exception(message);
    }
    if (!file.isFile()) {
      String messgage =
          file.getPath()
              + " "
              + Message.getString("sourceview.errdialog.notfile"); // is not a file.
      JOptionPane.showMessageDialog(
          this,
          messgage,
          Message.getString("dialog.common.error"), // error
          JOptionPane.ERROR_MESSAGE);
      throw new Exception(messgage);
    }
    viewSource(new SourceFile(file));
  }

  /**
   * Display the source file in a tab.
   *
   * @param source source file
   * @throws Exception Source file read error
   */
  public void viewSource(SourceFile source) throws Exception {
    CodeLine line = new CodeLine(source, source.getPath());
    viewSource(line);

    return;
  }

  /**
   * Display the source file in a tab.
   *
   * @param line Code line information
   * @throws Exception Source file acquisition error
   */
  public void viewSource(CodeLine line) throws Exception {

    if (line.getSourceFile() == null || line.getSourceFile().getFile() == null) {
      JOptionPane.showMessageDialog(
          this.getParent(),
          Message.getString("sourceview.errdialog.notset", line.getStatement()),
          Message.getString("dialog.common.error"), // error
          JOptionPane.ERROR_MESSAGE);
      throw new Exception(
          Message.getString(
              "sourceview.exception.missing")); // The source file could not be obtained.
    }

    SourceFile source = line.getSourceFile();
    File file = source.getFile();
    if (!file.isAbsolute()) {
      // Relative path to the project folder
      file = new File(this.projectFolder.getAbsolutePath() + File.separator + file.getPath());
    }
    if (!file.isFile()) {
      JOptionPane.showMessageDialog(
          this.getParent(),
          line.getStrSourceFile()
              + " "
              + Message.getString("sourceview.errdialog.notfile"), // is not a file.
          Message.getString("dialog.common.error"), // error
          JOptionPane.ERROR_MESSAGE);
      throw new Exception(
          file.getPath() + Message.getString("sourceview.errdialog.notfile")); // is not a file.
    }
    if (!file.exists()) {
      JOptionPane.showMessageDialog(
          this.getParent(),
          line.getStrSourceFile()
              + " "
              + Message.getString("sourceview.errdialog.filenotexist"), // does not exist.
          Message.getString("dialog.common.error"), // error
          JOptionPane.ERROR_MESSAGE);
      throw new Exception(
          file.getPath()
              + Message.getString("sourceview.errdialog.filenotexist")); // does not exist.
    }
    SourceFile readSource = new SourceFile(file);

    SourceCodePanel viewtab = null;

    // Check if the display file has already been displayed.
    int index = getSourceTabIndex(file);
    if (index != -1) {
      // Since it has been displayed, activate it.
      this.setSelectedIndex(index);
    } else {
      // Since it is hidden, search for blank tabs
      int newindex = -1;
      int count = this.getTabCount();
      for (int i = 0; i < count; i++) {
        if (isBlankTab(i)) {
          viewtab = getTab(i);
          newindex = i;
          break;
        }
      }

      // Create a new tab
      if (viewtab == null) {
        viewtab = new SourceCodePanel();
        this.addTab("blank:", viewtab);
        newindex = getTabCount() - 1;
      }

      // Read file
      viewtab.readFile(readSource);

      // Change tab title
      String name = file.getName();
      setTabTitle(newindex, name);

      // Activate the opened tab
      this.setSelectedIndex(newindex);

      // Set the source file panel context menu
      viewtab.setSourcePanelPopupMenu(menuSourcePanel);
    }

    // Activate the display source file in the source file tree
    fireChangedScrollCodePane();

    return;
  }

  /**
   * Get the displayed tab index of the specified file. <br>
   * Returns -1 if it does not exist.
   *
   * @param file file
   * @return tab index (if it doesn't exist = -1)
   */
  private int getSourceTabIndex(File file) {
    if (file == null) return -1;

    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      Component tab = this.getComponentAt(i);
      if (!(tab instanceof SourceCodePanel)) continue;
      if (((SourceCodePanel) tab).getModel() == null) continue;
      SourceFile tabsource = ((SourceCodePanel) tab).getModel().getSourceFile();
      File tabfile = tabsource.getFile();
      if (!tabfile.isAbsolute()) {
        // Relative path to the project folder
        tabfile =
            new File(this.projectFolder.getAbsolutePath() + File.separator + tabfile.getPath());
      }
      File destfile = file;
      if (!destfile.isAbsolute()) {
        // Relative path to the project folder
        destfile =
            new File(this.projectFolder.getAbsolutePath() + File.separator + destfile.getPath());
      }

      // Check if the same file is displayed.
      if (FileUtils.isEqualsFile(destfile, tabfile)) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Determine if the tab in the tab index is a blank (no file display) tab.
   *
   * @param index Tab index
   * @return true = blank (no file display) tab
   */
  private boolean isBlankTab(int index) {

    Component tab = this.getComponentAt(index);
    if (!(tab instanceof SourceCodePanel)) return false;
    String tabpath = ((SourceCodePanel) tab).getFilePath();

    if (tabpath == null || tabpath.isEmpty()) {
      // Blank (no file display) tab
      return true;
    }
    return false;
  }

  /**
   * Get the source pine of the tab index.
   *
   * @param index Tab index
   * @return true = blank (no file display) tab
   */
  private SourceCodePanel getTab(int index) {

    // (2012/5/25) added by Tomiyama
    if (index < 0) return null;
    Component tab = this.getComponentAt(index);
    if (!(tab instanceof SourceCodePanel)) return null;

    return (SourceCodePanel) tab;
  }

  /**
   * Get the currently selected text pine.
   *
   * @return Select text pine
   */
  public CodePane getSelectedCodePane() {
    int index = this.getSelectedIndex();
    SourceCodePanel pane = getTab(index);
    return pane.getSourcePane();
  }

  /** Close active tab */
  @Override
  public void closeTabComponent() {
    int index = this.getSelectedIndex();
    // close tab
    closeTab(index);
  }

  /**
   * Close tab
   *
   * @param index Close tab index
   */
  @Override
  protected void closeTab(int index) {
    if (index >= 0) {
      this.remove(index);
    }

    // (2012/4/18) changed by tomiyama and teraim
    /*
    int count = this.getTabCount ();
    if (count <= 0) {
        // Create a blank tab.
        SourceCodePanel scrollPane = new SourceCodePanel ();
        this.addTab ("blank:", scrollPane);
    }
    */

  }

  /** Clear all tabs */
  public void closeAllTabs() {
    this.removeAll();

    // (2012/4/18) changed by tomiyama and teraim
    // Create a blank tab.
    /*
    SourceCodePanel scrollPane = new SourceCodePanel();
    this.addTab("blank:", scrollPane);
    */
  }

  /**
   * Property change event
   *
   * @param event Event information
   */
  @Override
  public void propertyChange(PropertyChangeEvent event) {
    //        System.out.println(event.getPropertyName());

    // Change source view properties such as source display font, font color, etc.
    if (event.getNewValue() instanceof SourceProperties) {
      SourceProperties properties = (SourceProperties) event.getNewValue();

      int count = this.getTabCount();
      for (int i = 0; i < count; i++) {
        SourceCodePanel pane = getTab(i);
        pane.setSourceProperties(properties);
      }
    }
    // Change keyword properties
    else if (event.getNewValue() instanceof KeywordProperties) {
      KeywordProperties properties = (KeywordProperties) event.getNewValue();

      int count = this.getTabCount();
      for (int i = 0; i < count; i++) {
        SourceCodePanel pane = getTab(i);
        pane.setKeywordProperties(properties);
      }
    }
    // Change profiler properties
    else if (event.getNewValue() instanceof ProfilerProperties) {
      ProfilerProperties properties = (ProfilerProperties) event.getNewValue();
      int count = this.getTabCount();
      for (int i = 0; i < count; i++) {
        SourceCodePanel pane = getTab(i);
        pane.setProfilerProperties(properties);
      }
    }
    // Change variable access destination memory property
    else if (event.getNewValue() instanceof VariableMemoryProperties) {
      VariableMemoryProperties properties = (VariableMemoryProperties) event.getNewValue();
      int count = this.getTabCount();
      for (int i = 0; i < count; i++) {
        SourceCodePanel pane = getTab(i);
        pane.setVariableMemoryProperties(properties);
      }
    }
    // Change request B / F settings
    else if (event.getNewValue() instanceof RequiredBFProperties) {
      int count = this.getTabCount();
      for (int i = 0; i < count; i++) {
        SourceCodePanel pane = getTab(i);
        // Only redraw
        pane.applyKeyword();
      }
    }
  }

  /**
   * Close the tab where the specified file is displayed. <br>
   *
   * @param filename File name
   */
  public void closeSourceFile(String filename) {
    if (filename == null || filename.isEmpty()) return;

    int index = getSourceTabIndex(new File(filename));
    if (index < 0) return;

    // close tab
    closeTab(index);
  }

  /** Clear the selected block */
  public void clearSelectedBlock() {
    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      SourceCodePanel pane = getTab(i);
      pane.clearSelectedBlock();
    }
  }

  /**
   * Set the selected source line block
   *
   * @param line Selected source line block
   */
  public void setSelectedBlock(CodeLine[] line) {

    if (line == null || line.length <= 0) return;

    // Clear selection
    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      SourceCodePanel pane = getTab(i);
      // Clear selection
      pane.clearSelectedBlock();
    }

    // Setting the selection range
    for (int i = 0; i < line.length; i++) {
      if (line[i] == null) continue;
      if (line[i].getStartLine() <= 0) continue;
      if (line[i].getEndLine() <= 0) continue;
      if (line[i].getSourceFile() == null) continue;
      if (line[i].getSourceFile().getFile() == null) continue;

      // Set a selection block in the text panel that matches the source code
      SourceFile source = line[i].getSourceFile();
      File file = source.getFile();
      if (!file.isAbsolute()) {
        // Relative path to the project folder
        file = new File(this.projectFolder.getAbsolutePath() + File.separator + file.getPath());
      }
      // Find the source panel that matches the source file
      int index = getSourceTabIndex(file);
      if (index == -1) continue; // No matching source panel
      SourceCodePanel pane = getTab(index);

      // Selected source line block
      pane.addSelectedBlock(line[i]);
    }
  }

  /**
   * Set the selected source line range. Do not change the caret position
   *
   * @param line Selected source line range
   */
  public void setSelectedBlockNoCaret(CodeLine line) {

    if (line == null) return;

    // Clear selection
    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      SourceCodePanel pane = getTab(i);
      // Clear selection
      pane.clearSelectedBlock();
    }

    // Setting the selection range
    if (line.getStartLine() <= 0) return;
    if (line.getEndLine() <= 0) return;
    if (line.getSourceFile() == null) return;
    if (line.getSourceFile().getFile() == null) return;

    // Set a selection block in the text panel that matches the source code
    SourceFile source = line.getSourceFile();
    File file = source.getFile();
    if (!file.isAbsolute()) {
      // Relative path to the project folder
      file = new File(this.projectFolder.getAbsolutePath() + File.separator + file.getPath());
    }
    // Find the source panel that matches the source file
    int index = getSourceTabIndex(file);
    if (index == -1) return; // No matching source panel
    SourceCodePanel pane = getTab(index);

    // Selected source line block
    pane.addSelectedBlockNoCaret(line);
  }

  /**
   * Set the specified source line. <br>
   * Activate one line with the specified line number.
   *
   * @param line Source line
   */
  public void setSelectedLine(CodeLine line) {
    if (line == null) return;

    // Setting the selection range
    if (line.getStartLine() <= 0) return;
    if (line.getEndLine() <= 0) return;
    if (line.getSourceFile() == null) return;
    if (line.getSourceFile().getFile() == null) return;

    // Set a selection block in the text panel that matches the source code
    SourceFile source = line.getSourceFile();

    // Find the source panel that matches the source file
    int index = getSourceTabIndex(source.getFile());
    if (index == -1) return; // No matching source panel
    SourceCodePanel pane = getTab(index);
    if (pane == null) return;

    // Selected source line block
    pane.setLinePosition(line);
  }

  /**
   * Get a list of open files.
   *
   * @return List of open files
   */
  public SourceFile[] getOpenedSourceFile() {

    List<SourceFile> list = new ArrayList<SourceFile>();
    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      Component tab = this.getComponentAt(i);
      if (!(tab instanceof SourceCodePanel)) continue;
      SourceCodeModel model = ((SourceCodePanel) tab).getModel();
      if (model == null) continue;
      SourceFile file = model.getSourceFile();
      if (file == null) continue;
      list.add(file);
    }
    if (list.size() <= 0) return null;

    return list.toArray(new SourceFile[0]);
  }

  /**
   * Set the source file panel context menu
   *
   * @param menuSourcePanel Source File Panel Context Menu
   */
  public void setSourcePanelPopupMenu(SourcePanelPopupMenu menuSourcePanel) {
    this.menuSourcePanel = menuSourcePanel;
  }

  /**
   * Get selected source code line information
   *
   * @return Selected source code line information
   */
  public CodeLine getSelectedCodeLine() {
    int index = this.getSelectedIndex();
    SourceCodePanel pane = getTab(index);
    if (pane == null) {
      return null;
    }
    return pane.getSelectedCodeLine();
  }

  /**
   * Get the selected row range
   *
   * @return Selection line code information
   */
  public CodeLine getSelectedArea() {
    int index = this.getSelectedIndex();
    SourceCodePanel pane = getTab(index);
    if (pane == null) return null;
    return pane.getSelectedArea();
  }

  /**
   * Get the source file
   *
   * @return source file
   */
  public SourceFile getSelectedSourceFile() {
    int index = this.getSelectedIndex();
    SourceCodePanel pane = getTab(index);
    return pane.getSelectedSourceFile();
  }

  /**
   * Set search / trace keywords
   *
   * @param keywords Search / trace keywords
   */
  public void setSearchWords(Keyword[] keywords) {
    if (keywords == null) {
      // Clear search / trace keywords
      clearSearchWords();
      return;
    }

    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      SourceCodePanel pane = getTab(i);
      if (pane == null) continue;
      pane.setSearchWords(keywords);
    }
  }

  /** Clear search / trace keywords */
  public void clearSearchWords() {
    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      SourceCodePanel pane = getTab(i);
      if (pane == null) continue;
      pane.clearSearchWords();
    }
  }

  /**
   * Clear search / trace keywords
   *
   * @param type Clear keyword type
   */
  public void clearSearchWords(KEYWORD_TYPE type) {
    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      SourceCodePanel pane = getTab(i);
      if (pane == null) continue;
      pane.clearSearchWords(type);
    }
  }

  /** Clear the profile bar graph */
  public void clearBargraphData() {
    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      SourceCodePanel pane = getTab(i);
      if (pane == null) continue;
      pane.clearBargraphData();
    }
  }

  /** Copy the currently selected text to the clipboard */
  public void copyClipboard() {
    int index = this.getSelectedIndex();
    SourceCodePanel pane = getTab(index);
    if (pane == null) return;
    pane.copyClipboard();
  }

  /**
   * Set the project folder
   *
   * @param folder Project folder
   */
  public void setProjectFolder(File folder) {
    this.projectFolder = folder;
  }

  /**
   * Active tab change event
   *
   * @param event Event occurrence source
   */
  @Override
  public void stateChanged(ChangeEvent event) {
    super.stateChanged(event);

    // Activate the display source file in the source file tree
    fireChangedScrollCodePane();
  }

  /** Synchronize with the tree by changing the active source file. */
  private void fireChangedScrollCodePane() {

    // Display source file
    int index = this.getSelectedIndex();
    if (index < 0) return;
    SourceCodePanel pane = getTab(index);
    if (pane.getModel() == null) return;
    SourceFile file = pane.getModel().getSourceFile();
    if (file == null) return;

    // Activate the display source file in the source file tree
    MainFrame frame = (MainFrame) this.getParentComponent();
    if (frame == null) return;
    if (frame.getPanelExplorerView() == null) return;
    frame.getPanelExplorerView().setSelectedNode(file);
  }

  /**
   * Switch the display of the profile display footer
   *
   * @param visible true = Profile display Footer display
   */
  public void setVisibleBargraph(boolean visible) {
    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      Component tab = this.getComponentAt(i);
      if (tab instanceof SourceCodePanel) {
        ((SourceCodePanel) tab).setVisibleBargraph(visible);
      }
    }
  }

  /**
   * Switch the display of the profile display ruler
   *
   * @param visible true = Profile display Ruler display
   */
  public void setVisibleRuler(boolean visible) {
    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      Component tab = this.getComponentAt(i);
      if (tab instanceof SourceCodePanel) {
        ((SourceCodePanel) tab).setVisibleRuler(visible);
      }
    }
  }

  /**
   * Get the selection model
   *
   * @return Source model
   */
  public SourceCodeModel getSelectedModel() {

    int index = this.getSelectedIndex();
    SourceCodePanel pane = getTab(index);
    if (pane == null) return null;
    return pane.getModel();
  }

  /**
   * Set profile bar graph to source view
   *
   * @param bargraph Profile bar graph data
   */
  public void setProfilerBargraph(ISourceBargraph[] bargraph) {
    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      Component tab = this.getComponentAt(i);
      if (tab instanceof SourceCodePanel) {
        SourceCodeModel model = ((SourceCodePanel) tab).getModel();
        if (model == null) continue;
        // Set bar graph data only for the same source file.
        SourceFile srcfile = model.getSourceFile();
        if (srcfile == null) continue;
        List<ISourceBargraph> list = new ArrayList<ISourceBargraph>();
        // Maximum and minimum values of the entire data
        float max = Float.MIN_VALUE;
        float min = Float.MAX_VALUE;
        if (bargraph != null) {
          for (ISourceBargraph data : bargraph) {
            // Maximum value
            if (max <= data.getBarValue()) max = data.getBarValue();
            // minimum value
            if (min >= data.getBarValue()) min = data.getBarValue();
            // Do the source files match?
            SourceFile barfile = data.getSourceFile();
            if (srcfile.equals(barfile)) {
              list.add(data);
            }
          }
        }
        if (list.size() <= 0) list = null;
        model.setListBarData(list, max, min);
      }
    }
  }
}
