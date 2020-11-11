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

package jp.riken.kscope.menu;

import java.awt.Component;
import java.awt.event.ActionListener;
import java.util.Arrays;
import java.util.EventListener;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.JTree;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.tree.TreePath;
import jp.riken.kscope.Message;
import jp.riken.kscope.action.ActionBase;
import jp.riken.kscope.action.AnalysisMemoryAction;
import jp.riken.kscope.action.AnalysisOperandAction;
import jp.riken.kscope.action.AnalysisVariableAction;
import jp.riken.kscope.action.EditInformationEditAction;
import jp.riken.kscope.action.FileExportExploreAction;
import jp.riken.kscope.action.FileOpenSourceFileAction;
import jp.riken.kscope.action.LanguagePropertiesAction;
import jp.riken.kscope.action.ProfilerAddEprofAction;
import jp.riken.kscope.action.SearchTreeAction;
import jp.riken.kscope.action.TreeCollapseAllAction;
import jp.riken.kscope.action.TreeExpandAllAction;
import jp.riken.kscope.action.TreeExpandSelectAction;
import jp.riken.kscope.action.ViewOpenExploreBlockAction;
import jp.riken.kscope.action.ViewOpenLanguageTreeAction;
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.service.AppController;

/**
 * Structure tree pop-up menu class
 *
 * @author RIKEN
 */
public class LanguageTreePopupMenu extends JPopupMenu implements PopupMenuListener, ITreePopupMenu {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Application controller */
  private AppController controller;

  /** All storage listener */
  private ActionListener actionTreeCollapseAll;
  /** All deployment listener */
  private ActionListener actionTreeExpandAll;
  /** Selective expansion listener */
  private ActionListener actionTreeExpandSelect;
  /** Open file Listener */
  private EventListener actionOpenFile;

  /** Open file menu */
  private JMenuItem menuOpenFile;
  /** Edit: Edit additional information */
  JMenuItem menuEditInformationEdit;

  /** Analysis: Variable Characteristic List Action */
  private AnalysisVariableAction actionAnalysisVariable;
  /** Additional information editing action */
  private EditInformationEditAction actionEditInformation;
  /** Arithmetic count action */
  private AnalysisOperandAction actionAnalysisOperand;
  /** Explorer export action */
  private ActionListener actionExportExplore;

  /** Constructor */
  public LanguageTreePopupMenu() {
    // Create a menu.
    initialize();
  }

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public LanguageTreePopupMenu(AppController controller) {
    this.controller = controller;

    // Create a menu.
    initialize();
  }

  /** Create a menu. */
  private void initialize() {

    // Create menu
    // Open the source file (linked with the source view)
    menuOpenFile =
        new JMenuItem(
            Message.getString(
                "languagetreepopupmenu.menu.opensource")); // Show your selection in source view
    this.add(menuOpenFile);
    actionOpenFile = new ViewOpenExploreBlockAction(this.controller);
    menuOpenFile.addActionListener((ActionListener) actionOpenFile);

    // spacer
    this.add(new JSeparator());

    // Selective expansion
    JMenuItem menuExpandSelect =
        new JMenuItem(
            Message.getString("mainmenu.view.collapse-expand.selective")); // Selective expansion
    this.add(menuExpandSelect);
    this.actionTreeExpandSelect = new TreeExpandSelectAction(this.controller);
    menuExpandSelect.addActionListener(this.actionTreeExpandSelect);

    // store everything
    JMenuItem menuCollapseAll =
        new JMenuItem(Message.getString("treechooserdialog.tooltip.collapseall")); // All stored
    this.add(menuCollapseAll);
    this.actionTreeCollapseAll = new TreeCollapseAllAction(this.controller);
    menuCollapseAll.addActionListener(this.actionTreeCollapseAll);

    // Expand all
    JMenuItem menuExpandAll =
        new JMenuItem(Message.getString("treechooserdialog.tooltip.expandall")); // Expand all
    this.add(menuExpandAll);
    this.actionTreeExpandAll = new TreeExpandAllAction(this.controller);
    menuExpandAll.addActionListener(this.actionTreeExpandAll);

    // View: New Structure Tree
    JMenuItem menuViewOpenTree =
        new JMenuItem(
            Message.getString("languagetreepopupmenu.menu.newstructuretree")); // New structure tree
    this.add(menuViewOpenTree);
    menuViewOpenTree.addActionListener(new ViewOpenLanguageTreeAction(this.controller));

    // spacer
    this.add(new JSeparator());

    // Tree search
    JMenuItem menuSearchTree =
        new JMenuItem(Message.getString("mainmenu.search.tree")); // Tree search
    this.add(menuSearchTree);
    menuSearchTree.addActionListener(new SearchTreeAction(this.controller));

    // Analysis: List of variable characteristics
    JMenuItem menuAnalysisVariable =
        new JMenuItem(
            Message.getString(
                "mainmenu.analysis.valiableproperty")); // List of variable characteristics
    this.add(menuAnalysisVariable);
    actionAnalysisVariable = new AnalysisVariableAction(this.controller);
    menuAnalysisVariable.addActionListener(actionAnalysisVariable);

    // Analysis: Calculation count
    JMenuItem menuAnalysisCount =
        new JMenuItem(
            Message.getString("mainmenu.analysis.operation")); // Count the number of operations
    actionAnalysisOperand = new AnalysisOperandAction(this.controller, FRAME_VIEW.EXPLORE_VIEW);
    this.add(menuAnalysisCount);
    menuAnalysisCount.addActionListener(actionAnalysisOperand);

    // Profiler: Measurement interval setting
    JMenuItem menuViewAddEprofArea =
        new JMenuItem(
            Message.getString(
                "languagetreepopupmenu.menu.setmeaurementrange")); // Measurement interval setting
    this.add(menuViewAddEprofArea);
    menuViewAddEprofArea.addActionListener(
        new ProfilerAddEprofAction(this.controller, FRAME_VIEW.EXPLORE_VIEW));

    // spacer
    this.add(new JSeparator());

    // Analysis: Access destination setting
    JMenuItem menuAnalysisMemoryAccess =
        new JMenuItem(
            Message.getString("mainmenu.analysis.access")); // Variable access destination setting
    this.add(menuAnalysisMemoryAccess);
    menuAnalysisMemoryAccess.addActionListener(
        new AnalysisMemoryAction(
            this.controller,
            AnalysisMemoryAction.ACTION_MODE.ACCESS_SETTING,
            FRAME_VIEW.EXPLORE_VIEW));

    // Analysis: Request Byte / FLOP calculation
    JMenuItem menuAnalysisMemoryCalculate =
        new JMenuItem(
            Message.getString("mainmenu.analysis.calculate")); // Request Byte / FLOP calculation
    this.add(menuAnalysisMemoryCalculate);
    menuAnalysisMemoryCalculate.addActionListener(
        new AnalysisMemoryAction(
            this.controller,
            AnalysisMemoryAction.ACTION_MODE.MEMORY_CALCULATE,
            FRAME_VIEW.EXPLORE_VIEW));

    // spacer
    this.add(new JSeparator());

    // Edit: Edit additional information
    menuEditInformationEdit =
        new JMenuItem(Message.getString("mainmenu.edit.info")); // Edit additional information
    this.add(menuEditInformationEdit);
    actionEditInformation = new EditInformationEditAction(this.controller, FRAME_VIEW.EXPLORE_VIEW);
    menuEditInformationEdit.addActionListener(actionEditInformation);

    // File: Open the source file with an external tool
    JMenuItem menuFileOpenSourceFile =
        new JMenuItem(Message.getString("mainmenu.file.program")); // Open with an external tool
    this.add(menuFileOpenSourceFile);
    menuFileOpenSourceFile.addActionListener(
        new FileOpenSourceFileAction(this.controller, FRAME_VIEW.EXPLORE_VIEW));

    // spacer
    this.add(new JSeparator());

    // Block properties (linked with analysis view)
    JMenuItem menuFileProperties =
        new JMenuItem(Message.getString("mainmenu.project.property")); // Properties
    this.add(menuFileProperties);
    menuFileProperties.addActionListener(new LanguagePropertiesAction(this.controller));

    // Export (Hide pop-up: Create action only)
    actionExportExplore = new FileExportExploreAction(this.controller);

    this.addPopupMenuListener(this);
  }

  /**
   * Show the pop-up menu only on JTree nodes.
   *
   * @param invoker component
   * @param x coordinate X
   * @param y Coordinate Y
   */
  @Override
  public void show(Component invoker, int x, int y) {
    JTree tree = (JTree) invoker;
    TreePath[] selectepath = tree.getSelectionPaths();
    if (selectepath != null) {
      TreePath path = tree.getPathForLocation(x, y);
      if (path != null && Arrays.asList(selectepath).contains(path)) {
        super.show(invoker, x, y);
      }
    }
  }

  /**
   * Get all storage action listeners
   *
   * @return All storage action listener
   */
  @Override
  public ActionListener getActionTreeCollapseAll() {
    return actionTreeCollapseAll;
  }

  /**
   * Get all deployment action listeners
   *
   * @return All deployment action listener
   */
  @Override
  public ActionListener getActionTreeExpandAll() {
    return actionTreeExpandAll;
  }

  /**
   * Get Selective Deployment Action Listener
   *
   * @return Selective expansion action listener
   */
  @Override
  public ActionListener getActionTreeExpandSelect() {
    return actionTreeExpandSelect;
  }

  /**
   * Get the listener to open the file.
   *
   * @return Open file Listener
   */
  @Override
  public EventListener getActionOpenFile() {
    return actionOpenFile;
  }

  /**
   * Get the explorer export action
   *
   * @return explorer export action
   */
  @Override
  public ActionListener getActionExportExplore() {
    return actionExportExplore;
  }

  /**
   * Pop-up menu visible event. <br>
   * Check if the action is executable
   *
   * @param event Event information
   */
  @Override
  public void popupMenuWillBecomeVisible(PopupMenuEvent event) {

    // Check if the action is executable
    JPopupMenu menu = (JPopupMenu) event.getSource();
    int count = menu.getComponentCount();
    for (int i = 0; i < count; i++) {
      Object obj = menu.getComponent(i);
      if (!(obj instanceof JMenuItem)) continue;
      JMenuItem submenu = (JMenuItem) obj;
      ActionListener[] actions = submenu.getActionListeners();
      if (actions == null) continue;
      for (ActionListener action : actions) {
        if (action instanceof ActionBase) {
          boolean enabled = ((ActionBase) action).validateAction();
          submenu.setEnabled(enabled);
        }
      }
    }
  }

  /**
   * Events with the pop-up menu canceled
   *
   * @param event Event information
   */
  @Override
  public void popupMenuWillBecomeInvisible(PopupMenuEvent event) {}

  /**
   * Events with the pop-up menu canceled
   *
   * @param event Event information
   */
  @Override
  public void popupMenuCanceled(PopupMenuEvent event) {}
}
