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

import java.awt.event.ActionListener;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import jp.riken.kscope.Message;
import jp.riken.kscope.action.ActionBase;
import jp.riken.kscope.action.AnalysisMemoryAction;
import jp.riken.kscope.action.AnalysisReferenceAction;
import jp.riken.kscope.action.AnalysisTraceAction;
import jp.riken.kscope.action.EditClipboardCopyAction;
import jp.riken.kscope.action.FSourceTTreeAction;
import jp.riken.kscope.action.FileOpenSourceFileAction;
import jp.riken.kscope.action.ProfilerAddEprofAction;
import jp.riken.kscope.action.SearchFindAction;
import jp.riken.kscope.action.SearchGrepAction;
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.common.TRACE_DIR;
import jp.riken.kscope.service.AppController;

/**
 * Source File Panel pop-up menu class
 *
 * @author RIKEN
 */
public class SourcePanelPopupMenu extends JPopupMenu implements PopupMenuListener {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Application controller */
  private AppController controller;
  /** Double click action (2014/4/8 ohichi) * */
  private ActionListener action;

  /** Constructor */
  public SourcePanelPopupMenu() {
    // Create a menu.
    initialize();
  }

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public SourcePanelPopupMenu(AppController controller) {
    this.controller = controller;

    // Create a menu.
    initialize();
  }

  /** Create a menu. */
  private void initialize() {

    // Create menu
    // Select a node (2014/4/8 added ohichi)
    JMenuItem selectNode = new JMenuItem(Message.getString("mainmenu.edit.click"));
    this.add(selectNode);
    action = new FSourceTTreeAction(this.controller);
    selectNode.addActionListener(action);

    // Edit: Copy
    JMenuItem menuEditCopy = new JMenuItem(Message.getString("mainmenu.edit.copy")); // copy
    this.add(menuEditCopy);
    menuEditCopy.addActionListener(
        new EditClipboardCopyAction(this.controller, FRAME_VIEW.SOURCE_VIEW));

    // spacer
    this.add(new JSeparator());

    // Search: Search
    JMenuItem menuSearchFind =
        new JMenuItem(Message.getString("mainmenu.search.source")); // Source search
    this.add(menuSearchFind);
    menuSearchFind.addActionListener(new SearchFindAction(this.controller));
    // Search: File search ...
    JMenuItem menuSearchGrep =
        new JMenuItem(Message.getString("mainmenu.search.file")); // File search
    this.add(menuSearchGrep);
    menuSearchGrep.addActionListener(new SearchGrepAction(this.controller));

    // spacer
    this.add(new JSeparator());

    // Analysis: Trace: Previous
    JMenuItem menuAnalysisStart =
        new JMenuItem(Message.getString("trace_dir.enum.start")); // Trace: Start
    this.add(menuAnalysisStart);
    menuAnalysisStart.addActionListener(new AnalysisTraceAction(this.controller, TRACE_DIR.START));

    // Analysis: Data flow analysis (declaration / definition / reference)
    JMenuItem menuAnalysisReference =
        new JMenuItem(
            Message.getString(
                "mainmenu.analysis.dec-def-ref")); // Declaration / Definition / Reference
    this.add(menuAnalysisReference);
    menuAnalysisReference.addActionListener(
        new AnalysisReferenceAction(this.controller, FRAME_VIEW.SOURCE_VIEW));

    // Profiler: Measurement interval setting
    JMenuItem menuViewAddEprofArea =
        new JMenuItem(
            Message.getString(
                "languagetreepopupmenu.menu.setmeaurementrange")); // Measurement interval setting
    this.add(menuViewAddEprofArea);
    menuViewAddEprofArea.addActionListener(
        new ProfilerAddEprofAction(this.controller, FRAME_VIEW.SOURCE_VIEW));

    // spacer
    this.add(new JSeparator());

    // Analysis: Access destination setting
    JMenuItem menuAnalysisMemoryAccess =
        new JMenuItem(
            Message.getString(
                "mainmenu.analysis.access")); // KEY520 = Variable access destination setting
    this.add(menuAnalysisMemoryAccess);
    menuAnalysisMemoryAccess.addActionListener(
        new AnalysisMemoryAction(
            this.controller,
            AnalysisMemoryAction.ACTION_MODE.ACCESS_SETTING,
            FRAME_VIEW.SOURCE_VIEW));

    // Analysis: Request Byte / FLOP calculation
    JMenuItem menuAnalysisMemoryCalculate =
        new JMenuItem(
            Message.getString(
                "mainmenu.analysis.calculate")); // KEY521 = Request Byte / FLOP calculation
    this.add(menuAnalysisMemoryCalculate);
    menuAnalysisMemoryCalculate.addActionListener(
        new AnalysisMemoryAction(
            this.controller,
            AnalysisMemoryAction.ACTION_MODE.MEMORY_CALCULATE,
            FRAME_VIEW.SOURCE_VIEW));

    // spacer
    this.add(new JSeparator());

    // File: Open the source file with an external tool
    JMenuItem menuFileOpenSourceFile =
        new JMenuItem(Message.getString("mainmenu.file.program")); // Open with an external tool
    this.add(menuFileOpenSourceFile);
    menuFileOpenSourceFile.addActionListener(
        new FileOpenSourceFileAction(this.controller, FRAME_VIEW.SOURCE_VIEW));

    this.addPopupMenuListener(this);
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
   * Get select expansion action listener (2014/4/8 added ohichi)
   *
   * @return Selective expansion action listener
   */
  public ActionListener getAction() {
    return action;
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
