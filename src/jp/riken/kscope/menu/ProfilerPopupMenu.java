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
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import jp.riken.kscope.Message;
import jp.riken.kscope.action.ActionBase;
import jp.riken.kscope.action.EditClipboardCopyAction;
import jp.riken.kscope.action.ProfilerInformationEditAction;
import jp.riken.kscope.action.ViewOpenAnalysisLineAction;
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.service.AppController;

/**
 * Profiler table panel pop-up menu class
 *
 * @author RIKEN
 */
public class ProfilerPopupMenu extends JPopupMenu implements PopupMenuListener {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Application controller */
  private AppController controller;

  /** Analysis: Additional Information Action */
  private ProfilerInformationEditAction actionAnalysisInformation;
  /** Analysis result Action to open the relevant part */
  private ViewOpenAnalysisLineAction actionOpenAnalysisLine;
  /** Clipboard copy action */
  private EditClipboardCopyAction actionAnalysisCopy;

  /** Constructor */
  public ProfilerPopupMenu() {
    // Create a menu.
    initialize();
  }

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public ProfilerPopupMenu(AppController controller) {
    this.controller = controller;

    // Create a menu.
    initialize();
  }

  /** Create a menu. */
  private void initialize() {

    // Create menu

    // Analysis: Copy
    JMenuItem menuAnalysisCopy = new JMenuItem(Message.getString("mainmenu.edit.copy")); // copy
    actionAnalysisCopy = new EditClipboardCopyAction(this.controller, FRAME_VIEW.ANALYSIS_VIEW);
    this.add(menuAnalysisCopy);
    menuAnalysisCopy.addActionListener(actionAnalysisCopy);

    // Analysis: Additional information
    JMenuItem menuAnalysisInformation =
        new JMenuItem(Message.getString("mainmenu.edit.info")); // Additional information
    actionAnalysisInformation = new ProfilerInformationEditAction(this.controller);
    this.add(menuAnalysisInformation);
    menuAnalysisInformation.addActionListener(actionAnalysisInformation);

    // Hide menu
    // open selection
    actionOpenAnalysisLine = new ViewOpenAnalysisLineAction(this.controller);

    this.addPopupMenuListener(this);
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

  /**
   * Analysis: Get additional information actions
   *
   * @return Analysis: Additional Information Action
   */
  public ProfilerInformationEditAction getActionAnalysisInformation() {
    return actionAnalysisInformation;
  }

  /**
   * Get the action to open the relevant part
   *
   * @return Action to open the relevant part
   */
  public ViewOpenAnalysisLineAction getActionOpenAnalysisLine() {
    return this.actionOpenAnalysisLine;
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
}
