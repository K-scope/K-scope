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

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import jp.riken.kscope.Message;
import jp.riken.kscope.action.AnalysisOperandAction;
import jp.riken.kscope.action.AnalysisReferenceAction;
import jp.riken.kscope.action.AnalysisScopeAction;
import jp.riken.kscope.action.EditInformationEditAction;
import jp.riken.kscope.action.FileExportExploreAction;
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.service.AppController;

/**
 * Variable characteristic list pop-up menu class
 *
 * @author RIKEN
 */
public class VariablePopupMenu extends JPopupMenu implements PopupMenuListener {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Application controller */
  private AppController controller;

  /** Export action */
  @SuppressWarnings("unused")
  private FileExportExploreAction actionExportExplore;
  /** Analysis: Arithmetic Count Action */
  private AnalysisOperandAction actionAnalysisOperand;
  /** Analysis: Additional Information Action */
  private EditInformationEditAction actionAnalysisInformation;
  /** Analysis: Declaration / Definition / Reference Action */
  private AnalysisReferenceAction actionAnalysisReference;
  /** Analysis: Variable Scope Action */
  private AnalysisScopeAction actionAnalysisScope;

  /** Constructor */
  public VariablePopupMenu() {
    // Create a menu.
    initialize();
  }

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public VariablePopupMenu(AppController controller) {
    this.controller = controller;

    // Create a menu.
    initialize();
  }

  /** Create a menu. */
  private void initialize() {

    // Create menu

    // Analysis: Additional information
    JMenuItem menuAnalysisInformation =
        new JMenuItem(Message.getString("mainmenu.edit.info")); // Edit additional information
    actionAnalysisInformation =
        new EditInformationEditAction(this.controller, FRAME_VIEW.ANALYSIS_VIEW);
    this.add(menuAnalysisInformation);
    menuAnalysisInformation.addActionListener(actionAnalysisInformation);

    // Analysis: Calculation count
    JMenuItem menuAnalysisCount =
        new JMenuItem(
            Message.getString(
                "mainmenu.project.config.operation")); // Count the number of operations
    actionAnalysisOperand = new AnalysisOperandAction(this.controller, FRAME_VIEW.ANALYSIS_VIEW);
    this.add(menuAnalysisCount);
    menuAnalysisCount.addActionListener(actionAnalysisOperand);

    // Analysis: Reference list
    JMenuItem menuAnalysisReference =
        new JMenuItem(
            Message.getString(
                "mainmenu.analysis.dec-def-ref")); // Declaration / Definition / Reference
    actionAnalysisReference =
        new AnalysisReferenceAction(this.controller, FRAME_VIEW.ANALYSIS_VIEW);
    menuAnalysisReference.addActionListener(actionAnalysisReference);
    this.add(menuAnalysisReference);

    // Analysis: Variable scope
    JMenuItem menuAnalysisValid =
        new JMenuItem(Message.getString("mainmenu.analysis.valiablescope")); // Variable valid area
    actionAnalysisScope = new AnalysisScopeAction(this.controller, FRAME_VIEW.ANALYSIS_VIEW);
    menuAnalysisValid.addActionListener(actionAnalysisScope);
    this.add(menuAnalysisValid);

    // Export (Hide pop-up: Create action only)
    actionExportExplore = new FileExportExploreAction(this.controller);

    this.addPopupMenuListener(this);
  }

  /**
   * Pop-up menu visible event
   *
   * @param event Event information
   */
  @Override
  public void popupMenuWillBecomeVisible(PopupMenuEvent event) {}

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
   * Analysis: Get arithmetic count action
   *
   * @return Analysis: Arithmetic count action
   */
  public AnalysisOperandAction getActionAnalysisOperand() {
    return actionAnalysisOperand;
  }

  /**
   * Analysis: Get additional information actions
   *
   * @return Analysis: Additional Information Action
   */
  public EditInformationEditAction getActionAnalysisInformation() {
    return actionAnalysisInformation;
  }

  /**
   * Analysis: Get declaration / definition / reference actions
   *
   * @return Analysis: Declaration / Definition / Reference Action
   */
  public AnalysisReferenceAction getActionAnalysisReference() {
    return actionAnalysisReference;
  }

  /**
   * Analysis: Analysis: Get variable scope action
   *
   * @return Analysis: Analysis: Variable Scope Action
   */
  public AnalysisScopeAction getActionAnalysisScope() {
    return actionAnalysisScope;
  }
}
