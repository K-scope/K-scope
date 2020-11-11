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
package jp.riken.kscope.action;

import java.awt.event.ActionEvent;
import javax.swing.tree.DefaultMutableTreeNode;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.ReferenceModel;
import jp.riken.kscope.service.AnalysisReferenceService;
import jp.riken.kscope.service.AppController;

/**
 * Declaration / Definition / Reference List Action
 *
 * @author RIKEN
 */
public class AnalysisReferenceAction extends ActionBase {

  /** Declaration / definition / reference list Variable acquisition destination view */
  private FRAME_VIEW view;

  /**
   * Constructor
   *
   * @param controller Application controller
   * @param view Declaration / definition / reference list Variable acquisition destination view
   */
  public AnalysisReferenceAction(AppController controller, FRAME_VIEW view) {
    super(controller);
    this.view = view;
  }

  /**
   * Check if the action is executable. <br>
   * Check before executing the action and switch the menu enable. <br>
   *
   * @return true = Action can be executed
   */
  @Override
  public boolean validateAction() {

    if (this.view != FRAME_VIEW.SOURCE_VIEW) {
      // Get the selected node
      VariableDefinition variable = getSelectedVariable();
      return (variable != null);

    } else {
      // Get the selected line of source code
      CodeLine line = this.controller.getMainframe().getPanelSourceView().getSelectedCodeLine();
      if (line == null) return false;
      return (line.getStatement() != null && !line.getStatement().isEmpty());
    }
  }

  /**
   * Action occurrence event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {
    // Action check
    if (!validateAction()) {
      return;
    }

    // Status message
    final String message =
        Message.getString("mainmenu.analysis.dec-def-ref"); // Declaration / Definition / Reference
    Application.status.setMessageMain(message);

    if (this.view != FRAME_VIEW.SOURCE_VIEW) {
      // Select variable
      VariableDefinition variable = getSelectedVariable();
      if (variable == null) return;
      // Get the reference list
      analysisReference(variable);
    } else {
      // Get the selected line of source code
      CodeLine line = this.controller.getMainframe().getPanelSourceView().getSelectedCodeLine();
      if (line == null) return;

      // Get the reference list
      analysisReference(line);
    }
  }

  /**
   * Get declaration / definition / reference list variables
   *
   * @return Declaration / definition / reference list variable
   */
  private VariableDefinition getSelectedVariable() {

    VariableDefinition variable = null;
    if (this.view == FRAME_VIEW.EXPLORE_VIEW) {
      // Get the selected node
      DefaultMutableTreeNode node =
          this.controller.getMainframe().getPanelExplorerView().getSelectedNode();
      if (node == null || node.getUserObject() == null) return null;

      // Selected node
      Object obj = node.getUserObject();
      if (obj instanceof VariableDefinition) {
        variable = (VariableDefinition) obj;
      }
    } else if (this.view == FRAME_VIEW.ANALYSIS_VIEW) {
      variable = this.controller.getVariableTableModel().getSelectedVariable();
    }
    return variable;
  }

  /**
   * Create a declaration / definition / reference list
   *
   * @param variable Generated variable
   */
  public void analysisReference(VariableDefinition variable) {
    if (variable == null) return;

    // Fortran database
    Fortran fortran = this.controller.getFortranLanguage();
    // Get the reference list model
    ReferenceModel modelReference = this.controller.getReferenceModel();
    // Error information model
    ErrorInfoModel errorModel = this.controller.getErrorInfoModel();

    // Clear reference list
    modelReference.clearTreeModel();

    // Analysis service
    AnalysisReferenceService service = new AnalysisReferenceService(fortran);
    service.setErrorInfoModel(errorModel);
    service.setModelReference(modelReference);

    // Get the reference list
    service.analysisReference(variable);

    // Activate the Reference List tab
    this.controller.setSelectedAnalysisPanel(ANALYSIS_PANEL.REFERENCE);
  }

  /**
   * Create a declaration / definition / reference list
   *
   * @param line Selected line information
   */
  public void analysisReference(CodeLine line) {
    if (line == null) return;

    // Fortran database
    Fortran fortran = this.controller.getFortranLanguage();
    // Get the reference list model
    ReferenceModel modelReference = this.controller.getReferenceModel();
    // Error information model
    ErrorInfoModel errorModel = this.controller.getErrorInfoModel();

    // Clear reference list
    modelReference.clearTreeModel();

    // Analysis service
    AnalysisReferenceService service = new AnalysisReferenceService(fortran);
    service.setErrorInfoModel(errorModel);
    service.setModelReference(modelReference);

    // Get the reference list
    service.analysisReference(line);

    // Activate the Reference List tab
    this.controller.setSelectedAnalysisPanel(ANALYSIS_PANEL.REFERENCE);
  }
}
