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
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.ScopeModel;
import jp.riken.kscope.service.AnalysisScopeService;
import jp.riken.kscope.service.AppController;

/**
 * Variable scope action
 *
 * @author RIKEN
 */
public class AnalysisScopeAction extends ActionBase {

  /** Variable effective area Variable acquisition destination view */
  private FRAME_VIEW view;

  /**
   * Constructor
   *
   * @param controller Application controller
   * @param view Variable effective area Variable acquisition destination view
   */
  public AnalysisScopeAction(AppController controller, FRAME_VIEW view) {
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

    // Get the selected node
    VariableDefinition variable = getSelectedVariable();
    if (variable == null) return false;

    return true;
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
        Message.getString("mainmenu.analysis.valiablescope"); // Variable valid area
    Application.status.setMessageMain(message);

    // Select variable
    VariableDefinition variable = getSelectedVariable();
    if (variable == null) return;

    // Get the variable valid area
    analysisScope(variable);
  }

  /**
   * Get the target variable of the variable effective area
   *
   * @return Variable range target variable
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
   * Create a variable effective area table
   *
   * @param variable Variable range target variable
   */
  public void analysisScope(VariableDefinition variable) {
    if (variable == null) return;

    // Fortran database
    Fortran fortran = this.controller.getFortranLanguage();
    // Get the variable scope model
    ScopeModel modelScope = this.controller.getScopeModel();
    // Error information model
    ErrorInfoModel errorModel = this.controller.getErrorInfoModel();

    // Clear variable valid area
    modelScope.clear();

    // Analysis service
    AnalysisScopeService service = new AnalysisScopeService(fortran);
    service.setErrorInfoModel(errorModel);
    service.setModelScope(modelScope);

    // Get the variable valid area
    service.analysisScope(variable);

    // Activate the Variable Effectiveness tab
    this.controller.setSelectedAnalysisPanel(ANALYSIS_PANEL.SCOPE);
  }
}
