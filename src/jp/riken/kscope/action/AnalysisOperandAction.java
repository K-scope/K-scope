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
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.OperandTableModel;
import jp.riken.kscope.properties.OperationProperties;
import jp.riken.kscope.service.AnalysisOperandService;
import jp.riken.kscope.service.AppController;

/**
 * Computation count action
 *
 * @author RIKEN
 */
public class AnalysisOperandAction extends ActionBase {

  /** Additional information acquisition destination view */
  private FRAME_VIEW view;

  /**
   * Constructor
   *
   * @param controller Application controller
   * @param view Additional information acquisition destination view
   */
  public AnalysisOperandAction(AppController controller, FRAME_VIEW view) {
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

    // Get the selected block
    IBlock[] blocks = getSelectedBlocks();
    if (blocks == null) return false;

    return true;
  }

  /**
   * Action occurrence event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {

    // Execution check
    if (!validateAction()) return;

    // Status message
    final String message = Message.getString("mainmenu.analysis.operation"); // Calculation count
    Application.status.setMessageMain(message);

    // Get the selected block
    IBlock[] blocks = getSelectedBlocks();
    if (blocks == null) return;

    // Get the operation count
    analysisOperand(blocks);
  }

  /**
   * Get the selected block
   *
   * @return selection block
   */
  private IBlock[] getSelectedBlocks() {

    IBlock[] blocks = null;
    if (this.view == FRAME_VIEW.EXPLORE_VIEW) {
      // Get the selected block
      blocks = this.controller.getMainframe().getPanelExplorerView().getSelectedBlocks();
    } else if (this.view == FRAME_VIEW.ANALYSIS_VIEW) {
      IBlock block = this.controller.getMainframe().getPanelAnalysisView().getSelectedBlock();
      if (block != null) {
        blocks = new IBlock[1];
        blocks[0] = block;
      }
    }
    return blocks;
  }

  /**
   * Get the operation count
   *
   * @param blocks selection blocks
   */
  public void analysisOperand(IBlock[] blocks) {

    // Fortran database
    Fortran fortran = this.controller.getFortranLanguage();
    // Get the arithmetic count table model
    OperandTableModel modelOperand = this.controller.getOperandTableModel();
    // Get the built-in function operation count property
    OperationProperties propertiesOperand = this.controller.getPropertiesOperation();
    // Error information model
    ErrorInfoModel errorModel = this.controller.getErrorInfoModel();

    // Clear operation count
    modelOperand.clearOperand();

    // Analysis service
    AnalysisOperandService service = new AnalysisOperandService(fortran);
    service.setErrorInfoModel(errorModel);
    service.setModelOperand(modelOperand);
    service.setPropertiesOperand(propertiesOperand);

    // Get the operation count
    service.analysisOperand(blocks);

    // Activate the calculation count tab
    this.controller.setSelectedAnalysisPanel(ANALYSIS_PANEL.OPERAND);
  }
}
