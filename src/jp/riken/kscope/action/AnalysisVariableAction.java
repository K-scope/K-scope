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
import java.util.ArrayList;
import java.util.List;
import javax.swing.tree.DefaultMutableTreeNode;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.VariableTableModel;
import jp.riken.kscope.service.AnalysisVariableService;
import jp.riken.kscope.service.AppController;

/**
 * Reference list action
 *
 * @author RIKEN
 */
public class AnalysisVariableAction extends ActionBase {

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public AnalysisVariableAction(AppController controller) {
    super(controller);
  }

  /**
   * Check if the action is executable. <br>
   * Check before executing the action and switch the menu enable. <br>
   *
   * @return true = Action can be executed
   */
  @Override
  public boolean validateAction() {

    // Get the block list and variable declaration list
    List<IBlock> blocks = getSelectedBlocks();
    List<VariableDefinition> vars = getSelectedVariableDefinitions();
    // OK if either the block list or the variable declaration list exists
    if (blocks != null && blocks.size() > 0) {
      return true;
    }
    if (vars != null && vars.size() > 0) {
      return true;
    }

    return false;
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
    final String message =
        Message.getString("mainmenu.analysis.valiableproperty"); // List of variable characteristics
    Application.status.setMessageMain(message);

    // Create a block list and a variable declaration list
    List<IBlock> blocks = getSelectedBlocks();
    List<VariableDefinition> vars = getSelectedVariableDefinitions();
    if (blocks.size() <= 0 && vars.size() <= 0) return;

    // Fortran database
    Fortran fortran = this.controller.getFortranLanguage();
    // Get the variable characteristic information list model
    VariableTableModel modelValiable = this.controller.getVariableTableModel();
    // Error information model
    ErrorInfoModel errorModel = this.controller.getErrorInfoModel();

    // Clear variable characteristic information list
    modelValiable.clearVariable();

    // Analysis service
    AnalysisVariableService service = new AnalysisVariableService(fortran);
    service.setErrorInfoModel(errorModel);
    service.setModelVariable(modelValiable);

    // Get the variable characteristic information list
    // The block gives priority to selection.
    if (blocks.size() > 0) {
      service.analysisVariable(blocks.toArray(new IBlock[0]));
      this.controller.setLastVariable(blocks, null);
    } else if (vars.size() > 0) {
      service.analysisVariable(vars.toArray(new VariableDefinition[0]));
      this.controller.setLastVariable(null, vars);
    }

    // Activate the variable characteristic information list tab
    this.controller.setSelectedAnalysisPanel(ANALYSIS_PANEL.VALIABLE);
  }

  /** Update the variable characteristic information list. */
  public void refresh() {
    // Fortran database
    Fortran fortran = this.controller.getFortranLanguage();
    // Get the variable characteristic information list model
    VariableTableModel modelValiable = this.controller.getVariableTableModel();
    // Error information model
    ErrorInfoModel errorModel = this.controller.getErrorInfoModel();

    // Clear variable characteristic information list
    modelValiable.clearVariable();

    // Analysis service
    AnalysisVariableService service = new AnalysisVariableService(fortran);
    service.setErrorInfoModel(errorModel);
    service.setModelVariable(modelValiable);

    // Get the currently displayed dataset
    List<IBlock> lastBlocks = this.controller.getLastVariableBlocks();
    List<VariableDefinition> lastVars = this.controller.getLastVariableVars();
    if (lastBlocks != null && lastBlocks.size() > 0) {
      service.analysisVariable(lastBlocks.toArray(new IBlock[0]));
    } else if (lastVars != null && lastVars.size() > 0) {
      service.analysisVariable(lastVars.toArray(new VariableDefinition[0]));
    }
  }

  /**
   * Get the selected block
   *
   * @return selection block
   */
  private List<IBlock> getSelectedBlocks() {

    // Get the selected node
    DefaultMutableTreeNode[] nodes =
        this.controller.getMainframe().getPanelExplorerView().getSelectedNodes();
    if (nodes == null) return null;

    // Create a block list
    List<IBlock> blocks = new ArrayList<IBlock>();
    List<VariableDefinition> vars = new ArrayList<VariableDefinition>();
    for (DefaultMutableTreeNode node : nodes) {
      Object obj = node.getUserObject();
      if (obj instanceof VariableDefinition) {
        vars.add((VariableDefinition) obj);
      } else if (obj instanceof IBlock) {
        blocks.add((IBlock) obj);
      }
    }
    if (blocks.size() <= 0 && vars.size() <= 0) return null;

    return blocks;
  }

  /**
   * Get the selection variable declaration statement
   *
   * @return Selective variable declaration statement
   */
  private List<VariableDefinition> getSelectedVariableDefinitions() {

    // Get the selected block
    DefaultMutableTreeNode[] nodes =
        this.controller.getMainframe().getPanelExplorerView().getSelectedNodes();
    if (nodes == null) return null;

    // Create a variable declaration list
    List<VariableDefinition> vars = new ArrayList<VariableDefinition>();
    for (DefaultMutableTreeNode node : nodes) {
      Object obj = node.getUserObject();
      if (obj instanceof VariableDefinition) {
        vars.add((VariableDefinition) obj);
      }
    }
    if (vars.size() <= 0) return null;

    return vars;
  }
}
