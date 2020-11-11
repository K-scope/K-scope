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
import java.util.HashSet;
import java.util.List;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.component.FilterTreeNode;
import jp.riken.kscope.data.RequiredBFResult;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.model.LanguageTreeModel;
import jp.riken.kscope.model.RequiredBFModel;
import jp.riken.kscope.properties.RequiredBFProperties;
import jp.riken.kscope.properties.VariableMemoryProperties;
import jp.riken.kscope.service.AnalysisMemoryService;
import jp.riken.kscope.service.AppController;

/**
 * Bulk request Byte / FLOP action class
 *
 * @author ohichi
 */
public class AllAnalysisMemoryAction extends ActionBase {
  /** Analysis service * */
  private AnalysisMemoryService serviceMemory;
  /** List of calculation results * */
  private List<RequiredBFResult> list;
  /** Working set to store program units already added in the tree */
  private HashSet<Procedure> checkList = new HashSet<Procedure>();
  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public AllAnalysisMemoryAction(AppController controller) {
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

    // Get the selection tree model
    TreeModel modelTree = this.controller.getMainframe().getPanelExplorerView().getTreeModel();
    if (modelTree == null) {
      return false;
    }
    EXPLORE_PANEL view =
        this.controller.getMainframe().getPanelExplorerView().getSelectedPanel().getEnumPanel();
    if (view != EXPLORE_PANEL.LANGUAGE) {
      return false;
    }
    TreeNode root = (TreeNode) modelTree.getRoot();
    if (root.getChildCount() <= 0) {
      return false;
    }

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
    final String message = Message.getString("mainmenu.analysis.allcalculate");
    Application.status.setMessageMain(message);

    // Get the selection tree model
    TreeModel modelTree = this.controller.getMainframe().getPanelExplorerView().getTreeModel();
    // Get the root node
    FilterTreeNode root = (FilterTreeNode) modelTree.getRoot();
    if (root != null) {
      // Property settings
      setService();
      this.checkList.clear();
      list = new ArrayList<RequiredBFResult>();

      searchProcedure(root);
      if (list.size() <= 0) return;
      this.serviceMemory.setAnalysisPanel(list.toArray(new RequiredBFResult[0]));
    }
  }

  /**
   * Node search
   *
   * @param parent parent node
   */
  private void searchProcedure(FilterTreeNode parent) {
    int i;
    Procedure obj;
    if (parent.getUserObject() instanceof Procedure) {
      obj = (Procedure) parent.getUserObject();
      if (!(this.checkList.contains(obj))) {
        this.checkList.add(obj);
        list.add(cal((IBlock) obj));
      }
    }
    int n = parent.getChildCount();
    for (i = 0; i < n; i++) searchProcedure((FilterTreeNode) parent.getChildAt(i));
  }

  /** Property setting of memory performance calculation function */
  private void setService() {
    // Request Byte / FLOP configuration property
    RequiredBFProperties properitiesMemory = this.controller.getPropertiesMemory();
    // Variable access destination memory setting
    VariableMemoryProperties properitiesVariable = this.controller.getPropertiesVariable();
    RequiredBFModel modelRequired = this.controller.getRequiredByteFlopModel();
    LanguageTreeModel modelLanguage = this.controller.getLanguageTreeModel();
    modelRequired.setModelLanguageTree(modelLanguage);
    this.serviceMemory = new AnalysisMemoryService();
    this.serviceMemory.setProperitiesRequiredBF(properitiesMemory);
    this.serviceMemory.setPropertiesOperand(this.controller.getPropertiesOperation());
    this.serviceMemory.setModelRequiredBF(modelRequired);
    this.serviceMemory.setPropertiesVariableMemory(properitiesVariable);
  }

  /**
   * Calculation execution
   *
   * @param block Target block
   */
  private RequiredBFResult cal(IBlock block) {
    RequiredBFResult result = this.serviceMemory.calcRequiredBF(block);
    return result;
  }
}
