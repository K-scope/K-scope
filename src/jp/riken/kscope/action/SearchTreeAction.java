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
import java.util.concurrent.Callable;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.common.EXPLORE_PANEL;
// import jp.riken.kscope.data.ErrorInfo;
import jp.riken.kscope.dialog.SearchTreeDialog;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.service.AnalysisSearchService;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.FutureService;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Tree search action
 *
 * @author RIKEN
 */
public class SearchTreeAction extends ActionBase {
  /** Search service */
  private AnalysisSearchService service;
  /** Search panel */
  private EXPLORE_PANEL searchPanel;

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public SearchTreeAction(AppController controller) {
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
    final String message = Message.getString("mainmenu.search.tree"); // Tree search
    Application.status.setMessageMain(message);

    // Get the selection tree model
    TreeModel modelTree = this.controller.getMainframe().getPanelExplorerView().getTreeModel();
    this.searchPanel = this.controller.getMainframe().getPanelExplorerView().getSelectedEnumPanel();
    // Selected node
    TreeNode[] selectedNodes =
        this.controller.getMainframe().getPanelExplorerView().getSelectedNodes();

    // Display the search dialog.
    SearchTreeDialog dialog = this.controller.getMainframe().getDialogSearchTree();
    dialog.setReferenceTreeModel(this.searchPanel, modelTree);
    dialog.setSelectedTreeNodes(selectedNodes);

    int result = dialog.showDialog();
    if (result != Constant.OK_DIALOG) return;

    // Search string
    String searchText = dialog.getSearchText();
    // Search node
    TreeNode[] nodes = dialog.getSelectedTreeNodes();
    // Optional
    boolean regex = dialog.isSearchRegex();
    boolean word = dialog.isSearchWord();
    boolean sensitivecase = dialog.isSearchSensitivecase();

    // Delete the child node of the search node
    List<DefaultMutableTreeNode> list = new ArrayList<DefaultMutableTreeNode>();
    if (nodes != null) {
      for (TreeNode node : nodes) {
        if (!(node instanceof DefaultMutableTreeNode)) continue;
        if (isChildNode(nodes, (DefaultMutableTreeNode) node)) {
          // A child node of one of the node lists
          continue;
        }
        if (list.contains(node)) continue;

        // Add because it is not a child node
        list.add((DefaultMutableTreeNode) node);
      }
    }

    // Search service
    service = new AnalysisSearchService();
    // Error information model
    ErrorInfoModel errorModel = this.controller.getErrorInfoModel();
    service.setErrorInfoModel(errorModel);
    // Search result model
    service.setSearchModel(this.controller.getSearchResultModel());

    // Search criteria
    service.setSearchText(searchText);
    service.setRegex(regex);
    service.setWord(word);
    service.setSensitivecase(sensitivecase);
    if (list != null && list.size() > 0) {
      service.setSearchNodes(list.toArray(new TreeNode[0]));
    }
    // Search source explorer tree node
    service.setExploreTreeNode((DefaultMutableTreeNode) modelTree.getRoot());

    // Create a thread task service.
    FutureService<Integer> future =
        new FutureService<Integer>(
            /** Thread call class */
            new Callable<Integer>() {
              /** Perform thread execution */
              @Override
              public Integer call() {
                try {
                  // Perform a tree search except for the structure tree.
                  if (searchPanel != EXPLORE_PANEL.LANGUAGE) {
                    // Search execution
                    service.searchTree();
                  } else {
                    // Search execution
                    service.searchLanguage();
                  }
                  // Error message
                  String errorMessage = service.getErrorMessage();
                  if (!StringUtils.isNullOrEmpty(errorMessage)) {
                    return Constant.ERROR_RESULT;
                  }
                  return Constant.SUCCESS_RESULT;
                } catch (Exception e) {
                  e.printStackTrace();
                  return Constant.ERROR_RESULT;
                }
              }
            }) {
          /**
           * Thread execution completed. <br>
           * Perform post-processing when canceled.
           */
          @Override
          protected void done() {
            // Check if the end is due to cancellation.
            String errorMessage = service.getErrorMessage();
            if (!StringUtils.isNullOrEmpty(errorMessage)) {
              this.setMessage(errorMessage);
              Application.status.setMessageMain(
                  message + Message.getString("action.common.error.status")); // error
            } else if (this.isCancelled()) {
              Application.status.setMessageMain(
                  message + Message.getString("action.common.cancel.status")); // :Cancel
            } else {
              Application.status.setMessageMain(
                  message + Message.getString("action.common.done.status")); // : Done
            }
            // Stop service execution
            if (service != null) {
              service.cancelRunning();
            }
            super.done();
          }
        };

    // Clear status message
    Application.status.setMessageStatus(null);

    // Register the controller as a listener in the thread task: To call when the thread is
    // completed
    future.addPropertyChangeListener(this.controller);
    this.controller.setThreadFuture(future);

    // Display the progress dialog
    WindowProgressAction progress = new WindowProgressAction(this.controller);
    progress.showProgressDialog();

    // Thread start
    new Thread(future).start();

    // Activate the search results tab
    this.controller.setSelectedAnalysisPanel(ANALYSIS_PANEL.SEARCHRESULT);
  }

  /**
   * Check if the child node is one of the child nodes in the node list
   *
   * @param nodes node list
   * @param childnode Child node
   * @return true = Child node is one of the child nodes in the node list
   */
  private boolean isChildNode(TreeNode[] nodes, DefaultMutableTreeNode childnode) {

    for (TreeNode node : nodes) {
      if (!(node instanceof DefaultMutableTreeNode)) continue;
      if (node == childnode) continue;
      if (((DefaultMutableTreeNode) node).getUserObject() == childnode.getUserObject()) continue;
      if (SwingUtils.isChildNode((DefaultMutableTreeNode) node, childnode)) {
        return true;
      }
    }

    return false;
  }
}
