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
import java.io.File;
import java.util.ArrayList;
import java.util.Enumeration;
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
import jp.riken.kscope.data.CodeLine;
// import jp.riken.kscope.data.ErrorInfo;
import jp.riken.kscope.data.FILE_TYPE;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.dialog.SearchGrepDialog;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.SearchResultModel;
import jp.riken.kscope.service.AnalysisSearchService;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.FutureService;
import jp.riken.kscope.utils.FileUtils;
import jp.riken.kscope.utils.StringUtils;

/**
 * Search action
 *
 * @author RIKEN
 */
public class SearchGrepAction extends ActionBase {

  /** Search service */
  private AnalysisSearchService service;

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public SearchGrepAction(AppController controller) {
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

    // Files on the tree
    SourceFile[] files =
        this.controller
            .getMainframe()
            .getPanelExplorerView()
            .getPanelSourceTree()
            .getAllSourceFiles();
    if (files == null) return false;

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
    final String message = Message.getString("mainmenu.search.file"); // File search
    Application.status.setMessageMain(message);

    // Selected string
    String text = null;
    // Get the selected line of source code
    CodeLine line = this.controller.getMainframe().getPanelSourceView().getSelectedCodeLine();
    if (line != null) {
      // Selected string
      text = line.getStatement();
    }

    // Get the source tree model
    TreeModel modelTree =
        this.controller.getMainframe().getPanelExplorerView().getPanelSourceTree().getTreeModel();
    if (line == null) {
      if (this.controller.getMainframe().getPanelExplorerView().getSelectedEnumPanel()
          == EXPLORE_PANEL.XML) {
        // Get the XML tree when the XML tree is displayed
        modelTree =
            this.controller.getMainframe().getPanelExplorerView().getPanelXmlTree().getTreeModel();
      }
    } else if (FILE_TYPE.isXcodemlFile(line.getSourceFile().getFile())) {
      // Get the XML tree when the XML file is displayed
      modelTree =
          this.controller.getMainframe().getPanelExplorerView().getPanelXmlTree().getTreeModel();
    }

    // Get the file search dialog.
    SearchGrepDialog dialog = this.controller.getMainframe().getDialogSearchGrep();
    if (text != null && !text.isEmpty()) {
      // Search string
      dialog.setSearchText(text);
    }
    // Sourcetree model
    dialog.setReferenceTreeModel(modelTree);

    // Display the file search dialog.
    int result = dialog.showDialog();
    if (result != Constant.OK_DIALOG) return;

    // Search string
    String searchText = dialog.getSearchText();
    // Optional
    boolean regex = dialog.isRegex();
    boolean word = dialog.isWord();
    boolean sensitivecase = dialog.isSensitivecase();
    TreeNode[] nodes = dialog.getSelectedTreeNodes();
    // Get the search source file list from the selected node
    SourceFile[] files = getSearchFiles((DefaultMutableTreeNode) modelTree.getRoot(), nodes);

    // Search service
    service = new AnalysisSearchService();
    // Error information model
    ErrorInfoModel errorModel = this.controller.getErrorInfoModel();
    service.setErrorInfoModel(errorModel);
    SearchResultModel model = this.controller.getSearchResultModel();
    service.setSearchModel(model);
    service.setSearchText(searchText);
    service.setRegex(regex);
    service.setWord(word);
    service.setSensitivecase(sensitivecase);
    service.setExploreTreeNode((DefaultMutableTreeNode) modelTree.getRoot());
    // Search file
    service.setSearchFiles(files);

    // Create a thread task service.
    FutureService<Integer> future =
        new FutureService<Integer>(
            /** Thread call class */
            new Callable<Integer>() {
              /** Perform thread execution */
              @Override
              public Integer call() {
                try {
                  // Search execution
                  service.searchFile();
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
    // Highlight the search string in Source view
    this.controller.setSearchKeywords();
  }

  /**
   * Get a list of search source files from the selected node
   *
   * @param root root node
   * @param nodes Selected nodes
   * @return List of selected source files
   */
  private SourceFile[] getSearchFiles(DefaultMutableTreeNode root, TreeNode[] nodes) {
    List<SourceFile> list = new ArrayList<SourceFile>();

    // List tree nodes in the forward direction
    Enumeration<?> depth = root.preorderEnumeration();
    while (depth.hasMoreElements()) {
      DefaultMutableTreeNode treeNode = (DefaultMutableTreeNode) depth.nextElement();

      // Do a node search
      if (treeNode == null || treeNode.getUserObject() == null) {
        continue;
      }
      if (treeNode.getUserObject() instanceof SourceFile) {
        if (isSelectedFile(nodes, (SourceFile) treeNode.getUserObject())) {
          list.add((SourceFile) treeNode.getUserObject());
        }
      }
    }

    return list.toArray(new SourceFile[0]);
  }

  /**
   * Check if it is a file of the selected node
   *
   * @param nodes Selected nodes
   * @param nodeFile Check file
   * @return true = Selected file
   */
  private boolean isSelectedFile(TreeNode[] nodes, SourceFile nodeFile) {
    if (nodeFile == null) return false;
    // Add all if there are no selected nodes
    if (nodes == null) return true;

    for (TreeNode node : nodes) {
      if (node == null) continue;
      Object obj = ((DefaultMutableTreeNode) node).getUserObject();
      if (obj == null) continue;
      if (obj instanceof SourceFile) {
        SourceFile file = (SourceFile) ((DefaultMutableTreeNode) node).getUserObject();
        if (nodeFile.equals(file)) {
          // Selected file
          return true;
        }
      } else if (obj instanceof File) {
        File file = (File) obj;
        if (!file.isDirectory()) continue;
        // Check if it is a child / grandchild file under the folder
        if (FileUtils.isChildsFile(nodeFile.getFile(), file)) {
          // Child / grandchild files
          return true;
        }
      }
    }
    return false;
  }
}
