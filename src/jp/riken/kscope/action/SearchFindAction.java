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
import java.util.concurrent.Callable;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeModel;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.data.CodeLine;
// import jp.riken.kscope.data.ErrorInfo;
import jp.riken.kscope.data.FILE_TYPE;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.dialog.SearchFindDialog;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.SearchResultModel;
import jp.riken.kscope.service.AnalysisSearchService;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.FutureService;
import jp.riken.kscope.utils.StringUtils;

/**
 * Search action
 *
 * @author RIKEN
 */
public class SearchFindAction extends ActionBase {
  /** Search service */
  private AnalysisSearchService service;

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public SearchFindAction(AppController controller) {
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

    // Currently open file
    SourceFile[] files = this.controller.getMainframe().getPanelSourceView().getOpenedSourceFile();
    if (files == null) return false;

    // Get the selected line of source code
    CodeLine line = this.controller.getMainframe().getPanelSourceView().getSelectedCodeLine();
    if (line == null) return false;

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
    final String message = Message.getString("mainmenu.search.source"); // Source search
    Application.status.setMessageMain(message);

    // Get the selected line of source code
    CodeLine line = this.controller.getMainframe().getPanelSourceView().getSelectedCodeLine();
    if (line == null) return;

    // Selected string
    String text = line.getStatement();

    // Get the search dialog.
    SearchFindDialog dialog = this.controller.getMainframe().getDialogSearchFind();
    // Search string
    if (text != null) {
      dialog.setSearchText(text);
    }
    // Display the search dialog.
    int result = dialog.showDialog();
    if (result != Constant.OK_DIALOG) return;

    // Search string
    String searchText = dialog.getSearchText();
    // Optional
    boolean regex = dialog.isRegex();
    boolean word = dialog.isWord();
    boolean sensitivecase = dialog.isSensitivecase();
    boolean openfiles = dialog.isOpenFiles();

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
    // Get the source tree model
    TreeModel modelTree =
        this.controller.getMainframe().getPanelExplorerView().getPanelSourceTree().getTreeModel();
    if (FILE_TYPE.isXcodemlFile(line.getSourceFile().getFile())) {
      // Get the XML tree when the XML file is displayed
      modelTree =
          this.controller.getMainframe().getPanelExplorerView().getPanelXmlTree().getTreeModel();
    }
    service.setExploreTreeNode((DefaultMutableTreeNode) modelTree.getRoot());

    // Search file
    SourceFile[] files = {line.getSourceFile()};
    if (openfiles) {
      // Search other open files
      files = this.controller.getMainframe().getPanelSourceView().getOpenedSourceFile();
    }
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
}
