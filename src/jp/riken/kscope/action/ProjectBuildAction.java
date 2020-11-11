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

import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.util.List;
import java.util.concurrent.Callable;
import javax.swing.JOptionPane;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.data.FILE_TYPE;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.FileTreeModel;
import jp.riken.kscope.model.LanguageTreeModel;
import jp.riken.kscope.model.ModuleTreeModel;
// import jp.riken.kscope.model.ProjectModel;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.FutureService;
import jp.riken.kscope.service.LanguageService;
import jp.riken.kscope.xcodeml.XcodeMLParserStax;

/**
 * Structural analysis execution action. <br>
 * Perform structural analysis in a separate thread.
 *
 * @author RIKEN
 */
public class ProjectBuildAction extends ActionBase {
  /** Class that builds and searches the database */
  private LanguageService service;
  /** Project clear action */
  private ProjectClearLanguageAction clearAction;

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public ProjectBuildAction(AppController controller) {
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

    // Enable switching of structural analysis execution menu
    // XML file list
    List<SourceFile> listXml = this.controller.getProjectModel().getListSelectedFile();
    if (listXml == null || listXml.size() <= 0) {
      return false;
    }

    int count = 0;
    // Does the XML file exist?
    for (SourceFile file : listXml) {
      if (file == null) continue;
      if (file.getFile() == null) continue;
      if (FILE_TYPE.isXcodemlFile(file.getFile())) {
        if (file.getFile().exists()) {
          count++;
        }
      }
    }
    if (count <= 0) return false;

    // Check the execution status of the thread task
    return this.controller.isThreadTaskDone();
  }

  /**
   * Action occurrence event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {
    // Status message
    final String message =
        Message.getString("mainmenu.project.startanalysis"); // Structural analysis execution
    Application.status.setMessageMain(message);

    // Get the parent Frame.
    Frame frame = getWindowAncestor(event);

    // XML file list
    List<SourceFile> listXml = this.controller.getProjectModel().getListSelectedFile();
    if (listXml == null || listXml.size() <= 0) {
      // Error message
      JOptionPane.showMessageDialog(
          frame,
          Message.getString(
              "projectbuildaction.build.errdialog.xmlnotexist.message"), // There is no XML file to
          // parse.
          Message.getString(
              "projectbuildaction.build.errdialog.xmlnotexist.title"), // XML file error
          JOptionPane.ERROR_MESSAGE);
      Application.status.setMessageMain(
          message + Message.getString("action.common.error.status")); // :error
      return;
    }
    // Error information model
    ErrorInfoModel errorModel = this.controller.getErrorInfoModel();
    boolean error = false;
    for (SourceFile file : listXml) {
      if (file == null) continue;
      if (file.getFile() == null) continue;
      if (!file.getFile().exists()) {
        errorModel.addErrorInfo(
            Message.getString(
                "projectbuildaction.build.errinfo.notexist",
                file.getFile().getPath())); // does not exist.
        error = true;
      }
    }
    if (error) {
      return;
    }

    // Clear the Fortran database
    clearAction = new ProjectClearLanguageAction(this.controller);
    clearAction.clearFortranLanguage();

    // Fortran database
    Fortran fortran = this.controller.getFortranLanguage();
    // Create XML parser
    XcodeMLParserStax xmlParser = new XcodeMLParserStax();
    // Sourcetree model
    FileTreeModel fileModel = this.controller.getSourceTreeModel();
    // XML tree model
    FileTreeModel xmlModel = this.controller.getXmlTreeModel();
    // Structural tree model
    LanguageTreeModel languageModel = this.controller.getLanguageTreeModel();
    // Module tree model
    ModuleTreeModel moduleModel = this.controller.getModuleTreeModel();

    // Structural analysis service
    service = new LanguageService(listXml.toArray(new SourceFile[0]), fortran, xmlParser);
    // Set the structure tree model
    service.setLanguageTreeModel(languageModel);
    // Set up the module tree model
    service.setModuleTreeModel(moduleModel);
    // Set the error information model.
    service.setErrorInfoModel(errorModel);
    // Set the source tree model.
    service.setSourceTreeModel(fileModel);
    // Set up the XML tree model.
    service.setXmlTreeModel(xmlModel);
    // Set the project folder
    service.setProjectFolder(this.controller.getProjectModel().getProjectFolder());

    // Create a thread task service.
    FutureService<Integer> future =
        new FutureService<Integer>(
            /** Thread call class */
            new Callable<Integer>() {
              /** Perform thread execution */
              @Override
              public Integer call() {
                try {
                  // Analyze execution
                  service.parseSourceFile();
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
            if (this.isCancelled()) {
              // Clear the Fortran database
              clearAction.clearFortranLanguage();
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
    // Activate the structure tree
    this.controller.getMainframe().getPanelExplorerView().setSelectedPanel(EXPLORE_PANEL.LANGUAGE);
  }
}
