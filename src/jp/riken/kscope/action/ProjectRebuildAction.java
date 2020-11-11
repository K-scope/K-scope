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
import java.io.OutputStream;
import java.util.concurrent.Callable;
import javax.swing.JOptionPane;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.data.ProjectPropertyValue;
import jp.riken.kscope.gui.ConsolePanel;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.LanguageTreeModel;
import jp.riken.kscope.model.ProjectModel;
import jp.riken.kscope.properties.ProjectProperties;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.FutureService;
import jp.riken.kscope.service.LanguageService;
import jp.riken.kscope.service.ProjectMakeService;
import jp.riken.kscope.utils.StringUtils;

/**
 * Structural analysis re-execution action
 *
 * @author RIKEN
 */
public class ProjectRebuildAction extends ActionBase {

  private static boolean debug = (System.getenv("DEBUG") != null);
  private static boolean debug_l2 = false;

  /** make command execution service */
  private ProjectMakeService serviceMake;
  /** Structural analysis service */
  private LanguageService serviceLang;
  /** Explorer view update flag */
  @SuppressWarnings("unused")
  private boolean updateView;

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public ProjectRebuildAction(AppController controller) {
    super(controller);
    if (debug) {
      debug_l2 = (System.getenv("DEBUG").equalsIgnoreCase("high"));
    }
  }

  /**
   * Check if the action is executable. <br>
   * Check before executing the action and switch the menu enable. <br>
   *
   * @return true = Action can be executed
   */
  @Override
  public boolean validateAction() {
    // CHECK if have open project
    FileProjectSaveAction save_action = new FileProjectSaveAction(this.controller);
    if (!save_action.validateAction()) return false;

    ProjectProperties properties = this.controller.getPropertiesProject();
    if (properties == null) return false;

    // CHECK build_command
    ProjectPropertyValue value = properties.getPropertyValue(ProjectProperties.BUILD_COMMAND);
    if (value == null) return false;
    if (StringUtils.isNullOrEmpty(value.getValue())) return false;

    // Check the execution status of the thread task
    // return this.controller.isThreadTaskDone();
    return properties.canRebuild();
  }

  /**
   * Action occurrence event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {
    final String message =
        Message.getString("mainmenu.project.restertanalysis"); // Re-execute structural analysis
    Application.status.setMessageMain(message);
    Frame frame = getWindowAncestor(event);
    // Display a confirmation message.
    int result =
        JOptionPane.showConfirmDialog(
            frame,
            Message.getString(
                "projectrebuildaction.confirmdialog.message"), // Do you want to re-execute the
            // structural analysis?
            message, // Re-execute structural analysis
            JOptionPane.OK_CANCEL_OPTION,
            JOptionPane.QUESTION_MESSAGE);

    if (result != Constant.OK_DIALOG) {
      Application.status.setMessageMain(
          message + Message.getString("action.common.cancel.status")); // :Cancel
      return;
    }

    // Re-execute structural analysis
    rebuild();

    return;
  }

  /** Re-execute structural analysis. Execute the make command to reconfigure the database. */
  public void rebuild() {
    // Re-execute structural analysis
    final String message = Message.getString("mainmenu.project.restertanalysis");
    Application.status.setMessageMain(message);

    // Project model
    ProjectModel projectModel = this.controller.getProjectModel();

    // console
    this.controller.setSelectedAnalysisPanel(ANALYSIS_PANEL.CONSOLE);
    final ConsolePanel console =
        this.controller.getMainframe().getPanelAnalysisView().getPanelConsole();
    console.clearConsole();
    OutputStream out = console.getOutputStream();

    // Error model
    ErrorInfoModel modelError = this.controller.getErrorInfoModel();
    modelError.clearErrorList();

    // LanguageTreeModel
    LanguageTreeModel modelTree = this.controller.getLanguageTreeModel();
    this.updateView = !(modelTree.isSetLanguageTree());

    // make command execution service
    serviceMake = new ProjectMakeService(projectModel.getProjectFolder(), this.controller);
    serviceMake.setOutputStream(out);
    // Set the error information model.
    serviceMake.setErrorInfoModel(modelError);
    // Fortran database
    serviceMake.setFortranLanguage(this.controller.getFortranLanguage());
    // XML file search path
    serviceMake.setListSearchPath(this.controller.getProjectModel().getListSearchPath());
    // Project model
    serviceMake.setProjectModel(this.controller.getProjectModel());

    // Structural analysis service
    serviceLang = new LanguageService(this.controller.getFortranLanguage());
    // Set the structure tree model
    serviceLang.setLanguageTreeModel(this.controller.getLanguageTreeModel());
    // Set up the module tree model
    serviceLang.setModuleTreeModel(this.controller.getModuleTreeModel());
    // Set the error information model.
    serviceLang.setErrorInfoModel(modelError);
    // Set the source tree model.
    serviceLang.setSourceTreeModel(this.controller.getSourceTreeModel());
    // Set up the XML tree model.
    serviceLang.setXmlTreeModel(this.controller.getXmlTreeModel());

    // Create a thread task service.
    FutureService<Integer> future =
        new FutureService<Integer>(
            /** Thread call class */
            new Callable<Integer>() {
              /** Perform thread execution */
              @Override
              public Integer call() {
                try {
                  console.disable_horizontal_scroll = true; // disable scroll
                  if (debug) System.out.println("Calling executeCleanCommand method");
                  boolean result = serviceMake.executeCleanCommand();
                  if (!result) {
                    return Constant.CANCEL_RESULT;
                  }
                  result = serviceMake.executeMakeCommand();
                  console.disable_horizontal_scroll = false; // enable scroll
                  if (!result) {
                    return Constant.CANCEL_RESULT;
                  }
                  // Clear the analysis view.
                  clearAnalysisView();
                  // Analyze execution
                  if (serviceMake.rebuild()) {
                    // Clear Explorer view and Source view.
                    clearExplorerView();
                    // Update Explorer View
                    serviceLang.setExplorerView();
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
            if (this.isCancelled()) {
              Application.status.setMessageMain(
                  message + Message.getString("action.common.cancel.status")); // :Cancel
            } else {
              Application.status.setMessageMain(
                  message + Message.getString("action.common.done.status")); // : Done
            }
            // Stop service execution
            if (serviceMake != null) {
              serviceMake.cancelRunning();
            }
            if (serviceLang != null) {
              serviceLang.cancelRunning();
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
    Thread thread = new Thread(future);
    thread.start();
  }

  /** Clear the analysis view. */
  private void clearAnalysisView() {
    // Clear analysis information
    this.controller.getMainframe().getPanelAnalysisView().clearModels();
  }

  /** Clear Explorer view and Source view. */
  private void clearExplorerView() {
    // Tree model
    this.controller.getMainframe().getPanelExplorerView().clearTreeModel();
    // Clear Source View
    this.controller.getMainframe().getPanelSourceView().closeAllTabs();
  }
}
