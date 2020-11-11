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
import java.awt.HeadlessException;
import java.awt.event.ActionEvent;
import java.io.File;
import java.util.concurrent.Callable;
import javax.swing.JOptionPane;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.ProjectModel;
import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.FutureService;
import jp.riken.kscope.service.LanguageService;
import jp.riken.kscope.service.ProjectService;

/**
 * Save project action
 *
 * @author RIKEN
 */
public class FileProjectSaveAction extends ActionBase {
  private static boolean debug = (System.getenv("DEBUG") != null);
  private static boolean debug_l2 = false;

  /** Class that builds and searches the database */
  private LanguageService service;

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public FileProjectSaveAction(AppController controller) {
    super(controller);
    if (debug) debug_l2 = (System.getenv("DEBUG").equalsIgnoreCase("high"));
  }

  /**
   * Check if the action is executable. <br>
   * Check before executing the action and switch the menu enable. <br>
   *
   * @return true = Action can be executed
   */
  @Override
  public boolean validateAction() {
    // The project has been created
    ProjectModel model = this.controller.getProjectModel();
    if (model == null) return false;
    if (model.getProjectTitle() == null) return false;
    if (model.getProjectFolder() == null || !model.getProjectFolder().exists()) return false;

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
    String message = Message.getString("mainmenu.file.saveproject"); // Save project
    Application.status.setMessageMain(message);

    // Action check
    if (!validateAction()) {
      Application.status.setMessageMain(
          message + Message.getString("action.common.unavailable.status")); // : Impossible
      return;
    }

    // Get the parent Frame.
    Frame frame = getWindowAncestor(event);

    // Display a confirmation message.
    int option =
        JOptionPane.showConfirmDialog(
            frame,
            Message.getString(
                "fileprojectsaveaction.save.confirm.dialog.message"), // Do you want to save the
            // project?
            message, // Save project
            JOptionPane.OK_CANCEL_OPTION,
            JOptionPane.WARNING_MESSAGE);

    if (option != JOptionPane.OK_OPTION) {
      Application.status.setMessageMain(
          message + Message.getString("action.common.cancel.status")); // :Cancel
      return;
    }

    saveProject(frame);
  }

  /**
   * @param message
   * @param frame
   * @throws HeadlessException
   */
  public void saveProject(Frame frame) throws HeadlessException {
    // Project service
    ProjectService service = new ProjectService(this.controller.getProjectModel());
    // Keyword properties
    service.setPropertiesKeyword(this.controller.getPropertiesKeyword());
    // External tool properties
    service.setPropertiesExtension(this.controller.getPropertiesExtension());
    // Arithmetic count property
    service.setPropertiesOperand(this.controller.getPropertiesOperation());
    // Source view settings properties
    service.setPropertiesSource(this.controller.getPropertiesSource());
    // Profiler configuration properties
    service.setPropertiesProfiler(this.controller.getPropertiesProfiler());
    // Project settings properties
    service.setPropertiesProject(this.controller.getPropertiesProject());
    // Request Byte / FLOP configuration property
    service.setPropertiesMemory(this.controller.getPropertiesMemory());
    //
    service.setRBproperties(this.controller.getRBproperties());
    // Error model
    service.setErrorInfoModel(this.controller.getErrorInfoModel());

    try {
      // Save project
      File projectFolder = this.controller.getProjectModel().getProjectFolder();
      service.saveProject(projectFolder);

      // Serialize the Language class
      // settings folder
      File settingsFolder =
          new File(
              projectFolder.getAbsoluteFile() + File.separator + KscopeProperties.SETTINGS_FOLDER);
      writeLanguage(settingsFolder);

    } catch (Exception e) {
      e.printStackTrace();
      String message = Message.getString("mainmenu.file.saveproject"); // Save project
      // Error message
      JOptionPane.showMessageDialog(
          frame,
          Message.getString(
              "fileprojectsaveaction.save.failed.dialog.message"), // Failed to save the project.
          message + Message.getString("dialog.common.error"), // error
          JOptionPane.ERROR_MESSAGE);

      // Status message
      Application.status.setMessageMain(
          message + Message.getString("action.common.error.status")); // :error
    }
  }

  /**
   * Serialize the Language class
   *
   * @param folder Language class serialized folder
   */
  public void writeLanguage(final File folder) {
    // Status message
    final String message = Message.getString("mainmenu.file.saveproject"); // Save project

    // Fortran database
    Fortran fortran = this.controller.getFortranLanguage();
    // Error information model
    ErrorInfoModel errorModel = this.controller.getErrorInfoModel();

    // Structural analysis service
    service = new LanguageService(fortran);
    // Set the error information model.
    service.setErrorInfoModel(errorModel);

    // Create a thread task service.
    FutureService<Integer> future =
        new FutureService<Integer>(
            /** Thread call class */
            new Callable<Integer>() {
              /** Perform thread execution */
              @Override
              public Integer call() {
                try {
                  // Serialize execution
                  service.writeLanguage(folder);
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
    Application.status.setProgressStart(true);

    // Thread start
    new Thread(future).start();
  }
}
