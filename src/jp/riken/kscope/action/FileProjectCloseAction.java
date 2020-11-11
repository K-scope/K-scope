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
import javax.swing.JOptionPane;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.gui.ConsolePanel;
import jp.riken.kscope.model.FileTreeModel;
import jp.riken.kscope.model.ProjectModel;
import jp.riken.kscope.service.AppController;

/**
 * Project close action
 *
 * @author RIKEN
 */
public class FileProjectCloseAction extends ActionBase {

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public FileProjectCloseAction(AppController controller) {
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
    final String message = Message.getString("mainmenu.file.closeproject"); // close the project
    // Status message
    Application.status.setMessageMain(message);

    // Get the parent Frame.
    Frame frame = getWindowAncestor(event);

    try {

      // Display a confirmation message.
      int option =
          JOptionPane.showConfirmDialog(
              frame,
              Message.getString(
                  "fileprojectaloseaction.closeproject.dialog.message"), // Do you want to close the
                                                                         // project?
              message, // close the project
              JOptionPane.OK_CANCEL_OPTION,
              JOptionPane.WARNING_MESSAGE);
      if (option != JOptionPane.OK_OPTION) {
        // Status message
        Application.status.setMessageMain(
            message + Message.getString("action.common.cancel.status")); // :Cancel
        return;
      }

      closeProject();

    } catch (Exception ex) {
      ex.printStackTrace();

      // Error display on the error location panel
      this.controller.getErrorInfoModel().addErrorInfo(ex);

      // Error message
      JOptionPane.showMessageDialog(
          frame,
          Message.getString(
              "fileprojectaloseaction.clearerror.dialog.message"), // Project clear error
          message + Message.getString("action.common.error.status"), // :error
          JOptionPane.ERROR_MESSAGE);

      // Status message
      Application.status.setMessageMain(
          message + Message.getString("action.common.error.status")); // :error
    }
  }

  /**
   * @param message
   * @throws Exception
   */
  public void closeProject() throws Exception {
    // clear the console
    ConsolePanel console = this.controller.getMainframe().getPanelAnalysisView().getPanelConsole();
    console.clearConsole();

    // clear the project
    clearProject();

    final String message = Message.getString("mainmenu.file.closeproject"); // close the project
    // Status message
    Application.status.setMessageMain(
        message + Message.getString("action.common.done.status")); // : Done
  }

  /**
   * Clear the project
   *
   * @throws Exception Project clear error
   */
  public void clearProject() throws Exception {

    // Clear analysis information
    ProjectClearLanguageAction action = new ProjectClearLanguageAction(this.controller);
    action.clearFortranLanguage();

    // clear the XML tree
    FileTreeModel treeModel = this.controller.getXmlTreeModel();
    treeModel.setProjectFolder(null);
    treeModel.clearTreeModel();

    // Clear the project
    this.controller.clearProject();
  }
}
