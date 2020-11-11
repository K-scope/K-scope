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
import java.io.File;
import javax.swing.JOptionPane;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.gui.IAnalisysComponent;
import jp.riken.kscope.model.ProjectModel;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Analysis information export action class
 *
 * @author RIKEN
 */
public class FileExportAnalysisAction extends ActionBase {

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public FileExportAnalysisAction(AppController controller) {
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

    return true;
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
        Message.getString(
            "fileexportanalysisaction.exportanalysisinfo.status"); // Export analysis information
    Application.status.setMessageMain(message);

    // Get the parent Frame.
    Frame frame = getWindowAncestor(event);

    // Get tab of selection analysis information
    IAnalisysComponent tab =
        this.controller.getMainframe().getPanelAnalysisView().getSelectedPanel();
    ANALYSIS_PANEL enumPanel = tab.getEnumPanel();
    String defname = enumPanel.getFilename();

    if (!tab.isExportable()) {
      JOptionPane.showMessageDialog(
          frame,
          Message.getString(
              "fileexportanalysisaction.exportanalysisinfo.dialog.message"), // There is no analysis
          // information to
          // output. The process
          // ends.
          message,
          JOptionPane.CLOSED_OPTION);
      Application.status.setMessageMain(
          message + Message.getString("action.common.stop.status")); // Suspended
      return;
    }

    // Project folder
    File projectfolder = this.controller.getProjectModel().getProjectFolder();
    String folder = projectfolder != null ? projectfolder.getAbsolutePath() : null;

    // Display the file save dialog.
    File file = SwingUtils.showSaveFileDialog(frame, message, folder, defname);
    if (file == null) {
      Application.status.setMessageMain(
          message + Message.getString("action.common.cancel.status")); // Cancel
      return;
    }

    // Export analysis information
    tab.export(file);
    Application.status.setMessageMain(
        message + Message.getString("action.common.done.status")); // Done
  }
}
