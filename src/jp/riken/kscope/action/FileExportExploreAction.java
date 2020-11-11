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
import java.util.ArrayList;
import java.util.List;
import javax.swing.JOptionPane;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.gui.ITreeComponent;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.ProjectModel;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.LanguageService;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Export action class
 *
 * @author RIKEN
 */
public class FileExportExploreAction extends ActionBase {

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public FileExportExploreAction(AppController controller) {
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

    final String message =
        Message.getString(
            "fileexportexploreaction.exportexploer.status"); // Export tree information
    Application.status.setMessageMain(message);

    // Get the parent Frame.
    Frame frame = getWindowAncestor(event);

    // Get tab of selection analysis information
    ITreeComponent tab = this.controller.getMainframe().getPanelExplorerView().getSelectedPanel();

    // Project folder
    File projectfolder = this.controller.getProjectModel().getProjectFolder();
    String folder = projectfolder != null ? projectfolder.getAbsolutePath() : null;

    // Fortran database
    Fortran fortran = this.controller.getFortranLanguage();

    // Output procedure list
    List<String> procedure_names = new ArrayList<String>();
    String filename = null;
    if (tab.getEnumPanel() == EXPLORE_PANEL.LANGUAGE
        || tab.getEnumPanel() == EXPLORE_PANEL.MODULE) {
      // For structure tree and module tree, output from Fortran database.
      IBlock[] blocks = tab.getSelectedBlocks();

      // Output procedure check
      if (blocks != null) {
        for (IBlock block : blocks) {
          if (block instanceof Procedure && ((Procedure) block).get_name() != null) {
            procedure_names.add(((Procedure) block).get_name());
          }
        }
      }

      // Since the output procedure does not exist, search from the root
      if (procedure_names.size() <= 0) {
        if (fortran.getMainName() != null) {
          procedure_names.add(fortran.getMainName());
        }
      }
      if (procedure_names.size() <= 0) {
        // Error message
        JOptionPane.showMessageDialog(
            null,
            Message.getString(
                "fileexportexploreaction.exportexploer.dialog.message"), // The output procedure
                                                                         // does not exist.
            message,
            JOptionPane.ERROR_MESSAGE);
        Application.status.setMessageMain(
            message + Message.getString("action.common.error.status")); // :error
        return;
      }
      filename = procedure_names.get(0) + ".txt";
    } else if (tab.getEnumPanel() == EXPLORE_PANEL.SOURCE) {
      filename = "sourcetree.txt";
    } else if (tab.getEnumPanel() == EXPLORE_PANEL.XML) {
      filename = "xmltree.txt";
    }

    // Display the file save dialog.
    File file = SwingUtils.showSaveFileDialog(frame, message, folder, filename);
    if (file == null) {
      Application.status.setMessageMain(
          message + Message.getString("action.common.cancel.status")); // Cancel
      return;
    }

    if (tab.getEnumPanel() == EXPLORE_PANEL.LANGUAGE) {
      // For structure tree and module tree, output from Fortran database.
      IBlock[] blocks = tab.getSelectedBlocks();

      // Error information model
      ErrorInfoModel errorModel = this.controller.getErrorInfoModel();

      // Structural analysis service
      LanguageService service = new LanguageService(fortran);
      // Set the error information model.
      service.setErrorInfoModel(errorModel);

      // Output file
      service.exportLanguage(file, blocks);
    } else {
      // Export tree information
      tab.export(file);
    }

    Application.status.setMessageMain(
        message + Message.getString("action.common.done.status")); // Done
  }
}
