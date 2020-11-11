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
import javax.swing.JOptionPane;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.service.AppController;

/**
 * Structural analysis clear action
 *
 * @author RIKEN
 */
public class ProjectClearLanguageAction extends ActionBase {

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public ProjectClearLanguageAction(AppController controller) {
    super(controller);
  }

  /** Constructor */
  public ProjectClearLanguageAction() {
    super();
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
        Message.getString("mainmenu.project.clearanalysis"); // Clear structural analysis
    Application.status.setMessageMain(message);

    // Display a confirmation message.
    int option =
        JOptionPane.showConfirmDialog(
            this.controller.getMainframe(),
            Message.getString(
                "projectclearlanguageaction.clear.confirmdialog.message"), // Clear the analysis
            // result and analysis
            // result, is that okay?
            Message.getString(
                "projectclearlanguageaction.clear.confirmdialog.title"), // Clear structural
            // information
            JOptionPane.OK_CANCEL_OPTION,
            JOptionPane.WARNING_MESSAGE);
    if (option != JOptionPane.OK_OPTION) {
      Application.status.setMessageMain(
          message + Message.getString("action.common.cancel.status")); // Cancel
      return;
    }

    // Clear the Fortran database.
    clearFortranLanguage();
    Application.status.setMessageMain(
        message + Message.getString("action.common.done.status")); // Done
  }

  /** Clear the Fortran database. */
  public void clearFortranLanguage() {

    // clear
    // Fortran database
    this.controller.clearFortranLanguage();

    // Tree model
    this.controller.getMainframe().getPanelExplorerView().clearTreeModel();

    // Clear analysis information
    this.controller.getMainframe().getPanelAnalysisView().clearModels();
    // Close the console tab
    this.controller.getMainframe().getPanelAnalysisView().closeTab(ANALYSIS_PANEL.CONSOLE);

    // Clear Source View
    this.controller.getMainframe().getPanelSourceView().closeAllTabs();
  }
}
