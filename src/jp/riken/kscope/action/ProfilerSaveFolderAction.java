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
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.model.ProjectModel;
import jp.riken.kscope.profiler.ProfilerMeasureInfo;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.ProfilerService;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Profiler measurement interval folder save action class
 *
 * @author RIKEN
 */
public class ProfilerSaveFolderAction extends ActionBase {

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public ProfilerSaveFolderAction(AppController controller) {
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
    // Project information
    ProjectModel project = this.controller.getProjectModel();
    String projectFolder = null;
    if (project.getProjectFolder() != null) {
      projectFolder = project.getProjectFolder().getAbsolutePath();
    }
    if (projectFolder == null) {
      return false;
    }
    // Measurement section information
    if (this.controller.getProfilerInfo() == null) {
      return false;
    }
    if (this.controller.getProfilerInfo().getMeasureInfo() == null) {
      return false;
    }
    ProfilerMeasureInfo measureInfo = this.controller.getProfilerInfo().getMeasureInfo();
    if (measureInfo.getMeasureDataCount() <= 0) {
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
    // Status message
    final String message =
        Message.getString(
            "mainmenu.profiler.savefolder-mesuermentrange"); // Measurement interval: Save in folder
    Application.status.setMessageMain(message);
    // main frame
    Frame frame = this.controller.getMainframe();

    // Project information
    ProjectModel project = this.controller.getProjectModel();
    File projectFolder = project.getProjectFolder();

    String title =
        Message.getString(
            "profilersavefolderaction.savefolder.selectdialog.title"); // Measurement interval:
                                                                       // Select save folder
    // Display the folder selection dialog.
    File[] selected =
        SwingUtils.showSaveFolderDialog(frame, title, projectFolder.getAbsolutePath(), false);
    if (selected == null || selected.length <= 0) return;

    // Measurement section information
    ProfilerMeasureInfo measureInfo = this.controller.getProfilerInfo().getMeasureInfo();
    // Profiler service
    ProfilerService service = new ProfilerService();
    service.setErrorInfoModel(this.controller.getErrorInfoModel());
    service.setMeasureInfo(measureInfo);
    service.setProjectFolder(projectFolder);
    // Profiler properties
    service.setPropertiesProfiler(this.controller.getPropertiesProfiler());

    try {
      // Overwrite save execution
      service.saveMeasureFile(selected[0]);
    } catch (Exception ex) {
      ex.printStackTrace();
      Application.status.setMessageMain(
          message + Message.getString("action.common.failed.status") // : Failure
          );
      return;
    }

    Application.status.setMessageMain(
        message + Message.getString("action.common.done.status") // : Done
        );

    return;
  }
}
