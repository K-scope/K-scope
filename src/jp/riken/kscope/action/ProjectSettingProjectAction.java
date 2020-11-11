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
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.dialog.SettingProjectDialog;
import jp.riken.kscope.properties.ProjectProperties;
// import jp.riken.kscope.properties.RemoteBuildProperties;
import jp.riken.kscope.service.AppController;

public class ProjectSettingProjectAction extends ActionBase {

  private static boolean debug = (System.getenv("DEBUG") != null);
  private static boolean debug_l2 = false;

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public ProjectSettingProjectAction(AppController controller) {
    super(controller);
    if (debug) {
      debug_l2 = (System.getenv("DEBUG").equalsIgnoreCase("high"));
    }
  }

  @Override
  public void actionPerformed(ActionEvent event) {
    final String message =
        Message.getString("projectsettingprojectaction.setup.status"); // Project settings
    Application.status.setMessageMain(message);

    Frame frame = getWindowAncestor(event);

    // Last access folder
    String currentFolder = this.controller.getLastAccessFolder();

    // Display the project setting dialog.
    ProjectProperties properties = this.controller.getPropertiesProject();

    SettingProjectDialog dialog = new SettingProjectDialog(frame, true, properties);
    dialog.setLastAccessFolder(currentFolder);
    int result = dialog.showDialog();
    if (result != Constant.OK_DIALOG) {
      Application.status.setMessageMain(
          message + Message.getString("action.common.cancel.status")); // Cancel
      return;
    }
    properties = dialog.getProjectProperties();
    if (debug) {
      System.out.println("Project Properties has been changed. " + properties.toString());
    }
    String title = properties.getPropertyValue(ProjectProperties.PRJ_TITLE).getValue();
    this.controller.getProjectModel().setProjectTitle(title);

    Application.status.setMessageMain(
        message + Message.getString("action.common.done.status")); // Done
    return;
  }
}
