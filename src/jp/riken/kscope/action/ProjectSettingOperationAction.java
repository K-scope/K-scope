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
import jp.riken.kscope.dialog.SettingOperationDialog;
import jp.riken.kscope.properties.OperationProperties;
import jp.riken.kscope.service.AppController;

/**
 * Calculation count setting action class
 * @author RIKEN
 *
 */
public class ProjectSettingOperationAction extends ActionBase {

    /**
     * Constructor
     * @param controller Application controller
     */
    public ProjectSettingOperationAction(AppController controller) {
        super(controller);
    }

    /**
     * Action occurrence event
     * @param event Event information
     */
    @Override
    public void actionPerformed(ActionEvent event) {
        // Status message
        final String message = Message.getString("projectsettingoperationaction.setup.status"); // Calculation count setting
        Application.status.setMessageMain(message);

        // Get the parent Frame.
        Frame frame = getWindowAncestor( event );

        // Display the operation count setting dialog.
        OperationProperties properities = this.controller.getPropertiesOperation();

        SettingOperationDialog dialog = new SettingOperationDialog(frame, true, properities);
        int result = dialog.showDialog();
        if (result != Constant.OK_DIALOG) {
        	Application.status.setMessageMain(message + Message.getString("action.common.cancel.status")); // Cancel
        	return;
        }
        Application.status.setMessageMain(message + Message.getString("action.common.done.status")); // Done
        return;
    }

}
