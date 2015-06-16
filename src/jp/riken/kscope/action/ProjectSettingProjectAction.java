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
import jp.riken.kscope.data.ProjectPropertyValue;
import jp.riken.kscope.dialog.SettingProjectDialog;
import jp.riken.kscope.properties.ProjectProperties;
//import jp.riken.kscope.properties.RemoteBuildProperties;
import jp.riken.kscope.service.AppController;

public class ProjectSettingProjectAction extends ActionBase {

	/**
	 * コンストラクタ
     * @param controller	アプリケーションコントローラ
	 */
	public ProjectSettingProjectAction(AppController controller) {
		super(controller);
	}
	
	@Override
	public void actionPerformed(ActionEvent event) {
		final String message = Message.getString("projectsettingprojectaction.setup.status"); //プロジェクトの設定
		Application.status.setMessageMain(message);
		
		Frame frame = getWindowAncestor(event);
		
		// 最終アクセスフォルダ
        String currentFolder = this.controller.getLastAccessFolder();
		
		// プロジェクト設定ダイアログを表示する。
        ProjectProperties properties = this.controller.getPropertiesProject();
        
		SettingProjectDialog dialog = new SettingProjectDialog(frame, true, properties);
		dialog.setLastAccessFolder(currentFolder);
		int result = dialog.showDialog();
		if (result != Constant.OK_DIALOG) {
        	Application.status.setMessageMain(message + 
        			Message.getString("action.common.cancel.status")); //キャンセル
        	return;
        }
		
		this.controller.getProjectModel().setProjectTitle(properties.getPropertyValue(ProjectProperties.PRJ_TITLE).getValue());

        Application.status.setMessageMain(message +
    			Message.getString("action.common.done.status")); //完了
        return;

	}

}
