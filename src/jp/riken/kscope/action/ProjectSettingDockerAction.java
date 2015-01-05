package jp.riken.kscope.action;

import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.dialog.DockerIaaSPropertiesDialog;
import jp.riken.kscope.properties.DockerIaaSProperties;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.ProjectService;

public class ProjectSettingDockerAction extends ActionBase {

	
	public ProjectSettingDockerAction(AppController controller) {
		super(controller);
	}

	@Override
	public void actionPerformed(ActionEvent event) {
		// ステータスメッセージ
        final String message = Message.getString("projectsettingsshconnect.setup.status");  
        Application.status.setMessageMain(message);

        // 親Frameの取得を行う。
        Frame frame = getWindowAncestor( event );

       	openDialog(frame);
	}

	/**
	 *  設定ダイアログを表示する。
	 * @param frame
	 */
	public void openDialog(Frame frame) {
		String status = Message.getString("projectsettingsshconnect.setup.status");
		
        DockerIaaSProperties docker_iaas_properties = this.controller.getPropertiesDIAAS();
        
		DockerIaaSPropertiesDialog dialog = new DockerIaaSPropertiesDialog(frame, docker_iaas_properties);
		dialog.setModal(true);
		int result = dialog.showDialog(); 
		if (result != Constant.OK_DIALOG) {
        	Application.status.setMessageMain(status + 
        			Message.getString("action.common.cancel.status")); //キャンセル
        	return;
        }
        this.controller.setSSHproperties(docker_iaas_properties);
        // Save properties
        // TODO write code to save settings to properties.xml
       
	}
	
	/**
	 *  Message付きの設定ダイアログを表示する。
	 * @param frame
	 */
	public void openDialog(Frame frame, String message) {
		String status = Message.getString("projectsettingsshconnect.setup.status");
		
		DockerIaaSProperties docker_iaas_properties = this.controller.getPropertiesDIAAS();
        
		DockerIaaSPropertiesDialog dialog = new DockerIaaSPropertiesDialog(frame, docker_iaas_properties, message);
		dialog.setModal(true);
		int result = dialog.showDialog(); 
		if (result != Constant.OK_DIALOG) {
        	Application.status.setMessageMain(status + 
        			Message.getString("action.common.cancel.status")); //キャンセル
        	return;
        }
        this.controller.setSSHproperties(docker_iaas_properties);
	}
	
	
}
