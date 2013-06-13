package jp.riken.kscope.action;

import java.awt.Frame;
import java.awt.event.ActionEvent;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.dialog.SSHconnectPropertiesDialog;
import jp.riken.kscope.dialog.SettingMemoryDialog;
import jp.riken.kscope.properties.MemorybandProperties;
import jp.riken.kscope.properties.SSHconnectProperties;
import jp.riken.kscope.service.AppController;

public class ProjectSettingSSHAction extends ActionBase {

	
	public ProjectSettingSSHAction(AppController controller) {
		super(controller);
	}

	@Override
	public void actionPerformed(ActionEvent event) {
		// ステータスメッセージ
        final String message = "Set SSHconnect parameters"; // TODO add messages to properties files to Message.getString("projectsettingmemoryaction.setup.status");  
        Application.status.setMessageMain(message);

        // 親Frameの取得を行う。
        Frame frame = getWindowAncestor( event );

        // 要求Byte/FLOP設定ダイアログを表示する。
        SSHconnectProperties SSHproperties = null;
		try {
			SSHproperties = new SSHconnectProperties();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

        SSHconnectPropertiesDialog dialog = new SSHconnectPropertiesDialog(frame, SSHproperties); 
        int result = dialog.showDialog(); //TODO use result somewhere (look at projectPropertiesAction for sample.
	}

}
