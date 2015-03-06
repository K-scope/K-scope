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
 * 分析情報エクスポートアクションクラス
 * @author RIKEN
 *
 */
public class FileExportAnalysisAction extends ActionBase {

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     */
    public FileExportAnalysisAction(AppController controller) {
        super(controller);
    }

    /**
     * アクションが実行可能であるかチェックする.<br/>
     * アクションの実行前チェック、メニューのイネーブルの切替を行う。<br/>
     * @return		true=アクションが実行可能
     */
    @Override
    public boolean validateAction() {
        // プロジェクトが作成済みであること
        ProjectModel model = this.controller.getProjectModel();
        if (model == null) return false;
        if (model.getProjectTitle() == null) return false;
        if (model.getProjectFolder() == null || !model.getProjectFolder().exists()) return false;

        return true;
    }

    /**
     * アクション発生イベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {
    	
    	// ステータスメッセージ
    	final String message = Message.getString("fileexportanalysisaction.exportanalysisinfo.status"); //分析情報のエクスポート
    	Application.status.setMessageMain(message);

        // 親Frameの取得を行う。
        Frame frame = getWindowAncestor( event );

        // 選択分析情報のタブの取得
        IAnalisysComponent tab = this.controller.getMainframe().getPanelAnalysisView().getSelectedPanel();
        ANALYSIS_PANEL enumPanel = tab.getEnumPanel();
        String defname = enumPanel.getFilename();
        
        if (!tab.isExportable()) {
        	JOptionPane.showMessageDialog(frame,
        			Message.getString("fileexportanalysisaction.exportanalysisinfo.dialog.message"), //出力する分析情報がありません。処理を終了します。
        			message, JOptionPane.CLOSED_OPTION);
        	Application.status.setMessageMain(message + 
        			Message.getString("action.common.stop.status")); // 中断
        	return;
        }

        // プロジェクトフォルダ
        File projectfolder = this.controller.getProjectModel().getProjectFolder();
        String folder = projectfolder != null ? projectfolder.getAbsolutePath() : null;
        

        // ファイル保存ダイアログを表示する。
        File file = SwingUtils.showSaveFileDialog(frame, message, folder, defname);
        if (file == null) {
        	Application.status.setMessageMain(message + 
        			Message.getString("action.common.cancel.status")); //キャンセル
        	return;
        }

        // 分析情報のエクスポートを行う
        tab.export(file);
    	Application.status.setMessageMain(message + 
    			Message.getString("action.common.done.status")); //完了
    }


}
