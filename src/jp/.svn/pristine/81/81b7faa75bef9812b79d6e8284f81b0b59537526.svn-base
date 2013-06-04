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

import javax.swing.JOptionPane;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.gui.ConsolePanel;
import jp.riken.kscope.model.FileTreeModel;
import jp.riken.kscope.model.ProjectModel;
import jp.riken.kscope.service.AppController;

/**
 * プロジェクトの閉じるアクション
 * @author riken
 */
public class FileProjectCloseAction extends ActionBase {

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     */
    public FileProjectCloseAction(AppController controller) {
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

        // スレッドタスクの実行状態をチェックする
        return this.controller.isThreadTaskDone();
    }

    /**
     * アクション発生イベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {
        final String message = Message.getString("mainmenu.file.closeproject"); //プロジェクトを閉じる
        // ステータスメッセージ
        Application.status.setMessageMain(message);

        // 親Frameの取得を行う。
        Frame frame = getWindowAncestor( event );

        try {

            // 確認メッセージを表示する。
            int option = JOptionPane.showConfirmDialog(frame,
                    Message.getString("fileprojectaloseaction.closeproject.dialog.message"), //プロジェクトを閉じますか？
                    message, //プロジェクトを閉じる
                    JOptionPane.OK_CANCEL_OPTION,
                    JOptionPane.WARNING_MESSAGE);
            if (option != JOptionPane.OK_OPTION) {
                // ステータスメッセージ
                Application.status.setMessageMain(message + Message.getString("action.common.cancel.status")); //:キャンセル
                return;
            }

            // コンソールをクリアする
            ConsolePanel console = this.controller.getMainframe().getPanelAnalysisView().getPanelConsole();
    		console.clearConsole();

            // プロジェクトをクリアする
            clearProject();

            // ステータスメッセージ
            Application.status.setMessageMain(message + Message.getString("action.common.done.status")); //:完了

        } catch (Exception ex) {
            ex.printStackTrace();

            // エラー箇所パネルにエラー表示
            this.controller.getErrorInfoModel().addErrorInfo(ex);

            // エラーメッセージ
            JOptionPane.showMessageDialog(frame,
                    Message.getString("fileprojectaloseaction.clearerror.dialog.message"), //プロジェクトのクリアエラー
                    message + Message.getString("action.common.error.status"), //:エラー
                    JOptionPane.ERROR_MESSAGE);

            // ステータスメッセージ
            Application.status.setMessageMain(message + Message.getString("action.common.error.status")); //:エラー
        }
    }

    /**
     * プロジェクトをクリアする
     * @throws Exception     プロジェクトのクリアエラー
     */
    public void clearProject() throws Exception {

        // 解析情報のクリア
        ProjectClearLanguageAction action = new ProjectClearLanguageAction(this.controller);
        action.clearFortranLanguage();

        // XMLツリーをクリアする
        FileTreeModel treeModel = this.controller.getXmlTreeModel();
        treeModel.setProjectFolder(null);
        treeModel.clearTreeModel();

        // プロジェクトのクリア
        this.controller.clearProject();

    }
}
