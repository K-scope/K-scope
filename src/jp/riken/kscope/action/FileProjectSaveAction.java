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
import java.awt.HeadlessException;
import java.awt.event.ActionEvent;
import java.io.File;
import java.util.concurrent.Callable;

import javax.swing.JOptionPane;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.ProjectModel;
import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.FutureService;
import jp.riken.kscope.service.LanguageService;
import jp.riken.kscope.service.ProjectService;

/**
 * プロジェクトの保存アクション
 * @author riken
 */
public class FileProjectSaveAction extends ActionBase {

    /** データベースの構築、探索を行うクラス */
    private LanguageService service;

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     */
    public FileProjectSaveAction(AppController controller) {
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
        String message = Message.getString("mainmenu.file.saveproject"); //プロジェクトの保存
        Application.status.setMessageMain(message);

        // アクションチェック
        if (!validateAction()) {
            Application.status.setMessageMain(message + Message.getString("action.common.unavailable.status")); //:不可
            return;
        }

        // 親Frameの取得を行う。
        Frame frame = getWindowAncestor( event );

        // 確認メッセージを表示する。
        int option = JOptionPane.showConfirmDialog(frame,
                     Message.getString("fileprojectsaveaction.save.confirm.dialog.message"), //プロジェクトを保存しますか？
                     message, //プロジェクトの保存
                     JOptionPane.OK_CANCEL_OPTION,
                     JOptionPane.WARNING_MESSAGE);

        if (option != JOptionPane.OK_OPTION) {
            Application.status.setMessageMain(message + Message.getString("action.common.cancel.status")); //:キャンセル
            return;
        }

        saveProject(frame);
    }

	/**
	 * @param message
	 * @param frame
	 * @throws HeadlessException
	 */
	public void saveProject(Frame frame)	throws HeadlessException {
	// プロジェクトサービス
        ProjectService service = new ProjectService(this.controller.getProjectModel());
        // キーワードプロパティ
        service.setPropertiesKeyword(this.controller.getPropertiesKeyword());
        // 外部ツールプロパティ
        service.setPropertiesExtension(this.controller.getPropertiesExtension());
        // 演算カウントプロパティ
        service.setPropertiesOperand(this.controller.getPropertiesOperand());
        // ソースビュー設定プロパティ
        service.setPropertiesSource(this.controller.getPropertiesSource());
        // プロファイラ設定プロパティ
        service.setPropertiesProfiler(this.controller.getPropertiesProfiler());
        // プロジェクト設定プロパティ
        service.setPropertiesProject(this.controller.getPropertiesProject());
        // 要求Byte/FLOP設定プロパティ
        service.setPropertiesMemory(this.controller.getPropertiesMemory());
        //
        service.setPropertiesDIAAS(this.controller.getPropertiesDIAAS());
        // エラーモデル
        service.setErrorInfoModel(this.controller.getErrorInfoModel());

        try {
            // プロジェクト保存
            File projectFolder = this.controller.getProjectModel().getProjectFolder();
            service.saveProject(projectFolder);

            // Languageクラスのシリアライズを行う
            // settingsフォルダ
            File settingsFolder = new File(projectFolder.getAbsoluteFile() + File.separator + KscopeProperties.SETTINGS_FOLDER);
            writeLanguage(settingsFolder);

        } catch (Exception e) {
            e.printStackTrace();
            String message = Message.getString("mainmenu.file.saveproject"); //プロジェクトの保存
            // エラーメッセージ
            JOptionPane.showMessageDialog(frame,
                    Message.getString("fileprojectsaveaction.save.failed.dialog.message"), //プロジェクトの保存に失敗しました。
                    message + Message.getString("dialog.common.error"), //エラー
                    JOptionPane.ERROR_MESSAGE);

            // ステータスメッセージ
            Application.status.setMessageMain(message + Message.getString("action.common.error.status")); //:エラー
        }
    }

    /**
     * Languageクラスのシリアライズを行う
     * @param folder		Languageクラスのシリアライズフォルダ
     */
    public void writeLanguage(final File folder) {
        // ステータスメッセージ
        final String message = Message.getString("mainmenu.file.saveproject"); //プロジェクトの保存

        // フォートランデータベース
        Fortran fortran = this.controller.getFortranLanguage();
        // エラー情報モデル
        ErrorInfoModel errorModel = this.controller.getErrorInfoModel();

        // 構造解析サービス
        service = new LanguageService(fortran);
        // エラー情報モデルを設定する。
        service.setErrorInfoModel(errorModel);

        // スレッドタスクサービスの生成を行う。
        FutureService<Integer> future = new FutureService<Integer>(
                /**
                 * スレッド呼出クラス
                 */
                new Callable<Integer>() {
                    /**
                     * スレッド実行を行う
                     */
                    @Override
                    public Integer call() {
                        try {
                            // シリアライズ実行
                            service.writeLanguage(folder);
                            return Constant.SUCCESS_RESULT;
                        } catch (Exception e) {
                            e.printStackTrace();
                            return Constant.ERROR_RESULT;
                        }
                    }
                }
                ) {
                    /**
                     * スレッド実行完了.<br/>
                     * キャンセルされた時の後処理を行う。
                     */
                    @Override
                    protected void done() {
                        // キャンセルによる終了であるかチェックする。
                        if (this.isCancelled()) {
                            Application.status.setMessageMain(message + Message.getString("action.common.cancel.status")); //:キャンセル
                        }
                        else {
                            Application.status.setMessageMain(message + Message.getString("action.common.done.status")); //:完了
                        }

                        // サービス実行の停止
                        if (service != null) {
                            service.cancelRunning();
                        }

                        super.done();
                    }

        };
        // ステータスメッセージクリア
        Application.status.setMessageStatus(null);

        // スレッドタスクにコントローラをリスナ登録する：スレッド完了時の呼出の為
        future.addPropertyChangeListener(this.controller);
        this.controller.setThreadFuture(future);

        // プログレスダイアログを表示する
        WindowProgressAction progress = new WindowProgressAction(this.controller);
        progress.showProgressDialog();
        Application.status.setProgressStart(true);

        // スレッド起動
        new Thread(future).start();
    }

}
