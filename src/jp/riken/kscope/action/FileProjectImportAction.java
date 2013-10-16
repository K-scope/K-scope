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
import java.util.concurrent.Callable;

import javax.swing.JOptionPane;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.ProjectModel;
import jp.riken.kscope.model.ReplacementResultTableModel;
import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.FutureService;
import jp.riken.kscope.service.LanguageService;
import jp.riken.kscope.utils.SwingUtils;


/**
 * 構造情報の差替アクションクラス. <br/>
 * 構造情報の差替の削除 at 2013/04/10 by @hira
 * @author riken
 * @deprecated    「構造情報の差替」の廃止に伴う未使用クラス    at 2013/04/12 by @hira
 */
@Deprecated
public class FileProjectImportAction extends ActionBase {

    /** データベースの構築、探索を行うクラス */
    private LanguageService service;

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     */
    public FileProjectImportAction(AppController controller) {
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
        String message = Message.getString("mainmenu.file.replaceproject"); //構造情報の差替
        Application.status.setMessageMain(message);

        // 親Frameの取得を行う。
        Frame frame = getWindowAncestor( event );

        // エラー情報モデル
        ErrorInfoModel errorModel = this.controller.getErrorInfoModel();

        // プロジェクトフォルダ
        File projectfolder = this.controller.getProjectModel().getProjectFolder();
        if (projectfolder == null) {
            errorModel.addErrorInfo(Message.getString("fileprojectimportaction.replace.notexist.errinfo")); //プロジェクトが作成されていません。
            Application.status.setMessageMain(message +
            		Message.getString("action.common.error.status")); //エラー
            return;
        }
        String folder = projectfolder != null ? projectfolder.getAbsolutePath() : null;

        // プロジェクトフォルダ選択ダイアログを表示する。
        File[] selected = SwingUtils.showOpenFolderDialog(
                frame,
                Message.getString("fileprojectimportaction.selectbaseprojct.dialog.title"), //差替元のプロジェクトフォルダの選択
                folder, true);
        if (selected == null || selected.length <= 0) {
            Application.status.setMessageMain(message + Message.getString("action.common.cancel.status")); //:キャンセル
            return;
        }

        File selectedFolder = selected[0];
        // プロジェクトフォルダのチェック
        // プロジェクト設定ファイル
        File projectFile = new File(selectedFolder.getAbsolutePath() + File.separator + KscopeProperties.PROJECT_FILE);
        if (!projectFile.exists()) {
            // エラーメッセージ
            JOptionPane.showMessageDialog(
                    frame,
                    Message.getString("fileprojectimportaction.replace.notproject.dialog.message"), //プロジェクトフォルダではありません。
                    message + Message.getString("dialog.common.error"), //エラー
                    JOptionPane.ERROR_MESSAGE);
            Application.status.setMessageMain(message +
            		Message.getString("action.common.error.status")); //:エラー
            return;
        }
        // 現在、開いているプロジェクトフォルダと同じであるか？
        if (selectedFolder.equals(projectfolder)) {
            // エラーメッセージ
            JOptionPane.showMessageDialog(
                    frame,
                    Message.getString("fileprojectimportaction.replace.currentproject.dialog.message"), //現在開いているプロジェクトフォルダです。
                    message + Message.getString("dialog.common.error"), //エラー
                    JOptionPane.ERROR_MESSAGE);
            Application.status.setMessageMain(message +
            		Message.getString("action.common.error.status")); //エラー
            return;
        }

        // Languageクラスのデシリアライズを行う
        // settingsフォルダ
        File settingsFolder = new File(selectedFolder.getAbsoluteFile() + File.separator + KscopeProperties.SETTINGS_FOLDER);
        importLanguage(settingsFolder);

        Application.status.setMessageMain(message +
        		Message.getString("action.common.done.status")); //:完了

        return;
    }

    /**
     * Languageクラスの差替を行う
     * @param folder		Languageクラスのシリアライズフォルダ
     */
    public void importLanguage(final File folder) {
        // ステータスメッセージ
        final String message = Message.getString("mainmenu.file.replaceproject"); //構造情報の差替

        // フォートランデータベース
        Fortran fortran = this.controller.getFortranLanguage();
        // エラー情報モデル
        ErrorInfoModel errorModel = this.controller.getErrorInfoModel();
        // 差替結果モデル
        ReplacementResultTableModel modelReplace = this.controller.getReplaceTableModel();
        // 差替結果モデルをクリアする
        modelReplace.clearReplacement();

        // 構造解析サービス
        service = new LanguageService(fortran);
        // エラー情報モデルを設定する。
        service.setErrorInfoModel(errorModel);
        // 差替結果モデルを設定する。
        service.setModelReplace(modelReplace);
        // プロジェクトパスを設定する。
        service.setProjectFolder(this.controller.getProjectModel().getProjectFolder());

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
                            // デシリアライズ実行
                            service.importLanguage(folder);
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

        // スレッド起動
        new Thread(future).start();

        // 差替結果タブをアクティブにする
        this.controller.setSelectedAnalysisPanel(ANALYSIS_PANEL.REPLACE);
    }


}
