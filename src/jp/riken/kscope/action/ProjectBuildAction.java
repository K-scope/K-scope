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
import java.util.List;
import java.util.concurrent.Callable;

import javax.swing.JOptionPane;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.data.FILE_TYPE;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.FileTreeModel;
import jp.riken.kscope.model.LanguageTreeModel;
import jp.riken.kscope.model.ModuleTreeModel;
//import jp.riken.kscope.model.ProjectModel;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.FutureService;
import jp.riken.kscope.service.LanguageService;
import jp.riken.kscope.xcodeml.XcodeMLParserStax;

/**
 * 構造解析実行アクション.<br/>
 * 構造解析を別スレッドで行う。
 * @author RIKEN
 */
public class ProjectBuildAction extends ActionBase {
    /** データベースの構築、探索を行うクラス */
    private LanguageService service;
    /** プロジェクトのクリアアクション */
    private ProjectClearLanguageAction clearAction;

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     */
    public ProjectBuildAction(AppController controller) {
        super(controller);
    }

    /**
     * アクションが実行可能であるかチェックする.<br/>
     * アクションの実行前チェック、メニューのイネーブルの切替を行う。<br/>
     * @return		true=アクションが実行可能
     */
    @Override
    public boolean validateAction() {

        // 構造解析実行メニューのイネーブル切替
        // XMLファイルリスト
        List<SourceFile> listXml = this.controller.getProjectModel().getListSelectedFile();
        if (listXml == null || listXml.size() <= 0) {
            return false;
        }

        int count = 0;
        // XMLファイルが存在しているか？
        for (SourceFile file : listXml) {
            if (file == null) continue;
            if (file.getFile() == null) continue;
            if (FILE_TYPE.isXcodemlFile(file.getFile())) {
                if (file.getFile().exists()) {
                    count++;
                }
            }
        }
        if (count <= 0) return false;

        // スレッドタスクの実行状態をチェックする
        return this.controller.isThreadTaskDone();
    }


    /**
     * アクション発生イベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {
        // ステータスメッセージ
        final String message = Message.getString("mainmenu.project.startanalysis"); //構造解析実行
        Application.status.setMessageMain(message);

        // 親Frameの取得を行う。
        Frame frame = getWindowAncestor( event );

        // XMLファイルリスト
        List<SourceFile> listXml = this.controller.getProjectModel().getListSelectedFile();
        if (listXml == null || listXml.size() <= 0) {
            // エラーメッセージ
            JOptionPane.showMessageDialog(frame,
                    Message.getString("projectbuildaction.build.errdialog.xmlnotexist.message"), //解析対象XMLファイルがありません。
                    Message.getString("projectbuildaction.build.errdialog.xmlnotexist.title"), //XMLファイルエラー
                    JOptionPane.ERROR_MESSAGE);
            Application.status.setMessageMain(message + Message.getString("action.common.error.status")); //:エラー
            return;
        }
        // エラー情報モデル
        ErrorInfoModel errorModel = this.controller.getErrorInfoModel();
        boolean error = false;
        for (SourceFile file : listXml) {
            if (file == null) continue;
            if (file.getFile() == null) continue;
            if (!file.getFile().exists()) {
                errorModel.addErrorInfo(Message.getString("projectbuildaction.build.errinfo.notexist", file.getFile().getPath())); //が存在しません。
                error = true;
            }
        }
        if (error) {
            return;
        }

        // フォートランデータベースをクリアする
        clearAction = new ProjectClearLanguageAction(this.controller);
        clearAction.clearFortranLanguage();

        // フォートランデータベース
        Fortran fortran = this.controller.getFortranLanguage();
        // XMLパーサの作成
        XcodeMLParserStax xmlParser = new XcodeMLParserStax();
        // ソースツリーモデル
        FileTreeModel fileModel = this.controller.getSourceTreeModel();
        // XMLツリーモデル
        FileTreeModel xmlModel = this.controller.getXmlTreeModel();
        // 構造ツリーモデル
        LanguageTreeModel languageModel = this.controller.getLanguageTreeModel();
        // モジュールツリーモデル
        ModuleTreeModel moduleModel = this.controller.getModuleTreeModel();

        // 構造解析サービス
        service = new LanguageService(listXml.toArray(new SourceFile[0]), fortran, xmlParser);
        // 構造ツリーモデルを設定する
        service.setLanguageTreeModel(languageModel);
        // モジュールツリーモデルを設定する
        service.setModuleTreeModel(moduleModel);
        // エラー情報モデルを設定する。
        service.setErrorInfoModel(errorModel);
        // ソースツリーモデルを設定する。
        service.setSourceTreeModel(fileModel);
        // XMLツリーモデルを設定する。
        service.setXmlTreeModel(xmlModel);
        // プロジェクトフォルダを設定する
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
                            // 解析実行
                            service.parseSourceFile();
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
                            // フォートランデータベースをクリアする
                            clearAction.clearFortranLanguage();
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

        // スレッド起動
        new Thread(future).start();
        // 構造ツリーをアクティブにする
        this.controller.getMainframe().getPanelExplorerView().setSelectedPanel(EXPLORE_PANEL.LANGUAGE);
    }

}
