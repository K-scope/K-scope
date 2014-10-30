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

import java.awt.event.ActionEvent;
import java.util.concurrent.Callable;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeModel;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.data.CodeLine;
//import jp.riken.kscope.data.ErrorInfo;
import jp.riken.kscope.data.FILE_TYPE;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.dialog.SearchFindDialog;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.SearchResultModel;
import jp.riken.kscope.service.AnalysisSearchService;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.FutureService;
import jp.riken.kscope.utils.StringUtils;

/**
 * 検索アクション
 * @author RIKEN
 */
public class SearchFindAction extends ActionBase {
    /** 検索サービス */
    private AnalysisSearchService service;

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     */
    public SearchFindAction(AppController controller) {
        super(controller);
    }

    /**
     * アクションが実行可能であるかチェックする.<br/>
     * アクションの実行前チェック、メニューのイネーブルの切替を行う。<br/>
     * @return		true=アクションが実行可能
     */
    @Override
    public boolean validateAction() {

        // 現在開いているファイル
        SourceFile[] files = this.controller.getMainframe().getPanelSourceView().getOpenedSourceFile();
        if (files == null) return false;

        // ソースコードの選択行を取得する
        CodeLine line = this.controller.getMainframe().getPanelSourceView().getSelectedCodeLine();
        if (line == null) return false;

        return true;
    }


    /**
     * アクション発生イベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {
        // 実行チェック
        if (!validateAction()) return;

        // ステータスメッセージ
        final String message = Message.getString("mainmenu.search.source"); //ソース検索
        Application.status.setMessageMain(message);

        // ソースコードの選択行を取得する
        CodeLine line = this.controller.getMainframe().getPanelSourceView().getSelectedCodeLine();
        if (line == null) return;

        // 選択文字列
        String text = line.getStatement();

        // 検索ダイアログを取得する。
        SearchFindDialog dialog = this.controller.getMainframe().getDialogSearchFind();
        // 検索文字列
        if (text != null) {
            dialog.setSearchText(text);
        }
        // 検索ダイアログを表示する。
        int result = dialog.showDialog();
        if (result != Constant.OK_DIALOG) return;

        // 検索文字列
        String searchText = dialog.getSearchText();
        // オプション
        boolean regex = dialog.isRegex();
        boolean word = dialog.isWord();
        boolean sensitivecase = dialog.isSensitivecase();
        boolean openfiles = dialog.isOpenFiles();

        // 検索サービス
        service = new AnalysisSearchService();
        // エラー情報モデル
        ErrorInfoModel errorModel = this.controller.getErrorInfoModel();
        service.setErrorInfoModel(errorModel);
        SearchResultModel model = this.controller.getSearchResultModel();
        service.setSearchModel(model);
        service.setSearchText(searchText);
        service.setRegex(regex);
        service.setWord(word);
        service.setSensitivecase(sensitivecase);
        // ソースツリーモデルを取得する
        TreeModel modelTree = this.controller.getMainframe().getPanelExplorerView().getPanelSourceTree().getTreeModel();
        if (FILE_TYPE.isXcodemlFile(line.getSourceFile().getFile())) {
            // XMLファイルが表示されている時は、XMLツリーを取得する
            modelTree = this.controller.getMainframe().getPanelExplorerView().getPanelXmlTree().getTreeModel();
        }
        service.setExploreTreeNode((DefaultMutableTreeNode) modelTree.getRoot());

        // 検索ファイル
        SourceFile[] files = {line.getSourceFile()};
        if (openfiles) {
            // 開いている他のファイルも検索する
            files = this.controller.getMainframe().getPanelSourceView().getOpenedSourceFile();
        }
        // 検索ファイル
        service.setSearchFiles(files);

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
                        // 検索実行
                        service.searchFile();
                        // エラーメッセージ
                        String errorMessage = service.getErrorMessage();
                        if (!StringUtils.isNullOrEmpty(errorMessage)) {
                        	return Constant.ERROR_RESULT;
                        }
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
                    String errorMessage = service.getErrorMessage();
                    if (!StringUtils.isNullOrEmpty(errorMessage)) {
                    	this.setMessage(errorMessage);
                    	Application.status.setMessageMain(message + Message.getString("action.common.error.status")); //エラー
                    }
                    else if (this.isCancelled()) {
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

        // 検索結果タブをアクティブにする
        this.controller.setSelectedAnalysisPanel(ANALYSIS_PANEL.SEARCHRESULT);
        // ソースビューで検索文字列をハイライトする
        this.controller.setSearchKeywords();
    }

}
