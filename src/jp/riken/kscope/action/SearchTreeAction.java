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
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.data.ErrorInfo;
import jp.riken.kscope.dialog.SearchTreeDialog;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.service.AnalysisSearchService;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.FutureService;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * ツリー検索アクション
 * @author riken
 */
public class SearchTreeAction extends ActionBase {
	/** 検索サービス */
	private AnalysisSearchService service;
	/** 検索パネル */
	private EXPLORE_PANEL searchPanel;

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     */
    public SearchTreeAction(AppController controller) {
        super(controller);
    }

    /**
     * アクションが実行可能であるかチェックする.<br/>
     * アクションの実行前チェック、メニューのイネーブルの切替を行う。<br/>
     * @return		true=アクションが実行可能
     */
    @Override
    public boolean validateAction() {

        // 選択ツリーモデルを取得する
        TreeModel modelTree = this.controller.getMainframe().getPanelExplorerView().getTreeModel();
        if (modelTree == null) {
            return false;
        }
        TreeNode root = (TreeNode)modelTree.getRoot();
        if (root.getChildCount() <= 0) {
            return false;
        }

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
        final String message = Message.getString("mainmenu.search.tree"); //ツリー検索
        Application.status.setMessageMain(message);

        // 選択ツリーモデルを取得する
        TreeModel modelTree = this.controller.getMainframe().getPanelExplorerView().getTreeModel();
        this.searchPanel = this.controller.getMainframe().getPanelExplorerView().getSelectedEnumPanel();
        // 選択ノード
        TreeNode[] selectedNodes = this.controller.getMainframe().getPanelExplorerView().getSelectedNodes();

        // 検索ダイアログを表示する。
        SearchTreeDialog dialog = this.controller.getMainframe().getDialogSearchTree();
        dialog.setReferenceTreeModel(this.searchPanel, modelTree);
        dialog.setSelectedTreeNodes(selectedNodes);

        int result = dialog.showDialog();
        if (result != Constant.OK_DIALOG) return;

        // 検索文字列
        String searchText = dialog.getSearchText();
        // 検索ノード
        TreeNode[] nodes = dialog.getSelectedTreeNodes();
        // オプション
        boolean regex = dialog.isSearchRegex();
        boolean word = dialog.isSearchWord();
        boolean sensitivecase = dialog.isSearchSensitivecase();

        // 検索ノードの子ノードを削除する
        List<DefaultMutableTreeNode> list = new ArrayList<DefaultMutableTreeNode>();
        if (nodes != null) {
            for (TreeNode node : nodes) {
                if (!(node instanceof DefaultMutableTreeNode)) continue;
                if (isChildNode(nodes, (DefaultMutableTreeNode)node)) {
                    // ノードリストのいずれかの子ノードである
                    continue;
                }
                if (list.contains(node)) continue;

                // 子ノードではないので追加する
                list.add((DefaultMutableTreeNode)node);
            }
        }

        // 検索サービス
        service = new AnalysisSearchService();
        // エラー情報モデル
        ErrorInfoModel errorModel = this.controller.getErrorInfoModel();
        service.setErrorInfoModel(errorModel);
        // 検索結果モデル
        service.setSearchModel(this.controller.getSearchResultModel());

        // 検索条件
        service.setSearchText(searchText);
        service.setRegex(regex);
        service.setWord(word);
        service.setSensitivecase(sensitivecase);
        if (list != null && list.size() > 0) {
            service.setSearchNodes(list.toArray(new TreeNode[0]));
        }
        // 検索元エクスプローラツリーノード
        service.setExploreTreeNode((DefaultMutableTreeNode)modelTree.getRoot());

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
                        // 構造ツリー以外はツリー検索を行う.
                        if (searchPanel != EXPLORE_PANEL.LANGUAGE) {
                	        // 検索実行
                	        service.searchTree();
                        }
                        else {
                	        // 検索実行
                	        service.searchLanguage();
                        }
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

    }


    /**
     * 子ノードがノードリストのいずれかの子ノードであるかチェックする
     * @param nodes			ノードリスト
     * @param childnode		子ノード
     * @return				true=子ノードがノードリストのいずれかの子ノードである
     */
    private boolean isChildNode(TreeNode[] nodes, DefaultMutableTreeNode childnode) {

        for (TreeNode node : nodes) {
            if (!(node instanceof DefaultMutableTreeNode)) continue;
            if (node == childnode) continue;
            if (((DefaultMutableTreeNode)node).getUserObject() == childnode.getUserObject()) continue;
            if (SwingUtils.isChildNode((DefaultMutableTreeNode)node, childnode)) {
                return true;
            }
        }

        return false;
    }
}
