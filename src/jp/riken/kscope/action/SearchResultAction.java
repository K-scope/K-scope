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

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.KEYWORD_TYPE;
import jp.riken.kscope.common.TRACE_DIR;
import jp.riken.kscope.gui.SearchResultPanel;
import jp.riken.kscope.model.SearchResultModel;
import jp.riken.kscope.service.AppController;

/**
 * 検索動作（前へ移動、次へ移動、クリア）アクション
 * @author RIKEN
 */
public class SearchResultAction extends ActionBase {

    // 検索方向
    private TRACE_DIR searchDir;

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     * @param dir			検索方向
     */
    public SearchResultAction(AppController controller, TRACE_DIR dir) {
        super(controller);
        this.searchDir = dir;
    }

    /**
     * アクションが実行可能であるかチェックする.<br/>
     * アクションの実行前チェック、メニューのイネーブルの切替を行う。<br/>
     * @return		true=アクションが実行可能
     */
    @Override
    public boolean validateAction() {
        SearchResultModel model = this.controller.getSearchResultModel();
        String text = model.getSearchText();
        return (text != null && !text.isEmpty());
    }

    /**
     * アクション発生イベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {
    	
    	//ステータスバー
    	final String message = Application.status.getMessageMain().split(":")[0];
    	String actionMsg = null;

        if (!validateAction()) return;

        // 検索結果モデル
        SearchResultPanel panel = this.controller.getMainframe().getPanelAnalysisView().getPanelSearchResult();
        if (this.searchDir == TRACE_DIR.UP) {
            panel.moveUp();
            actionMsg = Message.getString("searchresultaction.backward.status"); //前へ
        }
        else if (this.searchDir == TRACE_DIR.DOWN) {
            panel.moveDown();
            actionMsg = Message.getString("searchresultaction.forward.status"); //次へ
        }
        else if (this.searchDir == TRACE_DIR.END) {
            panel.clearModel();

            // ソースビューで検索文字列をハイライトを再設定する
            this.controller.getMainframe().getPanelSourceView().clearSearchWords(KEYWORD_TYPE.SEARCH);
            actionMsg = Message.getString("searchresultaction.clear.status"); //クリア
        }
        else if (this.searchDir == TRACE_DIR.REFRESH) {
            // ソースビューで検索文字列をハイライトを再設定する
            this.controller.setSearchKeywords();
            actionMsg = Message.getString("searchresultaction.refresh.status"); //更新
        }

        // 検索結果タブをアクティブにする
        this.controller.setSelectedAnalysisPanel(ANALYSIS_PANEL.SEARCHRESULT);
        Application.status.setMessageMain(message + ":" + actionMsg);
    }

}
