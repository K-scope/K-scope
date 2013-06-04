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

import javax.swing.JOptionPane;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.service.AppController;

/**
 * 構造解析クリアアクション
 * @author riken
 */
public class ProjectClearLanguageAction extends ActionBase {

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     */
    public ProjectClearLanguageAction(AppController controller) {
        super(controller);
    }

    /**
     * コンストラクタ
     */
    public ProjectClearLanguageAction() {
        super();
    }

    /**
     * アクション発生イベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {
        // ステータスメッセージ
        final String message = Message.getString("mainmenu.project.clearanalysis"); //構造解析クリア
        Application.status.setMessageMain(message);

        // 確認メッセージを表示する。
        int option = JOptionPane.showConfirmDialog(this.controller.getMainframe(),
                  Message.getString("projectclearlanguageaction.clear.confirmdialog.message"), //解析結果、分析結果をクリアしますが、よろしいですか？
                  Message.getString("projectclearlanguageaction.clear.confirmdialog.title"), //構造情報のクリア
                  JOptionPane.OK_CANCEL_OPTION,
                  JOptionPane.WARNING_MESSAGE);
        if (option != JOptionPane.OK_OPTION) {
        	Application.status.setMessageMain(message +
        			Message.getString("action.common.cancel.status")); //キャンセル
            return;
        }

        // フォートランデータベースクリアを行う。
        clearFortranLanguage();
    	Application.status.setMessageMain(message +
    			Message.getString("action.common.done.status")); //完了
    }

    /**
     * フォートランデータベースクリアを行う。
     */
    public void clearFortranLanguage() {

        // クリア
        // フォートランデータベース
        this.controller.clearFortranLanguage();

        // ツリーモデル
        this.controller.getMainframe().getPanelExplorerView().clearTreeModel();

        // 分析情報クリア
        this.controller.getMainframe().getPanelAnalysisView().clearModels();
        // コンソールタブを閉じる
        this.controller.getMainframe().getPanelAnalysisView().closeTab(ANALYSIS_PANEL.CONSOLE);

        // ソースビュークリア
        this.controller.getMainframe().getPanelSourceView().closeAllTabs();
    }
}
