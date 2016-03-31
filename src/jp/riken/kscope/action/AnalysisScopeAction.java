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

import javax.swing.tree.DefaultMutableTreeNode;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.ScopeModel;
import jp.riken.kscope.service.AnalysisScopeService;
import jp.riken.kscope.service.AppController;

/**
 * 変数有効域アクション
 * @author RIKEN
 */
public class AnalysisScopeAction extends ActionBase {

    /** 変数有効域変数取得先ビュー */
    private FRAME_VIEW view;

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     * @param view 			変数有効域変数取得先ビュー
     */
    public AnalysisScopeAction(AppController controller, FRAME_VIEW view) {
        super(controller);
        this.view = view;
    }

    /**
     * アクションが実行可能であるかチェックする.<br/>
     * アクションの実行前チェック、メニューのイネーブルの切替を行う。<br/>
     * @return		true=アクションが実行可能
     */
    @Override
    public boolean validateAction() {

        // 選択ノードを取得する
        VariableDefinition variable = getSelectedVariable();
        if (variable == null) return false;

        return true;
    }

    /**
     * アクション発生イベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {
        // アクションチェック
        if (!validateAction()) {
            return;
        }

        // ステータスメッセージ
        final String message = Message.getString("mainmenu.analysis.variablescope"); //変数有効域
        Application.status.setMessageMain(message);

        // 選択変数
        VariableDefinition variable = getSelectedVariable();
        if (variable == null) return;

        // 変数有効域を取得する
        analysisScope(variable);
    }

    /**
     * 変数有効域の対象変数を取得する
     * @return		変数有効域の対象変数
     */
    private VariableDefinition getSelectedVariable() {

        VariableDefinition variable = null;
        if (this.view == FRAME_VIEW.EXPLORE_VIEW) {
            // 選択ノードを取得する
            DefaultMutableTreeNode node = this.controller.getMainframe().getPanelExplorerView().getSelectedNode();
            if (node == null || node.getUserObject() == null) return null;

            // 選択ノード
            Object obj = node.getUserObject();
            if (obj instanceof VariableDefinition) {
                variable = (VariableDefinition) obj;
            }
        }
        else if (this.view == FRAME_VIEW.ANALYSIS_VIEW) {
            variable = this.controller.getVariableTableModel().getSelectedVariable();
        }
        return variable;
    }


    /**
     * 変数有効域テーブルを作成する
     * @param variable		変数有効域の対象変数
     */
    public void analysisScope(VariableDefinition variable) {
        if (variable == null) return;

        // フォートランデータベース
        Fortran fortran = this.controller.getFortranLanguage();
        // 変数有効域モデルを取得する
        ScopeModel modelScope = this.controller.getScopeModel();
        // エラー情報モデル
        ErrorInfoModel errorModel = this.controller.getErrorInfoModel();

        // 変数有効域クリア
        modelScope.clear();

        // 分析サービス
        AnalysisScopeService service = new AnalysisScopeService(fortran);
        service.setErrorInfoModel(errorModel);
        service.setModelScope(modelScope);

        // 変数有効域を取得する
        service.analysisScope(variable);

        // 変数有効域タブをアクティブにする
        this.controller.setSelectedAnalysisPanel(ANALYSIS_PANEL.SCOPE);

    }

}
