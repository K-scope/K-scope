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
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.ReferenceModel;
import jp.riken.kscope.service.AnalysisReferenceService;
import jp.riken.kscope.service.AppController;

/**
 * 宣言・定義・参照一覧アクション
 * @author riken
 */
public class AnalysisReferenceAction extends ActionBase {

    /** 宣言・定義・参照一覧変数取得先ビュー */
    private FRAME_VIEW view;

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     * @param view 			宣言・定義・参照一覧変数取得先ビュー
     */
    public AnalysisReferenceAction(AppController controller, FRAME_VIEW view) {
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

        if (this.view != FRAME_VIEW.SOURCE_VIEW) {
            // 選択ノードを取得する
            VariableDefinition variable = getSelectedVariable();
            return (variable != null);

        }
        else {
            // ソースコードの選択行を取得する
            CodeLine line = this.controller.getMainframe().getPanelSourceView().getSelectedCodeLine();
            if (line == null) return false;
            return (line.getStatement() != null && !line.getStatement().isEmpty());
        }
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
        final String message = Message.getString("mainmenu.analysis.dec-def-ref"); //宣言・定義・参照
        Application.status.setMessageMain(message);

        if (this.view != FRAME_VIEW.SOURCE_VIEW) {
            // 選択変数
            VariableDefinition variable = getSelectedVariable();
            if (variable == null) return;
            // 参照一覧を取得する
            analysisReference(variable);
        }
        else {
            // ソースコードの選択行を取得する
            CodeLine line = this.controller.getMainframe().getPanelSourceView().getSelectedCodeLine();
            if (line == null) return;

            // 参照一覧を取得する
            analysisReference(line);

        }
    }

    /**
     * 宣言・定義・参照一覧変数を取得する
     * @return		宣言・定義・参照一覧変数
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
     * 宣言・定義・参照一覧を作成する
     * @param variable		生成変数
     */
    public void analysisReference(VariableDefinition variable) {
        if (variable == null) return;

        // フォートランデータベース
        Fortran fortran = this.controller.getFortranLanguage();
        // 参照一覧モデルを取得する
        ReferenceModel modelReference = this.controller.getReferenceModel();
        // エラー情報モデル
        ErrorInfoModel errorModel = this.controller.getErrorInfoModel();

        // 参照一覧クリア
        modelReference.clearTreeModel();

        // 分析サービス
        AnalysisReferenceService service = new AnalysisReferenceService(fortran);
        service.setErrorInfoModel(errorModel);
        service.setModelReference(modelReference);

        // 参照一覧を取得する
        service.analysisReference(variable);

        // 参照一覧タブをアクティブにする
        this.controller.setSelectedAnalysisPanel(ANALYSIS_PANEL.REFERENCE);

    }

    /**
     * 宣言・定義・参照一覧を作成する
     * @param line		選択行情報
     */
    public void analysisReference(CodeLine line) {
        if (line == null) return;

        // フォートランデータベース
        Fortran fortran = this.controller.getFortranLanguage();
        // 参照一覧モデルを取得する
        ReferenceModel modelReference = this.controller.getReferenceModel();
        // エラー情報モデル
        ErrorInfoModel errorModel = this.controller.getErrorInfoModel();

        // 参照一覧クリア
        modelReference.clearTreeModel();

        // 分析サービス
        AnalysisReferenceService service = new AnalysisReferenceService(fortran);
        service.setErrorInfoModel(errorModel);
        service.setModelReference(modelReference);

        // 参照一覧を取得する
        service.analysisReference(line);

        // 参照一覧タブをアクティブにする
        this.controller.setSelectedAnalysisPanel(ANALYSIS_PANEL.REFERENCE);

    }
}
