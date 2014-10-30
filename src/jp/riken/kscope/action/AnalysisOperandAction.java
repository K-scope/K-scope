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
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.OperandTableModel;
import jp.riken.kscope.properties.OperandProperties;
import jp.riken.kscope.service.AnalysisOperandService;
import jp.riken.kscope.service.AppController;

/**
 * 演算カウントアクション
 * @author RIKEN
 */
public class AnalysisOperandAction extends ActionBase {

    /** 付加情報取得先ビュー */
    private FRAME_VIEW view;

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     * @param view 			付加情報取得先ビュー
     */
    public AnalysisOperandAction(AppController controller, FRAME_VIEW view) {
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

        // 選択ブロックを取得する
        IBlock[] blocks = getSelectedBlocks();
        if (blocks == null) return false;

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
        final String message = Message.getString("mainmenu.analysis.operation"); //演算カウント
        Application.status.setMessageMain(message);

        // 選択ブロックを取得する
        IBlock[] blocks = getSelectedBlocks();
        if (blocks == null) return;

        // 演算カウントを取得する
        analysisOperand(blocks);

    }

    /**
     * 選択ブロックを取得する
     * @return		選択ブロック
     */
    private IBlock[] getSelectedBlocks() {

        IBlock[] blocks = null;
        if (this.view == FRAME_VIEW.EXPLORE_VIEW) {
            // 選択ブロックを取得する
            blocks = this.controller.getMainframe().getPanelExplorerView().getSelectedBlocks();
        }
        else if (this.view == FRAME_VIEW.ANALYSIS_VIEW) {
            IBlock block = this.controller.getMainframe().getPanelAnalysisView().getSelectedBlock();
            if (block != null) {
                blocks = new IBlock[1];
                blocks[0] = block;
            }
        }
        return blocks;
    }

    /**
     * 演算カウントを取得する
     * @param blocks		選択ブロック
     */
    public void analysisOperand(IBlock[] blocks) {

        // フォートランデータベース
        Fortran fortran = this.controller.getFortranLanguage();
        // 演算カウントテーブルモデルを取得する
        OperandTableModel modelOperand = this.controller.getOperandTableModel();
        // 組込み関数演算カウントプロパティを取得する
        OperandProperties propertiesOperand = this.controller.getPropertiesOperand();
        // エラー情報モデル
        ErrorInfoModel errorModel = this.controller.getErrorInfoModel();

        // 演算カウントクリア
        modelOperand.clearOperand();

        // 分析サービス
        AnalysisOperandService service = new AnalysisOperandService(fortran);
        service.setErrorInfoModel(errorModel);
        service.setModelOperand(modelOperand);
        service.setPropertiesOperand(propertiesOperand);

        // 演算カウントを取得する
        service.analysisOperand(blocks);

        // 演算カウントタブをアクティブにする
        this.controller.setSelectedAnalysisPanel(ANALYSIS_PANEL.OPERAND);
    }
}
