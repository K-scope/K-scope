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

import javax.swing.tree.DefaultMutableTreeNode;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.VariableTableModel;
import jp.riken.kscope.service.AnalysisVariableService;
import jp.riken.kscope.service.AppController;

/**
 * 参照一覧アクション
 * @author RIKEN
 */
public class AnalysisVariableAction extends ActionBase {

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     */
    public AnalysisVariableAction(AppController controller) {
        super(controller);
    }

    /**
     * アクションが実行可能であるかチェックする.<br/>
     * アクションの実行前チェック、メニューのイネーブルの切替を行う。<br/>
     * @return		true=アクションが実行可能
     */
    @Override
    public boolean validateAction() {

        // ブロックリストと変数宣言リストを取得する
        List<IBlock> blocks = getSelectedBlocks();
        List<VariableDefinition> vars = getSelectedVariableDefinitions();
        // ブロックリストと変数宣言リストのどちらかが存在していれば、OK
        if (blocks != null && blocks.size() > 0) {
            return true;
        }
        if (vars != null && vars.size() > 0) {
            return true;
        }

        return false;
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
        final String message = Message.getString("mainmenu.analysis.valiableproperty"); //変数特性一覧
        Application.status.setMessageMain(message);

        // ブロックリストと変数宣言リストを作成する
        List<IBlock> blocks = getSelectedBlocks();
        List<VariableDefinition> vars = getSelectedVariableDefinitions();
        if (blocks.size() <= 0 && vars.size() <= 0) return;

        // フォートランデータベース
        Fortran fortran = this.controller.getFortranLanguage();
        // 変数特性情報一覧モデルを取得する
        VariableTableModel modelValiable = this.controller.getVariableTableModel();
        // エラー情報モデル
        ErrorInfoModel errorModel = this.controller.getErrorInfoModel();

        // 変数特性情報一覧クリア
        modelValiable.clearVariable();

        // 分析サービス
        AnalysisVariableService service = new AnalysisVariableService(fortran);
        service.setErrorInfoModel(errorModel);
        service.setModelVariable(modelValiable);

        // 変数特性情報一覧を取得する
        // ブロックが選択を優先とする。
        if (blocks.size() > 0) {
            service.analysisVariable(blocks.toArray(new IBlock[0]));
            this.controller.setLastVariable(blocks, null);
        }
        else if (vars.size() > 0) {
            service.analysisVariable(vars.toArray(new VariableDefinition[0]));
            this.controller.setLastVariable(null, vars);
        }

        // 変数特性情報一覧タブをアクティブにする
        this.controller.setSelectedAnalysisPanel(ANALYSIS_PANEL.VALIABLE);

    }

    /**
     * 変数特性情報一覧を更新する.
     */
    public void refresh(){
        // フォートランデータベース
        Fortran fortran = this.controller.getFortranLanguage();
        // 変数特性情報一覧モデルを取得する
        VariableTableModel modelValiable = this.controller.getVariableTableModel();
        // エラー情報モデル
        ErrorInfoModel errorModel = this.controller.getErrorInfoModel();

        // 変数特性情報一覧クリア
        modelValiable.clearVariable();

        // 分析サービス
        AnalysisVariableService service = new AnalysisVariableService(fortran);
        service.setErrorInfoModel(errorModel);
        service.setModelVariable(modelValiable);

        // 現在表示中のデータセットを取得する
        List<IBlock> lastBlocks = this.controller.getLastVariableBlocks();
        List<VariableDefinition> lastVars = this.controller.getLastVariableVars();
    	if (lastBlocks != null && lastBlocks.size() > 0) {
    		service.analysisVariable(lastBlocks.toArray(new IBlock[0]));
    	}
    	else if (lastVars != null && lastVars.size() > 0) {
    		service.analysisVariable(lastVars.toArray(new VariableDefinition[0]));
    	}
    }

    /**
     * 選択ブロックを取得する
     * @return		選択ブロック
     */
    private List<IBlock> getSelectedBlocks() {

        // 選択ノードを取得する
        DefaultMutableTreeNode[] nodes = this.controller.getMainframe().getPanelExplorerView().getSelectedNodes();
        if (nodes == null) return null;

        // ブロックリストを作成する
        List<IBlock> blocks = new ArrayList<IBlock>();
        List<VariableDefinition> vars = new ArrayList<VariableDefinition>();
        for (DefaultMutableTreeNode node : nodes) {
            Object obj = node.getUserObject();
            if (obj instanceof VariableDefinition) {
                vars.add((VariableDefinition) obj);
            } else if (obj instanceof IBlock) {
                blocks.add((IBlock) obj);
            }
        }
        if (blocks.size() <= 0 && vars.size() <= 0) return null;

        return blocks;
    }

    /**
     * 選択変数宣言文を取得する
     * @return		選択変数宣言文
     */
    private List<VariableDefinition> getSelectedVariableDefinitions() {

        // 選択ブロックを取得する
        DefaultMutableTreeNode[] nodes = this.controller.getMainframe().getPanelExplorerView().getSelectedNodes();
        if (nodes == null) return null;

        // 変数宣言リストを作成する
        List<VariableDefinition> vars = new ArrayList<VariableDefinition>();
        for (DefaultMutableTreeNode node : nodes) {
            Object obj = node.getUserObject();
            if (obj instanceof VariableDefinition) {
                vars.add((VariableDefinition) obj);
            }
        }
        if (vars.size() <= 0) return null;

        return vars;
    }
}
