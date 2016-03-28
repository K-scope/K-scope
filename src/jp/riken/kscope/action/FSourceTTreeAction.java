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
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;

import javax.swing.SwingUtilities;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeModel;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.Block;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.ProcedureUsage;
import jp.riken.kscope.language.ProgramUnit;
import jp.riken.kscope.language.UseState;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.service.AppController;

/**
 * Filtered-ASTへの逆引き機能
 * @author ohichi
 **/
public class FSourceTTreeAction extends ActionBase implements MouseListener {

    /**ファイルオープンアクション**/
    ViewOpenExploreBlockAction action;

    /**
     * コンストラクタ
     * @param controller    アプリケーションコントローラ
     */
    public FSourceTTreeAction(AppController controller) {
        super(controller);
        action = new ViewOpenExploreBlockAction(this.controller);
    }

    /**
     * アクションが実行可能であるかチェックする.<br/>
     * アクションの実行前チェック、メニューのイネーブルの切替を行う。<br/>
     * @return        true=アクションが実行可能
     */
    @Override
    public boolean validateAction() {

        // 選択ツリーモデルを取得する
        TreeModel modelTree = this.controller.getMainframe().getPanelExplorerView().getTreeModel();
        if (modelTree == null) {
            return false;
        }
        return true;
    }

    /**
     * アクション発生イベント
     * @param event        イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {
        // ステータスメッセージ
        final String message = Message.getString("mainmenu.edit.click");
        Application.status.setMessageMain(message);

        getSelectedLine();
    }

    /**
     * 選択行を取得し対応ノードを探す
     */
    private void getSelectedLine(){
        // 実行チェック
        if (!validateAction()) return;
        CodeLine line = this.controller.getMainframe().getPanelSourceView().getSelectedCodeLine();
        EXPLORE_PANEL view = this.controller.getMainframe().getPanelExplorerView().getSelectedPanel().getEnumPanel();

        if (view == EXPLORE_PANEL.LANGUAGE) {
            // 選択ツリーモデルを取得する
            TreeModel modelTree = this.controller.getMainframe().getPanelExplorerView().getTreeModel();
            //ルートノードの取得
            DefaultMutableTreeNode root = (DefaultMutableTreeNode)modelTree.getRoot();
            if(root != null){
                searchLanguageNode(line,root);
            }
        }else if(view == EXPLORE_PANEL.MODULE){
            // 選択ツリーモデルを取得する
            TreeModel modelTree = this.controller.getMainframe().getPanelExplorerView().getTreeModel();
            //ルートノードの取得
            DefaultMutableTreeNode root = (DefaultMutableTreeNode)modelTree.getRoot();
            if(root != null){
                searchModuleNode(line,root);
            }
        }
    }

    /**
     * プログラム構造タブのノード探索
     * @param select    選択コードライン情報
     * @param node        探索ノード
     * @return true=対応ノードの発見
     */
    private boolean searchLanguageNode(CodeLine select, DefaultMutableTreeNode node){
        int i,n;
        int start_l = select.getStartLine();
        String f_name = select.getSourceFile().getFile().getName();

        if(node.getUserObject() instanceof Block){
            //探索ノードのコード情報取得
            CodeLine line = ((Block)node.getUserObject()).get_start().getLineInfo();
            //ファイル名の比較
            if(line.getSourceFile().getFile().getName().compareTo(f_name) == 0){
                //行番号が同じ場合
                if(line.getStartLine() <= start_l && start_l <= line.getEndLine()){
                    n = node.getChildCount();
                    for(i=0;i<n;i++)
                        if(searchLanguageNode(select, (DefaultMutableTreeNode)node.getChildAt(i))){
                            return true;
                        }
                    viewSelectedBlock(node);
                    return true;
                }
            }
        }

        n = node.getChildCount();
        for(i=0;i<n;i++)
            if(searchLanguageNode(select, (DefaultMutableTreeNode)node.getChildAt(i)))
                return true;
        return false;
    }

    /**
     * モジュールタブのノード探索
     * @param select    選択コードライン情報
     * @param node        探索ノード
     * @return true=対応ノードの発見
     */
    private boolean searchModuleNode(CodeLine select, DefaultMutableTreeNode node){
        int i,n;
        int start_l = select.getStartLine();
        String f_name = select.getSourceFile().getFile().getName();

        if(node.getUserObject() instanceof UseState){
            //探索ノードのコード情報取得
            if (((UseState)node.getUserObject()).get_start() != null) {
                CodeLine line = ((UseState)node.getUserObject()).get_start().getLineInfo();
                //ファイル名の比較
                if(line.getSourceFile() != null) {
                    if(line.getSourceFile().getFile().getName().compareTo(f_name) == 0){
                        //行番号が同じ場合
                        if(line.getStartLine() == start_l){
                            viewSelectedBlock(node);
                            return true;
                        }
                    }
                }
            }
        }else if(node.getUserObject() instanceof ProgramUnit){
            CodeLine line = ((ProgramUnit)node.getUserObject()).getStartCodeLine();
            if(line != null && line.getSourceFile() != null){ //NO_Moduleを除く
                if(line.getSourceFile().getFile().getName().compareTo(f_name) == 0){
                    if(line.getStartLine() == start_l || line.getEndLine() == start_l){
                        viewSelectedBlock(node);
                        return true;
                    }
                }
            }
        }else if(node.getUserObject() instanceof VariableDefinition){
            CodeLine line = ((VariableDefinition)node.getUserObject()).getStartCodeLine();
            if(line.getSourceFile() != null) {
                if(line.getSourceFile().getFile().getName().compareTo(f_name) == 0) {
                    if(line.getStartLine() == start_l){
                        selectDef(line,(DefaultMutableTreeNode)node.getParent(), start_l);
                        return true;
                    }
                }
            }
        }

        n = node.getChildCount();
        for (i=0;i<n;i++) {
            if(searchModuleNode(select, (DefaultMutableTreeNode)node.getChildAt(i))) {
                return true;
            }
        }
        return false;
    }

    /**
     * 対象ノードを選択状態にする
     * @param node        選択ノード
     */
    private void viewSelectedBlock(DefaultMutableTreeNode node) {
        DefaultMutableTreeNode b_node = this.controller.getMainframe().getPanelExplorerView().getSelectedNode();

        if(node.getUserObject() instanceof ProcedureUsage){
            //連続して二度同じノードを選択した場合
            if(b_node == node && node.getChildCount() == 1){
                node = ((DefaultMutableTreeNode)node.getChildAt(0));
            }
        }

        this.controller.getMainframe().getPanelExplorerView().setSelectedNode(node);
        action.openFile();
    }

    /**
     * 対象ノードを選択状態にする（変数宣言の場合）
     * @param select_l    １つ目の変数のコードライン情報
     * @param node        選択ノード
     * @param line_l    選択行番号
     */
    private void selectDef(CodeLine select_l, DefaultMutableTreeNode parent, int line_n){
        int n = parent.getChildCount();
        DefaultMutableTreeNode node;
        ArrayList<IBlock> list_b = new ArrayList<IBlock>();

        CodeLine[] line_c = {select_l};
        this.controller.setSelectedBlock(line_c);

        //対応するノードをリストに追加する
        for(int i=0; i<n; i++){
            node = (DefaultMutableTreeNode)parent.getChildAt(i);
            if(node.getUserObject() instanceof VariableDefinition){
                CodeLine line = ((VariableDefinition)node.getUserObject()).getStartCodeLine();
                if(line.getStartLine() == line_n)
                    list_b.add((IBlock)node.getUserObject());
            }
        }
        this.controller.getMainframe().getPanelExplorerView().setSelectedNodes(list_b.toArray(new IBlock[0]));
    }

    /**
     * マウスクリックイベント
     * @param event            イベント情報
     */
    @Override
    public void mouseClicked(MouseEvent event) {
        // ダブルクリックチェック
        if (SwingUtilities.isLeftMouseButton(event) && event.getClickCount() == 2) {
            getSelectedLine();
        }
    }

    @Override
    public void mousePressed(MouseEvent e) {}

    @Override
    public void mouseReleased(MouseEvent e) {}

    @Override
    public void mouseEntered(MouseEvent e) {}

    @Override
    public void mouseExited(MouseEvent e) {}

}

