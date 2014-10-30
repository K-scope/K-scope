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
import java.util.HashSet;
import java.util.List;

import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.component.FilterTreeNode;
import jp.riken.kscope.data.RequiredBFResult;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.model.LanguageTreeModel;
import jp.riken.kscope.model.RequiredBFModel;
import jp.riken.kscope.properties.RequiredBFProperties;
import jp.riken.kscope.properties.VariableMemoryProperties;
import jp.riken.kscope.service.AnalysisMemoryService;
import jp.riken.kscope.service.AppController;

/**
 * 一括要求Byte/FLOPアクションクラス
 * @author ohichi
 */
public class AllAnalysisMemoryAction extends ActionBase {
	/**解析サービス**/
	private AnalysisMemoryService serviceMemory;
	/**算出結果一覧**/
	private List<RequiredBFResult> list;
    /** ツリーにおいて既に追加されたプログラム単位を格納する作業用セット */
    private HashSet<Procedure> checkList = new HashSet<Procedure>();
    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     */
    public AllAnalysisMemoryAction(AppController controller) {
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
        EXPLORE_PANEL view = this.controller.getMainframe().getPanelExplorerView().getSelectedPanel().getEnumPanel();   
	if (view != EXPLORE_PANEL.LANGUAGE) {
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
        final String message = Message.getString("mainmenu.analysis.allcalculate");
        Application.status.setMessageMain(message);

        // 選択ツリーモデルを取得する
        TreeModel modelTree = this.controller.getMainframe().getPanelExplorerView().getTreeModel();
        //ルートノードの取得
        FilterTreeNode root = (FilterTreeNode)modelTree.getRoot();
        if(root != null){
        	//プロパティの設定
        	setService();
        	this.checkList.clear();
        	list = new ArrayList<RequiredBFResult>();
        	
        	searchProcedure(root);
        	if (list.size() <= 0) return;
        	this.serviceMemory.setAnalysisPanel(list.toArray(new RequiredBFResult[0]));
        }

     }

    /**
     * ノード検索
     * @param parent		親ノード
     */
    private void searchProcedure(FilterTreeNode parent){
    	int i;
    	Procedure obj;
    	if (parent.getUserObject() instanceof Procedure){
    		obj = (Procedure)parent.getUserObject();
    		if(!(this.checkList.contains(obj))){
    			this.checkList.add(obj);
    			list.add(cal((IBlock)obj));
    		}
    	}
    	int n = parent.getChildCount();
    	for(i=0; i<n; i++)
    		searchProcedure((FilterTreeNode)parent.getChildAt(i));
    }

    /**
     * メモリ性能算出機能のプロパティ設定
     * 
     */   
    private void setService(){
       // 要求Byte/FLOP設定プロパティ
        RequiredBFProperties properitiesMemory = this.controller.getPropertiesMemory();
        // 変数アクセス先メモリ設定
        VariableMemoryProperties properitiesVariable = this.controller.getPropertiesVariable();
        RequiredBFModel modelRequired = this.controller.getRequiredByteFlopModel(); 
        LanguageTreeModel modelLanguage = this.controller.getLanguageTreeModel();
        modelRequired.setModelLanguageTree(modelLanguage);
        this.serviceMemory = new AnalysisMemoryService();
        this.serviceMemory.setProperitiesRequiredBF(properitiesMemory);
        this.serviceMemory.setPropertiesOperand(this.controller.getPropertiesOperand());
        this.serviceMemory.setModelRequiredBF(modelRequired);
        this.serviceMemory.setPropertiesVariableMemory(properitiesVariable);
        
    }
    
    /**
     * 計算実行
     * @param block		対象ブロック
     */      
    private RequiredBFResult cal(IBlock block){
    	RequiredBFResult result = this.serviceMemory.calcRequiredBF(block);
    	return result;
    }
}
    
