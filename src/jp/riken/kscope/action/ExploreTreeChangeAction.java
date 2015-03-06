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
import java.io.File;
//import java.util.List;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;

import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.EXPLORE_PANEL;
//import jp.riken.kscope.information.InformationBlock;
//import jp.riken.kscope.information.TextInfo;
import jp.riken.kscope.language.IInformation;
//import jp.riken.kscope.language.Program;
import jp.riken.kscope.language.utils.InformationEntry;
import jp.riken.kscope.language.utils.LanguageVisitor;
//import jp.riken.kscope.language.utils.VariableMemoryEntry;
import jp.riken.kscope.model.InformationModel;
import jp.riken.kscope.service.AppController;

/**
 * エクスプローラツリー変更アクションクラス
 * @author RIKEN
 */
public class ExploreTreeChangeAction extends ActionBase implements TreeSelectionListener, ChangeListener {

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     */
    public ExploreTreeChangeAction(AppController controller) {
        super(controller);
    }

    /**
     * アクション発生イベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {
        // 選択されているツリーノードの情報を表示する
        viewSelectedNodeInfo();
    }

    /**
     * ツリーの選択変更イベント
     * @param event			イベント情報
     */
    @Override
    public void valueChanged(TreeSelectionEvent event) {
    	if (event.getNewLeadSelectionPath() == null) return;

        // 選択されているツリーノードの情報を表示する
        viewSelectedNodeInfo();
    }

    /**
     * タブの選択変更イベント
     * @param event			イベント情報
     */
    @Override
    public void stateChanged(ChangeEvent event) {
        // 選択されているツリーノードの情報を表示する
        viewSelectedNodeInfo();
    }

    /**
     * 選択されているツリーノードの情報を表示する
     */
    public void viewSelectedNodeInfo() {
        EXPLORE_PANEL panel = this.controller.getMainframe().getPanelExplorerView().getSelectedEnumPanel();

        // 構造ツリー又はモジュールツリー
        if (panel == EXPLORE_PANEL.LANGUAGE || panel == EXPLORE_PANEL.MODULE) {
            // 構造ツリー,モジュールツリーのプロパティ設定アクション
            LanguagePropertiesAction action = new LanguagePropertiesAction(this.controller);
            // プロパティの設定を行う。
            action.setProperties();

            if (this.controller.getMainframe().getPanelAnalysisView().getSelectedPanel() != null
                && this.controller.getMainframe().getPanelAnalysisView().getSelectedPanel().getEnumPanel() == ANALYSIS_PANEL.INFORMATION) {
                // 付加情報の表示を行う
                setInformation();
            }
        }
        else if (panel == EXPLORE_PANEL.SOURCE || panel == EXPLORE_PANEL.XML) {
            // ファイルのプロパティ設定アクション
            FilePropertiesAction action = new FilePropertiesAction(this.controller);
            // プロパティの設定を行う。
            action.setProperties();
        }
    }

    /**
     * 付加情報の表示を行う。
     */
    public void setInformation() {

        // プロジェクトフォルダ
        File projectFolder = this.controller.getProjectModel().getProjectFolder();

        // 選択構造ツリーノード
        DefaultMutableTreeNode[] nodes = this.controller.getMainframe().getPanelExplorerView().getSelectedNodes();
        if (nodes == null) return;
        // ルートノードの場合は、子ノードとする
        if (nodes.length == 1 && nodes[0].isRoot()) {
            Object obj = nodes[0].getUserObject();
            if (obj != null && !(obj instanceof IInformation)) {
            	DefaultMutableTreeNode[] newnodes = new DefaultMutableTreeNode[nodes[0].getChildCount()];
	        	for (int i=0; i<nodes[0].getChildCount(); i++) {
	        		newnodes[i] = (DefaultMutableTreeNode)nodes[0].getChildAt(i);
	        	}
	        	nodes = newnodes;
            }
        }

        // 付加情報パネルモデルの設定
        InformationModel infoModel = this.controller.getMainframe().getPanelAnalysisView().getPanelInformation().getModel();
        // プロジェクトフォルダの設定
        infoModel.setProjectFolder(projectFolder);

        // モデルのクリア
        infoModel.clearInformation();

        // 付加情報の追加
        for (DefaultMutableTreeNode node : nodes) {
            Object obj = node.getUserObject();
            if (obj == null) continue;
            if (!(obj instanceof IInformation)) continue;

            // 付加情報を探索する
            InformationEntry entry = new InformationEntry(this.controller.getFortranLanguage());
            LanguageVisitor visitor = new LanguageVisitor(entry);
            visitor.entryInformation((IInformation)obj);
            IInformation[] infos = entry.getListInformation();
            if (infos != null) {
            	for (IInformation info : infos) {
                    // 付加情報の設定
                    infoModel.setTitle(info.toString());
                    infoModel.addInformation(info);
            	}
            }
        }
    }

}



