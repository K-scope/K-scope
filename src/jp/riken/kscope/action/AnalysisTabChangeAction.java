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

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.tree.DefaultMutableTreeNode;

import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.gui.IAnalisysComponent;
import jp.riken.kscope.service.AppController;

/**
 * 分析タブ変更アクションクラス
 * @author riken
 */
public class AnalysisTabChangeAction extends ActionBase implements ChangeListener {

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     */
    public AnalysisTabChangeAction(AppController controller) {
        super(controller);
    }

    /**
     * アクション発生イベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {
        // 選択されている分析タブのキーワードをソースビューに表示する
    	changeAnalisysTab();
    }

    /**
     * タブの選択変更イベント
     * @param event			イベント情報
     */
    @Override
    public void stateChanged(ChangeEvent event) {
        // 選択されているツリーノードの情報を表示する
    	changeAnalisysTab();
    }

    /**
     * 選択分析ビューの変更イベント.
     * <p>
     * 選択されている分析タブのキーワードをソースビューに表示する.<br/>
     * プロファイラバーグラフをソースビューに設定する.<br/>
     * 付加情報の表示を行う.
     * </p>
     */
    public void changeAnalisysTab() {
        // 選択されている分析タブのキーワードをソースビューに表示する
        viewKeywardSource();
        // プロファイラバーグラフをソースビューに設定する
        setProfilerBargraph();
        // 付加情報の表示を行う。
        setInformation();
    }

    /**
     * 分析タブのキーワードをソースビューに設定する
     */
    public void viewKeywardSource() {
        IAnalisysComponent panel = this.controller.getMainframe().getPanelAnalysisView().getSelectedPanel();
        if (panel == null) {
            return;
        }
        if (panel.getEnumPanel() == ANALYSIS_PANEL.SEARCHRESULT) {
            // 検索キーワードを設定する
            this.controller.setSearchKeywords();
        }
        else if (panel.getEnumPanel() == ANALYSIS_PANEL.TRACE) {
        	// トレースキーワードを設定する
        	this.controller.setTraceKeywords();
        }
    }

    /**
     * プロファイラバーグラフをソースビューに設定する
     */
    public void setProfilerBargraph() {
        this.controller.setProfilerBargraph();

        return;
    }

    /**
     * 付加情報の表示を行う。
     */
    public void setInformation() {

        IAnalisysComponent panel = this.controller.getMainframe().getPanelAnalysisView().getSelectedPanel();
        if (panel == null) {
            return;
        }
        if (panel.getEnumPanel() != ANALYSIS_PANEL.INFORMATION) {
        	return;
        }


        // エクスプローラツリー変更アクションクラス
        ExploreTreeChangeAction action = new ExploreTreeChangeAction(this.controller);
        action.setInformation();
    }
}



