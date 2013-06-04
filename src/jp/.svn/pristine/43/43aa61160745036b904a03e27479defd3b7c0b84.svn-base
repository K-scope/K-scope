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
package jp.riken.kscope.gui;

import jp.riken.kscope.common.ANALYSIS_PANEL;

/**
 * 分析情報パネル基底クラス
 * @author riken
 */
public abstract class AnalisysPanelBase extends javax.swing.JPanel implements ITabComponent {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** 分析情報パネル識別子 */
    private ANALYSIS_PANEL enumPanel;

    /** 親コンポーネント */
    private ITabComponent parentCompornent = null;

    /**
     * コンストラクタ
     * @param panel		分析情報パネル識別子
     */
    public AnalisysPanelBase(ANALYSIS_PANEL panel) {
        this.enumPanel = panel;
    }

    /**
     * コンストラクタ
     */
    public AnalisysPanelBase() {
        super();
    }

    /**
     * 分析情報パネル識別子を設定する
     * @return enumPanel		分析情報パネル識別子
     */
    public ANALYSIS_PANEL getEnumPanel() {
        return this.enumPanel;
    }

    /**
     * 親コンポーネントを取得する.
     * @return		親コンポーネント
     */
    @Override
    public ITabComponent getParentComponent() {
        return this.parentCompornent;
    }

    /**
     * 親コンポーネントを設定する.
     * @param component		親コンポーネント
     */
    @Override
    public void setParentComponent(ITabComponent component) {
        this.parentCompornent = component;
    }

    /**
     * タブを閉じる
     */
    @Override
    public void closeTabComponent() {
        // 親のタブパインにてタブを閉じる。
        if (this.parentCompornent != null) {
            this.parentCompornent.closeTabComponent();
        }
    }


    /**
     * 自身の分析情報パネルをアクティブにする.
     */
    public void setSelectedPanel() {
    	if (this.parentCompornent instanceof AnalysisView) {
    		AnalysisView view = (AnalysisView)this.parentCompornent;
    		view.setSelectedPanel(this.enumPanel);
    	}
    }

}
