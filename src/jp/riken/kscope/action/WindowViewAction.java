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

import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.model.LanguageTreeModel;
import jp.riken.kscope.model.TraceResultModel;
import jp.riken.kscope.service.AppController;

/**
 * タブパネルを表示する.
 * @author riken
 */
public class WindowViewAction extends ActionBase {

    /** 表示を行うエクスプローラパネル */
    private EXPLORE_PANEL panelExplore;

    /** 表示を行う分析パネル */
    private ANALYSIS_PANEL panelAnalysis;

    /** 表示を行う構造パネルモデル */
    private LanguageTreeModel modelLanguage;

    /** 表示を行うトレースパネルモデル */
    private TraceResultModel modelTrace;

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     * @param panel			表示分析パネル
     */
    public WindowViewAction(AppController controller, ANALYSIS_PANEL panel) {
        super(controller);
        this.panelAnalysis = panel;
        this.panelExplore = null;
    }

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     * @param panel			表示エクスプローラパネル
     */
    public WindowViewAction(AppController controller, EXPLORE_PANEL panel) {
        super(controller);
        this.panelAnalysis = null;
        this.panelExplore = panel;
    }

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     * @param model			表示を行う構造パネルモデル
     */
    public WindowViewAction(AppController controller, LanguageTreeModel model) {
        super(controller);
        this.panelAnalysis = null;
        this.modelLanguage = model;
    }

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     * @param model			表示を行うトレースパネルモデル
     */
    public WindowViewAction(AppController controller, TraceResultModel model) {
        super(controller);
        this.panelAnalysis = null;
        this.modelTrace = model;
    }


    /**
     * アクション発生イベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {

        // 構造パネルを表示する
        if (this.modelLanguage != null) {
            this.controller.getMainframe().getPanelExplorerView().viewLanguageTree(this.modelLanguage);
        }
        // トレースパネルを表示する
        else if (this.modelTrace != null) {
            this.controller.getMainframe().getPanelAnalysisView().viewAnalysisTrace(this.modelTrace);
        }
        // エクスプローラパネルを表示する
        else if (this.panelExplore != null) {
            this.controller.getMainframe().getPanelExplorerView().setSelectedPanel(this.panelExplore);
        }
        // 分析パネルを表示する
        else if (this.panelAnalysis != null) {
            this.controller.getMainframe().getPanelAnalysisView().setSelectedPanel(this.panelAnalysis);
        }
    }

}
