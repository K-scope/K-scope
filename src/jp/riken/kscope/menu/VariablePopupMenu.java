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

package jp.riken.kscope.menu;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;

import jp.riken.kscope.Message;
import jp.riken.kscope.action.AnalysisOperandAction;
import jp.riken.kscope.action.AnalysisReferenceAction;
import jp.riken.kscope.action.AnalysisScopeAction;
import jp.riken.kscope.action.EditInformationEditAction;
import jp.riken.kscope.action.FileExportExploreAction;
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.service.AppController;

/**
 * 変数特性一覧ポップアップメニュークラス
 * @author RIKEN
 */
public class VariablePopupMenu extends JPopupMenu implements PopupMenuListener {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** アプリケーションコントローラ */
    private AppController controller;

    /** エクスポートアクション */
    @SuppressWarnings("unused")
    private FileExportExploreAction actionExportExplore;
    /** 分析:演算カウントアクション */
    private AnalysisOperandAction actionAnalysisOperand;
    /** 分析:付加情報アクション */
    private EditInformationEditAction actionAnalysisInformation;
    /** 分析:宣言・定義・参照アクション */
    private AnalysisReferenceAction actionAnalysisReference;
    /** 分析:変数有効域アクション */
    private AnalysisScopeAction actionAnalysisScope;

    /**
     * コンストラクタ
     */
    public VariablePopupMenu() {
        // メニューの作成を行う。
        initialize();
    }


    /**
     * コンストラクタ
     * @param controller		アプリケーションコントローラ
     */
    public VariablePopupMenu(AppController controller) {
        this.controller = controller;

        // メニューの作成を行う。
        initialize();
    }

    /**
     * メニューの作成を行う。
     */
    private void initialize() {

        // メニューの作成

        // 分析:付加情報
        JMenuItem menuAnalysisInformation = new JMenuItem(Message.getString("mainmenu.edit.info"));    //付加情報編集
        actionAnalysisInformation = new EditInformationEditAction(this.controller, FRAME_VIEW.ANALYSIS_VIEW);
        this.add(menuAnalysisInformation);
        menuAnalysisInformation.addActionListener(actionAnalysisInformation);

        // 分析:演算数カウント
        JMenuItem menuAnalysisCount = new JMenuItem(Message.getString("mainmenu.project.config.operation"));//演算数カウント
        actionAnalysisOperand = new AnalysisOperandAction(this.controller, FRAME_VIEW.ANALYSIS_VIEW);
        this.add(menuAnalysisCount);
        menuAnalysisCount.addActionListener(actionAnalysisOperand);

        // 分析:参照一覧
        JMenuItem menuAnalysisReference = new JMenuItem(Message.getString("mainmenu.analysis.dec-def-ref"));//宣言・定義・参照
        actionAnalysisReference = new AnalysisReferenceAction(this.controller, FRAME_VIEW.ANALYSIS_VIEW);
        menuAnalysisReference.addActionListener(actionAnalysisReference);
        this.add(menuAnalysisReference);

        // 分析:変数有効域
        JMenuItem menuAnalysisValid = new JMenuItem(Message.getString("mainmenu.analysis.valiablescope"));//変数有効域
        actionAnalysisScope = new AnalysisScopeAction(this.controller, FRAME_VIEW.ANALYSIS_VIEW);
        menuAnalysisValid.addActionListener(actionAnalysisScope);
        this.add(menuAnalysisValid);

        // エクスポート（ポップアップ非表示：アクション作成のみ）
        actionExportExplore = new FileExportExploreAction(this.controller);

        this.addPopupMenuListener(this);
    }

    /**
     * ポップアップメニュー可視イベント
     * @param event		イベント情報
     */
    @Override
    public void popupMenuWillBecomeVisible(PopupMenuEvent event) {

    }

    /**
     * ポップアップメニューが取り消されたイベント
     * @param event		イベント情報
     */
    @Override
    public void popupMenuWillBecomeInvisible(PopupMenuEvent event) { }


    /**
     * ポップアップメニューが取り消されたイベント
     * @param event		イベント情報
     */
    @Override
    public void popupMenuCanceled(PopupMenuEvent event) { }

    /**
     * 分析:演算カウントアクションを取得する
     * @return		分析:演算カウントアクション
     */
    public AnalysisOperandAction getActionAnalysisOperand() {
        return actionAnalysisOperand;
    }

    /**
     * 分析:付加情報アクションを取得する
     * @return		分析:付加情報アクション
     */
    public EditInformationEditAction getActionAnalysisInformation() {
        return actionAnalysisInformation;
    }

    /**
     * 分析:宣言・定義・参照アクションを取得する
     * @return		分析:宣言・定義・参照アクション
     */
    public AnalysisReferenceAction getActionAnalysisReference() {
        return actionAnalysisReference;
    }

    /**
     * 分析:分析:変数有効域アクションを取得する
     * @return		分析:分析:変数有効域アクション
     */
    public AnalysisScopeAction getActionAnalysisScope() {
        return actionAnalysisScope;
    }
}
