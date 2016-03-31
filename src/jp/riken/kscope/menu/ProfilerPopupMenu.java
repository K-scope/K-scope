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

import java.awt.event.ActionListener;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;

import jp.riken.kscope.Message;
import jp.riken.kscope.action.ActionBase;
import jp.riken.kscope.action.EditClipboardCopyAction;
import jp.riken.kscope.action.ProfilerInformationEditAction;
import jp.riken.kscope.action.ViewOpenAnalysisLineAction;
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.service.AppController;

/**
 * プロファイラテーブルパネルポップアップメニュークラス
 * @author RIKEN
 */
public class ProfilerPopupMenu extends JPopupMenu implements PopupMenuListener {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** アプリケーションコントローラ */
    private AppController controller;

    /** 分析:付加情報アクション */
    private ProfilerInformationEditAction actionAnalysisInformation;
    /** 分析結果該当個所を開くアクション */
    private ViewOpenAnalysisLineAction actionOpenAnalysisLine;
    /** クリップボードコピーアクション */
    private EditClipboardCopyAction actionAnalysisCopy;

    /**
     * コンストラクタ
     */
    public ProfilerPopupMenu() {
        // メニューの作成を行う。
        initialize();
    }


    /**
     * コンストラクタ
     * @param controller		アプリケーションコントローラ
     */
    public ProfilerPopupMenu(AppController controller) {
        this.controller = controller;

        // メニューの作成を行う。
        initialize();
    }

    /**
     * メニューの作成を行う。
     */
    private void initialize() {

        // メニューの作成

        // 分析:コピー
        JMenuItem menuAnalysisCopy = new JMenuItem(Message.getString("mainmenu.edit.copy")); //コピー
        actionAnalysisCopy = new EditClipboardCopyAction(this.controller, FRAME_VIEW.ANALYSIS_VIEW);
        this.add(menuAnalysisCopy);
        menuAnalysisCopy.addActionListener(actionAnalysisCopy);

        // 分析:付加情報
        JMenuItem menuAnalysisInformation = new JMenuItem(Message.getString("mainmenu.edit.info")); //付加情報
        actionAnalysisInformation = new ProfilerInformationEditAction(this.controller);
        this.add(menuAnalysisInformation);
        menuAnalysisInformation.addActionListener(actionAnalysisInformation);

        // メニュー非表示
        // 選択箇所を開く
        actionOpenAnalysisLine = new ViewOpenAnalysisLineAction(this.controller);

        this.addPopupMenuListener(this);
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
     * 分析:付加情報アクションを取得する
     * @return		分析:付加情報アクション
     */
    public ProfilerInformationEditAction getActionAnalysisInformation() {
        return actionAnalysisInformation;
    }


    /**
     * 該当個所を開くアクションを取得する
     * @return 該当個所を開くアクション
     */
    public ViewOpenAnalysisLineAction getActionOpenAnalysisLine() {
        return this.actionOpenAnalysisLine;
    }

    /**
     * ポップアップメニュー可視イベント.<br/>
     * アクションが実行可能かチェックする
     * @param event		イベント情報
     */
    @Override
    public void popupMenuWillBecomeVisible(PopupMenuEvent event) {

        // アクションが実行可能かチェックする
        JPopupMenu menu = (JPopupMenu) event.getSource();
        int count = menu.getComponentCount();
        for (int i=0; i<count; i++) {
            Object obj = menu.getComponent(i);
            if (!(obj instanceof JMenuItem)) continue;
            JMenuItem submenu = (JMenuItem)obj;
            ActionListener[] actions = submenu.getActionListeners();
            if (actions == null) continue;
            for (ActionListener action : actions) {
                if (action instanceof ActionBase) {
                    boolean enabled = ((ActionBase)action).validateAction();
                    submenu.setEnabled(enabled);
                }
            }
        }
    }

}
