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
import javax.swing.JSeparator;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;

import jp.riken.kscope.Message;
import jp.riken.kscope.action.ActionBase;
import jp.riken.kscope.action.AnalysisMemoryAction;
import jp.riken.kscope.action.AnalysisReferenceAction;
import jp.riken.kscope.action.AnalysisTraceAction;
import jp.riken.kscope.action.EditClipboardCopyAction;
import jp.riken.kscope.action.FileOpenSourceFileAction;
import jp.riken.kscope.action.ProfilerAddEprofAction;
import jp.riken.kscope.action.SearchFindAction;
import jp.riken.kscope.action.SearchGrepAction;
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.common.TRACE_DIR;
import jp.riken.kscope.service.AppController;

/**
 * ソースファイルパネルポップアップメニュークラス
 * @author riken
 */
public class SourcePanelPopupMenu extends JPopupMenu implements PopupMenuListener {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** アプリケーションコントローラ */
    private AppController controller;

    /**
     * コンストラクタ
     */
    public SourcePanelPopupMenu() {
        // メニューの作成を行う。
        initialize();
    }


    /**
     * コンストラクタ
     * @param controller		アプリケーションコントローラ
     */
    public SourcePanelPopupMenu(AppController controller) {
        this.controller = controller;

        // メニューの作成を行う。
        initialize();
    }

    /**
     * メニューの作成を行う。
     */
    private void initialize() {

        // メニューの作成

        // 編集:コピー
        JMenuItem menuEditCopy = new JMenuItem(Message.getString("mainmenu.edit.copy")); //コピー
        this.add(menuEditCopy);
        menuEditCopy.addActionListener(new EditClipboardCopyAction(this.controller, FRAME_VIEW.SOURCE_VIEW));

        // スペーサー
        this.add(new JSeparator());

        // 検索:検索
        JMenuItem menuSearchFind = new JMenuItem(Message.getString("mainmenu.search.source")); //ソース検索
        this.add(menuSearchFind);
        menuSearchFind.addActionListener(new SearchFindAction(this.controller));
        // 検索:ファイル検索...
        JMenuItem menuSearchGrep = new JMenuItem(Message.getString("mainmenu.search.file")); //ファイル検索
        this.add(menuSearchGrep);
        menuSearchGrep.addActionListener(new SearchGrepAction(this.controller));

        // スペーサー
        this.add(new JSeparator());

        // 解析:トレース：前へ
        JMenuItem menuAnalysisStart = new JMenuItem(Message.getString("trace_dir.enum.start")); //トレース：開始
        this.add(menuAnalysisStart);
        menuAnalysisStart.addActionListener(new AnalysisTraceAction(this.controller, TRACE_DIR.START));

        // 解析:データフロー解析（宣言・定義・参照）
        JMenuItem menuAnalysisReference = new JMenuItem(Message.getString("mainmenu.analysis.dec-def-ref")); //宣言・定義・参照
        this.add(menuAnalysisReference);
        menuAnalysisReference.addActionListener(new AnalysisReferenceAction(this.controller, FRAME_VIEW.SOURCE_VIEW));

        // プロファイラ：測定区間設定
        JMenuItem menuViewAddEprofArea = new JMenuItem(Message.getString("languagetreepopupmenu.menu.setmeaurementrange")); // 測定区間設定
        this.add(menuViewAddEprofArea);
        menuViewAddEprofArea.addActionListener(new ProfilerAddEprofAction(this.controller, FRAME_VIEW.SOURCE_VIEW));

        // スペーサー
        this.add(new JSeparator());

        // 分析:アクセス先設定
        JMenuItem menuAnalysisMemoryAccess = new JMenuItem(Message.getString("mainmenu.analysis.access"));  // KEY520=変数アクセス先設定
        this.add(menuAnalysisMemoryAccess);
        menuAnalysisMemoryAccess.addActionListener(
        				new AnalysisMemoryAction(
        							this.controller,
        							AnalysisMemoryAction.ACTION_MODE.ACCESS_SETTING,
        							FRAME_VIEW.SOURCE_VIEW));

        // 分析:要求Byte/FLOP算出
        JMenuItem menuAnalysisMemoryCalculate = new JMenuItem(Message.getString("mainmenu.analysis.calculate"));  // KEY521=要求Byte/FLOP算出
        this.add(menuAnalysisMemoryCalculate);
        menuAnalysisMemoryCalculate.addActionListener(
						new AnalysisMemoryAction(
									this.controller,
									AnalysisMemoryAction.ACTION_MODE.MEMORY_CALCULATE,
									FRAME_VIEW.SOURCE_VIEW));

        // スペーサー
        this.add(new JSeparator());

        // ファイル:外部ツールでソースファイルを開く
        JMenuItem menuFileOpenSourceFile = new JMenuItem(Message.getString("mainmenu.file.program")); //外部ツールで開く
        this.add(menuFileOpenSourceFile);
        menuFileOpenSourceFile.addActionListener(new FileOpenSourceFileAction(this.controller, FRAME_VIEW.SOURCE_VIEW));

        this.addPopupMenuListener(this);
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
}
