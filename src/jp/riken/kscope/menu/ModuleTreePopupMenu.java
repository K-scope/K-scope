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

import java.awt.Component;
import java.awt.event.ActionListener;
import java.util.Arrays;
import java.util.EventListener;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.JTree;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.tree.TreePath;

import jp.riken.kscope.Message;
import jp.riken.kscope.action.ActionBase;
import jp.riken.kscope.action.AnalysisOperandAction;
import jp.riken.kscope.action.AnalysisReferenceAction;
import jp.riken.kscope.action.AnalysisScopeAction;
import jp.riken.kscope.action.AnalysisVariableAction;
import jp.riken.kscope.action.EditInformationEditAction;
import jp.riken.kscope.action.FileExportExploreAction;
import jp.riken.kscope.action.FileOpenSourceFileAction;
import jp.riken.kscope.action.LanguagePropertiesAction;
import jp.riken.kscope.action.SearchTreeAction;
import jp.riken.kscope.action.TreeCollapseAllAction;
import jp.riken.kscope.action.TreeExpandAllAction;
import jp.riken.kscope.action.TreeExpandSelectAction;
import jp.riken.kscope.action.ViewOpenExploreBlockAction;
import jp.riken.kscope.action.ViewOpenLanguageTreeAction;
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.service.AppController;

/**
 * モジュールツリーポップアップメニュークラス
 * @author riken
 */
public class ModuleTreePopupMenu extends JPopupMenu implements PopupMenuListener, ITreePopupMenu {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** アプリケーションコントローラ */
    private AppController controller;

    /** すべて収納リスナ */
    private ActionListener actionTreeCollapseAll;
    /** すべて展開リスナ */
    private ActionListener actionTreeExpandAll;
    /** 選択展開リスナ */
    private ActionListener actionTreeExpandSelect;
    /** ファイルを開くリスナ */
    private EventListener actionOpenFile;

    /** ファイルを開くメニュー */
    private JMenuItem menuOpenFile;
    /** 編集:付加情報編集 */
    JMenuItem menuEditInformationEdit;

    /** 解析:変数特性一覧アクション */
    private AnalysisVariableAction actionAnalysisVariable;
    /** 付加情報編集アクション */
    private EditInformationEditAction actionEditInformation;
    /** エクスプローラエクスポートアクション */
    private ActionListener actionExportExplore;

    /**
     * コンストラクタ
     */
    public ModuleTreePopupMenu() {
        // メニューの作成を行う。
        initialize();
    }


    /**
     * コンストラクタ
     * @param controller		アプリケーションコントローラ
     */
    public ModuleTreePopupMenu(AppController controller) {
        this.controller = controller;

        // メニューの作成を行う。
        initialize();
    }

    /**
     * メニューの作成を行う。
     */
    private void initialize() {

        // メニューの作成
        // すべて収納
        JMenuItem menuCollapseAll = new JMenuItem(Message.getString("treechooserdialog.tooltip.collapseall")); //すべて収納
        this.add(menuCollapseAll);
        this.actionTreeCollapseAll = new TreeCollapseAllAction(this.controller);
        menuCollapseAll.addActionListener(this.actionTreeCollapseAll);

        // すべて展開
        JMenuItem menuExpandAll = new JMenuItem(Message.getString("treechooserdialog.tooltip.expandall")); //すべて展開
        this.add(menuExpandAll);
        this.actionTreeExpandAll = new TreeExpandAllAction(this.controller);
        menuExpandAll.addActionListener(this.actionTreeExpandAll);

        // 選択展開
        JMenuItem menuExpandSelect = new JMenuItem(Message.getString("mainmenu.view.collapse-expand.selective")); //選択展開
        this.add(menuExpandSelect);
        this.actionTreeExpandSelect = new TreeExpandSelectAction(this.controller);
        menuExpandSelect.addActionListener(this.actionTreeExpandSelect);

        // スペーサー
        this.add(new JSeparator());

        // ツリー検索
        JMenuItem menuSearchTree = new JMenuItem(Message.getString("mainmenu.search.tree")); //ツリー検索
        this.add(menuSearchTree);
        menuSearchTree.addActionListener(new SearchTreeAction(this.controller));

        // 解析:変数特性一覧
        JMenuItem menuAnalysisVariable = new JMenuItem(Message.getString("mainmenu.analysis.valiableproperty")); //変数特性一覧
        this.add(menuAnalysisVariable);
        actionAnalysisVariable = new AnalysisVariableAction(this.controller);
        menuAnalysisVariable.addActionListener(actionAnalysisVariable);

        // 解析:演算カウント
        JMenuItem menuAnalysisCount = new JMenuItem(Message.getString("mainmenu.project.config.operation")); //演算カウント
        AnalysisOperandAction actionAnalysisOperand = new AnalysisOperandAction(this.controller, FRAME_VIEW.EXPLORE_VIEW);
        this.add(menuAnalysisCount);
        menuAnalysisCount.addActionListener(actionAnalysisOperand);

        // 編集:付加情報編集
        menuEditInformationEdit = new JMenuItem(Message.getString("mainmenu.edit.info")); //付加情報編集
        this.add(menuEditInformationEdit);
        actionEditInformation = new EditInformationEditAction(this.controller, FRAME_VIEW.EXPLORE_VIEW);
        menuEditInformationEdit.addActionListener(actionEditInformation);

        // 解析:参照
        JMenuItem menuAnalysisReference = new JMenuItem(Message.getString("mainmenu.analysis.dec-def-ref")); //宣言・定義・参照
        this.add(menuAnalysisReference);
        menuAnalysisReference.addActionListener(new AnalysisReferenceAction(this.controller, FRAME_VIEW.EXPLORE_VIEW));

        // 分析:変数有効域
        JMenuItem menuAnalysisValid = new JMenuItem(Message.getString("mainmenu.analysis.valiablescope")); //変数有効域
        this.add(menuAnalysisValid);
        menuAnalysisValid.addActionListener(new AnalysisScopeAction(this.controller, FRAME_VIEW.EXPLORE_VIEW));

        // スペーサー
        this.add(new JSeparator());

        // 表示:新規構造ツリー
        JMenuItem menuViewOpenTree = new JMenuItem(Message.getString("languagetreepopupmenu.menu.newstructuretree")); //新規構造ツリー
        this.add(menuViewOpenTree);
        menuViewOpenTree.addActionListener(new ViewOpenLanguageTreeAction(this.controller));

        // ファイルを開く
        menuOpenFile = new JMenuItem(Message.getString("languagetreepopupmenu.menu.opensource")); //選択箇所をソースビューに表示
        this.add(menuOpenFile);
        actionOpenFile = new ViewOpenExploreBlockAction(this.controller);
        menuOpenFile.addActionListener((ActionListener) actionOpenFile);

        // ファイル:外部ツールでソースファイルを開く
        JMenuItem menuFileOpenSourceFile = new JMenuItem(Message.getString("mainmenu.file.program")); //外部ツールで開く
        this.add(menuFileOpenSourceFile);
        menuFileOpenSourceFile.addActionListener(new FileOpenSourceFileAction(this.controller, FRAME_VIEW.EXPLORE_VIEW));

        // プロパティ
        JMenuItem menuFileProperties = new JMenuItem(Message.getString("mainmenu.project.property")); //プロパティ
        this.add(menuFileProperties);
        menuFileProperties.addActionListener(new LanguagePropertiesAction(this.controller));

        // エクスポート（ポップアップ非表示：アクション作成のみ）
        actionExportExplore = new FileExportExploreAction(this.controller);

        this.addPopupMenuListener(this);
    }

    /**
     * ポップアップメニューをJTreeのノード上でのみ表示する。
     * @param invoker		コンポーネント
     * @param x				座標X
     * @param y				座標Y
     */
    @Override
    public void show(Component invoker, int x, int y) {
        JTree tree = (JTree)invoker;
        TreePath[] selectepath = tree.getSelectionPaths();
        if(selectepath!=null) {
            TreePath path = tree.getPathForLocation(x, y);
            if(path!=null && Arrays.asList(selectepath).contains(path)) {
                super.show(invoker, x, y);
            }
        }
    }

    /**
     * すべて収納アクションリスナを取得する
     * @return		すべて収納アクションリスナ
     */
    @Override
    public ActionListener getActionTreeCollapseAll() {
        return actionTreeCollapseAll;
    }

    /**
     * すべて展開アクションリスナを取得する
     * @return		すべて展開アクションリスナ
     */
    @Override
    public ActionListener getActionTreeExpandAll() {
        return actionTreeExpandAll;
    }

    /**
     * 選択展開アクションリスナを取得する
     * @return		選択展開アクションリスナ
     */
    @Override
    public ActionListener getActionTreeExpandSelect() {
        return actionTreeExpandSelect;
    }

    /**
     * ファイルを開くリスナを取得する。
     * @return			ファイルを開くリスナ
     */
    @Override
    public EventListener getActionOpenFile() {
        return actionOpenFile;
    }

    /**
     * エクスプローラエクスポートアクションを取得する
     * @return		エクスプローラエクスポートアクション
     */
    @Override
    public ActionListener getActionExportExplore() {
        return actionExportExplore;
    }


    /**
     * ポップアップメニュー可視イベント
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
