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
import jp.riken.kscope.action.FileExportExploreAction;
import jp.riken.kscope.action.FilePropertiesAction;
import jp.riken.kscope.action.ProjectAddFileAction;
import jp.riken.kscope.action.ProjectAddFolderAction;
import jp.riken.kscope.action.ProjectDeleteFileAction;
import jp.riken.kscope.action.TreeCollapseAllAction;
import jp.riken.kscope.action.TreeExpandAllAction;
import jp.riken.kscope.action.TreeExpandSelectAction;
import jp.riken.kscope.action.ViewOpenExploreBlockAction;
import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.service.AppController;

/**
 * XMLエクスプローラパネルポップアップメニュー
 * @author riken
 *
 */
public class XmlTreePopupMenu extends JPopupMenu implements PopupMenuListener, ITreePopupMenu {

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
    /** エクスプローラエクスポートアクション */
    private ActionListener actionExportExplore;

    /** ファイルを開くメニュー */
    private JMenuItem menuOpenFile;

    /**
     * コンストラクタ
     */
    public XmlTreePopupMenu() {
        // メニューの作成を行う。
        initialize();
    }


    /**
     * コンストラクタ
     * @param controller		アプリケーションコントローラ
     */
    public XmlTreePopupMenu(AppController controller) {
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
        JMenuItem menuExpandSelect = new JMenuItem(Message.getString("xmltreepopupmenu.menu.selective")); //選択展開
        this.add(menuExpandSelect);
        this.actionTreeExpandSelect = new TreeExpandSelectAction(this.controller);
        menuExpandSelect.addActionListener(this.actionTreeExpandSelect);

        // スペーサー
        this.add(new JSeparator());

        // XMLフォルダ追加...
        JMenuItem menuAddXmlFolder = new JMenuItem(Message.getString("xmltreepopupmenu.menu.addxmlfolder")); //XMLフォルダ追加
        this.add(menuAddXmlFolder);
        menuAddXmlFolder.addActionListener(new ProjectAddFolderAction(this.controller, EXPLORE_PANEL.XML));

        // XMLファイル追加...
        JMenuItem menuAddXmlFile = new JMenuItem(Message.getString("xmltreepopupmenu.menu.addxmlfile")); //XMLファイル追加
        this.add(menuAddXmlFile);
        menuAddXmlFile.addActionListener(new ProjectAddFileAction(this.controller, EXPLORE_PANEL.XML));

        // XMLファイル削除
        JMenuItem menuDeleteXmlFile = new JMenuItem(Message.getString("xmltreepopupmenu.menu.deletexmlfile")); //XMLファイル削除
        this.add(menuDeleteXmlFile);
        menuDeleteXmlFile.addActionListener(new ProjectDeleteFileAction(this.controller, EXPLORE_PANEL.XML));

        // スペーサー
        this.add(new JSeparator());

        // ファイルを開く
        menuOpenFile = new JMenuItem(Message.getString("sourcetreepopupmenu.menu.openfile")); //選択ファイルをソースビューに表示
        this.add(menuOpenFile);
        actionOpenFile = new ViewOpenExploreBlockAction(this.controller);
        menuOpenFile.addActionListener((ActionListener) actionOpenFile);

        // プロパティ
        JMenuItem menuFileProperties = new JMenuItem(Message.getString("mainmenu.project.property")); //プロパティ
        this.add(menuFileProperties);
        menuFileProperties.addActionListener(new FilePropertiesAction(this.controller));

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
     * @return		ファイルを開くリスナ
     */
    @Override
    public EventListener getActionOpenFile() {
        return actionOpenFile;
    }


    /**
     * ポップアップメニューが可視イベント
     * @param event		イベント情報
     */
    @Override
    public void popupMenuWillBecomeVisible(PopupMenuEvent event) {
        // 現在の選択ノードを取得
        SourceFile[] files = this.controller.getMainframe().getPanelExplorerView().getSelectedSourceFiles();

        // ファイルを開くメニューのイネーブルの設定
        menuOpenFile.setEnabled((files != null && files.length > 0));
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
     * エクスプローラエクスポートアクションを取得する
     * @return		エクスプローラエクスポートアクション
     */
    @Override
    public ActionListener getActionExportExplore() {
        return actionExportExplore;
    }


}
