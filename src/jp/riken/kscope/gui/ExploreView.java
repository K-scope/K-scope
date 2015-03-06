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

import java.awt.Component;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JPanel;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;

import jp.riken.kscope.Message;
import jp.riken.kscope.action.ExploreTreeChangeAction;
import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.common.FILTER_TYPE;
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.menu.ITreePopupMenu;
import jp.riken.kscope.menu.LanguageTreePopupMenu;
import jp.riken.kscope.menu.ModuleTreePopupMenu;
import jp.riken.kscope.model.LanguageTreeModel;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.utils.SwingUtils;

/**
 * エクスプローラビュークラス.<br/>
 * 構造、モジュール、ソース、XMLタブを配置する。
 * @author RIKEN
 */
public class ExploreView extends ClosableTabbedPane implements ITabComponent, PropertyChangeListener {

    /** シリアル番号  */
    private static final long serialVersionUID = 1L;

    /** モジュールエクスプローラパネル */
    private ModuleTreePanel moduleTreePanel;
    /** ソースエクスプローラパネル */
    private FileTreePanel sourceTreePanel;
    /** XMLエクスプローラパネル */
    private FileTreePanel xmlTreePanel;
    /** 構造ツリーポップアップメニュー */
    private LanguageTreePopupMenu menuLanguagePopup;
    /** ツリーの変更リスナ */
    private ExploreTreeChangeAction actionTreeChange;

    /**
     * コンストラクタ
     */
    public ExploreView() {
        super(FRAME_VIEW.EXPLORE_VIEW);
        initGUI();
    }


    /**
     * 初期化を行う.<br/>
     * 構造、モジュール、ソース、XMLをタブを配置する。
     */
    private void initGUI() {
        try {
            // 構造ツリーパネル
            LanguageTreePanel languageTreePanel = new LanguageTreePanel(EXPLORE_PANEL.LANGUAGE);
            this.addTab(EXPLORE_PANEL.LANGUAGE.getTabName(),  languageTreePanel);

            // モジュールツリーパネル
            moduleTreePanel = new ModuleTreePanel(EXPLORE_PANEL.MODULE);
            this.addTab(
                    Message.getString("mainmenu.window.explore.module"), //モジュール
                    moduleTreePanel);

            // ソースファイルパネル
            sourceTreePanel = new FileTreePanel(EXPLORE_PANEL.SOURCE);
            sourceTreePanel.initTreeTitle(Message.getString("exploreview.treename.source")); //ソースファイル
            this.addTab(
                    Message.getString("mainmenu.window.explore.source"), //ソース
                    sourceTreePanel);

            // XMLファイルパネル
            xmlTreePanel = new FileTreePanel(EXPLORE_PANEL.XML);
            xmlTreePanel.initTreeTitle(Message.getString("exploreview.treename.xml")); //XMLファイル
            this.addTab(
                    Message.getString("mainmenu.window.explore.xml"), //XML
                    xmlTreePanel);

            // 初期表示を構造ツリータブとする。
            this.setSelectedIndex(0);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }


    /**
     * タブを閉じる
     */
    @Override
    public void closeTabComponent() {
        int index = this.getSelectedIndex();
        this.remove(index);
    }

    /**
     * XMLエクスプローラパネルがアクティブであるかチェックする
     * @return   true=XMLエクスプローラパネルがアクティブ
     */
    public boolean isActiveXmlTreePanel() {
        if (this.getSelectedComponent() == this.xmlTreePanel) {
            return true;
        }
        return false;
    }

    /**
     * 選択タブのツリーをすべて収納する。
     */
    public void collapseTreeAll() {
        ITreeComponent tree = (ITreeComponent)this.getSelectedComponent();
        if(tree == null) return;
        tree.collapseTreeAll();
    }


    /**
     * 選択タブのツリーをすべて展開する。
     */
    public void expandTreeAll() {
        ITreeComponent tree = (ITreeComponent)this.getSelectedComponent();
        if(tree == null) return;
        tree.expandTreeAll();
    }


    /**
     * 選択タブの選択ツリー配下ノードを展開する。
     */
    public void expandTreeSelect() {
        ITreeComponent tree = (ITreeComponent)this.getSelectedComponent();
        if(tree == null) return;
        tree.expandTreeSelect();
    }


    /**
     * 選択ファイルを取得する
     * @return		選択ファイル
     */
    public SourceFile[] getSelectedSourceFiles() {
        ITreeComponent tree = (ITreeComponent)this.getSelectedComponent();
        if(tree == null) return null;
        return tree.getSelectedSourceFiles();
    }


    /**
     * 選択ノードのフォルダ・ファイルを取得する
     * @return		選択フォルダ・ファイル
     */
    public File[] getSelectedNodeFiles() {
        ITreeComponent tree = (ITreeComponent)this.getSelectedComponent();
        if(tree == null) return null;
        return tree.getSelectedNodeFiles();
    }

    /**
     * 選択ソースコード行情報を取得する
     * @return		選択ソースコード行情報
     */
    public CodeLine[] getSelectedCodeLines() {
        ITreeComponent tree = (ITreeComponent)this.getSelectedComponent();
        if(tree == null) return null;
        return tree.getSelectedCodeLines();
    }


    /**
     * 選択ブロックを取得する
     * @return		選択ブロック
     */
    public IBlock[] getSelectedBlocks() {
        ITreeComponent tree = (ITreeComponent)this.getSelectedComponent();
        if(tree == null) return null;
        return tree.getSelectedBlocks();
    }


    /**
     * 現在選択されているノードを取得する。
     * @return		選択ノード
     */
    public DefaultMutableTreeNode getSelectedNode() {
        ITreeComponent tree = (ITreeComponent)this.getSelectedComponent();
        if(tree == null) return null;
        return tree.getSelectedNode();
    }

    /**
     * 現在選択されているノードリストを取得する。
     * @return		選択ノードリスト
     */
    public DefaultMutableTreeNode[] getSelectedNodes() {
        ITreeComponent tree = (ITreeComponent)this.getSelectedComponent();
        if(tree == null) return null;
        return tree.getSelectedNodes();
    }


    /**
     * タブを閉じる
     * @param index		閉じるタブインデックス
     */
    @Override
    protected void closeTab(int index) {

        if (index < 0) return;

        // 閉じるタブ
        Component tab = this.getComponentAt(index);
        tab.setVisible(false);
        if (tab instanceof ITabComponent) {
            ((ITabComponent)tab).closeTabComponent();
        }
        else {
            this.remove(index);
        }
    }

    /**
     * 構造エクスプローラパネルを取得する
     * @return		構造エクスプローラパネル
     */
    public LanguageTreePanel getPanelLanguageTree() {

        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            Component comp = this.getComponentAt(i);
            if ( comp instanceof ITreeComponent) {
                EXPLORE_PANEL type = ((ITreeComponent)comp).getEnumPanel();
                if (type == EXPLORE_PANEL.LANGUAGE) {
                    LanguageTreePanel panel = (LanguageTreePanel)comp;
                    return panel;
                }
            }
        }
        return null;
    }

    /**
     * モジュールエクスプローラパネルを取得する
     * @return		モジュールエクスプローラパネル
     */
    public ModuleTreePanel getPanelModuleTree() {
        return this.moduleTreePanel;
    }

    /**
     * ソースエクスプローラパネルを取得する
     * @return		ソースエクスプローラパネル
     */
    public FileTreePanel getPanelSourceTree() {
        return this.sourceTreePanel;
    }

    /**
     * XMLエクスプローラパネルを取得する
     * @return		XMLエクスプローラパネル
     */
    public FileTreePanel getPanelXmlTree() {
        return this.xmlTreePanel;
    }


    /**
     * ソースツリーポップアップメニューの設定を行う
     * @param menuPopup		ソースツリーポップアップメニュー
     */
    public void setSourcePopupMenu(ITreePopupMenu menuPopup) {
        sourceTreePanel.setPopupMenu(menuPopup);
    }


    /**
     * XMLファイルツリーのポップアップメニューを設定する
     * @param menuPopup		XMLファイルツリーポップアップメニュー
     */
    public void setXmlPopupMenu(ITreePopupMenu menuPopup) {
        xmlTreePanel.setPopupMenu(menuPopup);
    }


    /**
     * 構造ツリーのポップアップメニューを設定する
     * @param menuPopup		構造ツリーポップアップメニュー
     */
    public void setLanguagePopupMenu(LanguageTreePopupMenu menuPopup) {

        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            Component comp = this.getComponentAt(i);
            if ( comp instanceof ITreeComponent) {
                EXPLORE_PANEL type = ((ITreeComponent)comp).getEnumPanel();
                if (type == EXPLORE_PANEL.LANGUAGE) {
                    LanguageTreePanel panel = (LanguageTreePanel)comp;
                    panel.setPopupMenu(menuPopup);
                }
            }
        }
        this.menuLanguagePopup = menuPopup;
    }

    /**
     * 現在選択されているツリーパネルの取得を行う。
     * @return		選択ツリーパネル
     */
    public ITreeComponent getSelectedPanel() {
        int index = this.getSelectedIndex();
        if (index < 0) return null;
        Component comp = this.getComponentAt(index);
        if (comp instanceof ITreeComponent) {
            return (ITreeComponent)comp;
        }
        return null;
    }


    /**
     * モジュールツリーのポップアップメニューを設定する
     * @param menuPopup		モジュールツリーポップアップメニュー
     */
    public void setModulePopupMenu(ModuleTreePopupMenu menuPopup) {
        this.moduleTreePanel.setPopupMenu(menuPopup);
    }

    /**
     * ツリーの変更リスナの登録を行う。
     * @param action		ツリーの変更リスナ
     */
    public void addTreeSelectionListener(ExploreTreeChangeAction action) {

        // ツリーの変更リスナの登録
        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            Component comp = this.getComponentAt(i);
            if ( comp instanceof ITreeComponent) {
                ((ITreeComponent)comp).addTreeSelectionListener(action);
            }
        }

        // タブの変更リスナの登録
        this.addChangeListener(action);

        this.actionTreeChange = action;
    }

    /**
     * 選択ツリーパネル識別子を取得する
     * @return		選択ツリーパネル識別子
     */
    public EXPLORE_PANEL getSelectedEnumPanel() {
        ITreeComponent tree = getSelectedPanel();
        if (tree == null) return EXPLORE_PANEL.UNKNOWN;

        return tree.getEnumPanel();
    }


    /**
     * 選択ノードを設定する
     * @param node		選択ノード
     */
    public void setSelectedNode(Object node) {

        // 選択ノードの設定
        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            Component comp = this.getComponentAt(i);
            if ( comp instanceof ITreeComponent) {
                ((ITreeComponent)comp).setSelectedNode(node);
            }
        }
    }

    /**
     * 選択ノードを設定する
     * @param nodes		選択ノード
     */
    public void setSelectedNodes(Object[] nodes) {

        // 選択ノードの設定
        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            Component comp = this.getComponentAt(i);
            if ( comp instanceof ITreeComponent) {
                ((ITreeComponent)comp).setSelectedNodes(nodes);
            }
        }
    }


    /**
     * ノード範囲を選択する
     * @param startnode		選択開始ノード
     * @param endnode		選択終了ノード
     */
    public void setSelectedNodeArea(Object startnode, Object endnode) {

        // 選択ノードの設定
        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            Component comp = this.getComponentAt(i);
            if ( comp instanceof ITreeComponent) {
                ((ITreeComponent)comp).setSelectedNodeArea(startnode, endnode);
            }
        }
    }

    /**
     * ノード選択範囲を追加する
     * @param startnode		選択開始ノード
     * @param endnode		選択終了ノード
     */
    public void addSelectedNodeArea(Object startnode, Object endnode) {

        // 選択ノードの設定
        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            Component comp = this.getComponentAt(i);
            if ( comp instanceof ITreeComponent) {
                ((ITreeComponent)comp).addSelectedNodeArea(startnode, endnode);
            }
        }
    }

    /**
     * 指定エクスプローラ情報パネルをアクティブにする.<br/>
     * 閉じている場合は開く
     * @param panel		選択分析情報パネル識別子
     */
    public void setSelectedPanel(EXPLORE_PANEL panel) {

        Component viewpanel = null;
        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            Component comp = this.getComponentAt(i);
            if ( comp instanceof ITreeComponent) {
                if ( ((ITreeComponent)comp).getEnumPanel() == panel) {
                    viewpanel = comp;
                    this.setSelectedIndex(i);
                    return;
                }
            }
        }

        // タブが表示されていない。
        if (viewpanel == null) {

            /** 構造エクスプローラパネル */
            if (panel == EXPLORE_PANEL.LANGUAGE) {
                viewpanel = createLanguageTreePanel();
            }
            /** モジュールエクスプローラパネル */
            if (moduleTreePanel.getEnumPanel() == panel) {
                viewpanel = moduleTreePanel;
            }
            /** ソースエクスプローラパネル */
            else if (sourceTreePanel.getEnumPanel() == panel) {
                viewpanel = sourceTreePanel;
            }
            /** XMLエクスプローラパネル */
            else if (xmlTreePanel.getEnumPanel() == panel) {
                viewpanel = xmlTreePanel;
            }

        }
        if (viewpanel == null) return;

        // タブが表示されていないので、追加する
        this.addTab(panel.getTabName(),  viewpanel);
        this.setSelectedIndex(this.getTabCount()-1);

        return;
    }


    /**
     * 指定エクスプローラ情報パネルを閉じる.<br/>
     * @param panel		選択分析情報パネル識別子
     */
    public void closePanel(EXPLORE_PANEL panel) {
        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            Component comp = this.getComponentAt(i);
            if ( comp instanceof ITreeComponent) {
                if ( ((ITreeComponent)comp).getEnumPanel() == panel) {
                    this.closeTab(i);
                    return;
                }
            }
        }
        return;
    }

    /**
     * ツリーモデルを取得する
     * @return		ツリーモデル
     */
    public TreeModel getTreeModel() {
        ITreeComponent tree = getSelectedPanel();
        if (tree == null) return null;

        return tree.getTreeModel();
    }


    /**
     * ツリーノード選択を行う.
     * @param path		選択ツリーパス
     */
    public void setSelectionPath(TreePath path) {

        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            Component comp = this.getComponentAt(i);
            if ( comp instanceof ITreeComponent) {
                ((ITreeComponent)comp).setSelectionPath(path);
            }
        }
    }

    /**
     * 新規の構造ツリータブを開く.<br/>
     * 表示済みの場合は、アクティブにするのみとする。
     * @param model			構造モデル
     */
    public void viewLanguageTree(LanguageTreeModel model) {
        if (model == null) return;

        // ルートから２つ目までのノードで表示済みであるかチェックする
        DefaultMutableTreeNode root = model.getRootNode();
        if (root == null || root.getChildCount() <= 0) return;
        DefaultMutableTreeNode child = (DefaultMutableTreeNode) root.getChildAt(0);
        TreePath path = SwingUtils.getTreePath(child);

        LanguageTreePanel languageTreePanel = null;
        boolean isblank = false;
        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            Component comp = this.getComponentAt(i);
            if ( comp instanceof ITreeComponent) {
                EXPLORE_PANEL type = ((ITreeComponent)comp).getEnumPanel();
                if (type == EXPLORE_PANEL.LANGUAGE) {
                    LanguageTreePanel panel = (LanguageTreePanel)comp;
                    if (panel.existsTreePath(path)) {
                        languageTreePanel = panel;
                        break;
                    }
                    else if (panel.isBlankTreeModel()) {
                        languageTreePanel = panel;
                        isblank = true;
                        break;
                    }
                }
            }
        }

        if (languageTreePanel == null) {
            // 同一構造ツリーが存在しないので新規に作成する
            languageTreePanel = createLanguageTreePanel();
            // モデルを設定する
            languageTreePanel.setModel(model);
            // タブ名を変更する
            setLanguageTabname(languageTreePanel, model);
        }
        else if (isblank) {
            // モデルを設定する
            languageTreePanel.setModel(model);
        }

        // アクティブにする
        this.setSelectedComponent(languageTreePanel);
    }

    /**
     * 構造タブ名を設定する
     * @param panel			構造タブ
     * @param model			構造ツリーモデル
     */
    private void setLanguageTabname(LanguageTreePanel panel, LanguageTreeModel model) {
        if (model == null) return;

        // ルートから２つ目のノードからタブ名を生成する
        DefaultMutableTreeNode root = model.getRootNode();
        if (root == null || root.getChildCount() <= 0) return;
        DefaultMutableTreeNode child = (DefaultMutableTreeNode) root.getChildAt(0);
        Object obj = child.getUserObject();
        if (obj == null) return;
        String tabname = null;
        if (obj instanceof Procedure) {
            tabname = ((Procedure)obj).get_name();
        }
        if (tabname == null) return;

        tabname = EXPLORE_PANEL.LANGUAGE.getTabName() + " (" + tabname + ")";
        int count = this.getTabCount();
        int index = 0;
        for (int i=0; i<count; i++) {
            Component comp = this.getComponentAt(i);
            if ( comp == panel) {
                index = i;
                break;
            }
        }

        // タブ名を設定する
        setTabTitle(index, tabname);
    }

    /**
     * 構造ツリーパネルを作成する
     * @return		構造ツリーパネル
     */
    public LanguageTreePanel createLanguageTreePanel() {
        LanguageTreePanel languageTreePanel = new LanguageTreePanel(EXPLORE_PANEL.LANGUAGE);
        this.addTab(EXPLORE_PANEL.LANGUAGE.getTabName(),  languageTreePanel);

        // 構造ツリーポップアップメニュー
        if (this.menuLanguagePopup != null) {
            languageTreePanel.setPopupMenu(this.menuLanguagePopup);
        }
        // ツリーの変更リスナ
        if (this.actionTreeChange != null) {
            languageTreePanel.addTreeSelectionListener(this.actionTreeChange);
        }

        return languageTreePanel;
    }


    /**
     * XMLツリー以外のツリーモデルをクリアする.<br/>
     * フォートランデータベースのクリアに伴う関連情報のクリアを行う.
     */
    public void clearTreeModel() {

        int count = this.getTabCount();
        // 最初の構造タブインデックスの取得
        int languageIndex = -1;
        for (int i=0; i<count; i++) {
            Component comp = this.getComponentAt(i);
            if ( comp instanceof ITreeComponent) {
                EXPLORE_PANEL type = ((ITreeComponent)comp).getEnumPanel();
                if (type == EXPLORE_PANEL.LANGUAGE) {
                    languageIndex = i;
                    break;
                }
            }
        }

        for (int i=count-1; i>=0; i--) {
            Component comp = this.getComponentAt(i);
            if ( comp instanceof ITreeComponent) {
                EXPLORE_PANEL type = ((ITreeComponent)comp).getEnumPanel();
                if (type == EXPLORE_PANEL.LANGUAGE) {
                    LanguageTreePanel panel = (LanguageTreePanel)comp;
                    panel.getModel().clearTreeModel();
                    if (languageIndex != i) {
                        // 最初の構造タブ以外は削除する
                        this.remove(i);
                    }
                }
                else if (type == EXPLORE_PANEL.MODULE) {
                    ModuleTreePanel panel = (ModuleTreePanel)comp;
                    panel.getModel().clearTreeModel();
                }
                else if (type == EXPLORE_PANEL.SOURCE) {
                    FileTreePanel panel = (FileTreePanel)comp;
                    panel.getModel().clearTreeModel();
                }
            }
        }
    }


    /**
     * 構造タブのモデルを取得する
     * @return		構造モデルリスト
     */
    public LanguageTreeModel[] getLanguageModels() {

        List<LanguageTreeModel>list = new ArrayList<LanguageTreeModel>();
        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            Component comp = this.getComponentAt(i);
            if ( comp instanceof ITreeComponent) {
                EXPLORE_PANEL type = ((ITreeComponent)comp).getEnumPanel();
                if (type == EXPLORE_PANEL.LANGUAGE) {
                    LanguageTreePanel panel = (LanguageTreePanel)comp;
                    // ツリーを表示しているパネルのみ取得する
                    if (!panel.isBlankTreeModel()) {
                        list.add(panel.getModel());
                    }
                }
            }
        }
        if (list.size() <= 0) return null;

        return list.toArray(new LanguageTreeModel[0]);
    }


    /**
     * 構造ツリーにフィルタを設定する
     * @param filters		構造ツリーフィルタ
     */
    public void setLanguageTreeFilter(FILTER_TYPE[] filters) {

        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            Component comp = this.getComponentAt(i);
            if ( comp instanceof ITreeComponent) {
                EXPLORE_PANEL type = ((ITreeComponent)comp).getEnumPanel();
                if (type == EXPLORE_PANEL.LANGUAGE) {
                    LanguageTreePanel panel = (LanguageTreePanel)comp;
                    panel.setLanguageTreeFilter(filters);
                }
            }
        }
    }


    /**
     * ツリーノード選択を行う.
     * @param blocks		選択ブロックリスト
     */
    public void setSelectedBlocks(IBlock[] blocks) {

        // 選択ノードの設定
        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            Component comp = this.getComponentAt(i);
            if ( comp instanceof LanguageTreePanel) {
                ((LanguageTreePanel)comp).setSelectedBlocks(blocks);
            }
        }
    }


    /**
     * パネルの描画更新を行う。
     */
    @Override
    public void updateUI() {
        super.updateUI();

        // 選択ノードの設定
        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            Component comp = this.getComponentAt(i);
            if ( comp instanceof JPanel) {
                ((JPanel)comp).updateUI();
            }
        }
    }


    /**
     * 選択ノードを追加する
     * @param nodes		選択ノード
     */
    public void addSelectedNodes(Object[] nodes) {

        // 選択ノードの設定
        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            Component comp = this.getComponentAt(i);
            if ( comp instanceof ITreeComponent) {
                ((ITreeComponent)comp).addSelectedNodes(nodes);
            }
        }
    }


    /**
     * プロパティ変更イベント
     * @param event   イベント情報
     */
	@Override
	public void propertyChange(PropertyChangeEvent event) {

        // ソース表示フォント、フォント色等のソースビュープロパティの変更
        if (event.getNewValue() instanceof SourceProperties) {
            SourceProperties properties = (SourceProperties)event.getNewValue();
            int count = this.getTabCount();
            for (int i=0; i<count; i++) {
                Component comp = this.getComponentAt(i);
                if ( comp instanceof ITreeComponent) {
                    EXPLORE_PANEL type = ((ITreeComponent)comp).getEnumPanel();
                    if (type == EXPLORE_PANEL.LANGUAGE) {
                        LanguageTreePanel panel = (LanguageTreePanel)comp;
                        panel.setSourceProperties(properties);
                    }
                    else if (type == EXPLORE_PANEL.MODULE) {
                        ModuleTreePanel panel = (ModuleTreePanel)comp;
                        panel.setSourceProperties(properties);
                    }
                    else if (type == EXPLORE_PANEL.SOURCE || type == EXPLORE_PANEL.XML) {
                        FileTreePanel panel = (FileTreePanel)comp;
                        panel.setSourceProperties(properties);
                    }
                }
            }
        }
	}


    /**
     * 選択ノードの変更イベントを発生させる
     */
    public void fireSelectNodeChanged() {

        // 選択ノードの設定
        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            Component comp = this.getComponentAt(i);
            if ( comp instanceof ITreeComponent) {
                ((ITreeComponent)comp).fireSelectNodeChanged();
            }
        }
        // 再描画
        updateUI();
    }
}




