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

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionListener;
import java.awt.event.MouseListener;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.List;
import java.util.Observable;
import java.util.Observer;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.border.LineBorder;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;

import jp.riken.kscope.Message;
import jp.riken.kscope.action.ExploreTreeChangeAction;
import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.component.ObjectTree;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.menu.ITreePopupMenu;
import jp.riken.kscope.model.FileTreeModel;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.utils.ResourceUtils;


/**
 * ファイルツリータブパネルクラス.
 * @author RIKEN
 *
 */
public class FileTreePanel extends javax.swing.JPanel implements ITabComponent, ITreeComponent, Observer {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;
    /** ツリー展開ボタン */
    private JButton btnExpand;
    /** ツリー収納ボタン */
    private JButton btnCollapse;
    /** ツリーパイン */
    private ObjectTree  treeExplore;
    /** ボタンパネル */
    private JPanel panelButtons;
    /** ファイルを開くボタン */
    private JButton btnOpenFile;
    /** エクスポートボタン */
    private JButton btnExport;

    /** ファイルツリーモデル */
    private FileTreeModel model;

    /** エクスプローラパネル識別子 */
    private EXPLORE_PANEL enumPanel;
    /** 親コンポーネント */
    private ITabComponent parentCompornent = null;

    /** XMLファイルツリーポップアップメニュー */
    @SuppressWarnings("unused")
    private ITreePopupMenu menuPopup;

    /**
     * コンストラクタ
     */
    public FileTreePanel() {
        super();

        // 初期化を行う
        initialize();
    }

    /**
     * コンストラクタ
     * @param panel		エクスプローラパネル識別子
     */
    public FileTreePanel(EXPLORE_PANEL panel) {
        super();

        // パネル識別子
        this.enumPanel = panel;

        // 初期化を行う
        initialize();
    }

    /**
     * 初期化を行う
     */
    private void initialize() {
        // GUI初期化を行う
        initGUI();

        // ファイルツリーモデルの生成
        this.model = new FileTreeModel();
        this.model.addObserver(this);

    }

    /**
     * GUI初期化を行う
     */
    private void initGUI() {
        try {
            BorderLayout thisLayout = new BorderLayout();
            this.setLayout(thisLayout);
            setPreferredSize(new Dimension(200, 300));
            {
                JPanel panelContent = new JPanel();
                BorderLayout panelContentLayout = new BorderLayout();
                panelContent.setLayout(panelContentLayout);
                this.add(panelContent, BorderLayout.NORTH);
                panelContent.setPreferredSize(new java.awt.Dimension(200, 24));
                panelContent.setBorder(new LineBorder(new java.awt.Color(0,0,0), 1, false));
                {
                    panelButtons = new JPanel();
                    panelButtons.setLayout(new BoxLayout(panelButtons, BoxLayout.LINE_AXIS));
                    panelButtons.setPreferredSize(new java.awt.Dimension(120, 24));
                    panelContent.add(panelButtons, BorderLayout.EAST);
                    java.awt.Dimension buttonSize = new java.awt.Dimension(18, 18);
                    {
                        Icon icon = ResourceUtils.getIcon("expandall.gif");
                        btnExpand = new JButton(icon);
                        btnExpand.setContentAreaFilled(false);
                        btnExpand.setBorderPainted(false);
                        btnExpand.setPreferredSize(buttonSize);
                        btnExpand.setMinimumSize(buttonSize);
                        btnExpand.setMaximumSize(buttonSize);
                        panelButtons.add(btnExpand);
                    }
                    // 余白設定
                    panelButtons.add(Box.createHorizontalStrut(5));
                    {
                        Icon icon = ResourceUtils.getIcon("collapseall.gif");
                        btnCollapse = new JButton(icon);
                        btnCollapse.setContentAreaFilled(false);
                        btnCollapse.setBorderPainted(false);
                        btnCollapse.setPreferredSize(buttonSize);
                        btnCollapse.setMinimumSize(buttonSize);
                        btnCollapse.setMaximumSize(buttonSize);
                        panelButtons.add(btnCollapse);
                    }
                    // 余白設定
                    panelButtons.add(Box.createHorizontalStrut(5));
                    {
                        Icon icon = ResourceUtils.getIcon("openfile.gif");
                        btnOpenFile = new JButton(icon);
                        btnOpenFile.setContentAreaFilled(false);
                        btnOpenFile.setBorderPainted(false);
                        btnOpenFile.setPreferredSize(buttonSize);
                        btnOpenFile.setMinimumSize(buttonSize);
                        btnOpenFile.setMaximumSize(buttonSize);
                        panelButtons.add(btnOpenFile);
                    }
                    // 余白設定
                    panelButtons.add(Box.createHorizontalStrut(5));
                    {
                        Icon icon = ResourceUtils.getIcon("save.gif");
                        btnExport = new JButton(icon);
                        btnExport.setContentAreaFilled(false);
                        btnExport.setBorderPainted(false);
                        btnExport.setPreferredSize(buttonSize);
                        btnExport.setMinimumSize(buttonSize);
                        btnExport.setMaximumSize(buttonSize);
                        panelButtons.add(btnExport);
                    }
                }
            }
            {
                JScrollPane scrollPane = new JScrollPane();
                this.add(scrollPane, BorderLayout.CENTER);
                {
                    treeExplore = new ObjectTree();
                    treeExplore.setRootVisible(true);
                    treeExplore.setShowsRootHandles(true);
                    scrollPane.setViewportView(treeExplore);
                }
            }

            // ツールチップ設定
            btnExpand.setToolTipText(Message.getString("treechooserdialog.tooltip.expandall")); //すべて展開
            btnCollapse.setToolTipText(Message.getString("treechooserdialog.tooltip.collapseall")); //すべて収納
            btnOpenFile.setToolTipText(Message.getString("filetreepanel.tooltip.selective")); //選択箇所を開く
            btnExport.setToolTipText(Message.getString("mainmenu.file.export")); //エクスポート

        } catch (Exception e) {
            e.printStackTrace();
        }
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
     * フォーカスリスナを設定する
     * @param listener		フォーカスリスナ
     */
    @Override
    public void addTabFocusListener(TabFocusListener listener) {
        this.addFocusListener(listener);
        if (this.treeExplore != null) {
            this.treeExplore.addFocusListener(listener);
        }
    }

    /**
     * アクティブタブを閉じる。
     */
    @Override
    public void closeTabComponent() {
        // 親コンポーネントで閉じる
        this.parentCompornent.closeTabComponent();
    }

    /**
     * ファイルツリーモデルを取得する
     * @return model		ファイルツリーモデル
     */
    public FileTreeModel getModel() {
        return model;
    }

    /**
     * 初期ツリー表示タイトルを設定する
     * @param title			タイトル
     */
    public void initTreeTitle(String title) {
        this.model.setInitTreeTitle(title);
        this.treeExplore.setModel(this.model.getTreeModel());
    }


    /**
     * 選択されているノード配下のソースファイルを取得する。
     *
     * @return ソースファイルリスト
     */
    @Override
    public SourceFile[] getSelectedSourceFiles() {

        ArrayList<SourceFile> list = new ArrayList<SourceFile>();

        // 選択されたファイルのソースファイルオブジェクトを取得する。
        TreePath[] paths = this.treeExplore.getSelectionPaths();
        if (paths == null)  return null;

        for (int i = 0; i < paths.length; i++) {
            int count = paths[i].getPath().length;
            // ルート以外
            if (count >= 2) {
                DefaultMutableTreeNode node = (DefaultMutableTreeNode) paths[i].getPath()[count - 1];
                Object obj = node.getUserObject();
                // ソースファイルオブジェクトであるか？
                if (obj instanceof SourceFile) {
                    // ソースファイルの追加
                    list.add((SourceFile) obj);
                }
            }
        }

        return list.toArray(new SourceFile[0]);
    }

    /**
     * 選択されているノード配下のソースファイルを取得する。
     *
     * @return ソースファイルリスト
     */
    public SourceFile[] getSelectChildSourceFiles() {

        ArrayList<SourceFile> list = new ArrayList<SourceFile>();

        // 選択されたファイルのソースファイルオブジェクトを取得する。
        TreePath[] paths = this.treeExplore.getSelectionPaths();
        if (paths == null)  return null;

        for (int i = 0; i < paths.length; i++) {
            int count = paths[i].getPath().length;
            // ルート以外
            if (count >= 2) {
                MutableTreeNode node = (MutableTreeNode) paths[i].getPath()[count - 1];
                SourceFile[] child_list = getSourceFiles((DefaultMutableTreeNode) node);
                if (child_list != null) {
                    // ソースファイルの追加
                    list.addAll(Arrays.asList(child_list));
                }
            } else if (count == 1) {
                // ルートの場合、全要素の追加
                return getAllSourceFiles();
            }
        }

        return list.toArray(new SourceFile[0]);
    }

    /**
     * 指定ノードの子要素のソースファイルを取得する。
     *
     * @param node    子要素検索ノード
     * @return ソースファイルリスト
     */
    private SourceFile[] getSourceFiles(DefaultMutableTreeNode node) {

        if (node == null) return null;

        ArrayList<SourceFile> list = new ArrayList<SourceFile>();

        // 自身の追加
        Object obj = node.getUserObject();
        // ソースファイルオブジェクトであるか？
        if (obj instanceof SourceFile) {
            // ソースファイルの追加
            list.add((SourceFile) obj);
        }

        // 子要素の検索
        int count = node.getChildCount();
        for (int i = 0; i < count; i++) {
            DefaultMutableTreeNode child = (DefaultMutableTreeNode) node
                    .getChildAt(i);
            Object child_obj = child.getUserObject();
            if (child_obj == null) continue;

            // 子要素からソースファイルの取得
            SourceFile[] child_list = getSourceFiles(child);
            if (child_list != null) {
                // ソースファイルの追加
                list.addAll(Arrays.asList(child_list));
            }
        }

        if (list.size() <= 0) return null;

        return list.toArray(new SourceFile[0]);
    }


    /**
     * ツリー上のすべてのソースファイルを取得する。
     *
     * @return ソースファイルリスト
     */
    public SourceFile[] getAllSourceFiles() {


        // ルートノード
        DefaultMutableTreeNode node = (DefaultMutableTreeNode) this.treeExplore.getModel().getRoot();
        if (node == null) return null;

        ArrayList<SourceFile> list = new ArrayList<SourceFile>();
        SourceFile[] child_list = getSourceFiles(node);
        if (child_list != null) {
            // ソースファイルの追加
            list.addAll(Arrays.asList(child_list));
        }
        if (list.size() <= 0) return null;

        return list.toArray(new SourceFile[0]);
    }

    /**
     * 選択タブのツリーをすべて収納する。
     */
    @Override
    public void collapseTreeAll() {
        int row = this.treeExplore.getRowCount()-1;
        while(row>=0) {
            this.treeExplore.collapseRow(row);
            row--;
        }
        // ルートノードのみ展開
        this.treeExplore.expandRow(0);
    }

    /**
     * 選択タブのツリーをすべて展開する。
     */
    @Override
    public void expandTreeAll() {
        int row = 0;
        while(row<this.treeExplore.getRowCount()) {
            this.treeExplore.expandRow(row);
            row++;
        }
    }

    /**
     * 選択タブの選択ツリー配下ノードを展開する。
     */
    @Override
    public void expandTreeSelect() {
        TreePath[] paths = this.treeExplore.getSelectionPaths();
        if (paths == null) return;

        for (int i=0; i<paths.length; i++) {
            // ツリーパス配下を展開する。
            visitAll(paths[i], true);
        }
    }


    /**
     * ツリーパス配下を展開、折り畳み表示を行う。
     *
     * @param parent		ツリーパス
     * @param expand		展開(true)/折り畳み(false)
     */
    public void visitAll(TreePath parent, boolean expand) {
        TreeNode node = (TreeNode)parent.getLastPathComponent();
        if(!node.isLeaf() && node.getChildCount()>=0) {
            Enumeration<?> e = node.children();
            while(e.hasMoreElements()) {
                TreeNode n = (TreeNode) e.nextElement();
                TreePath path = parent.pathByAddingChild(n);
                visitAll(path, expand);
            }
        }
        if(expand) this.treeExplore.expandPath(parent);
        else       this.treeExplore.collapsePath(parent);

        return;
    }

    /**
     * ファイルツリーのポップアップメニューを設定する
     * @param menuPopup		ファイルツリーポップアップメニュー
     */
    public void setPopupMenu(ITreePopupMenu menuPopup) {
        // ポップアップメニュー設定
        this.treeExplore.setComponentPopupMenu((JPopupMenu) menuPopup);

        // 展開ボタンにアクションリスナを設定する
        this.btnExpand.addActionListener(menuPopup.getActionTreeExpandAll());
        // 収納ボタンにアクションリスナを設定する
        this.btnCollapse.addActionListener(menuPopup.getActionTreeCollapseAll());
        // ファイルを開くボタンにアクションリスナを設定する
        this.btnOpenFile.addActionListener((ActionListener) menuPopup.getActionOpenFile());
        // ダブルクリックイベント(ファイルを開く)の登録
        this.treeExplore.addMouseListener((MouseListener) menuPopup.getActionOpenFile());
        // エクスポートの登録
        this.btnExport.addActionListener(menuPopup.getActionExportExplore());

        this.menuPopup = menuPopup;
    }


    /**
     * 選択ファイルを取得する
     * @return		選択ファイル
     */
    @Override
    public File[] getSelectedNodeFiles() {

        ArrayList<File> list = new ArrayList<File>();

        // 選択されたファイルのソースファイルオブジェクトを取得する。
        TreePath[] paths = this.treeExplore.getSelectionPaths();
        if (paths == null)  return null;

        for (int i = 0; i < paths.length; i++) {
            int count = paths[i].getPath().length;
            DefaultMutableTreeNode node = (DefaultMutableTreeNode) paths[i].getPath()[count - 1];

            Object obj = node.getUserObject();
            // ソースファイルオブジェクトであるか？
            if (obj instanceof SourceFile) {
                // ソースファイルの追加
                list.add(((SourceFile)obj).getFile());
            }
            else if (obj instanceof File) {
                // ソースファイルの追加
                list.add((File)obj);
            }
            else if (obj instanceof String) {
                node = (DefaultMutableTreeNode) paths[i].getPath()[0];
                if ( node.getUserObject() instanceof File) {
                    StringBuffer buf = new StringBuffer();
                    obj = node.getUserObject();
                    buf.append(((File)obj).getAbsoluteFile());
                    buf.append(File.separator);
                    for (int j=1; j<count; j++) {
                        node = (DefaultMutableTreeNode) paths[i].getPath()[j];
                        obj = node.getUserObject();
                        buf.append((String)obj);
                        buf.append(File.separator);
                    }
                    buf.deleteCharAt(buf.length()-1);
                    list.add(new File(buf.toString()));
                }
            }
        }

        return list.toArray(new File[0]);
    }

    /**
     * プロパティモデルの変更通知イベント
     * @param o			通知元
     * @param arg		通知項目
     */
    @Override
    public void update(Observable o, Object arg) {

        // 現在のファイルリスト
        SourceFile[] oldlist = getAllSourceFiles();
        int oldcount = oldlist != null ? oldlist.length : 0;

        FileTreeModel observer = (FileTreeModel)o;
        // 現在のノード状態の待避
        this.treeExplore.storeTreeNode();

        // ツリー再描画
        this.treeExplore.setModel(observer.getTreeModel());
        this.treeExplore.updateUI();

        // ノード状態の復元
        this.treeExplore.restoreTreeNode();

        // 最初のファイル設定であるので、すべて展開表示を行う
        if (oldcount == 0) {
            // すべて展開
            this.expandTreeAll();
        }
    }

    /**
     * 選択ソースコード行情報を取得する
     * @return		選択ソースコード行情報
     */
    @Override
    public CodeLine[] getSelectedCodeLines() {
        // 選択されているノード配下のソースファイルを取得する。
        SourceFile[] files = getSelectedSourceFiles();
        if (files == null || files.length <= 0) return null;

        CodeLine[] lines = new CodeLine[files.length];
        for (int i=0; i<files.length; i++) {

            lines[i] = new CodeLine(files[i], files[i].getPath());
        }

        return lines;
    }

    /**
     * 選択ブロックを取得する
     * @return		選択ブロック
     */
    @Override
    public IBlock[] getSelectedBlocks() {
        // ソースツリーの場合はブロックはない
        return null;
    }

    /**
     * エクスプローラツリーをエクスポートする
     * @param file		出力ファイル
     */
    @Override
    public void export(File file) {
        if (this.model == null) return;

        model.writeFile(file);
    }


    /**
     * エクスプローラパネル識別子を取得する
     * @return		エクスプローラパネル識別子
     */
    @Override
    public EXPLORE_PANEL getEnumPanel() {
        return this.enumPanel;
    }


    /**
     * 現在選択されているノードを取得する。
     * @return		選択ノード
     */
    @Override
    public DefaultMutableTreeNode getSelectedNode() {
        Object obj = treeExplore.getLastSelectedPathComponent();
        if (obj == null) return null;

        if (obj instanceof DefaultMutableTreeNode) {
            return (DefaultMutableTreeNode)obj;
        }

        return null;
    }

    /**
     * 現在選択されているノードリストを取得する。
     * @return		選択ノードリスト
     */
    @Override
    public DefaultMutableTreeNode[] getSelectedNodes() {
        TreePath[] paths = treeExplore.getSelectionPaths();
        if (paths == null) return null;

        List<DefaultMutableTreeNode> list = new ArrayList<DefaultMutableTreeNode>();
        for (int i=0; i<paths.length; i++) {
            Object obj = paths[i].getLastPathComponent();
            if (obj == null) continue;
            if (obj instanceof DefaultMutableTreeNode) {
                list.add((DefaultMutableTreeNode)obj);
            }
        }
        if (list.size() <= 0) return null;

        return list.toArray(new DefaultMutableTreeNode[0]);
    }

    /**
     * ツリーの変更リスナの登録を行う。
     * @param action		ツリーの変更リスナ
     */
    @Override
    public void addTreeSelectionListener(ExploreTreeChangeAction action) {
        this.treeExplore.addTreeSelectionListener(action);
    }

    /**
     * 選択ノードを設定する
     * @param selectnode		選択ノード
     */
    @Override
    public void setSelectedNode(Object selectnode) {
        if (selectnode == null) return;

        // 選択ノードを設定する
        this.treeExplore.setSelectedNode(selectnode);

        return;
    }

    /**
     * 複数選択ノードを設定する
     * @param selectnodes		選択ノードリスト
     */
    @Override
    public void setSelectedNodes(Object[] selectnodes) {
        if (selectnodes == null) return;

        // 選択ノードを設定する
        this.treeExplore.setSelectedNodes(selectnodes);

        return;
    }

    /**
     * ツリーモデルを取得する
     * @return		ツリーモデル
     */
    @Override
    public TreeModel getTreeModel() {
        return this.treeExplore.getModel();
    }

    /**
     * ツリーノード選択を行う.
     * @param path		選択ツリーパス
     */
    @Override
    public void setSelectionPath(TreePath path) {
        // 実際のツリーパスを取得する
        TreePath real = this.treeExplore.getRealTreePath(path);
        if (real != null) {
            // ツリーパスを選択する
            this.treeExplore.setSelectionPath(real);

            // パスノードを表示するようにスクロースする。
            this.treeExplore.scrollPathToVisibleForVertical(real);

            // タブをアクティブにする
            ((JTabbedPane)this.parentCompornent).setSelectedComponent(this);
        }
    }


    /**
     * パネルの描画更新を行う。
     */
    @Override
    public void updateUI() {
        if (treeExplore != null) {
            // 再描画
            treeExplore.updateUI();
        }
        super.updateUI();
    }

    /**
     * ノード範囲を選択する
     * @param startnode		選択開始ノード
     * @param endnode		選択終了ノード
     */
    @Override
    public void setSelectedNodeArea(Object startnode, Object endnode) {
        if (startnode == null && endnode == null) return;

        // 選択ノードを設定する
        this.treeExplore.setSelectedNodeArea(startnode, endnode);
    }
    /**
     * ノード選択範囲を追加する
     * @param startnode		選択開始ノード
     * @param endnode		選択終了ノード
     */
    @Override
    public void addSelectedNodeArea(Object startnode, Object endnode) {
        if (startnode == null && endnode == null) return;

        // 選択ノードを設定する
        this.treeExplore.addSelectedNodeArea(startnode, endnode);
    }

    /**
     * 複数選択ノードを追加する
     * @param selectnodes		選択ノードリスト
     */
    @Override
    public void addSelectedNodes(Object[] selectnodes) {
        if (selectnodes == null) return;

        // 選択ノードを設定する
        this.treeExplore.addSelectedNodes(selectnodes);

        return;
    }

    /**
     * ソースビュープロパティを設定する
     * @param properties		ソースビュープロパティ
     */
    public void setSourceProperties(SourceProperties properties) {
        this.treeExplore.setSourceProperties(properties);
    }


    /**
     * 選択ノードの変更イベントを発生させる
     */
	@Override
	public void fireSelectNodeChanged() {
		this.treeExplore.fireSelectNodeChanged();
	}
}


