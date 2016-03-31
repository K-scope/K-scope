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
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;

import jp.riken.kscope.Message;
import jp.riken.kscope.action.ExploreTreeChangeAction;
import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.common.FILTER_TYPE;
import jp.riken.kscope.component.FilterTreeModel;
import jp.riken.kscope.component.ObjectTree;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.menu.LanguageTreePopupMenu;
import jp.riken.kscope.model.LanguageTreeModel;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.utils.ResourceUtils;


/**
 * 構造ツリータブパネルクラス
 * @author RIKEN
 *
 */
public class LanguageTreePanel extends javax.swing.JPanel implements ITabComponent, ITreeComponent, Observer {
    /** シリアル番号 */
    private static final long serialVersionUID = 1L;
    /** すべて展開ボタン */
    private JButton btnExpand;
    /** すべて収納ボタン */
    private JButton btnCollapse;
    /** 構造ツリー */
    private ObjectTree treeExplore;
    /** 構造ツリーモデル */
    private LanguageTreeModel model;

    /** エクスプローラパネル識別子 */
    private EXPLORE_PANEL enumPanel;
    /** 親コンポーネント */
    private ITabComponent parentCompornent;
    /** ファイルを開くボタン */
    private JButton btnOpenFile;
    /** エクスポートボタン */
    private JButton btnExport;

    /**
     * コンストラクタ
     */
    public LanguageTreePanel() {
        super();

        //初期化を行う
        initialize();
    }

    /**
     * コンストラクタ
     * @param panel		エクスプローラパネル識別子
     */
    public LanguageTreePanel(EXPLORE_PANEL panel) {
        this.enumPanel = panel;

        //初期化を行う
        initialize();
    }

    /**
     * 初期化を行う
     */
    private void initialize() {

        // モデルの生成を行う
        model = new LanguageTreeModel();
        // オブザーバを設定する。
        model.addObserver(this);

        // GUI初期化を行う
        initGUI();
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
                    JPanel panelButtons = new JPanel();
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
                    scrollPane.setViewportView(treeExplore);
                    treeExplore.setModel(model.getTreeModel());
                    treeExplore.setRootVisible(true);
                    treeExplore.setShowsRootHandles(true);

                    // ダブルクリックによるノードの展開を行わない。
                    treeExplore.setToggleClickCount(0);
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
     * ツリーモデルの変更通知イベント
     * @param o			通知元
     * @param arg		通知項目
     */
    @Override
    public void update(Observable o, Object arg) {
        // ツリーモデルを更新する
        updateModel();
    }

    /**
     * ツリーモデルを更新する
     */
    private void updateModel() {

        // 現在のノード状態の待避
        this.treeExplore.storeTreeNode();

        // データベースをセットする.
        this.treeExplore.setLanguageDb(this.model.getLanguageDb());
        // ツリーモデル
        FilterTreeModel treeModel = this.model.getTreeModel();
        // フィルタ実行
        this.model.filter();
        // ツリーモデル設定
        this.treeExplore.setModel(treeModel);

        // 再描画
        this.treeExplore.updateUI();

        // ノード状態の復元
        this.treeExplore.restoreTreeNode();
    }

    /**
     * 選択ノードを設定する
     * @param selectnode		選択ノード
     */
    @Override
    public void setSelectedNode(Object selectnode) {
        if (selectnode == null) return;
	// 引数がノードの場合はそのままノードパスを設定するように変更(2014/4/8 ohichi)
        if(selectnode instanceof  DefaultMutableTreeNode){
        	TreePath path = new TreePath(((DefaultMutableTreeNode)selectnode).getPath());
        	this.treeExplore.setSelectionPath(path);
        	this.treeExplore.scrollPathToVisibleForVertical(path);
        }else{
        	// 選択ノードを展開する
        	this.treeExplore.expandObjectPath(selectnode);
        	// 選択ノードを設定する
        	this.treeExplore.setSelectedNode(selectnode);
        }

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
    	// modify 2012/03/01 by @hira
        if(expand) this.treeExplore.expandPath(parent);
        else       this.treeExplore.collapsePath(parent);

        TreeNode node = (TreeNode)parent.getLastPathComponent();
        if(!node.isLeaf() && node.getChildCount()>=0) {
            Enumeration<?> e = node.children();
            while(e.hasMoreElements()) {
                TreeNode n = (TreeNode) e.nextElement();
                TreePath path = parent.pathByAddingChild(n);
                visitAll(path, expand);
            }
        }

        return;
    }


    /**
     * 選択されているノード配下のソースファイルを取得する。
     *
     * @return ソースファイルリスト
     */
    @Override
    public SourceFile[] getSelectedSourceFiles() {
        CodeLine[] lines = getSelectedCodeLines();
        if (lines == null || lines.length <= 0) return null;

        List<SourceFile> list = new ArrayList<SourceFile>();
        for (int i=0; i<lines.length; i++) {
            if (lines[i].getSourceFile() == null) continue;
            if (lines[i].getSourceFile().getFile() == null) continue;
            list.add(lines[i].getSourceFile());
        }
        if (list.size() <= 0) return null;
        return list.toArray(new SourceFile[0]);
    }


    /**
     * 選択ファイルを取得する
     * @return		選択ファイル
     */
    @Override
    public File[] getSelectedNodeFiles() {
        SourceFile[] files = getSelectedSourceFiles();
        if (files == null || files.length <= 0) return null;
        File[] list = new File[files.length];
        for (int i=0; i<files.length; i++) {
            list[i] = files[i].getFile();
        }

        return list;
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
     * タブフォーカスリスナを追加する
     * @param listener			タブフォーカスリスナ
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
     * 選択ソースコード行情報を取得する
     * @return		選択ソースコード行情報
     */
    @Override
    public CodeLine[] getSelectedCodeLines() {

        ArrayList<CodeLine> list = new ArrayList<CodeLine>();

        IBlock[] blocks = getSelectedBlocks();
        if (blocks == null) return null;

        for (int i = 0; i < blocks.length; i++) {
            CodeLine start = blocks[i].getStartCodeLine();
            CodeLine end = blocks[i].getEndCodeLine();
            if (start == null) continue;

            // 開始行番号 + 終了行番号で１つのCodeLineの作成
            //if (end != null && start.getEndLine() < end.getEndLine()) {
            if (end != null) {
                start.setEndLine(end.getEndLine());
            }
            list.add(start);
        }
        if (list.size() <= 0) return null;

        return list.toArray(new CodeLine[0]);
    }


    /**
     * 選択ブロックを取得する
     * @return		選択ブロック
     */
    @Override
    public IBlock[] getSelectedBlocks() {

        ArrayList<IBlock> list = new ArrayList<IBlock>();

        // 選択されたファイルのソースファイルオブジェクトを取得する。
        TreePath[] paths = this.treeExplore.getSelectionPaths();
        if (paths == null)  return null;

        for (int i = 0; i < paths.length; i++) {
            int count = paths[i].getPath().length;
            // ルート以外
            if (count >= 2) {
                DefaultMutableTreeNode node = (DefaultMutableTreeNode) paths[i].getPath()[count - 1];
                Object obj = node.getUserObject();
                // ブロックオブジェクトであるか？
                if (obj instanceof IBlock) {
                    list.add((IBlock) obj);
                }
            }
        }
        if (list.size() <= 0) return null;

        return list.toArray(new IBlock[0]);
    }

    /**
     * 構造ツリーのポップアップメニューを設定する
     * @param menuPopup		構造ツリーポップアップメニュー
     */
    public void setPopupMenu(LanguageTreePopupMenu menuPopup) {

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
    }


    /**
     * エクスプローラツリーをエクスポートする
     * @param file		出力ファイル
     */
    @Override
    public void export(File file) { }


    /**
     * エクスプローラパネル識別子を取得する
     * @return		エクスプローラパネル識別子
     */
    @Override
    public EXPLORE_PANEL getEnumPanel() {
        return this.enumPanel;
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
     * ツリーモデルを取得する
     * @return		ツリーモデル
     */
    @Override
    public TreeModel getTreeModel() {
        return this.treeExplore.getModel();
    }

    /**
     * ツリーモデルがブランクであるかチェックする
     * @return		true=ブランクモデル
     */
    public boolean isBlankTreeModel() {

        TreeModel modelTree = this.treeExplore.getModel();
        DefaultMutableTreeNode root = (DefaultMutableTreeNode)modelTree.getRoot();

        // ルートノード配下に子ノードが存在していれば、有効ノード
        return (root.getChildCount() <= 0);
    }

    /**
     * ツリーノード選択を行う.
     * @param path		選択ツリーパス
     */
    @Override
    public void setSelectionPath(TreePath path) {
    	TreePath real = null;
    	int loop = 0;
    	while (true) {
	        // 実際のツリーパスを取得する
	        real = this.treeExplore.getRealTreePath(path);
	        if (real == null) {
	            // 選択ツリーパスが存在しないので、親ノードを選択する
	            Object[] objs = path.getPath();
	            if (objs == null || objs.length <= 0) return;
	            DefaultMutableTreeNode[] pathNodes = new DefaultMutableTreeNode[objs.length];
	            for (int i=0; i<objs.length; i++) {
	                pathNodes[i] = (DefaultMutableTreeNode) objs[i];
	            }
	            // パスノードの末尾から親ノードを検索する
	            // 末尾は既に存在しないので１つ前から検索する。
	            for (int i=pathNodes.length-2; i>=0; i--) {
	                DefaultMutableTreeNode node = pathNodes[i];
	                TreePath nodePath = new TreePath(node.getPath());
	                real = this.treeExplore.getRealTreePath(nodePath);
	                if (real != null) {
	                    break;
	                }
	            }
	        }
	        if (real == null) break;

	        // ツリーパスを選択する
	        this.treeExplore.expandSelectionPath(real);
	        this.treeExplore.setSelectionPath(real);
	        loop++;
	        if (path.getPathCount() <= real.getPathCount()) {
	        	break;
	        }
	        if (path.getPathCount() <= loop) {
	        	break;
	        }
    	}
        if (real == null) return;
        if (real.getPathCount() != path.getPathCount()) return;

        // パスノードを表示するようにスクロースする。
        this.treeExplore.scrollPathToVisibleForVertical(real);

        // タブをアクティブにする
        ((JTabbedPane)this.parentCompornent).setSelectedComponent(this);
    }

    /**
     * ツリーパスが存在するかチェックする.<br/>
     * ツリーノードのUserObjectによりパスを検索する.
     * @param path		検索ツリーパス
     * @return			true=ツリーパスが存在する
     */
    public boolean existsTreePath(TreePath path) {
        if (path == null) return false;
        // 実際のツリーパスを取得する
        TreePath real = this.treeExplore.getRealTreePath(path);
        return (real != null);
    }


    /**
     * 構造ツリーモデルを取得する
     * @return		構造ツリーモデル
     */
    public LanguageTreeModel getModel() {
        return this.model;
    }

    /**
     * 構造ツリーモデルを設定する
     * @param model		構造ツリーモデル
     */
    public void setModel(LanguageTreeModel model) {
        this.model = model;

        // オブザーバを設定する。
        this.model.addObserver(this);

        updateModel();
    }

    /**
     * 構造ツリーにフィルタを設定する
     * @param filters		構造ツリーフィルタ
     */
    public void setLanguageTreeFilter(FILTER_TYPE[] filters) {
        this.model.setListFilter(filters);
    }


    /**
     * 選択ブロックを設定する
     * @param blocks		選択ブロック階層リスト
     */
    public void setSelectedBlocks(IBlock[] blocks) {
        if (blocks == null) return;
        if (blocks.length <= 0) return;

        // 選択ノードを設定する
        //this.treeExplore.setSelectedChainNode(blocks);
        for (IBlock block : blocks) {
            this.treeExplore.expandObjectPath(block);
        	this.treeExplore.setSelectedNode(block);
        }

        return;
    }

    /**
     * パネルの描画更新を行う。
     */
    @Override
    public void updateUI() {
        if (treeExplore != null) {
            // 再描画
            // treeExplore.updateUI();
        }
        super.updateUI();
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

