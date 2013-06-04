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
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Observable;
import java.util.Observer;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import javax.swing.border.CompoundBorder;
import javax.swing.border.LineBorder;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import jp.riken.kscope.Message;
import jp.riken.kscope.action.ViewOpenAnalysisLineAction;
import jp.riken.kscope.action.ViewOpenLanguageTreeAction;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.KEYWORD_TYPE;
import jp.riken.kscope.common.TRACE_DIR;
import jp.riken.kscope.component.SearchTree;
import jp.riken.kscope.component.SearchTreeModel;
import jp.riken.kscope.component.SearchTreeNode;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.Keyword;
import jp.riken.kscope.data.SearchOption;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.menu.MainMenu;
import jp.riken.kscope.model.SearchResultModel;
import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * 検索結果パネルクラス
 * @author riken
 *
 */
public class SearchResultPanel extends AnalisysPanelBase implements Observer, IAnalisysComponent, ActionListener, MouseListener {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;
    /** 参照一覧ツリー */
    private SearchTree treeSearch;
    /** クリアボタン */
    private JButton btnClear;
    /** エクスポートボタン */
    private JButton btnExport;
    /** ファイルを開く */
    private JButton btnOpenFile;
    /** 移動:上ボタン */
    private JButton btnMoveUp;
    /** 移動:下ボタン */
    private JButton btnMoveDown;
    /** 検索結果ラベル */
    private JLabel label;
    /** すべて展開ボタン */
    private JButton btnExpand;
    /** すべて収納ボタン */
    private JButton btnCollapse;
    /** 新規構造ツリー */
    private JButton btnNewTree;
    /** リフレッシュボタン */
    private JButton btnRefresh;

    /** 検索結果テーブルモデル */
    private SearchResultModel model;
    /** 該当個所を開くアクション */
    private ViewOpenAnalysisLineAction actionOpen;
    /** 新規構造ツリーを開くアクション */
    private ViewOpenLanguageTreeAction actionOpenLanguageTree;

    /**
     * コンストラクタ
     */
    public SearchResultPanel() {
        super();

        // 初期化を行う。
        initialize();

    }

    /**
     * コンストラクタ
     * @param panel		分析情報パネル識別子
     */
    public SearchResultPanel(ANALYSIS_PANEL panel) {
        super(panel);

        // 初期化を行う。
        initialize();

    }

    /**
     * 初期化を行う。
     */
    private void initialize() {

        // モデルの生成を行う
        model = new SearchResultModel();
        // オブザーバを設定する。
        model.addObserver(this);

        // GUI初期化を行う。
        initGUI();
    }


    /**
     * GUI初期化を行う。
     */
    private void initGUI() {
        try {
            BorderLayout thisLayout = new BorderLayout();
            this.setLayout(thisLayout);
//            setPreferredSize(new Dimension(400, 64));

            // 上部の情報ラベル、ボタンの配置パネル
            {
                JPanel panelTop = new JPanel();
                panelTop.setLayout(new BorderLayout());
                this.add(panelTop, BorderLayout.NORTH);
                panelTop.setBorder(new CompoundBorder(
                                            new LineBorder(Color.BLACK, 1),
                                            BorderFactory.createEmptyBorder(0, 5, 0, 20)));
                // ボタン配置パネル
                {
                    JPanel panelButtons = new JPanel();
                    panelButtons.setLayout(new BoxLayout(panelButtons, BoxLayout.LINE_AXIS));
                    panelTop.add(panelButtons, BorderLayout.EAST);
                    java.awt.Dimension buttonSize = new java.awt.Dimension(24, 24);
                    // クリアボタン
                    {
                        Icon icon = ResourceUtils.getIcon("removeall.gif");
                        btnClear = new JButton(icon);
                        panelButtons.add(btnClear);
                        btnClear.setContentAreaFilled(false);
                        btnClear.setBorderPainted(false);
                        btnClear.setPreferredSize(buttonSize);
                        btnClear.setMinimumSize(buttonSize);
                        btnClear.setMaximumSize(buttonSize);
                    }
                    // リフレッシュボタン
                    {
                        Icon icon = ResourceUtils.getIcon("refresh.gif");
                        btnRefresh = new JButton(icon);
                        panelButtons.add(btnRefresh);
                        btnRefresh.setContentAreaFilled(false);
                        btnRefresh.setBorderPainted(false);
                        btnRefresh.setPreferredSize(buttonSize);
                        btnRefresh.setMinimumSize(buttonSize);
                        btnRefresh.setMaximumSize(buttonSize);
                    }
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
                    {
                        Icon icon = ResourceUtils.getIcon("arrow_up.gif");
                        btnMoveUp = new JButton(icon);
                        btnMoveUp.setContentAreaFilled(false);
                        btnMoveUp.setBorderPainted(false);
                        btnMoveUp.setPreferredSize(buttonSize);
                        btnMoveUp.setMinimumSize(buttonSize);
                        btnMoveUp.setMaximumSize(buttonSize);
                        panelButtons.add(btnMoveUp);
                    }
                    {
                        Icon icon = ResourceUtils.getIcon("arrow_down.gif");
                        btnMoveDown = new JButton(icon);
                        btnMoveDown.setContentAreaFilled(false);
                        btnMoveDown.setBorderPainted(false);
                        btnMoveDown.setPreferredSize(buttonSize);
                        btnMoveDown.setMinimumSize(buttonSize);
                        btnMoveDown.setMaximumSize(buttonSize);
                        panelButtons.add(btnMoveDown);
                    }
                    {
                        Icon icon = ResourceUtils.getIcon("new_tree.gif");
                        btnNewTree = new JButton(icon);
                        btnNewTree.setContentAreaFilled(false);
                        btnNewTree.setBorderPainted(false);
                        btnNewTree.setPreferredSize(buttonSize);
                        btnNewTree.setMinimumSize(buttonSize);
                        btnNewTree.setMaximumSize(buttonSize);
                        panelButtons.add(btnNewTree);
                    }
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
                // ラベル配置
                {
                    label = new JLabel();
                    panelTop.add(label, BorderLayout.CENTER);
                    //label.setText();
                }
            }
            {
                {
                    // 検索結果ツリー
                    treeSearch = new SearchTree();

                    SearchTreeNode rootNode = new SearchTreeNode(Message.getString("mainmenu.window.analysis.search")); //検索結果
                    SearchTreeModel treeModel = new SearchTreeModel(rootNode);
                    treeSearch.setModel(treeModel);
                    treeSearch.setRootVisible(true);
                    treeSearch.setShowsRootHandles(true);

                    // ダブルクリックによるノードの展開を行わない。
                    treeSearch.setToggleClickCount(0);
                    // 一行だけ選択可能
                    treeSearch.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);

                    // スクロールパイン
                    JScrollPane scrollTable = new JScrollPane(treeSearch);
                    scrollTable.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
                    scrollTable.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
                    scrollTable.getViewport().setBackground(Color.WHITE);

                    add(scrollTable);

                }

            }

            // ツールチップ設定
            btnClear.setToolTipText(Message.getString("informationdialog.button.clear.tooltip")); //クリア
            btnOpenFile.setToolTipText(Message.getString("searchresultpanel.tooltip.open")); //検索結果箇所を開く
            btnExport.setToolTipText(Message.getString("mainmenu.file.export")); //エクスポート
            btnMoveUp.setToolTipText(Message.getString("searchresultpanel.tooltip.preview")); //前へ
            btnMoveDown.setToolTipText(Message.getString("searchresultpanel.tooltip.next")); //次へ
            btnExpand.setToolTipText(Message.getString("treechooserdialog.tooltip.expandall")); //すべて展開
            btnCollapse.setToolTipText(Message.getString("treechooserdialog.tooltip.collapseall")); //すべて収納
            btnNewTree.setToolTipText(Message.getString("searchresultpanel.tooltip.new")); //新規構造ツリー
            btnRefresh.setToolTipText(Message.getString("searchresultpanel.tooltip.update")); //検索結果更新

            // イベント追加
            btnMoveUp.addActionListener(this);
            btnMoveDown.addActionListener(this);
            btnExpand.addActionListener(this);
            btnCollapse.addActionListener(this);
            btnOpenFile.addActionListener(this);
            treeSearch.addMouseListener(this);
            btnNewTree.addActionListener(this);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * 検索結果モデルの変更通知イベント
     * @param o			通知元
     * @param arg		通知項目
     */
    @Override
    public void update(Observable o, Object arg) {
        // 検索結果モデルを更新する.
        updateModel();
    }

    /**
     * 検索結果モデルを更新する.
     */
    private void updateModel() {
        // 検索結果モデルの設定
        this.treeSearch.setModel(this.model.getTreeModel());

        // パネルタイトル
        this.label.setText(this.model.getTitle());

        // 新規構造ツリーボタンのイネーブル切替
        boolean enabled = false;
        DefaultMutableTreeNode root = this.model.getRootNode();
        if (root != null && root.getChildCount() > 0) {
            // 最初の子ノードはProcedureであること。
            DefaultMutableTreeNode child = (DefaultMutableTreeNode) root.getChildAt(0);
            if (child != null
                && child.getUserObject() != null
                && child.getUserObject() instanceof Procedure) {
                enabled = true;
            }
        }
        Icon icon = null;
        if (enabled) {
            // イネーブル用ボタンアイコン
            icon = ResourceUtils.getIcon("new_tree.gif");
        }
        else {
            // ディスイネーブル用ボタンアイコン
            icon = ResourceUtils.getIcon("new_tree_gray.gif");
        }
        this.btnNewTree.setIcon(icon);
        this.btnNewTree.setEnabled(enabled);

    }


    /**
     * 検索結果モデルを取得する
     * @return		検索結果モデル
     */
    public SearchResultModel getModel() {
        return model;
    }


    /**
     * フォーカスリスナを設定する
     * @param listener		フォーカスリスナ
     */
    @Override
    public void addTabFocusListener(TabFocusListener listener) {
        this.addFocusListener(listener);
        // 子コンポーネントにもフォーカスリスナを設定する
        SwingUtils.addChildFocusListener(this, listener);
    }


    /**
     * エクスポートを行う
     */
    @Override
    public void export(File file) {
        if (this.model == null) return;

        model.writeFile(file);
    }

    /**
     * パネルにアクションリスナを設定する.<br/>
     * メニューバーに作成済みのアクションリスナをパネルボタンに割り当てる。
     * @param menu		メニューバー
     */
    @Override
    public void setActionListener(MainMenu menu) {
        // 分析情報エクスポートアクション
        this.btnExport.addActionListener(menu.getActionExportAnalysis());
        // 検索結果箇所を開く
        actionOpen = (ViewOpenAnalysisLineAction) menu.getActionOpenAnalysisLine();
        // 新規構造ツリーアクション
        this.actionOpenLanguageTree = menu.getActionOpenLanguageTree();
        // クリア
        this.btnClear.addActionListener(menu.getActionSearchResult(TRACE_DIR.END));
        // 検索結果更新
        this.btnRefresh.addActionListener(menu.getActionSearchResult(TRACE_DIR.REFRESH));

    }

    /**
     * 選択行のコード情報を取得する.<br/>
     * テーブルモデルの1列目にはコード情報オブジェクトが設定されている。
     * @return		コード情報
     */
    @Override
	public CodeLine getSelectedCodeLine() {

        // 選択されたファイルのソースファイルオブジェクトを取得する。
        DefaultMutableTreeNode node = (DefaultMutableTreeNode) this.treeSearch.getLastSelectedPathComponent();
        if (node == null)  return null;
        if (node.getUserObject() == null) return null;

        // ブロックオブジェクトであるか？
        if (node.getUserObject() instanceof IBlock) {
            IBlock block = (IBlock)node.getUserObject();
            return block.getStartCodeLine();
        }
        else if (node.getUserObject() instanceof CodeLine) {
            return (CodeLine)node.getUserObject();
        }

        return null;
    }

    /**
     * モデルのクリアを行う。
     */
    @Override
    public void clearModel() {
        // モデルクリア
        model.clearSearchResult();
    }

    /**
     * タブのクローズを行う
     */
    @Override
    public void closeTab() { }

    /**
     * ボタンのクリックイベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {

        if (event.getSource() == this.btnMoveUp) {
            moveUp();
        }
        else if (event.getSource() == this.btnMoveDown) {
            moveDown();
        }
        else if (event.getSource() == this.btnExpand) {
            // すべて展開
            expandTreeAll();
        }
        else if (event.getSource() == this.btnCollapse) {
            // すべて収納
            collapseTreeAll();
        }
        else if (event.getSource() == this.btnOpenFile) {
            // 該当個所を開く
            openAnalysisLine();
        }
        else if (event.getSource() == this.btnNewTree) {
            // 新規構造ツリー
            DefaultMutableTreeNode node = (DefaultMutableTreeNode)this.treeSearch.getLastSelectedPathComponent();
            if (node == null) return;
            if (node.getUserObject() == null) return;
            if (!(node.getUserObject() instanceof IBlock)) return;
            this.actionOpenLanguageTree.openLanguageTree((IBlock)node.getUserObject());
        }
    }

    /**
     * 選択行を上に移動する.<br/>
     * 未選択の場合は、先頭行を選択する.<br/>
     * 先頭行の場合は、末尾行を選択する.
     */
    public void moveUp() {
        // 選択ノード
        SearchTreeNode selectnode = (SearchTreeNode)this.treeSearch.getLastSelectedPathComponent();
        SearchTreeNode root = (SearchTreeNode)this.treeSearch.getModel().getRoot();

        // ツリーノードを順方向で列挙
        SearchTreeNode movenode = null;
        SearchTreeNode prevnode = null;

        // ツリーノードを順方向で列挙
        Enumeration<?> depth = root.preorderEnumeration();
        while(depth.hasMoreElements()) {
            SearchTreeNode treeNode = (SearchTreeNode)depth.nextElement();
            boolean match = treeNode.isMatch();
            // ノード検索を行う
            if (treeNode == selectnode) {
                if (prevnode != null) {
                    movenode = prevnode;
                    break;
                }
            }
            // 直前の検索ノード
            if (match) {
                prevnode = treeNode;
            }
        }
        if (movenode == null) {
            movenode = prevnode;
        }
        if (movenode == null) return;

        // 選択ノードのパスを取得する
        TreePath path = SwingUtils.getTreePath(movenode);

        // 選択パスを選択状態にする
        this.treeSearch.setSelectionPath(path);
        this.treeSearch.scrollPathToVisible(path);

        // 該当個所を開く
        this.btnOpenFile.doClick();

    }


    /**
     * 選択行を下に移動する.<br/>
     * 未選択の場合は、先頭行を選択する.<br/>
     * 末尾行の場合は、先頭行を選択する.
     */
    public void moveDown() {

        // 選択ノード
        SearchTreeNode selectnode = (SearchTreeNode)this.treeSearch.getLastSelectedPathComponent();
        SearchTreeNode root = (SearchTreeNode)this.treeSearch.getModel().getRoot();

        // ツリーノードを順方向で列挙
        SearchTreeNode movenode = null;
        SearchTreeNode firstnode = null;
        SearchTreeNode currentnode = null;

        // ツリーノードを順方向で列挙
        Enumeration<?> depth = root.preorderEnumeration();
        while(depth.hasMoreElements()) {
            SearchTreeNode treeNode = (SearchTreeNode)depth.nextElement();
            boolean match = treeNode.isMatch();
            // 最初の検索ノード
            if (firstnode == null && match) {
                firstnode = treeNode;
                // 現在選択ノードが無いので、最初の検索ノードを選択状態とするので、以降検索の必要無し。
                if (selectnode == null) {
                    break;
                }
            }
            // ノード検索を行う
            if (treeNode == selectnode) {
                currentnode = treeNode;
                continue;
            }
            if (currentnode != null && match) {
                movenode = treeNode;
                break;
            }
        }
        if (movenode == null) {
            movenode = firstnode;
        }
        if (movenode == null) return;

        // 選択ノードのパスを取得する
        TreePath path = SwingUtils.getTreePath(movenode);

        // 選択パスを選択状態にする
        this.treeSearch.setSelectionPath(path);
        this.treeSearch.scrollPathToVisible(path);

        // 該当個所を開く
        this.btnOpenFile.doClick();

    }

    /**
     * 該当個所を開く
     */
    public void openAnalysisLine() {
        // 選択ツリーパス
        TreePath path = this.treeSearch.getSelectionPath();
        // 選択ツリーパスを選択する
        this.actionOpen.openSearchLine(path);
    }


    /**
     * 選択ブロックを取得する
     * @return		選択ブロック
     */
    @Override
    public IBlock getSelectedBlock() {

        // 選択されたファイルのソースファイルオブジェクトを取得する。
        DefaultMutableTreeNode node = (DefaultMutableTreeNode) this.treeSearch.getLastSelectedPathComponent();
        if (node == null)  return null;
        if (node.getUserObject() == null) return null;

        // ブロックオブジェクトであるか？
        if (node.getUserObject() instanceof IBlock) {
            return (IBlock)node.getUserObject();
        }

        return null;
    }

    /**
     * 選択付加情報を取得する
     * @return		選択付加情報
     */
    @Override
    public IInformation getSelectedInformation() {

        // 選択されたファイルのソースファイルオブジェクトを取得する。
        DefaultMutableTreeNode node = (DefaultMutableTreeNode) this.treeSearch.getLastSelectedPathComponent();
        if (node == null)  return null;
        if (node.getUserObject() == null) return null;

        // 付加情報オブジェクトであるか？
        if (node.getUserObject() instanceof IInformation) {
            return (IInformation)node.getUserObject();
        }

        return null;
    }

    /**
     * ソースビュープロパティを設定する
     * @param properties		ソースビュープロパティ
     */
    @Override
    public void setSourceProperties(SourceProperties properties) {
        // トレースツリーのマーキング色の設定を行う。
        // 文字色
        this.treeSearch.setForecolor(properties.getSearchFontColor());
        // 背景色
        this.treeSearch.setBackcolor(properties.getSearchBackgroundColor());
        // スタイル
        this.treeSearch.setFontstyle(KscopeProperties.SEARCHTREE_FONTSTYLE);

        // ツリーの再描画
        this.treeSearch.invalidate();
        this.treeSearch.validate();
        this.treeSearch.repaint();

    }

    /**
     * 選択タブのツリーをすべて収納する。
     */
    public void collapseTreeAll() {
        int row = this.treeSearch.getRowCount()-1;
        while(row>=0) {
            this.treeSearch.collapseRow(row);
            row--;
        }
        // ルートノードのみ展開
        this.treeSearch.expandRow(0);
    }


    /**
     * 選択タブのツリーをすべて展開する。
     */
    public void expandTreeAll() {
        int row = 0;
        while(row<this.treeSearch.getRowCount()) {
            this.treeSearch.expandRow(row);
            row++;
        }
    }


    /**
     * マウスクリックイベント
     * @param event			マウスイベント情報
     */
    @Override
    public void mouseClicked(MouseEvent event) {
        // ダブルクリックチェック
        if (SwingUtilities.isLeftMouseButton(event) && event.getClickCount() == 2) {
            // 該当個所を開く
            this.btnOpenFile.doClick();
        }
    }

    /**
     * マウスボタンダウンイベント
     * @param e		マウスイベント情報
     */
    @Override
    public void mousePressed(MouseEvent e) { }

    /**
     * マウスボタンアップイベント
     * @param e		マウスイベント情報
     */
    @Override
    public void mouseReleased(MouseEvent e) {}

    /**
     * マウスオーバーイベント
     * @param e		マウスイベント情報
     */
    @Override
    public void mouseEntered(MouseEvent e) {}

    /**
     * マウスアウトイベント
     * @param e		マウスイベント情報
     */
    @Override
    public void mouseExited(MouseEvent e) {}

    /**
     * 検索結果のキーワードリストを取得する
     * @return		検索キーワードリスト
     */
    public Keyword[] getSearchKeywords() {

        SearchTreeModel model = (SearchTreeModel)this.treeSearch.getModel();
        if (model == null) return null;
        SearchTreeNode root = (SearchTreeNode)model.getRoot();

        // キーワードリスト
        List<Keyword> list = new ArrayList<Keyword>();

        // 検索条件
        SearchOption option = model.getSearchOption();

        // ツリーノードを順方向で列挙
        Enumeration<?> depth = root.preorderEnumeration();
        while(depth.hasMoreElements()) {
            DefaultMutableTreeNode treeNode = (DefaultMutableTreeNode)depth.nextElement();

            // ノード検索を行う
            if (treeNode == null || treeNode.getUserObject() == null) {
                continue;
            }
            if (treeNode.getUserObject() instanceof CodeLine) {
                CodeLine line = (CodeLine)treeNode.getUserObject();
                Keyword word = new Keyword(KEYWORD_TYPE.SEARCH);
                word.setKeyword(option.getSearchText());
                word.setSensitivecase(option.isSensitivecase());
                word.setRegex(option.isRegex());
                word.setSearchWord(option.isWord());
                word.setSearchVariable(false);
                word.setSearchLine(line);

                list.add(word);
            }
        }

        if (list.size() <= 0) return null;

        return list.toArray(new Keyword[0]);
    }

    /**
     * 選択項目をクリップボードにコピーする.
     */
    @Override
    public void copyClipboard() {

        // 選択されたファイルのソースファイルオブジェクトを取得する。
        DefaultMutableTreeNode node = (DefaultMutableTreeNode) this.treeSearch.getLastSelectedPathComponent();
        if (node == null)  return;
        if (node.getUserObject() == null) return;
        String text = node.getUserObject().toString();

        // クリップボードにコピーする
        SwingUtils.copyClipboard(text);
    }

    /**
     * エキスポートする情報があるか否か
     */
	@Override
	public boolean isExportable() {
		if (this.model == null) return false;
		return (!this.model.isEmpty());
	}
}


