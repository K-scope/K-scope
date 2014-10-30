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
import java.util.Arrays;
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
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import jp.riken.kscope.Message;
import jp.riken.kscope.action.ViewOpenAnalysisLineAction;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.KEYWORD_TYPE;
import jp.riken.kscope.common.TRACE_DIR;
import jp.riken.kscope.component.SearchTree;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.Keyword;
import jp.riken.kscope.data.SearchOption;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.menu.MainMenu;
import jp.riken.kscope.model.TraceResultModel;
import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * トレース結果パネルクラス
 * @author RIKEN
 *
 */
public class TraceResultPanel extends AnalisysPanelBase implements Observer, IAnalisysComponent, ActionListener, MouseListener, TreeSelectionListener {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;
    /** トレースツリー */
    private SearchTree treeTrace;
    /** エクスポートボタン */
    private JButton btnExport;
    /** ファイルを開く */
    private JButton btnOpenFile;
    /** 移動:上ボタン */
    private JButton btnMoveUp;
    /** 移動:下ボタン */
    private JButton btnMoveDown;
    /** 移動:INボタン */
    private JButton btnMoveIn;
    /** 移動:Outボタン */
    private JButton btnMoveOut;
    /** 移動:Forwardボタン */
    private JButton btnMoveForward;
    /** リフレッシュボタン */
    private JButton btnRefresh;

    /** トレース結果ラベル */
    private JLabel label;

    /** トレース結果テーブルモデル */
    private TraceResultModel model;
    /** 該当個所を開くアクション */
    private ViewOpenAnalysisLineAction actionOpen;

    /**
     * コンストラクタ
     */
    public TraceResultPanel() {
        super();

        // 初期化を行う。
        initialize();

    }

    /**
     * コンストラクタ
     * @param panel		分析情報パネル識別子
     */
    public TraceResultPanel(ANALYSIS_PANEL panel) {
        super(panel);

        // 初期化を行う。
        initialize();

    }

    /**
     * 初期化を行う。
     */
    private void initialize() {

        // モデルの生成を行う
        model = new TraceResultModel();
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
                        Icon icon = ResourceUtils.getIcon("arrow_right.gif");
                        btnMoveIn = new JButton(icon);
                        btnMoveIn.setContentAreaFilled(false);
                        btnMoveIn.setBorderPainted(false);
                        btnMoveIn.setPreferredSize(buttonSize);
                        btnMoveIn.setMinimumSize(buttonSize);
                        btnMoveIn.setMaximumSize(buttonSize);
                        panelButtons.add(btnMoveIn);
                    }
                    {
                        Icon icon = ResourceUtils.getIcon("arrow_left.gif");
                        btnMoveOut = new JButton(icon);
                        btnMoveOut.setContentAreaFilled(false);
                        btnMoveOut.setBorderPainted(false);
                        btnMoveOut.setPreferredSize(buttonSize);
                        btnMoveOut.setMinimumSize(buttonSize);
                        btnMoveOut.setMaximumSize(buttonSize);
                        panelButtons.add(btnMoveOut);
                    }
                    {
                        Icon icon = ResourceUtils.getIcon("forward.gif");
                        btnMoveForward = new JButton(icon);
                        btnMoveForward.setContentAreaFilled(false);
                        btnMoveForward.setBorderPainted(false);
                        btnMoveForward.setPreferredSize(buttonSize);
                        btnMoveForward.setMinimumSize(buttonSize);
                        btnMoveForward.setMaximumSize(buttonSize);
                        panelButtons.add(btnMoveForward);
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
                    // トレースツリー
                    treeTrace = new SearchTree();

                    DefaultMutableTreeNode rootNode = new DefaultMutableTreeNode(Message.getString("mainmenu.window.analysis.trace")); //トレース
                    DefaultTreeModel treeModel = new DefaultTreeModel(rootNode);
                    treeTrace.setModel(treeModel);
                    treeTrace.setRootVisible(true);
                    treeTrace.setShowsRootHandles(true);

                    // ダブルクリックによるノードの展開を行わない。
                    treeTrace.setToggleClickCount(0);
                    // 一行だけ選択可能
                    treeTrace.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);

                    // スクロールパイン
                    JScrollPane scrollTable = new JScrollPane(treeTrace);
                    scrollTable.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
                    scrollTable.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
                    scrollTable.getViewport().setBackground(Color.WHITE);

                    add(scrollTable);

                }

            }

            // ツールチップ設定
            btnOpenFile.setToolTipText(Message.getString("traceresultpanel.tooltip.open"));//トレース箇所を開く
            btnExport.setToolTipText(Message.getString("mainmenu.file.export")); //エクスポート
            btnMoveUp.setToolTipText(Message.getString("traceresultpanel.tooltip.up")); //トレース:アップ
            btnMoveDown.setToolTipText(Message.getString("traceresultpanel.tooltip.down")); //トレース:ダウン
            btnMoveIn.setToolTipText(Message.getString("traceresultpanel.tooltip.in")); //トレース:イン
            btnMoveOut.setToolTipText(Message.getString("traceresultpanel.tooltip.out")); //トレース:アウト
            btnMoveForward.setToolTipText(Message.getString("traceresultpanel.tooltip.foward")); //トレース:フォワード
            btnRefresh.setToolTipText(Message.getString("traceresultpanel.tooltip.update")); //トレース:更新

            // ボタンイベント
            btnMoveUp.addActionListener(this);
            btnMoveDown.addActionListener(this);
            btnOpenFile.addActionListener(this);
            treeTrace.addMouseListener(this);
            treeTrace.addTreeSelectionListener(this);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * トレース結果モデルの変更通知イベント
     * @param o			通知元
     * @param arg		通知項目
     */
    @Override
    public void update(Observable o, Object arg) {
        // トレース結果モデルを更新する.
        updateModel();
    }

    /**
     * トレース結果モデルを更新する.
     */
    private void updateModel() {
        // ツリーモデルの設定
        this.treeTrace.setModel(this.model.getTreeModel());
        // 選択ノードを設定する
        if (this.model.getSelectedBlock() != null) {
            this.treeTrace.setSelectedNode(this.model.getSelectedBlock());
            // 該当個所を開く
            openTraceBlock();
        }

        // 検索条件を設定する
        SearchOption searchOption = new SearchOption();
        searchOption.setSearchText(this.model.getTraceWord());	// トレース変数
        searchOption.setVariable(true);							// 変数検索
        this.treeTrace.setSearchOption(searchOption);

        // パネルタイトル
        this.label.setText(this.model.getTitle());

        // 選択ノードによってボタンのイネーブルを切り替える
        setEnableButtons();

    }


    /**
     * トレース結果モデルを取得する
     * @return		トレース結果モデル
     */
    public TraceResultModel getModel() {
        return model;
    }

    /**
     * トレース結果モデルを設定する
     * @param  model		トレース結果モデル
     */
    public void setModel(TraceResultModel model) {
        if (model == null) return;
        this.model = model;

        // オブザーバを設定する。
        this.model.deleteObservers();
        this.model.addObserver(this);

        // トレース結果モデルを更新する.
        updateModel();
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
        // トレース
        this.btnMoveIn.addActionListener(menu.getActionAnalysisTrace(TRACE_DIR.IN));
        this.btnMoveOut.addActionListener(menu.getActionAnalysisTrace(TRACE_DIR.OUT));
        this.btnMoveForward.addActionListener(menu.getActionAnalysisTrace(TRACE_DIR.FORWARD));
        // トレース:更新
        this.btnRefresh.addActionListener(menu.getActionAnalysisTrace(TRACE_DIR.REFRESH));

        // 検索結果箇所を開く
        actionOpen = (ViewOpenAnalysisLineAction) menu.getActionOpenAnalysisLine();
    }

    /**
     * 選択行のコード情報を取得する.<br/>
     * テーブルモデルの1列目にはコード情報オブジェクトが設定されている。
     * @return		コード情報
     */
    @Override
    public CodeLine getSelectedCodeLine() {

        IBlock block = getSelectedBlock();
        if (block == null) return null;

        return block.getStartCodeLine();
    }

    /**
     * モデルのクリアを行う。
     */
    @Override
    public void clearModel() {
        // モデルクリア
        model.clearTreeModel();
    }

    /**
     * タブのクローズを行う
     */
    @Override
    public void closeTab() {
    }

    /**
     * 選択ブロックを取得する
     * @return		選択ブロック
     */
    @Override
    public IBlock getSelectedBlock() {

        // 選択されたファイルのソースファイルオブジェクトを取得する。
        DefaultMutableTreeNode node = (DefaultMutableTreeNode) this.treeTrace.getLastSelectedPathComponent();
        if (node == null)  return null;
        if (node.getUserObject() == null) return null;

        // ブロックオブジェクトであるか？
        if (node.getUserObject() instanceof IBlock) {
            // 開始CodeLineが存在するか？
            IBlock block = (IBlock)node.getUserObject();
            CodeLine line = block.getStartCodeLine();
            if (line == null) {
                IBlock parent = block.getMotherBlock();
                if (parent != null) {
                    return parent;
                }
            }
            return block;
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
        DefaultMutableTreeNode node = (DefaultMutableTreeNode) this.treeTrace.getLastSelectedPathComponent();
        if (node == null)  return null;
        if (node.getUserObject() == null) return null;

        // 付加情報オブジェクトであるか？
        if (node.getUserObject() instanceof IInformation) {
            return (IInformation)node.getUserObject();
        }

        return null;
    }

    /**
     * ボタンのクリックイベント
     * @param event			イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {

        if (event.getSource() == this.btnMoveUp) {
            // トレース：アップ
            traceUp();
        }
        else if (event.getSource() == this.btnMoveDown) {
            // トレース：アップ
            traceDown();
        }
        else if (event.getSource() == this.btnOpenFile) {
            // 該当個所を開く
            openTraceBlock();
        }

    }

    /**
     * 該当個所を開く
     */
    private void openTraceBlock() {
        List<IBlock> list = new ArrayList<IBlock>();
        // トレースパス
        IBlock[] paths = this.model.getTracePath();
        if (paths != null && paths.length > 0) {
            list.addAll(Arrays.asList(paths));
        }
        // ルート選択子ノードを追加する
        if (this.treeTrace.getLastSelectedPathComponent() != this.treeTrace.getModel().getRoot()) {
	        // 選択ブロック
	        IBlock block = getSelectedBlock();
	        if (block != null) {
	            list.add(block);
	        }
        }
        // 選択ブロックを開く
        this.actionOpen.openTraceBlocks(list.toArray(new IBlock[0]));

    }

    /**
     * トレース：アップを行う
     */
    public void traceUp() {

        DefaultMutableTreeNode currentnode = (DefaultMutableTreeNode) this.treeTrace.getLastSelectedPathComponent();

        TreeModel model = this.treeTrace.getModel();
        if (model == null) return;
        DefaultMutableTreeNode root = (DefaultMutableTreeNode)model.getRoot();
        if (root == null) return;
        TreePath selectPath = null;
        TreePath lastPath = null;
        if (currentnode == null) {
            selectPath = new TreePath(root);
        }
        else {
            // ツリーノードを順方向で列挙
            Enumeration<?> depth = root.preorderEnumeration();
            while (depth.hasMoreElements()) {
                DefaultMutableTreeNode node = (DefaultMutableTreeNode)depth.nextElement();
                if (currentnode == node) {
                    // 選択ノードがルートパスの場合, selectPath=nullとなる。
                    selectPath = lastPath;
                    break;
                }
                // １つ手前のノードパス
                lastPath = new TreePath(node.getPath());
            }
        }

        // 選択パスが存在しなければ、ルートパスを選択する
        // 選択パスが存在しない場合は、選択ノードがルートパスの場合
        if (selectPath == null) {
            selectPath = new TreePath(root);
        }

        // 選択パスを選択状態にする
        this.treeTrace.setSelectionPath(selectPath);
        this.treeTrace.scrollPathToVisible(selectPath);

        // 該当個所を開く
        this.btnOpenFile.doClick();

    }

    /**
     * トレース：ダウンを行う
     */
    public void traceDown() {

        DefaultMutableTreeNode currentnode = (DefaultMutableTreeNode) this.treeTrace.getLastSelectedPathComponent();

        TreeModel model = this.treeTrace.getModel();
        if (model == null) return;
        DefaultMutableTreeNode root = (DefaultMutableTreeNode)model.getRoot();
        if (root == null) return;
        TreePath currentPath = null;
        TreePath selectPath = null;
        if (currentnode == null) {
            selectPath = new TreePath(root);
        }
        else {
            // ツリーノードを順方向で列挙
            Enumeration<?> depth = root.preorderEnumeration();
            while (depth.hasMoreElements()) {
                DefaultMutableTreeNode node = (DefaultMutableTreeNode)depth.nextElement();
                if (currentPath != null) {
                    selectPath = new TreePath(node.getPath());
                    break;
                }
                if (currentnode == node) {
                    // 現在のノードパス
                    currentPath = new TreePath(node.getPath());
                }
            }
        }

        // 選択パスが存在しない場合は、選択ノードが最終パスの場合
        if (selectPath != null) {
            this.treeTrace.setSelectionPath(selectPath);
            this.treeTrace.scrollPathToVisible(selectPath);
        }

        // 該当個所を開く
        this.btnOpenFile.doClick();
    }

    /**
     * ソースビュープロパティを設定する
     * @param properties		ソースビュープロパティ
     */
    @Override
    public void setSourceProperties(SourceProperties properties) {
        // トレースツリーのマーキング色の設定を行う。
        // 文字色
        this.treeTrace.setForecolor(properties.getSearchFontColor());
        // 背景色
        this.treeTrace.setBackcolor(properties.getSearchBackgroundColor());
        // スタイル
        this.treeTrace.setFontstyle(KscopeProperties.SEARCHTREE_FONTSTYLE);

        // ツリーの再描画
        this.treeTrace.invalidate();
        this.treeTrace.validate();
        this.treeTrace.repaint();

    }

    /**
     * トレース結果のキーワードリストを取得する
     * @return		トレースキーワードリスト
     */
    public Keyword[] getTraceKeywords() {

        TreeModel model = this.treeTrace.getModel();
        if (model == null) return null;
        DefaultMutableTreeNode root = (DefaultMutableTreeNode)model.getRoot();

        // キーワードリスト
        List<Keyword> list = new ArrayList<Keyword>();

        // トレース変数
        String variable = this.model.getTraceWord();

        // ツリーノードを順方向で列挙
        Enumeration<?> depth = root.preorderEnumeration();
        while(depth.hasMoreElements()) {
            DefaultMutableTreeNode treeNode = (DefaultMutableTreeNode)depth.nextElement();

            // ノード検索を行う
            if (treeNode == null || treeNode.getUserObject() == null) {
                continue;
            }
            if (treeNode.getUserObject() instanceof IBlock) {
                IBlock block = (IBlock)treeNode.getUserObject();
                CodeLine line = block.getStartCodeLine();
                if (line == null) {
                    IBlock parent = block.getMotherBlock();
                    if (parent != null) {
                        line = parent.getStartCodeLine();
                    }
                }
                Keyword word = new Keyword(KEYWORD_TYPE.TRACE);
                word.setKeyword(variable);
                word.setSearchWord(true);
                word.setSensitivecase(false);
                word.setSearchVariable(true);
                word.setSearchLine(line);

                list.add(word);
            }
        }

        if (list.size() <= 0) return null;

        return list.toArray(new Keyword[0]);
    }


    /**
     * マウスクリックイベント
     * @param event		マウスイベント情報
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
     * トレースツリーの選択変更イベント
     * @param event         イベント情報
     */
    @Override
    public void valueChanged(TreeSelectionEvent event) {
        if (event.getSource() == this.treeTrace) {
            DefaultMutableTreeNode currentnode = (DefaultMutableTreeNode) this.treeTrace.getLastSelectedPathComponent();
            if (currentnode == null) return;
            if (currentnode.getUserObject() instanceof IBlock) {
                // 選択ブロックを設定する
                this.model.setSelectedBlock((IBlock)currentnode.getUserObject());

                // 選択ノードによってボタンのイネーブルを切り替える
                setEnableButtons();
            }
        }
    }

    /**
     * 選択ノードによってボタンのイネーブルを切り替える。
     */
    private void setEnableButtons() {
        DefaultMutableTreeNode currentnode = (DefaultMutableTreeNode) this.treeTrace.getLastSelectedPathComponent();
        if (currentnode == null) return;
        if (currentnode.getUserObject() instanceof IBlock) {
            // 選択ノード
            // ルート選択
            boolean enabledMoveUp = true;
            boolean enabledMoveDown = true;
            if (currentnode == this.treeTrace.getModel().getRoot()) {
                enabledMoveUp = false;
            }
            if (currentnode == this.treeTrace.getLastTreeNode()) {
                enabledMoveDown = false;
            }

            Icon iconMoveUp = null;
            if (enabledMoveUp) {
                iconMoveUp = ResourceUtils.getIcon("arrow_up.gif");
            }
            else {
                iconMoveUp = ResourceUtils.getIcon("arrow_up_gray.gif");
            }
            Icon iconMoveDown = null;
            if (enabledMoveDown) {
                iconMoveDown = ResourceUtils.getIcon("arrow_down.gif");
            }
            else {
                iconMoveDown = ResourceUtils.getIcon("arrow_down_gray.gif");
            }
            btnMoveUp.setIcon(iconMoveUp);
            btnMoveUp.setEnabled(enabledMoveUp);
            btnMoveDown.setIcon(iconMoveDown);
            btnMoveDown.setEnabled(enabledMoveDown);
        }

        // フォワードボタンのイネーブル切替
        boolean enabled = (this.model.getTracePath() != null && this.model.getTracePath().length > 2);
        Icon icon = null;
        if (enabled) {
            icon = ResourceUtils.getIcon("forward.gif");
        }
        else {
            icon = ResourceUtils.getIcon("forward_gray.gif");
        }
        this.btnMoveForward.setEnabled(enabled);
        this.btnMoveForward.setIcon(icon);

    }

    /**
     * 選択項目をクリップボードにコピーする.
     */
    @Override
    public void copyClipboard() {

        // 選択されたファイルのソースファイルオブジェクトを取得する。
        DefaultMutableTreeNode node = (DefaultMutableTreeNode) this.treeTrace.getLastSelectedPathComponent();
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


