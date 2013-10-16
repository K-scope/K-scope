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
import java.awt.event.MouseListener;
import java.io.File;
import java.util.Observable;
import java.util.Observer;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.border.CompoundBorder;
import javax.swing.border.LineBorder;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeSelectionModel;

import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.component.ObjectTree;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.menu.MainMenu;
import jp.riken.kscope.model.ReferenceModel;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * 参照一覧パネルクラス
 * @author riken
 *
 */
public class ReferencePanel extends AnalisysPanelBase implements Observer, IAnalisysComponent, ActionListener {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;
    /** 参照一覧ツリー */
    private ObjectTree treeReference;
    /** クリアボタン */
    private JButton btnClear;
    /** エクスポートボタン */
    private JButton btnExport;
    /** ファイルを開く */
    private JButton btnOpenFile;
    /** 参照一覧ラベル */
    private JLabel label;
    /** すべて展開ボタン */
    private JButton btnExpand;
    /** すべて収納ボタン */
    private JButton btnCollapse;

    /** 参照一覧パネルモデル */
    private ReferenceModel model;

    /**
     * コンストラクタ
     */
    public ReferencePanel() {
        super();

        // 初期化を行う。
        initialize();

    }

    /**
     * コンストラクタ
     * @param panel		分析情報パネル識別子
     */
    public ReferencePanel(ANALYSIS_PANEL panel) {
        super(panel);

        // 初期化を行う。
        initialize();

    }

    /**
     * 初期化を行う。
     */
    private void initialize() {

        // モデルの生成を行う
        model = new ReferenceModel();
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
                        btnClear.addActionListener( new ActionListener() {
                            @Override
                            public void actionPerformed(ActionEvent e) {
                                // モデルクリア
                                clearModel();
                            }
                        });
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
                    // 参照一覧ツリー
                    treeReference = new ObjectTree();
                    treeReference.setModel(model.getTreeModel());
                    treeReference.setRootVisible(true);
                    treeReference.setShowsRootHandles(true);

                    // ダブルクリックによるノードの展開を行わない。
                    treeReference.setToggleClickCount(0);
                    // 一行だけ選択可能
                    treeReference.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);

                    // スクロールパイン
                    JScrollPane scrollTable = new JScrollPane(treeReference);
                    scrollTable.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
                    scrollTable.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
                    scrollTable.getViewport().setBackground(Color.WHITE);

                    add(scrollTable);

                }

            }

            // ツールチップ設定
            btnClear.setToolTipText(Message.getString("informationdialog.button.clear.tooltip")); //クリア
            btnOpenFile.setToolTipText(Message.getString("referencepanel.tooltip.result")); //検索結果箇所を開く
            btnExport.setToolTipText(Message.getString("mainmenu.file.export")); //エクスポート
            btnExpand.setToolTipText(Message.getString("treechooserdialog.tooltip.expandall")); //すべて展開
            btnCollapse.setToolTipText(Message.getString("treechooserdialog.tooltip.collapseall")); //すべて収納

            // イベント追加
            btnExpand.addActionListener(this);
            btnCollapse.addActionListener(this);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * 参照一覧モデルの変更通知イベント
     * @param o			通知元
     * @param arg		通知項目
     */
    @Override
    public void update(Observable o, Object arg) {
        // 参照一覧モデル
        ReferenceModel observer = (ReferenceModel)o;
        treeReference.setModel(model.getTreeModel());

        // パネルタイトル
        this.label.setText(observer.getTitle());

    }

    /**
     * 参照一覧モデルを取得する
     * @return		検索結果モデル
     */
    public ReferenceModel getModel() {
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
        if (this.model.isEmpty()) return;

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
        // 参照一覧箇所を開く
        this.btnOpenFile.addActionListener((ActionListener) menu.getActionOpenAnalysisLine());
        this.treeReference.addMouseListener((MouseListener) menu.getActionOpenAnalysisLine());
    }

    /**
     * 選択ノードのコード情報を取得する.<br/>
     * @return		コード情報
     */
    @Override
    public CodeLine getSelectedCodeLine() {
        IBlock block = getSelectedBlock();
        if (block == null) return null;

        return block.getStartCodeLine();
    }

    /**
     * 選択ノードのブロックを取得する
     * @return		選択ブロック
     */
    @Override
    public IBlock getSelectedBlock() {

        // 選択されたファイルのソースファイルオブジェクトを取得する。
        DefaultMutableTreeNode node = (DefaultMutableTreeNode) this.treeReference.getLastSelectedPathComponent();
        if (node == null)  return null;
        if (node.getUserObject() == null) return null;

        // ブロックオブジェクトであるか？
        if (node.getUserObject() instanceof IBlock) {
            return (IBlock)node.getUserObject();
        }

        return null;
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
    public void closeTab() { }


    /**
     * 選択付加情報を取得する
     * @return		選択付加情報
     */
    @Override
    public IInformation getSelectedInformation() {

        // 選択されたファイルのソースファイルオブジェクトを取得する。
        DefaultMutableTreeNode node = (DefaultMutableTreeNode) this.treeReference.getLastSelectedPathComponent();
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
    public void setSourceProperties(SourceProperties properties) {}

    /**
     * ボタンのクリックイベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {
        if (event.getSource() == this.btnExpand) {
            // すべて展開
            expandTreeAll();
        }
        else if (event.getSource() == this.btnCollapse) {
            // すべて収納
            collapseTreeAll();
        }
    }


    /**
     * 選択タブのツリーをすべて収納する。
     */
    public void collapseTreeAll() {
        int row = this.treeReference.getRowCount()-1;
        while(row>=0) {
            this.treeReference.collapseRow(row);
            row--;
        }
        // ルートノードのみ展開
        this.treeReference.expandRow(0);
    }


    /**
     * 選択タブのツリーをすべて展開する。
     */
    public void expandTreeAll() {
        int row = 0;
        while(row<this.treeReference.getRowCount()) {
            this.treeReference.expandRow(row);
            row++;
        }
    }

    /**
     * 選択項目をクリップボードにコピーする.
     */
    @Override
    public void copyClipboard() {

        // 選択されたファイルのソースファイルオブジェクトを取得する。
        DefaultMutableTreeNode node = (DefaultMutableTreeNode) this.treeReference.getLastSelectedPathComponent();
        if (node == null)  return;
        if (node.getUserObject() == null) return;
        String text = node.getUserObject().toString();

        // クリップボードにコピーする
        SwingUtils.copyClipboard(text);
    }

    /**
     * エキスポート情報があるか否か
     */
	@Override
	public boolean isExportable() {
		if (this.model == null) return false;
		return (!this.model.isEmpty());
	}
}


