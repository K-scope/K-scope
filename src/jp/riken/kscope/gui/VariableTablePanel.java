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
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.FlowLayout;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.util.Arrays;
import java.util.Observable;
import java.util.Observer;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.RowSorter;
import javax.swing.SortOrder;
import javax.swing.SwingUtilities;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.JTableHeader;

import jp.riken.kscope.Message;
import jp.riken.kscope.action.AnalysisOperandAction;
import jp.riken.kscope.action.AnalysisReferenceAction;
import jp.riken.kscope.action.AnalysisScopeAction;
import jp.riken.kscope.action.EditInformationEditAction;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.component.JStripeTable;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.menu.MainMenu;
import jp.riken.kscope.menu.VariablePopupMenu;
import jp.riken.kscope.model.VariableTableModel;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * 変数特性一覧パネルクラス
 * @author riken
 *
 */
public class VariableTablePanel extends AnalisysPanelBase implements Observer, IAnalisysComponent, MouseListener, ActionListener {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** クリアボタン */
    private JButton btnClear;
    /** 付加情報ボタン */
    private JButton btnEdit;
    /** 演算カウントボタン */
    private JButton btnOperand;
    /** 宣言・定義・参照ボタン */
    private JButton btnReference;
    /** 変数有効域ボタン */
    private JButton btnScope;
    /** ファイルを開くボタン */
    private JButton btnOpenFile;
    /** エクスポートボタン */
    private JButton btnExport;
    /** 変数特性一覧ラベル */
    private JLabel label;
    /** コンテンツボックス */
    private Box contentInfo;
    /** 余白ボックス */
    private final Component glue = Box.createVerticalGlue();
    /** スクロールパイン */
    private JScrollPane scrollPane;

    /** 変数特性一覧テーブルモデル */
    private VariableTableModel model;

    /** 展開ボタンアイコン */
    private Icon expand_icon = ResourceUtils.getIcon("expand_arrow.gif");
    /** 収納ボタンアイコン */
    private Icon collapse_icon = ResourceUtils.getIcon("collapse_arrow.gif");

    /** 変数特性一覧コンテキストメニュー */
    private VariablePopupMenu variablePopupMenu;

    /** 選択付加情報ノードパネル */
    private NodePanel selectedInfo;

    /** 付加情報編集アクション */
    private EditInformationEditAction actionEdit;
    /** 演算カウントアクション */
    private AnalysisOperandAction actionOperand;
    /** 宣言・定義・参照アクション */
    private AnalysisReferenceAction actionReference;
    /** 変数有効域アクション */
    private AnalysisScopeAction actionScope;
    /** 選択パネル背景色 */
    private Color colorSelectedPanel;

    /**
     * コンストラクタ
     */
    public VariableTablePanel() {
        super();

        // モデルの生成を行う
        model = new VariableTableModel();
        // オブザーバを設定する。
        model.addObserver(this);

        // GUI初期化を行う。
        initGUI();

    }

    /**
     * コンストラクタ
     * @param proparties		分析情報パネル識別子
     */
    public VariableTablePanel(ANALYSIS_PANEL proparties) {
        super(proparties);

        // モデルの生成を行う
        model = new VariableTableModel();
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
//            setPreferredSize(new Dimension(400, 24));

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
                        btnClear.setPreferredSize(buttonSize);
                        btnClear.setMinimumSize(buttonSize);
                        btnClear.setMaximumSize(buttonSize);
                        btnClear.setContentAreaFilled(false);
                        btnClear.setBorderPainted(false);
                    }
                    // 余白設定
                    //panelButtons.add(Box.createHorizontalStrut(5));
                    // 付加情報編集ボタン
                    {
                        Icon icon = ResourceUtils.getIcon("edit_info.gif");
                        btnEdit = new JButton(icon);
                        panelButtons.add(btnEdit);
                        btnEdit.setContentAreaFilled(false);
                        btnEdit.setBorderPainted(false);
                        btnEdit.setPreferredSize(buttonSize);
                        btnEdit.setMinimumSize(buttonSize);
                        btnEdit.setMaximumSize(buttonSize);
                    }
                    // 演算カウントボタン
                    {
                        Icon icon = ResourceUtils.getIcon("count.gif");
                        btnOperand = new JButton(icon);
                        panelButtons.add(btnOperand);
                        btnOperand.setContentAreaFilled(false);
                        btnOperand.setBorderPainted(false);
                        btnOperand.setPreferredSize(buttonSize);
                        btnOperand.setMinimumSize(buttonSize);
                        btnOperand.setMaximumSize(buttonSize);
                    }
                    // 宣言・定義・参照ボタン
                    {
                        Icon icon = ResourceUtils.getIcon("reference.gif");
                        btnReference = new JButton(icon);
                        panelButtons.add(btnReference);
                        btnReference.setContentAreaFilled(false);
                        btnReference.setBorderPainted(false);
                        btnReference.setPreferredSize(buttonSize);
                        btnReference.setMinimumSize(buttonSize);
                        btnReference.setMaximumSize(buttonSize);
                    }
                    // 変数有効域ボタン
                    {
                        Icon icon = ResourceUtils.getIcon("area.gif");
                        btnScope = new JButton(icon);
                        panelButtons.add(btnScope);
                        btnScope.setContentAreaFilled(false);
                        btnScope.setBorderPainted(false);
                        btnScope.setPreferredSize(buttonSize);
                        btnScope.setMinimumSize(buttonSize);
                        btnScope.setMaximumSize(buttonSize);
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
                    //label.setText("");
                }
            }
            {
                {
                    // 変数特性一覧パネル
                    contentInfo = Box.createVerticalBox();
                    //contentInfo.setBorder(BorderFactory.createLineBorder(Color.RED, 1));
                    contentInfo.setOpaque(false);

                    // スクロールパイン
                    scrollPane = new JScrollPane();
                    scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
                    scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
                    scrollPane.setViewportView(contentInfo);
                    scrollPane.getViewport().setBackground(Color.WHITE);
                    // スクロール量を調整
                    scrollPane.getVerticalScrollBar().setUnitIncrement(Constant.VERTICALSCROLL_INCREMENT);
                    add(scrollPane);
                }

            }
            // イベント追加
            btnClear.addActionListener(this);
            btnEdit.addActionListener(this);
            btnOperand.addActionListener(this);
            btnReference.addActionListener(this);
            btnScope.addActionListener(this);

            // ツールチップ設定
            btnClear.setToolTipText(Message.getString("informationdialog.button.clear.tooltip")); //クリア
            btnEdit.setToolTipText(Message.getString("replacementresulttablepanel.tooltip.info")); //付加情報
            btnOperand.setToolTipText(Message.getString("mainmenu.project.config.operation")); //演算数カウント
            btnReference.setToolTipText(Message.getString("mainmenu.analysis.dec-def-ref")); //宣言・定義・参照
            btnScope.setToolTipText(Message.getString("mainmenu.analysis.valiablescope")); //変数有効域
            btnOpenFile.setToolTipText(Message.getString("informationpanel.tooltip.openblock")); //選択箇所を開く
            btnExport.setToolTipText(Message.getString("mainmenu.file.export")); //エクスポート

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * 変数特性一覧モデルの変更通知イベント
     * @param o			通知元
     * @param arg		通知項目
     */
    @Override
    public void update(Observable o, Object arg) {
        // 変数特性一覧のクリア
        clearComponent();

        // テーブルモデル
        VariableTableModel observer = (VariableTableModel)o;

        // パネルタイトル
        this.label.setText(observer.getTitle());

        int count = observer.getListProcedureInfoCount();
        for (int i=0; i<count; i++) {
            VariableTableModel.ProcedureInfo info = observer.getProcedureInfo(i);

            // 変数特性一覧を追加する
            addVariableInfo(info);
        }

    }

    /**
     * 変数特性一覧を追加する
     * @param info			変数特性一覧情報
     */
    public void addVariableInfo(VariableTableModel.ProcedureInfo info) {

        // 追加コンポーネントの作成
        JComponent component = makeRowsPanel(info);

        // コンテンツパネルにコンポーネントを追加する
        addComponent(component);

    }


    /**
     * コンテンツパネルをクリアする
     */
    public void clearComponent() {
        if (this.contentInfo.getComponentCount() > 0) {
            this.contentInfo.removeAll();
        }

        // 再描画
        refreshPanel();

        // 選択付加情報ブロックのクリア
        this.selectedInfo = null;
    }

    /**
     * コンテンツパネルにコンポーネントを追加する
     * @param component		追加コンポーネント
     */
    private void addComponent(final JComponent component) {
        // 追加コンポーネントサイズの変更
        component.setMaximumSize(new Dimension(Short.MAX_VALUE, component.getPreferredSize().height));

        // 追加コンポーネントは左詰めで配置する
        component.setAlignmentX(Component.LEFT_ALIGNMENT);

        // コンポーネントの追加
        this.contentInfo.remove(glue);
//        this.contentInfo.add(Box.createVerticalStrut(5));
        this.contentInfo.add(component);
        this.contentInfo.add(glue);

        EventQueue.invokeLater(new Runnable() {
            @Override public void run() {
                component.scrollRectToVisible(component.getBounds());
                scrollPane.getViewport().setViewPosition(new Point(0, 0));
            }
        });

        // 再描画
        refreshPanel();

        return;
    }

    /**
     * パネルを再描画する
     */
    private void refreshPanel() {

        // 再描画
        this.contentInfo.revalidate();
        this.validate();
        this.repaint();

    }

    /**
     * 変数特性一覧パネルの追加
     * @param info		変数特性一覧情報
     * @return			変数特性一覧パネル
     */
    private JComponent makeRowsPanel(VariableTableModel.ProcedureInfo info) {
        // 変数特性一覧パネル
        NodePanel rows = new NodePanel(info);
        rows.setLayout(new BoxLayout(rows, BoxLayout.Y_AXIS));
        rows.setOpaque(false);
        rows.setBorder(BorderFactory.createLineBorder(Color.GRAY, 1));
        rows.addMouseListener(this);

        // 名前パネル
        JPanel panelName = new JPanel();
        panelName.setLayout(new FlowLayout(FlowLayout.LEFT, 5, 0));
        panelName.setOpaque(false);

        // パネル展開ボタン：初期表示は展開ボタン
        java.awt.Dimension buttonSize = new java.awt.Dimension(18, 18);
        JButton button = new JButton(expand_icon);
        button.setContentAreaFilled(false);
        button.setBorderPainted(false);
        button.setPreferredSize(buttonSize);
        button.setMinimumSize(buttonSize);
        button.setMaximumSize(buttonSize);

        // 名前ラベル
        panelName.add(button);
        String name = "BLOCK";
        if (info.getBlock() != null) {
            name = info.getBlock().toString();
        }
        JLabel label = new JLabel(name);
        label.setOpaque(false);
        panelName.add(label);

        // 変数特性一覧パネル
        JPanel panelTable = new JPanel();
        panelTable.setLayout(new BoxLayout(panelTable, BoxLayout.Y_AXIS));
        panelTable.setOpaque(false);
        panelTable.setBorder(new EmptyBorder(5, 40, 5, 40));

        // 変数特性一覧テーブル
        JTable tableVariable = new JStripeTable();
        tableVariable.setModel(info.getTableModel());

        tableVariable.setAutoCreateColumnsFromModel(false);
        tableVariable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION );
        tableVariable.setColumnSelectionAllowed(false);
        tableVariable.addMouseListener(this);
        // テーブル変更イベント
        VariableListSelectionListener listener = new VariableListSelectionListener(tableVariable);
        tableVariable.getSelectionModel().addListSelectionListener(listener);

        // テーブル列幅設定
        DefaultTableColumnModel columnModel = (DefaultTableColumnModel)tableVariable.getColumnModel();
        model.setTableColumnWidth(columnModel);

        // テーブルヘッダーと行を別々に描画する
        JTableHeader tableHeader = tableVariable.getTableHeader();

        tableVariable.setShowGrid(false);
        tableVariable.setIntercellSpacing(new Dimension(0,0));

        // ソート
        tableVariable.setAutoCreateRowSorter(true);
        int index = 1;
        tableVariable.getRowSorter().setSortKeys(
            Arrays.asList(new RowSorter.SortKey(index, SortOrder.DESCENDING)));

        // 変数特性一覧テーブルのボーダー設定
        LineBorder border = new LineBorder(Color.GRAY, 1, false);
        tableVariable.setBorder(border);
        tableHeader.setBorder(border);

        // 変数特性一覧パネルに追加
        panelTable.add(tableHeader);
        panelTable.add(tableVariable);

        tableVariable.validate();
        tableVariable.updateUI();

        // 変数特性一覧展開ボタンのアクションリスナの設定
        button.addActionListener(new VariableExpandAction(panelTable));

        // 変数特性一覧パネルに名前パネル、本文パネルの追加
        rows.add(panelName);
        rows.add(panelTable);

        // テーブルを設定
        rows.setTableVariable(tableVariable);

        // コンテキストメニューを設定する
        tableVariable.setComponentPopupMenu(this.variablePopupMenu);

        return rows;
    }

    /**
     * 変数特性ノードパネル
     * @author riken
     */
    private class NodePanel extends JPanel {
        /** シリアル番号 */
        private static final long serialVersionUID = 1L;

        /** 変数特性ブロック */
        private VariableTableModel.ProcedureInfo block;
        /** 変数特性テーブル */
        private JTable tableVariable;

        /**
         * コンストラクタ
         * @param block		変数特性ブロック
         */
        public NodePanel(VariableTableModel.ProcedureInfo block) {
            this.block = block;
        }

        /**
         * 変数特性ブロックを取得する
         * @return		変数特性ブロック
         */
        public VariableTableModel.ProcedureInfo getBlock() {
            return this.block;
        }

        /**
         * 変数特性テーブルを設定する
         * @param tableVariable		変数特性テーブル
         */
        public void setTableVariable(JTable tableVariable) {
            this.tableVariable = tableVariable;
        }

        /**
         * 変数特性テーブルを取得する
         * @return		変数特性テーブル
         */
        public JTable getTableVariable() {
            return this.tableVariable;
        }

    }


    /**
     * 変数特性一覧テーブルモデルを取得する
     * @return		変数特性一覧テーブルモデル
     */
    public VariableTableModel getModel() {
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

        // 該当箇所を開く
        this.btnOpenFile.addActionListener((ActionListener) menu.getActionOpenAnalysisLine());
    }

    /**
     * モデルのクリアを行う。
     */
    @Override
    public void clearModel() {
        // モデルクリア
        model.clearVariable();
    }

    /**
     * 変数特性パネル展開ボタンアクションリスナ
     * @author riken
     */
    private class VariableExpandAction implements ActionListener {
        /** 変数特性一覧テーブルパネル */
        private JPanel panelRow;

        /**
         * コンストラクタ
         * @param panel		表示切替を行う変数特性一覧パネル
         */
        public VariableExpandAction(JPanel panel) {
            this.panelRow = panel;
        }

        /**
         * ボタンのクリックイベント
         * @param event		イベント情報
         */
        @Override
        public void actionPerformed(ActionEvent event) {
            JButton btn = (JButton) event.getSource();
            // 変数特性一覧パネルの表示をトグルする
            if (panelRow.isVisible()) {
                // 変数特性一覧パネルを非表示する
                btn.setIcon(collapse_icon);
                panelRow.setVisible(false);
            }
            else {
                // 変数特性一覧パネルを表示する
                btn.setIcon(expand_icon);
                panelRow.setVisible(true);
            }

            return;
        }
    }

    /**
     * マウスクリックイベント
     * @param event		マウスイベント情報
     */
    @Override
    public void mouseClicked(MouseEvent event) {
        // クリックチェック
        if (SwingUtilities.isLeftMouseButton(event)) {

            NodePanel panel = null;
            if (event.getSource() instanceof NodePanel) {
                panel = (NodePanel)event.getSource();
            }
            else if (event.getSource() instanceof JTable) {
                // 親の親パネル
                Container cont = ((JTable)event.getSource()).getParent().getParent();
                if (cont instanceof NodePanel) {
                    panel = (NodePanel)cont;
                }
            }

            // 選択付加情報ブロックを設定する
            this.selectedInfo = panel;

            // 選択パネルの背景色を設定する.
            setSelectedBackgroud(this.selectedInfo);

            // ダブルクリック
            if (event.getClickCount() == 2) {
                // 該当個所を開く
                this.btnOpenFile.doClick();
            }
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
     * タブのクローズを行う
     */
    @Override
    public void closeTab() { }


    private class VariableListSelectionListener implements ListSelectionListener {
        /** リスナ対象テーブル */
        private JTable table;

        /**
         * コンストラクタ
         * @param table		リスナ対象テーブル
         */
        public VariableListSelectionListener(JTable table) {
            this.table = table;
        }

        /**
         * 変数特性一覧テーブル選択変更イベント.
         * @param event		イベント情報
         */
        @Override
        public void valueChanged(ListSelectionEvent event) {
            if (event.getValueIsAdjusting()) return;

            int selection = table.getSelectedRow();
            // テーブル・モデルの行数に変換
            int modelRow = table.convertRowIndexToModel(selection);
            if (modelRow < 0) return;
            Object cell = this.table.getModel().getValueAt(modelRow, 0);
            if (cell == null) return;
            if (cell instanceof VariableDefinition) {
                // 選択変数宣言文を設定する
                VariableTablePanel.this.model.setSelectedVariable((VariableDefinition)cell);
            }
        }
    }

    /**
     * 選択ソースコード行情報を取得する(未使用)
     * @return		選択ソースコード行情報
     */
    @Override
    public CodeLine getSelectedCodeLine() {
        return null;
    }


    /**
     * コンテキストメニューを設定する
     * @param variablePopupMenu		コンテキストメニュー
     */
    public void setPopupMenu(VariablePopupMenu variablePopupMenu) {
        this.variablePopupMenu = variablePopupMenu;

        // 付加情報
        this.actionEdit = variablePopupMenu.getActionAnalysisInformation();
        // 演算カウント
        this.actionOperand = variablePopupMenu.getActionAnalysisOperand();
        // 宣言・定義・参照
        this.actionReference = variablePopupMenu.getActionAnalysisReference();
        // 変数有効域
        this.actionScope = variablePopupMenu.getActionAnalysisScope();
    }

    /**
     * 選択ブロックを取得する
     * @return		選択ブロック
     */
    @Override
    public IBlock getSelectedBlock() {
        VariableDefinition value = this.model.getSelectedVariable();
        if (value instanceof IBlock) {
            return (IBlock)value;
        }
        return null;
    }

    /**
     * 選択付加情報を取得する
     * @return		選択付加情報
     */
    @Override
    public IInformation getSelectedInformation() {
        VariableDefinition value = this.model.getSelectedVariable();
        if (value instanceof IInformation) {
            return (IInformation)value;
        }
        return null;
    }

    /**
     * ソースビュープロパティを設定する
     * @param properties		ソースビュープロパティ
     */
    @Override
    public void setSourceProperties(SourceProperties properties) {
    	// 選択パネルの背景色
    	this.colorSelectedPanel = properties.getBackgoundView2Color();
        // 選択パネルの背景色を設定する.
        setSelectedBackgroud(this.selectedInfo);
    }

    /**
     * ボタンのクリックイベント
     * @param event			イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {
        // クリア
        if (event.getSource() == this.btnClear) {
            // モデルクリア
            clearModel();
        }
        // 付加情報編集
        else if (event.getSource() == this.btnEdit) {
            VariableDefinition value = this.model.getSelectedVariable();
            if (value instanceof IInformation) {
                this.actionEdit.editInformation((IInformation)value);
            }
        }
        // 演算数カウント
        else if (event.getSource() == this.btnOperand) {
            IBlock block = this.getSelectedBlock();
            if (block != null) {
                IBlock[] blocks = {block};
                actionOperand.analysisOperand(blocks);
            }
        }
        // 宣言・定義・参照
        else if (event.getSource() == this.btnReference) {
            VariableDefinition variable = this.model.getSelectedVariable();
            actionReference.analysisReference(variable);
        }
        // 変数有効域
        else if (event.getSource() == this.btnScope) {
            VariableDefinition variable = this.model.getSelectedVariable();
            actionScope.analysisScope(variable);
        }
    }

    /**
     * 選択項目をクリップボードにコピーする.
     */
    @Override
    public void copyClipboard() {
        if (this.selectedInfo == null) return;
        String text = SwingUtils.toCsvOfSeletedRows(this.selectedInfo.getTableVariable());
        if (text == null) return;

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

	/**
	 * 選択パネルの背景色を設定する.
	 */
	private void setSelectedBackgroud(JPanel panel) {
        // すべてクリア
		if (this.contentInfo != null) {
			SwingUtils.setBackgroundChildPanel(this.contentInfo, null);
		}
        // 選択パネルの背景色を選択色に変更する
        if (panel != null) {
            SwingUtils.setBackgroundChildPanel(panel, this.colorSelectedPanel);
        }
	}
}


