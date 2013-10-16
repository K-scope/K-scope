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
import javax.swing.SwingUtilities;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.JTableHeader;

import jp.riken.kscope.Message;
import jp.riken.kscope.action.EditInformationEditAction;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.component.JStripeTable;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.information.ReplacementResult;
import jp.riken.kscope.information.ReplacementResult.RESULT_STATUS;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.menu.MainMenu;
import jp.riken.kscope.menu.ReplacePopupMenu;
import jp.riken.kscope.model.ReplacementResultTableModel;
import jp.riken.kscope.model.ReplacementResultTableModel.ReplacementResultBlock;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * 変数特性一覧パネルクラス
 * @author riken
 *
 */
public class ReplacementResultTablePanel extends AnalisysPanelBase implements Observer, IAnalisysComponent, MouseListener, ActionListener {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** クリアボタン */
    private JButton btnClear;
    /** 付加情報ボタン */
    private JButton btnEdit;
    /** ファイルを開くボタン */
    private JButton btnOpenFile;
    /** エクスポートボタン */
    private JButton btnExport;
    /** 差替結果ラベル */
    private JLabel label;
    /** コンテンツボックス */
    private Box contentInfo;
    /** 余白ボックス */
    private final Component glue = Box.createVerticalGlue();
    /** スクロールパイン */
    private JScrollPane scrollPane;

    /** 差し替えテーブルモデル */
    private ReplacementResultTableModel model;

    /** 展開ボタンアイコン */
    private Icon expand_icon = ResourceUtils.getIcon("expand_arrow.gif");
    /** 収納ボタンアイコン */
    private Icon collapse_icon = ResourceUtils.getIcon("collapse_arrow.gif");

    /** 差替結果コンテキストメニュー */
    private ReplacePopupMenu replacePopupMenu;

    /** 付加情報編集アクション */
    private EditInformationEditAction actionEdit;
    /** 選択付加情報パネル */
    private NodePanel selectedPanel;
    /** 選択パネル背景色 */
    private Color colorSelectedPanel;

    /**
     * コンストラクタ
     */
    public ReplacementResultTablePanel() {
        super();

        // モデルの生成を行う
        model = new ReplacementResultTableModel();
        // オブザーバを設定する。
        model.addObserver(this);

        // GUI初期化を行う。
        initGUI();

    }

    /**
     * コンストラクタ
     * @param proparties        分析情報パネル識別子
     */
    public ReplacementResultTablePanel(ANALYSIS_PANEL proparties) {
        super(proparties);

        // モデルの生成を行う
        model = new ReplacementResultTableModel();
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
                    // 差替結果パネル
                    contentInfo = Box.createVerticalBox();
                    contentInfo.setOpaque(false);

                    // スクロールパイン
                    scrollPane = new JScrollPane();
                    scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
                    scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
                    scrollPane.setViewportView(contentInfo);
                    scrollPane.getViewport().setBackground(Color.WHITE);

                    add(scrollPane);
                }

            }
            // イベント追加
            btnClear.addActionListener(this);

            // ツールチップ設定
            btnClear.setToolTipText(Message.getString("informationdialog.button.clear.tooltip")); //クリア
            btnEdit.setToolTipText(Message.getString("replacementresulttablepanel.tooltip.info")); //付加情報
            btnOpenFile.setToolTipText(Message.getString("informationpanel.tooltip.openblock")); //選択箇所を開く
            btnExport.setToolTipText(Message.getString("mainmenu.file.export")); //エクスポート

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * 差替モデルの変更通知イベント
     * @param o         通知元
     * @param arg       通知項目
     */
    @Override
    public void update(Observable o, Object arg) {
        // 差替結果のクリア
        clearComponent();

        // テーブルモデル
        ReplacementResultTableModel observer = (ReplacementResultTableModel)o;

        // パネルタイトル
        this.label.setText(observer.getTitle());

        for (RESULT_STATUS status : observer.STATUSVIEW_ORDER) {
            ReplacementResultBlock info = observer.getReplacementResultBlock(status);
            if (info != null && info.size() > 0) {
	            // 差替結果を追加する
	            addReplacementResultBlock(info);
            }
        }

    }

    /**
     * 差替結果を追加する
     * @param info          差替結果情報
     */
    public void addReplacementResultBlock(ReplacementResultTableModel.ReplacementResultBlock info) {

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
    }

    /**
     * コンテンツパネルにコンポーネントを追加する
     * @param component     追加コンポーネント
     */
    private void addComponent(final JComponent component) {
        // 追加コンポーネントサイズの変更
        component.setMaximumSize(new Dimension(Short.MAX_VALUE, component.getPreferredSize().height));

        // 追加コンポーネントは左詰めで配置する
        component.setAlignmentX(Component.LEFT_ALIGNMENT);

        // コンポーネントの追加
        this.contentInfo.remove(glue);
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
     * 差替結果パネルの追加
     * @param info      差替結果情報
     * @return          差替結果パネル
     */
    private JComponent makeRowsPanel(ReplacementResultBlock info) {
        // 差替結果パネル
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
        String name = info.getStatusText();
        JLabel label = new JLabel(name);
        label.setOpaque(false);
        panelName.add(label);

        // 差替結果パネル
        JPanel panelTable = new JPanel();
        panelTable.setLayout(new BoxLayout(panelTable, BoxLayout.Y_AXIS));
        panelTable.setOpaque(false);
        panelTable.setBorder(new EmptyBorder(5, 40, 5, 40));

        // 差替結果テーブル
        JTable tableVariable = new JStripeTable();
        tableVariable.setModel(info.getTableModel());

        tableVariable.setAutoCreateColumnsFromModel(false);
        tableVariable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
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
        tableVariable.setIntercellSpacing(new Dimension(0, 0));

        // 差替結果テーブルのボーダー設定
        LineBorder border = new LineBorder(Color.GRAY, 1, false);
        tableVariable.setBorder(border);
        tableHeader.setBorder(border);

        // 差替結果パネルに追加
        panelTable.add(tableHeader);
        panelTable.add(tableVariable);

        tableVariable.validate();
        tableVariable.updateUI();

        // 差替結果展開ボタンのアクションリスナの設定
        button.addActionListener(new VariableExpandAction(panelTable));

        // 差替結果パネルに名前パネル、本文パネルの追加
        rows.add(panelName);
        rows.add(panelTable);

        // テーブルを設定
        rows.setTableVariable(tableVariable);

        // コンテキストメニューを設定する
        tableVariable.setComponentPopupMenu(this.replacePopupMenu);

        return rows;
    }

    /**
     * 差替結果ノードパネル
     * @author riken
     */
    private class NodePanel extends JPanel {
        /** シリアル番号 */
        private static final long serialVersionUID = 1L;

        /** 差替結果ブロック */
        private ReplacementResultBlock block;
        /** 差替結果テーブル */
        private JTable tableVariable;

        /**
         * コンストラクタ
         * @param info     差替結果ブロック
         */
        public NodePanel(ReplacementResultBlock info) {
            this.block = info;
        }

        /**
         * 差替結果ブロックを取得する
         * @return      差替結果ブロック
         */
        public ReplacementResultBlock getBlock() {
            return this.block;
        }

        /**
         * 差替結果テーブルを設定する
         * @param tableVariable     差替結果テーブル
         */
        public void setTableVariable(JTable tableVariable) {
            this.tableVariable = tableVariable;
        }

        /**
         * 差替結果テーブルを取得する
         * @return      差替結果テーブル
         */
        public JTable getTableVariable() {
            return this.tableVariable;
        }

    }


    /**
     * 差し替えテーブルモデルを取得する
     * @return      差し替えテーブルモデル
     */
    public ReplacementResultTableModel getModel() {
        return model;
    }


    /**
     * フォーカスリスナを設定する
     * @param listener      フォーカスリスナ
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
     * @param menu      メニューバー
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
     * 差替結果パネル展開ボタンアクションリスナ
     * @author riken
     */
    private class VariableExpandAction implements ActionListener {
        /** 差替結果テーブルパネル */
        private JPanel panelRow;

        /**
         * コンストラクタ
         * @param panel     表示切替を行う差替結果パネル
         */
        public VariableExpandAction(JPanel panel) {
            this.panelRow = panel;
        }

        /**
         * ボタンのクリックイベント
         * @param event     イベント情報
         */
        @Override
        public void actionPerformed(ActionEvent event) {
            JButton btn = (JButton) event.getSource();
            // 差替結果パネルの表示をトグルする
            if (panelRow.isVisible()) {
                // 差替結果パネルを非表示する
                btn.setIcon(collapse_icon);
                panelRow.setVisible(false);
            }
            else {
                // 差替結果パネルを表示する
                btn.setIcon(expand_icon);
                panelRow.setVisible(true);
            }

            return;
        }
    }

    /**
     * マウスクリックイベント
     * @param event     マウスイベント情報
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

        	// 選択パネルの背景色を選択色に変更する
            this.selectedPanel = panel;
            setSelectedBackgroud(this.selectedPanel);

            if (panel != null) {
                JTable table = panel.getTableVariable();
                // モデルに選択付加情報差替情報を設定する
                setSelectedIInformation(table);
            }

            // ダブルクリック
            if (event.getClickCount() == 2) {
                // 該当個所を開く
                this.btnOpenFile.doClick();
            }
        }
    }


    /**
     * マウスボタンダウンイベント
     * @param e     マウスイベント情報
     */
    @Override
    public void mousePressed(MouseEvent e) { }

    /**
     * マウスボタンアップイベント
     * @param e     マウスイベント情報
     */
    @Override
    public void mouseReleased(MouseEvent e) {}

    /**
     * マウスオーバーイベント
     * @param e     マウスイベント情報
     */
    @Override
    public void mouseEntered(MouseEvent e) {}

    /**
     * マウスアウトイベント
     * @param e     マウスイベント情報
     */
    @Override
    public void mouseExited(MouseEvent e) {}

    /**
     * タブのクローズを行う
     */
    @Override
    public void closeTab() { }

    /**
     * 選択変更リスナ
     * @author riken
     */
    private class VariableListSelectionListener implements ListSelectionListener {
        /** リスナ対象テーブル */
        private JTable table;

        /**
         * コンストラクタ
         * @param table     リスナ対象テーブル
         */
        public VariableListSelectionListener(JTable table) {
            this.table = table;
        }

        /**
         * 差替結果テーブル選択変更イベント.
         * @param event     イベント情報
         */
        @Override
        public void valueChanged(ListSelectionEvent event) {
            if (event.getValueIsAdjusting()) return;
            // モデルに選択付加情報差替情報を設定する
            setSelectedIInformation(this.table);
        }
    }

	/**
	 * モデルに選択付加情報差替情報を設定する
	 */
	private void setSelectedIInformation(JTable table) {

        int selection = table.getSelectedRow();
        // テーブル・モデルの行数に変換
        int modelRow = table.convertRowIndexToModel(selection);
        if (modelRow < 0) return;
        // 選択付加情報
        {
	        Object cell = table.getModel().getValueAt(modelRow, 0);
	        if (cell == null) return;
	        if (cell instanceof IInformation) {
	            // 選択付加情報を設定する
	            ReplacementResultTablePanel.this.model.setSelectedIInformation((IInformation)cell);
	        }
	        else if (cell instanceof ReplacementResult) {
	            // 選択付加情報を設定する
	            ReplacementResultTablePanel.this.model.setSelectedIInformation(((ReplacementResult)cell).getCurrentStartPosition());
	        }
        }
        // 選択付加情報差替結果
        {
	        Object cell = table.getModel().getValueAt(modelRow, 4);
	        if (cell == null) return;
	        if (cell instanceof ReplacementResult) {
	            // 選択付加情報差替結果を設定する
	            ReplacementResultTablePanel.this.model.setSelectedReplacementResult((ReplacementResult)cell);
	        }
        }
	}

    /**
     * 選択ソースコード行情報を取得する(未使用)
     * @return      選択ソースコード行情報
     */
    @Override
    public CodeLine getSelectedCodeLine() {
        return null;
    }


    /**
     * コンテキストメニューを設定する
     * @param replacePopupMenu     コンテキストメニュー
     */
    public void setPopupMenu(ReplacePopupMenu replacePopupMenu) {
        this.replacePopupMenu = replacePopupMenu;

        // 付加情報
        this.actionEdit = replacePopupMenu.getActionAnalysisInformation();
        this.btnEdit.addActionListener(this.actionEdit);
    }

    /**
     * 選択ブロックを取得する
     * @return      選択ブロック
     */
    @Override
    public IBlock getSelectedBlock() {
        IInformation value = this.model.getSelectedIInformation();
        if (value instanceof IBlock) {
            return (IBlock) value;
        }
        return null;
    }

    /**
     * 選択付加情報を取得する
     * @return      選択付加情報
     */
    @Override
    public IInformation getSelectedInformation() {
        IInformation value = this.model.getSelectedIInformation();
        return value;
    }

    /**
     * ソースビュープロパティを設定する
     * @param properties        ソースビュープロパティ
     */
    @Override
    public void setSourceProperties(SourceProperties properties) {
    	// 選択パネルの背景色
    	this.colorSelectedPanel = properties.getBackgoundView2Color();
        // 選択パネルの背景色を設定する.
        setSelectedBackgroud(this.selectedPanel);
    }

    /**
     * ボタンのクリックイベント
     * @param event         イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {
        // クリア
        if (event.getSource() == this.btnClear) {
            // モデルクリア
            clearModel();
        }
    }

    /**
     * 選択項目をクリップボードにコピーする.
     */
    @Override
    public void copyClipboard() {
    }

    /**
     * エキスポート可能な情報があるか否か
     */
	@Override
	public boolean isExportable() {
		if (this.model == null) return false;
		return (!this.model.isEmpty());
	}

    /**
     * 選択パネルの背景色を選択色に変更する
     * @param panel		選択パネル
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


