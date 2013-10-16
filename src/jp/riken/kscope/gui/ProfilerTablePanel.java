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
import java.util.ArrayList;
import java.util.List;
import java.util.Observable;
import java.util.Observer;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableModel;

import jp.riken.kscope.Message;
import jp.riken.kscope.action.ProfilerInformationEditAction;
import jp.riken.kscope.action.ViewOpenAnalysisLineAction;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.common.PROFILERINFO_TYPE;
import jp.riken.kscope.component.JStripeTable;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.menu.ProfilerPopupMenu;
import jp.riken.kscope.menu.MainMenu;
import jp.riken.kscope.model.ProfilerCallGraphModel;
import jp.riken.kscope.model.ProfilerCostTableModel;
import jp.riken.kscope.model.ProfilerEventCounterModel;
import jp.riken.kscope.model.ProfilerTableBaseModel;
import jp.riken.kscope.profiler.ProfilerBaseData;
import jp.riken.kscope.properties.ProfilerProperties;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * プロファイラ:コスト情報パネルクラス
 * @author riken
 *
 */
public class ProfilerTablePanel extends AnalisysPanelBase implements Observer, IAnalisysComponent, MouseListener, ActionListener {

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
    /** テーブル列の表示切替ボタン */
    private JButton btnPulldown;
    /** ソートボタン */
    //private JButton btnSort;
    /** プロファイラ:コスト情報ラベル */
    private JLabel label;
    /** コンテンツボックス */
    private Box contentInfo;
    /** 余白ボックス */
    private final Component glue = Box.createVerticalGlue();
    /** スクロールパイン */
    private JScrollPane scrollPane;

    /** プロファイラ:コスト情報テーブルモデル */
    private ProfilerTableBaseModel model;

    /** 展開ボタンアイコン */
    private Icon expand_icon = ResourceUtils.getIcon("expand_arrow.gif");
    /** 収納ボタンアイコン */
    private Icon collapse_icon = ResourceUtils.getIcon("collapse_arrow.gif");

    /** プロファイラ:コスト情報コンテキストメニュー */
    private ProfilerPopupMenu costinfoPopupMenu;

    /** 選択テーブル */
    private JTable selectedTable;
    /** 選択プロファイラパネル */
    private NodePanel selectedPanel;
    /** 付加情報編集アクション */
    private ProfilerInformationEditAction actionEdit;
    /** 該当個所を開くアクション */
    private ViewOpenAnalysisLineAction actionOpenAnalysis;
    private JPopupMenu menuVisibledColumns;

    /** ソート状態 */
    private boolean viewSort = false;
    /** 選択パネル背景色 */
    private Color colorSelectedPanel;

    /**
     * コンストラクタ
     * @param panel		分析情報パネル識別子
     * @throws Exception     分析情報パネル識別子不正
     */
    public ProfilerTablePanel(ANALYSIS_PANEL panel) throws Exception {
        super(panel);

        // モデルの生成を行う
        model = factoryProfilerTableModel(panel);

        // オブザーバを設定する。
        model.addObserver(this);

        // GUI初期化を行う。
        initGUI();
    }

    /**
     * プロファイラモデルを生成する
     * @param panel		パネル識別子
     * @return			プロファイラモデル
     * @throws Exception		パネル識別子例外
     */
    private ProfilerTableBaseModel factoryProfilerTableModel(ANALYSIS_PANEL panel) throws Exception {

        ProfilerTableBaseModel profilerModel = null;
        // モデルの生成を行う
        switch (panel) {
        case COST_LINE:
            profilerModel = new ProfilerCostTableModel(PROFILERINFO_TYPE.COST_LINE);
            break;
        case COST_LOOP:
            profilerModel = new ProfilerCostTableModel(PROFILERINFO_TYPE.COST_LOOP);
            break;
        case COST_PROCEDURE:
            profilerModel = new ProfilerCostTableModel(PROFILERINFO_TYPE.COST_PROCEDURE);
            break;
        case CALLGRAPH:
            profilerModel = new ProfilerCallGraphModel(PROFILERINFO_TYPE.CALLGRAPH);
            break;
        case EVENTCOUNTER_CACHE:
            profilerModel = new ProfilerEventCounterModel(PROFILERINFO_TYPE.EVENTCOUNTER_CACHE);
            break;
        case EVENTCOUNTER_INSTRUCTIONS:
            profilerModel = new ProfilerEventCounterModel(PROFILERINFO_TYPE.EVENTCOUNTER_INSTRUCTIONS);
            break;
        case EVENTCOUNTER_MEM_ACCESS:
            profilerModel = new ProfilerEventCounterModel(PROFILERINFO_TYPE.EVENTCOUNTER_MEM_ACCESS);
            break;
        case EVENTCOUNTER_PERFORMANCE:
            profilerModel = new ProfilerEventCounterModel(PROFILERINFO_TYPE.EVENTCOUNTER_PERFORMANCE);
            break;
        case EVENTCOUNTER_STATISTICS:
            profilerModel = new ProfilerEventCounterModel(PROFILERINFO_TYPE.EVENTCOUNTER_STATISTICS);
            break;
        default:
            throw new Exception(Message.getString("profilertablepanel.exception.panelidentiferinvalid")); //分析情報パネル識別子が不正です。
        }

        return profilerModel;
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
                    {
                    //    Icon icon = ResourceUtils.getIcon("sort_gray.gif");
                    //    btnSort = new JButton(icon);
                    //    btnSort.setContentAreaFilled(false);
                    //    btnSort.setBorderPainted(false);
                    //    btnSort.setPreferredSize(buttonSize);
                    //    btnSort.setMinimumSize(buttonSize);
                    //    btnSort.setMaximumSize(buttonSize);
                    //    panelButtons.add(btnSort);
                    }
                    {
                        Icon icon = ResourceUtils.getIcon("popupmenu.gif");
                        btnPulldown = new JButton(icon);
                        btnPulldown.setContentAreaFilled(false);
                        btnPulldown.setBorderPainted(false);
                        btnPulldown.setPreferredSize(buttonSize);
                        btnPulldown.setMinimumSize(buttonSize);
                        btnPulldown.setMaximumSize(buttonSize);
                        panelButtons.add(btnPulldown);
                    }
                }

                // ラベル配置
                {
                    label = new JLabel();
                    panelTop.add(label, BorderLayout.CENTER);
                    label.setText("Profiler Infomation");
                }
            }
            {
                {
                    // プロファイラ:コスト情報パネル
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
            btnOpenFile.addActionListener(this);
            btnPulldown.addActionListener(this);
            //btnSort.addActionListener(this);

            // ツールチップ設定
            btnClear.setToolTipText(Message.getString("informationdialog.button.clear.tooltip")); //クリア
            btnEdit.setToolTipText(Message.getString("profilertablepanel.tooltip.info")); //付加情報
            btnOpenFile.setToolTipText(Message.getString("profilertablepanel.tooltip.open")); //選択箇所を開く
            btnExport.setToolTipText(Message.getString("mainmenu.file.export")); //エクスポート
            btnPulldown.setToolTipText(Message.getString("profilertablepanel.tooltip.column")); //表示列の選択
            //btnSort.setToolTipText(Message.getString("profilertablepanel.tooltip.sort")); //行番号でソート

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * プロファイラ:コスト情報モデルの変更通知イベント
     * @param o			通知元
     * @param arg		通知項目
     */
    @Override
    public void update(Observable o, Object arg) {
        // プロファイラ:コスト情報のクリア
        clearComponent();

        // テーブルモデル
        ProfilerTableBaseModel observer = (ProfilerTableBaseModel)o;

        // パネルタイトル
        this.label.setText(observer.getTitle());

        int count = observer.getInfoMapCount();
        for (int i=0; i<count; i++) {
            String key = observer.getSubTitle(i);
            if (key == null) continue;
            TableModel table = observer.getInfoTableModel(i);

            // プロファイラ:コスト情報を追加する
            addInfoTable(key, table);
        }

    }

    /**
     * プロファイラ情報を追加する
     * @param key			プロファイラ情報識別文字列
     * @param table			プロファイラ情報テーブルモデル
     */
    public void addInfoTable(String key, TableModel table) {

        // 追加コンポーネントの作成
        JComponent component = makeRowsPanel(key, table);

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

        // 選択テーブルのクリア
        this.selectedPanel = null;
        this.selectedTable = null;
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
     * プロファイラ:コスト情報パネルの追加
     * @param key			コスト情報識別文字列
     * @param table			コスト情報テーブルモデル
     * @return			プロファイラ:コスト情報パネル
     */
    private JComponent makeRowsPanel(String key, TableModel table) {
        // プロファイラ:コスト情報パネル
        NodePanel rows = new NodePanel(key);
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
        JLabel label = new JLabel(key);
        label.setOpaque(false);
        panelName.add(label);

        // プロファイラ:コスト情報パネル
        JPanel panelTable = new JPanel();
        panelTable.setLayout(new BoxLayout(panelTable, BoxLayout.Y_AXIS));
        panelTable.setOpaque(false);
        panelTable.setBorder(new EmptyBorder(5, 40, 5, 40));

        // プロファイラ:コスト情報テーブル
        // セル配置
        JTable tableInfo = new JStripeTable(this.model.getTableColumnAlignments(), SwingConstants.LEFT);
        tableInfo.setModel(table);

        tableInfo.setAutoCreateColumnsFromModel(false);
        tableInfo.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION );
        tableInfo.setColumnSelectionAllowed(false);
        tableInfo.addMouseListener(this);
        // テーブル変更イベント
        ProfilerInfoListSelectionListener listener = new ProfilerInfoListSelectionListener(tableInfo);
        tableInfo.getSelectionModel().addListSelectionListener(listener);

        // テーブル列幅設定
        DefaultTableColumnModel columnModel = (DefaultTableColumnModel)tableInfo.getColumnModel();
        model.setTableColumnWidth(columnModel);

        // テーブルヘッダーと行を別々に描画する
        JTableHeader tableHeader = tableInfo.getTableHeader();

        tableInfo.setShowGrid(false);
        tableInfo.setIntercellSpacing(new Dimension(0,0));

        // ソート
//        tableCostInfo.setAutoCreateRowSorter(true);
//        int index = 1;
//        tableCostInfo.getRowSorter().setSortKeys(
//            Arrays.asList(new RowSorter.SortKey(index, SortOrder.DESCENDING)));

        // プロファイラ:コスト情報テーブルのボーダー設定
        LineBorder border = new LineBorder(Color.GRAY, 1, false);
        tableInfo.setBorder(border);
        tableHeader.setBorder(border);

        // プロファイラ:コスト情報パネルに追加
        panelTable.add(tableHeader);
        panelTable.add(tableInfo);

        tableInfo.validate();
        tableInfo.updateUI();

        // プロファイラ:コスト情報展開ボタンのアクションリスナの設定
        button.addActionListener(new ProfilerInfoExpandAction(panelTable));

        // プロファイラ:コスト情報パネルに名前パネル、本文パネルの追加
        rows.add(panelName);
        rows.add(panelTable);

        // テーブルを設定
        rows.setTableInfo(tableInfo);

        // コンテキストメニューを設定する
        tableInfo.setComponentPopupMenu(this.costinfoPopupMenu);

        return rows;
    }

    /**
     * コスト情報ノードパネル
     * @author riken
     */
    private class NodePanel extends JPanel {
        /** シリアル番号 */
        private static final long serialVersionUID = 1L;

        /** コスト情報識別文字列 */
        private String key;
        /** コスト情報テーブル */
        private JTable tableInfo;

        /**
         * コンストラクタ
         * @param key			コスト情報キー文字列
         */
        public NodePanel(String key) {
            this.key = key;
        }

        /**
         * コスト情報識別文字列を取得する
         * @return		コスト情報識別文字列
         */
        public String getKey() {
            return key;
        }

        /**
         * コスト情報識別文字列を設定する
         * @param key		コスト情報識別文字列
         */
        public void setKey(String key) {
            this.key = key;
        }

        /**
         * コスト情報テーブルを取得する
         * @return		コスト情報テーブル
         */
        public JTable getTableInfo() {
            return tableInfo;
        }

        /**
         * コスト情報テーブルを設定する
         * @param table		コスト情報テーブル
         */
        public void setTableInfo(JTable table) {
            this.tableInfo = table;
        }
    }


    /**
     * プロファイラ:コスト情報テーブルモデルを取得する
     * @return		プロファイラ:コスト情報テーブルモデル
     */
    public ProfilerTableBaseModel getModel() {
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
    }

    /**
     * モデルのクリアを行う。
     */
    @Override
    public void clearModel() {
        // モデルクリア
        model.clearModel();
    }

    /**
     * コスト情報パネル展開ボタンアクションリスナ
     * @author riken
     */
    private class ProfilerInfoExpandAction implements ActionListener {
        /** プロファイラ:コスト情報テーブルパネル */
        private JPanel panelRow;

        /**
         * コンストラクタ
         * @param panel		表示切替を行うプロファイラ:コスト情報パネル
         */
        public ProfilerInfoExpandAction(JPanel panel) {
            this.panelRow = panel;
        }

        /**
         * ボタンのクリックイベント
         * @param event		イベント情報
         */
        @Override
        public void actionPerformed(ActionEvent event) {
            JButton btn = (JButton) event.getSource();
            // プロファイラ:コスト情報パネルの表示をトグルする
            if (panelRow.isVisible()) {
                // プロファイラ:コスト情報パネルを非表示する
                btn.setIcon(collapse_icon);
                panelRow.setVisible(false);
            }
            else {
                // プロファイラ:コスト情報パネルを表示する
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
            // 選択パネル
            this.selectedPanel = panel;
            // 選択列の背景色を選択色に変更する
            setSelectedBackgroud(this.selectedPanel);

            if (panel != null) {
                JTable table = panel.getTableInfo();
                int selection = table.getSelectedRow();
                if (selection < 0) {
                    // １行目を選択状態とする。
                    table.setRowSelectionInterval(0, 0);
                }
                // テーブル・モデルの行数に変換
                int modelRow = table.convertRowIndexToModel(selection);
                if (modelRow >= 0) {
                    Object cell = table.getModel().getValueAt(modelRow, 0);
                    if (cell != null && cell instanceof ProfilerBaseData) {
                        // 選択プロファイラデータを設定する
                        this.model.setSelectedInfo((ProfilerBaseData)cell);
                    }
                }
                // 選択テーブルを設定する
                this.selectedTable = table;
            }
            //
            ITabComponent parent = this.getParentComponent();
            if (parent instanceof AnalysisView) {
                ((AnalysisView)parent).changeAnalisysTab();
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
     * @param event		マウスイベント情報
     */
    @Override
    public void mousePressed(MouseEvent event) {
    }

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

    /**
     * コスト情報テーブル選択変更リスナ
     * @author riken
     */
    private class ProfilerInfoListSelectionListener implements ListSelectionListener {
        /** リスナ対象テーブル */
        private JTable table;

        /**
         * コンストラクタ
         * @param table		リスナ対象テーブル
         */
        public ProfilerInfoListSelectionListener(JTable table) {
            this.table = table;
        }

        /**
         * プロファイラ:コスト情報テーブル選択変更イベント.
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
            if (cell instanceof ProfilerBaseData) {
                // 選択プロファイラデータを設定する
                ProfilerTablePanel.this.model.setSelectedInfo((ProfilerBaseData)cell);
            }
            // 選択テーブルを設定する
            ProfilerTablePanel.this.selectedTable = table;
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
     * @param popupMenu		コンテキストメニュー
     */
    public void setPopupMenu(ProfilerPopupMenu popupMenu) {
        this.costinfoPopupMenu = popupMenu;

        // 付加情報
        this.actionEdit = popupMenu.getActionAnalysisInformation();

        // 該当箇所を開く
        actionOpenAnalysis = (ViewOpenAnalysisLineAction) popupMenu.getActionOpenAnalysisLine();

    }

    /**
     * 選択ブロックを取得する
     * @return		選択ブロック
     */
    @Override
    public IBlock getSelectedBlock() {
        ProfilerBaseData value = this.model.getSelectedInfo();
        return value.getBlock();
    }

    /**
     * 選択付加情報を取得する
     * @return		選択付加情報
     */
    @Override
    public IInformation getSelectedInformation() {
        IInformation[] infos = getSelectedInformations();
        if (infos == null) return null;
        return infos[0];
    }

    /**
     * 選択付加情報を取得する
     * @return		選択付加情報範囲
     */
    public IInformation[] getSelectedInformations() {
        ProfilerBaseData value = this.model.getSelectedInfo();
        IBlock[] blocks = value.getBlocks();
        if (blocks == null) return null;
        List<IInformation> list = new ArrayList<IInformation>();
        for (IBlock block : blocks) {
            if (block instanceof IInformation) {
                list.add((IInformation) block);
            }
            else {
                return null;
            }
        }
        if (list.size() <= 0) return null;
        return list.toArray(new IInformation[0]);
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
        setSelectedBackgroud(this.selectedPanel);
    }

    /**
     * プロファイラプロパティを設定する
     * @param properties		プロファイラプロパティ
     */
    public void setProfilerProperties(ProfilerProperties properties) {
        if (this.model == null) return;
        this.model.setProfilerProperties(properties);
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
        // コスト情報の該当行を開く
        if (event.getSource() == this.btnOpenFile) {
            ProfilerBaseData value = this.model.getSelectedInfo();
            if (value == null) return;
            if (value.getAreas() != null) {
                this.actionOpenAnalysis.viewSelectedAreas(value.getAreas());
            }
            else if (value.getCodeLine() != null && value.getCodeLine().getSourceFile() != null) {
                this.actionOpenAnalysis.clearSourceBlock();
                this.actionOpenAnalysis.viewSourceLine(value.getCodeLine());
            }
        }
        // 付加情報編集
        else if (event.getSource() == this.btnEdit) {
            ProfilerBaseData value = this.model.getSelectedInfo();
            String text = this.model.getSelectedText();
            if (value == null) return;
            this.actionEdit.editInformations(value, text);
        }
        else if (event.getSource() == this.btnPulldown) {
            menuVisibledColumns = makeVisibledColumnMenu();
            menuVisibledColumns.show(this.btnPulldown, 0, this.btnPulldown.getHeight());
        }
        // ソート
        //else if (event.getSource() == this.btnSort) {
        //	Icon icon = null;
        //	if (this.viewSort) {
        //		icon = ResourceUtils.getIcon("sort_gray.gif");
        //		this.viewSort = false;
        //	} else {
        //		icon = ResourceUtils.getIcon("sort.gif");
        //		this.viewSort = true;
        //	}
        //	this.model.setViewSort(this.viewSort);
        //	this.refreshPanel();
        //	this.btnSort.setIcon(icon);
        //}
    }


    /**
     * 選択項目をクリップボードにコピーする.
     */
    @Override
    public void copyClipboard() {
        if (this.selectedTable == null) return;
        String text = SwingUtils.toCsvOfSeletedRows(this.selectedTable);
        if (text == null) return;

        // クリップボードにコピーする
        SwingUtils.copyClipboard(text);
    }

    /**
     * テーブル列の表示切替プルダウンメニューの作成を行う。
     * @return		表示切替プルダウンメニュー
     */
    private JPopupMenu makeVisibledColumnMenu() {
        String[] columns = this.model.getHeaderColumns();
        boolean[] visibled = this.model.getVisibledColumns();
        JPopupMenu menu = new JPopupMenu();
        for (int i=0; i<columns.length; i++) {
            if (columns[i].isEmpty()) continue;
            final int col = i;
            JCheckBoxMenuItem checkItem = new JCheckBoxMenuItem(columns[i]);
            checkItem.setSelected(visibled[i]);
            checkItem.addActionListener(
                new ActionListener(){
                    public void actionPerformed(ActionEvent event) {
                        JCheckBoxMenuItem item = (JCheckBoxMenuItem)event.getSource();
                        boolean checked = item.isSelected();
                        ProfilerTablePanel.this.model.setVisibledColumns(col, checked);
                    }
                }
            );
            menu.add(checkItem);
        }

        return menu;
    }

    /**
     * エキスポートできるデータがあるか否か
     */
	@Override
	public boolean isExportable() {
		if(this.model == null) return false;
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


