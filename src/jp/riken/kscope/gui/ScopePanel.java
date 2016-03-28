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
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
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
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.border.CompoundBorder;
import javax.swing.border.LineBorder;
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;

import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.component.JStripeTable;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.menu.MainMenu;
import jp.riken.kscope.model.ScopeModel;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * 変数有効域テーブルパネルクラス
 * @author RIKEN
 *
 */
public class ScopePanel extends AnalisysPanelBase
        implements Observer, IAnalisysComponent, MouseListener {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** クリアボタン */
    private JButton btnClear;
    /** ファイルを開くボタン add by @hira at 2015/10/01 */
    private JButton btnOpenFile;
    /** エクスポートボタン */
    private JButton btnExport;
    /** 変数有効域ラベル */
    private JLabel label;
    /** スクロールパイン */
    private JScrollPane scrollPane;
    /** 変数有効域テーブル */
    private JTable tableScope;

    /** 変数有効域モデル */
    private ScopeModel model;
    /** テーブル幅 */
    private int TABLE_COLUMN_WIDTH = 640;
    /**
     * コンストラクタ
     */
    public ScopePanel() {
        super();

        // モデルの生成を行う
        model = new ScopeModel();
        // オブザーバを設定する。
        model.addObserver(this);

        // GUI初期化を行う。
        initGUI();

    }

    /**
     * コンストラクタ
     * @param proparties        分析情報パネル識別子
     */
    public ScopePanel(ANALYSIS_PANEL proparties) {
        super(proparties);

        // モデルの生成を行う
        model = new ScopeModel();
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
                        btnClear.addActionListener( new ActionListener() {
                            @Override
                            public void actionPerformed(ActionEvent e) {
                                // モデルクリア
                                clearModel();
                            }
                        });
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
                    // 変数有効域テーブル
                    tableScope = new JStripeTable();
                    tableScope.setModel(this.model.getTableModel());
                    tableScope.setAutoCreateColumnsFromModel(false);
                    tableScope.setSelectionMode(ListSelectionModel.SINGLE_SELECTION );
                    tableScope.setColumnSelectionAllowed(false);
                    tableScope.setAutoResizeMode(JTable.AUTO_RESIZE_LAST_COLUMN);
                    // テーブル列モデル
                    DefaultTableColumnModel columnModel = (DefaultTableColumnModel)tableScope.getColumnModel();
                    // テーブル幅
                    model.setTableColumnWidth(columnModel);

                    // スクロールパイン
                    scrollPane = new JScrollPane();
                    scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
                    scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
                    scrollPane.setViewportView(tableScope);
                    scrollPane.getViewport().setBackground(Color.WHITE);

                    add(scrollPane);

                    tableScope.addMouseListener(this);

                }

            }

            // ツールチップ設定
            btnClear.setToolTipText(Message.getString("informationdialog.button.clear.tooltip")); //クリア
            btnOpenFile.setToolTipText(Message.getString("informationpanel.tooltip.openblock")); //選択箇所を開く
            btnExport.setToolTipText(Message.getString("mainmenu.file.export")); //エクスポート

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * 変数有効域モデルの変更通知イベント
     * @param o            通知元
     * @param arg        通知項目
     */
    @Override
    public void update(Observable o, Object arg) {
        // 変数有効域モデル
        ScopeModel observer = (ScopeModel)o;

        // テーブルモデル
        tableScope.setModel(observer.getTableModel());

        // パネルタイトル
        this.label.setText(observer.getTitle());

    }


    /**
     * 変数有効域モデルを取得する
     * @return        変数特性一覧テーブルモデル
     */
    public ScopeModel getModel() {
        return model;
    }


    /**
     * フォーカスリスナを設定する
     * @param listener        フォーカスリスナ
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
     * @param menu        メニューバー
     */
    @Override
    public void setActionListener(MainMenu menu) {
        // 該当箇所を開く
        this.btnOpenFile.addActionListener((ActionListener) menu.getActionOpenAnalysisLine());

        // 分析情報エクスポートアクション
        this.btnExport.addActionListener(menu.getActionExportAnalysis());
    }

    /**
     * モデルのクリアを行う。
     */
    @Override
    public void clearModel() {
        // モデルクリア
        model.clear();
    }

    /**
     * タブのクローズを行う
     */
    @Override
    public void closeTab() { }

    /**
     * 選択ソースコード行情報を取得する
     * @return        選択ソースコード行情報
     */
    @Override
    public CodeLine getSelectedCodeLine() {
        return null;
    }

    /**
     * 選択ブロックを取得する
     * @return        選択ブロック
     */
    @Override
    public IBlock getSelectedBlock() {
        //  選択行
        int row = this.tableScope.getSelectedRow();
        if (row < 0) return null;

        // 1列目がブロック情報
        DefaultTableModel tableModel = (DefaultTableModel) this.tableScope.getModel();
        Object obj = tableModel.getValueAt(row, 0);
        if (obj == null) return null;
        if (obj instanceof IBlock) {
            return (IBlock)obj;
        }
        return null;
    }

    /**
     * 選択付加情報を取得する
     * @return        選択付加情報
     */
    @Override
    public IInformation getSelectedInformation() {
        return null;
    }

    /**
     * ソースビュープロパティを設定する
     * @param properties        ソースビュープロパティ
     */
    @Override
    public void setSourceProperties(SourceProperties properties) {}

    /**
     * 選択項目をクリップボードにコピーする.
     */
    @Override
    public void copyClipboard() {
        if (this.tableScope == null) return;
        String text = SwingUtils.toCsvOfSeletedRows(this.tableScope);
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
     * マウスクリックイベント
     * @param event        マウスイベント情報
     */
    @Override
    public void mouseClicked(MouseEvent event) {
        // クリックチェック
        if (SwingUtilities.isLeftMouseButton(event)) {
            // ダブルクリック
            if (event.getClickCount() == 2) {
                // 該当個所を開く
                this.btnOpenFile.doClick();
            }
        }
    }

    /**
     * マウスボタンダウンイベント
     * @param e        マウスイベント情報
     */
    @Override
    public void mousePressed(MouseEvent e) { }

    /**
     * マウスボタンアップイベント
     * @param e        マウスイベント情報
     */
    @Override
    public void mouseReleased(MouseEvent e) {}

    /**
     * マウスオーバーイベント
     * @param e        マウスイベント情報
     */
    @Override
    public void mouseEntered(MouseEvent e) {}

    /**
     * マウスアウトイベント
     * @param e        マウスイベント情報
     */
    @Override
    public void mouseExited(MouseEvent e) {}

}


