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
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.border.CompoundBorder;
import javax.swing.border.LineBorder;
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.DefaultTableModel;

import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.component.JStripeTable;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.menu.MainMenu;
import jp.riken.kscope.model.OperandTableModel;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * 演算カウントパネルクラス
 * @author riken
 *
 */
public class OperandTablePanel extends AnalisysPanelBase implements Observer, IAnalisysComponent {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** クリアボタン */
    private JButton btnClear;
    /** エクスポートボタン */
    private JButton btnExport;
    /** 該当個所を開く */
    private JButton btnOpenFile;
    /** 演算カウントラベル */
    private JLabel label;
    /** スクロールパイン */
    private JScrollPane scrollPane;
    /** 演算カウントテーブル */
    private JTable tableOperand;

    /** 演算カウントテーブルモデル */
    private OperandTableModel model;


    /**
     * コンストラクタ
     */
    public OperandTablePanel() {
        super();

        // モデルの生成を行う
        model = new OperandTableModel();
        // オブザーバを設定する。
        model.addObserver(this);

        // GUI初期化を行う。
        initGUI();

    }

    /**
     * コンストラクタ
     * @param proparties		分析情報パネル識別子
     */
    public OperandTablePanel(ANALYSIS_PANEL proparties) {
        super(proparties);

        // モデルの生成を行う
        model = new OperandTableModel();
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
                    // 演算カウントテーブル
                    tableOperand = new JStripeTable();
                    tableOperand.setModel(this.model.getTableModel());
                    tableOperand.setAutoCreateColumnsFromModel(false);
                    tableOperand.setSelectionMode(ListSelectionModel.SINGLE_SELECTION );
                    tableOperand.setColumnSelectionAllowed(false);

                    // テーブル列幅設定
                    DefaultTableColumnModel columnModel = (DefaultTableColumnModel)tableOperand.getColumnModel();
                    model.setTableColumnWidth(columnModel);

                    // スクロールパイン
                    scrollPane = new JScrollPane();
                    scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
                    scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
                    scrollPane.setViewportView(tableOperand);
                    scrollPane.getViewport().setBackground(Color.WHITE);

                    add(scrollPane);
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
     * 演算カウントモデルの変更通知イベント
     * @param o			通知元
     * @param arg		通知項目
     */
    @Override
    public void update(Observable o, Object arg) {
        // 演算カウントモデル
        OperandTableModel observer = (OperandTableModel)o;

        // テーブルモデル
        tableOperand.setModel(observer.getTableModel());

        // パネルタイトル
        this.label.setText(observer.getTitle());

    }


    /**
     * 演算カウントテーブルモデルを取得する
     * @return		変数特性一覧テーブルモデル
     */
    public OperandTableModel getModel() {
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
        this.tableOperand.addMouseListener((MouseListener) menu.getActionOpenAnalysisLine());
    }

    /**
     * モデルのクリアを行う。
     */
    @Override
    public void clearModel() {
        // モデルクリア
        model.clearOperand();
    }

    /**
     * タブのクローズを行う
     */
    @Override
    public void closeTab() { }

    /**
     * 選択ソースコード行情報を取得する
     * @return		選択ソースコード行情報
     */
    @Override
    public CodeLine getSelectedCodeLine() {
        //  選択行
        IBlock block = getSelectedBlock();
        if (block == null) return null;
        return block.getStartCodeLine();
    }

    /**
     * 選択ブロックを取得する
     * @return		選択ブロック
     */
    @Override
    public IBlock getSelectedBlock() {
        //  選択行
        int row = this.tableOperand.getSelectedRow();
        if (row < 0) return null;

        // 1列目がブロック情報
        DefaultTableModel tableModel = (DefaultTableModel) this.tableOperand.getModel();
        Object obj = tableModel.getValueAt(row, 0);
        if (obj == null) return null;
        if (obj instanceof IBlock) {
            return (IBlock)obj;
        }
        return null;
    }

    /**
     * 選択付加情報を取得する
     * @return		選択付加情報
     */
    @Override
    public IInformation getSelectedInformation() {
        //  選択行
        int row = this.tableOperand.getSelectedRow();
        if (row < 0) return null;

        // 1列目がブロック情報
        DefaultTableModel tableModel = (DefaultTableModel) this.tableOperand.getModel();
        Object obj = tableModel.getValueAt(row, 0);
        if (obj == null) return null;
        if (obj instanceof IInformation) {
            return (IInformation)obj;
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
     * 選択項目をクリップボードにコピーする.
     */
    @Override
    public void copyClipboard() {
        if (this.tableOperand == null) return;
        String text = SwingUtils.toCsvOfSeletedRows(this.tableOperand);
        if (text == null) return;

        // クリップボードにコピーする
        SwingUtils.copyClipboard(text);
    }

    /**
     * エクスポート可能か否か
     */
	@Override
	public boolean isExportable() {
		if (this.model == null) return false;
		return (!this.model.isEmpty());
	}
}


