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

package jp.riken.kscope.dialog;
import java.awt.BorderLayout;
//import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
//import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;

import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.data.OperationCount;
import jp.riken.kscope.properties.OperationProperties;
import jp.riken.kscope.utils.StringUtils;

/**
 * 演算カウントダイアログクラス
 * @author RIKEN
 *
 */
public class SettingOperationDialog extends javax.swing.JDialog implements ActionListener, ListSelectionListener {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** キャンセルボタン */
    private JButton btnCancel;
    /** OKボタン */
    private JButton btnOk;
    /** 組込み関数ラベル */
    private JLabel lblName;
    /** 組込み関数名テキストボックス */
    private JTextField txtName;
    /** Op(+)テキストボックス */
    private JTextField txtOpAdd;
    /** Op(*)テキストボックス */
    private JTextField txtOpMul;
    /** 登録ボタン */
    private JButton btnReg;
    /** 追加ボタン */
    private JButton btnAdd;
    /** 削除ボタン */
    private JButton btnDel;
    /** クリアボタン */
    private JButton btnClear;

    /** 演算カウントリスト */
    private JTable tblOperand;
    /** 演算カウントリストデータ */
    private DefaultTableModel modelOperation;
    /** 演算カウント設定パネル */
    private JPanel panelOperand;
    /** 四則演算FLOP設定:+ */
	private JTextField txtAdd;
	/** 四則演算FLOP設定:* */
	private JTextField txtMul;
	/** 四則演算FLOP設定:- */
	private JTextField txtSub;
	/** 四則演算FLOP設定:/ */
	private JTextField txtDiv;
    /** ダイアログの戻り値 */
    private int result = Constant.CANCEL_DIALOG;

    /** 演算カウントプロパティ */
    OperationProperties properities;

    private final String[] COLUMN_HEADER = { Message.getString("settingoperationdialog.columnheader.instinsic"), "op(+)", "op(*)", "op(-)", "op(/)" }; //組込関数
    private final int[] COLUMN_MINWIDTH = { 110, 48, 48, -1, -1 };
    private final int[] COLUMN_MAXWIDTH = { 0, 48, 48, -1, -1 };
    private final int COLUMN_COUNT = 6;


    /**
     * コンストラクタ
     * @param frame		親フレーム
     */
    public SettingOperationDialog(JFrame frame) {
        super(frame);
        initGUI();

        // 初期表示
        clearOperation();
    }

    /**
     * コンストラクタ
     * @param frame		親フレーム
     * @param modal		true=モーダルダイアログを表示する
     */
    public SettingOperationDialog(Frame frame, boolean modal) {
        super(frame, modal);
        initGUI();

        // 初期表示
        clearOperation();
    }

    /**
     * コンストラクタ
     * @param frame		親フレーム
     * @param modal		true=モーダルダイアログを表示する
     * @param properities	外部ツールプロパティ
     */
    public SettingOperationDialog(Frame frame, boolean modal, OperationProperties properities) {
        super(frame, modal);
        initGUI();
        setOperandProperties(properities);

        // 初期表示
        clearOperation();
    }

    /**
     * GUI初期化を行う。
     */
    private void initGUI() {
        try {

            getContentPane().setLayout(new BorderLayout());

            // ボタンパネル
            {
                JPanel panelButtons = new JPanel();
                FlowLayout jPanel1Layout = new FlowLayout();
                jPanel1Layout.setHgap(10);
                jPanel1Layout.setVgap(10);
                panelButtons.setLayout(jPanel1Layout);
                getContentPane().add(panelButtons, BorderLayout.SOUTH);
                panelButtons.setPreferredSize(new java.awt.Dimension(390, 45));

                // メインボタンサイズ
                java.awt.Dimension buttonSize = new java.awt.Dimension(96, 22);
                {
                    btnOk = new JButton();
                    panelButtons.add(btnOk);
                    btnOk.setText(Message.getString("dialog.common.button.ok")); //OK
                    btnOk.setPreferredSize(buttonSize);
                    btnOk.addActionListener(this);
                }
                {
                    btnCancel = new JButton();
                    panelButtons.add(btnCancel);
                    btnCancel.setText(Message.getString("dialog.common.button.cancel")); //キャンセル
                    btnCancel.setPreferredSize(buttonSize);
                    btnCancel.setMargin(new Insets(5, 5, 5, 5));
                    btnCancel.addActionListener(this);
                }
            }
            // 四則演算FLOPカウント
            {
                JPanel panelFlop = new JPanel();
                getContentPane().add(panelFlop, BorderLayout.NORTH);
                // 四則演算のFLOP設定
                String title = Message.getString("settingoperationdialog.panelflop.title"); //四則演算の演算数設定
                TitledBorder borderTitle = new TitledBorder(BorderFactory.createEtchedBorder(), title);
                Border border = new CompoundBorder( new EmptyBorder(7,7,7,7), borderTitle);
                panelFlop.setBorder(border);
                GridBagLayout layout = new GridBagLayout();
                layout.columnWidths = new int[] {24, 32, 24, 32, 24, 32, 24, 32, 24};
                layout.rowHeights = new int[] {24};
                layout.columnWeights = new double[] {0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0};
                layout.rowWeights = new double[] {0.0};
                panelFlop.setLayout(layout);

                int col = 0;
                // +
                JLabel lblAdd = new JLabel("(+) :");
                this.txtAdd = new JTextField();
                panelFlop.add(lblAdd, new GridBagConstraints(col++, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 24, 0, 0), 0, 0));
                panelFlop.add(txtAdd, new GridBagConstraints(col++, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 7, 0, 0), 0, 0));
                // *
                JLabel lblMul = new JLabel("(*) :");
                this.txtMul = new JTextField();
                panelFlop.add(lblMul, new GridBagConstraints(col++, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 24, 0, 0), 0, 0));
                panelFlop.add(txtMul, new GridBagConstraints(col++, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 7, 0, 0), 0, 0));
                // -
                JLabel lblSub = new JLabel("(-) :");
                this.txtSub = new JTextField();
                panelFlop.add(lblSub, new GridBagConstraints(col++, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 24, 0, 0), 0, 0));
                panelFlop.add(txtSub, new GridBagConstraints(col++, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 7, 0, 0), 0, 0));
                // /
                JLabel lblDiv = new JLabel("(/) :");
                this.txtDiv = new JTextField();
                panelFlop.add(lblDiv, new GridBagConstraints(col++, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 24, 0, 0), 0, 0));
                panelFlop.add(txtDiv, new GridBagConstraints(col++, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 7, 0, 0), 0, 0));
                // (FLOP)
                JLabel lblUnit = new JLabel("(FLOP)");
                panelFlop.add(lblUnit, new GridBagConstraints(col++, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 24, 0, 24), 0, 0));

            }
            // コンテンツパネル
            {
                JPanel panelContent = new JPanel();
                BorderLayout panelContentLayout = new BorderLayout();
                getContentPane().add(panelContent, BorderLayout.CENTER);
                String title = Message.getString("settingoperationdialog.contentspanel.title"); //組込み関数の演算数設定
                TitledBorder titleOp = new TitledBorder(BorderFactory.createEtchedBorder(), title);
                Border borderOut = new CompoundBorder( new EmptyBorder(7,7,7,7), titleOp);
                Border border = new CompoundBorder( borderOut, new EmptyBorder(7,7,7,7));
                panelContent.setBorder(border);
                panelContent.setLayout(panelContentLayout);

                // 演算カウントリスト
                {
                    JPanel panelList = new JPanel();
                    BorderLayout panelListLayout = new BorderLayout();
                    panelList.setLayout(panelListLayout);
                    panelContent.add(panelList, BorderLayout.CENTER);
                    {
                        JScrollPane scrollList = new JScrollPane();
                        scrollList.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
                        scrollList.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
                        panelList.add(scrollList, BorderLayout.CENTER);
                        {
                            modelOperation = new DefaultTableModel();
                            modelOperation.setColumnCount(COLUMN_COUNT);
                            // ヘッダー列名
                            String[] columns = COLUMN_HEADER;
                            modelOperation.setColumnIdentifiers(columns);
                            tblOperand = new JTable();
                            scrollList.setViewportView(tblOperand);
                            tblOperand.setModel(modelOperation);
                            tblOperand.setSelectionMode(ListSelectionModel.SINGLE_SELECTION );
                            tblOperand.getSelectionModel().addListSelectionListener(this);
                            tblOperand.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
                            tblOperand.setColumnSelectionAllowed(false);
                            tblOperand.setDefaultEditor(Object.class, null);

                            // 列幅設定
                            for (int i=0; i<tblOperand.getColumnModel().getColumnCount(); i++) {
                                TableColumn col = tblOperand.getColumnModel().getColumn(i);
                                // 列幅を設定する。
                                if (i<COLUMN_MINWIDTH.length) {
                                    col.setMinWidth(COLUMN_MINWIDTH[i]);
                                    // 最大列幅が設定されている(>0)場合は、最大列幅を設定して固定列幅とする。
                                    if (COLUMN_MAXWIDTH[i] > 0) {
                                        col.setMaxWidth(COLUMN_MAXWIDTH[i]);
                                        col.setResizable(false);
                                    }
                                    // 最大列幅が-1で設定されている場合は、非表示列(=0)とする。
                                    else if (COLUMN_MAXWIDTH[i] < 0) {
                                        col.setMaxWidth(0);
                                        col.setResizable(false);
                                    }
                                }
                                // 列幅が設定されていない列は非表示とする。
                                else {
                                    col.setMinWidth(0);
                                    col.setMaxWidth(0);
                                }

                            }
                        }
                    }
                }

                // 設定パネル
                {
                    JPanel panelSettings = new JPanel();
                    BorderLayout panelSettingsLayout = new BorderLayout();
                    panelContent.add(panelSettings, BorderLayout.EAST);
                    Border borderSettings = new EmptyBorder(0,7,0,0);
                    panelSettings.setBorder(borderSettings);
                    panelSettings.setLayout(panelSettingsLayout);

                    panelOperand = new JPanel();
                    GridBagLayout panelKeywordLayout = new GridBagLayout();
                    panelKeywordLayout.columnWidths = new int[] {100, 100, 7};
                    panelKeywordLayout.rowHeights = new int[] {30, 30, 30, 30, 30, 30, 7};
                    panelKeywordLayout.columnWeights = new double[] {0, 0, 0};
                    panelKeywordLayout.rowWeights = new double[] {0, 0, 0, 0, 0, 0, 1};
                    panelSettings.add(panelOperand, BorderLayout.CENTER);
                    panelOperand.setLayout(panelKeywordLayout);
                    panelOperand.setPreferredSize(new java.awt.Dimension(230, 234));

                    // 演算カウント枠

                    String titleOperand = Message.getString("settingkeyworddialog.label.preference"); // 設定
                    TitledBorder borderOutOperand = new TitledBorder(BorderFactory.createEtchedBorder(), titleOperand);
                    Border borderOperand = new CompoundBorder( borderOutOperand, new EmptyBorder(0,7,0,7));
                    panelOperand.setBorder(borderOperand);

                    // 組込み関数名
                    {
                        lblName = new JLabel();
                        panelOperand.add(lblName, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                        lblName.setText(Message.getString("settingoperationdialog.label.intrinsicname")); //組込み関数
                    }
                    {
                        txtName = new JTextField();
                        panelOperand.add(txtName, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
                    }
                    // 演算子:+
                    {
                        JLabel lblOpPlus = new JLabel();
                        panelOperand.add(lblOpPlus, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                        lblOpPlus.setText(Message.getString("settingoperationdialog.label.plus")); //演算数(+)
                    }
                    {
                        txtOpAdd = new JTextField();
                        panelOperand.add(txtOpAdd, new GridBagConstraints(1, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
                    }
                    // 演算子:*
                    {
                        JLabel lblOpProduct = new JLabel();
                        panelOperand.add(lblOpProduct, new GridBagConstraints(0, 2, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                        lblOpProduct.setText(Message.getString("settingoperationdialog.label.product")); //演算数(*)
                    }
                    {
                        txtOpMul = new JTextField();
                        panelOperand.add(txtOpMul, new GridBagConstraints(1, 2, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
                    }
                    /***********
                    // 演算子:-
                    {
                        JLabel lblOpMinus = new JLabel();
                        panelOperand.add(lblOpMinus, new GridBagConstraints(0, 3, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                        lblOpMinus.setText("演算数(-)");
                    }
                    {
                        txtOpSub = new JTextField();
                        panelOperand.add(txtOpSub, new GridBagConstraints(1, 3, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
                    }
                    // 演算子:/
                    {
                        JLabel lblOpQuotient = new JLabel();
                        panelOperand.add(lblOpQuotient, new GridBagConstraints(0, 4, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                        lblOpQuotient.setText("演算数(/)");
                    }
                    {
                        txtOpDiv = new JTextField();
                        panelOperand.add(txtOpDiv, new GridBagConstraints(1, 4, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
                    }
                    ***************/

                    // 演算カウント追加削除ボタンパネル
                    JPanel panelAddButtons = new JPanel();
                    FlowLayout panelAddButtonsLayout = new FlowLayout(FlowLayout.RIGHT);
                    Border borderAddButtons = new EmptyBorder(0,7,0,0);
                    panelAddButtons.setBorder(borderAddButtons);
                    panelAddButtons.setLayout(panelAddButtonsLayout);
                    panelSettings.add(panelAddButtons, BorderLayout.SOUTH);

                    java.awt.Dimension minSize = new java.awt.Dimension(60, 22);
                    Insets minInsets = new Insets(3, 3, 3, 3);
                    {
                        btnAdd = new JButton();
                        panelAddButtons.add(btnAdd);
                        btnAdd.setText(Message.getString("dialog.common.button.add")); //追加
                        btnAdd.setPreferredSize(minSize);
                        btnAdd.setMargin(minInsets);
                        btnAdd.addActionListener(this);
                    }
                    {
                        btnReg = new JButton();
                        panelAddButtons.add(btnReg);
                        btnReg.setText(Message.getString("dialog.common.button.update")); //更新
                        btnReg.setPreferredSize(minSize);
                        btnReg.setMargin(minInsets);
                        btnReg.addActionListener(this);
                    }
                    {
                        btnDel = new JButton();
                        panelAddButtons.add(btnDel);
                        btnDel.setText(Message.getString("dialog.common.button.delete")); //削除
                        btnDel.setPreferredSize(minSize);
                        btnDel.setMargin(minInsets);
                        btnDel.addActionListener(this);
                    }
                    {
                        btnClear = new JButton();
                        panelAddButtons.add(btnClear);
                        btnClear.setText(Message.getString("dialog.common.button.clear")); //クリア
                        btnClear.setPreferredSize(minSize);
                        btnClear.setMargin(minInsets);
                        btnClear.addActionListener(this);
                    }
                }
            }

            setTitle(Message.getString("settingoperationdialog.dialog.title")); //演算数カウント
            this.setSize(560, 420);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * ダイアログを表示する。
     * @return    ダイアログの閉じた時のボタン種別
     */
    public int showDialog() {

        // 親フレーム中央に表示する。
        this.setLocationRelativeTo(this.getOwner());

        // ダイアログ表示
        this.setVisible(true);

        return this.result;
    }

    /**
     * 演算カウントプロパティを設定する。
     * @param properities		演算カウントプロパティ
     */
    public void setOperandProperties(OperationProperties properities) {
        this.properities = properities;

        // 組込み関数(キー)名でソートする
        Object[] keyList = properities.keySet().toArray();
        java.util.Arrays.sort(keyList);

        // 演算カウントリストを作成する
        for (Object key : keyList) {
            OperationCount value = (OperationCount) properities.get(key);

            // 行データの作成
            Object[] column = createOperationRowData(value);
            // 行追加
            modelOperation.addRow(column);
        }

        // 四則演算FLOP設定
        this.txtAdd.setText(String.valueOf(properities.getFlopAdd()));
        this.txtMul.setText(String.valueOf(properities.getFlopMul()));
        this.txtSub.setText(String.valueOf(properities.getFlopSub()));
        this.txtDiv.setText(String.valueOf(properities.getFlopDiv()));

    }

    /**
     * 演算カウントを取得する。
     * @return		演算カウントプロパティ
     */
    public OperationProperties getOperandProperties() {

        // 演算カウントプロパティのクリア
        properities.clear();

        // 演算カウントリストから演算カウントの取得を行う
        int count = modelOperation.getRowCount();
        for (int i=0; i<count; i++) {
            OperationCount opc = new OperationCount();

            Object cell;
            String name = null;
            // 名前
            cell = modelOperation.getValueAt(i, 0);
            if (cell != null && !((String)cell).isEmpty() ) {
                opc.setName((String) cell);
                name = (String) cell;
            }
            // 演算数(+)
            cell = modelOperation.getValueAt(i, 1);
            if (cell != null && !cell.toString().isEmpty() && StringUtils.isNumeric(cell.toString())) {
                opc.setAdd( Integer.parseInt(cell.toString()));
            }
            // 演算数(*)
            cell = modelOperation.getValueAt(i, 2);
            if (cell != null && !(cell.toString()).isEmpty() && StringUtils.isNumeric(cell.toString())) {
                opc.setMul( Integer.parseInt(cell.toString()));
            }
            // 演算数(−)
            cell = modelOperation.getValueAt(i, 3);
            if (cell != null && !(cell.toString()).isEmpty() && StringUtils.isNumeric(cell.toString())) {
                opc.setSub( Integer.parseInt(cell.toString()));
            }
            // 演算数(/)
            cell = modelOperation.getValueAt(i, 4);
            if (cell != null && !(cell.toString()).isEmpty() && StringUtils.isNumeric(cell.toString())) {
                opc.setDiv( Integer.parseInt(cell.toString()));
            }

            // 演算カウントの追加
            if (name != null && !name.isEmpty()) {
                properities.addOperationProperty(name, opc);
            }
        }

        // 四則演算FLOP設定
        {
	        int value = 0;
	        if (StringUtils.isNumeric(this.txtAdd.getText())) {
	        	value = Integer.parseInt(this.txtAdd.getText());
	        }
	        properities.setFlopAdd(value);
        }
        {
	        int value = 0;
	        if (StringUtils.isNumeric(this.txtMul.getText())) {
	        	value = Integer.parseInt(this.txtMul.getText());
	        }
	        properities.setFlopMul(value);
        }
        {
	        int value = 0;
	        if (StringUtils.isNumeric(this.txtSub.getText())) {
	        	value = Integer.parseInt(this.txtSub.getText());
	        }
	        properities.setFlopSub(value);
        }
        {
	        int value = 0;
	        if (StringUtils.isNumeric(this.txtDiv.getText())) {
	        	value = Integer.parseInt(this.txtDiv.getText());
	        }
	        properities.setFlopDiv(value);
        }

        return properities;
    }

    /**
     * ボタンクリックイベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {

        // OK
        if (event.getSource() == this.btnOk) {
            this.result = Constant.OK_DIALOG;

            // 変更内容を取得する。
            getOperandProperties();

            // 変更イベントを発生
            this.properities.firePropertyChange();

            // ダイアログを閉じる。
            dispose();
            return;
        }
        // 閉じる
        else if (event.getSource() == this.btnCancel) {
            this.result = Constant.CANCEL_DIALOG;
            // ダイアログを閉じる。
            dispose();
            return;
        }
        // 更新
        else if (event.getSource() == this.btnReg) {
            // 入力チェック
            if (this.validateOperand(false) == false) {
                // 入力ミス
                return;
            }

            // 更新を行う
            OperationCount opc = this.getOperationCount();
            setOperandList(opc);

        }
        // 追加
        else if (event.getSource() == this.btnAdd) {
            // 入力チェック
            if (this.validateOperand(true) == false) {
                // 入力ミス
                return;
            }

            // 追加を行う
            OperationCount opc = this.getOperationCount();
            addOperandList(opc);
        }
        // 削除
        else if (event.getSource() == this.btnDel) {
            // 選択行
            int selectedrow = this.tblOperand.getSelectedRow();
            if (selectedrow < 0) return;

            int option = JOptionPane.showConfirmDialog(this, Message.getString("settingoperationdialog.confirmdialog.delete.message"), //削除してもよろしいですか？
                    Message.getString("settingoperationdialog.confirmdialog.delete.title"), JOptionPane.OK_CANCEL_OPTION); //演算カウントの削除
            if (option == JOptionPane.OK_OPTION) {
                // 削除を行う
                OperationCount opc = this.getOperationCount();
                removeOperationList(opc);
            }
        }
        // 新規
        else if (event.getSource() == this.btnClear) {
            // 演算カウントパネルをクリアする。
            clearOperation();
        }

    }


    /**
     * 演算カウントリストの変更イベント.<br/>
     * 選択行の演算カウント情報を設定パネルにセットする。
     * @param event		イベント情報
     */
    @Override
    public void valueChanged(ListSelectionEvent event) {

        if (event.getSource() == this.tblOperand.getSelectionModel()) {
            // 選択行を取得する。
            int selectedrow = this.tblOperand.getSelectedRow();
            if (selectedrow < 0) return;

            Object cell;
            // 名前
            cell = modelOperation.getValueAt(selectedrow, 0);
            this.txtName.setText((String) cell);

            // 演算数(+)
            cell = modelOperation.getValueAt(selectedrow, 1);
            this.txtOpAdd.setText(cell!=null ? cell.toString() : null);

            // 演算数(*)
            cell = modelOperation.getValueAt(selectedrow, 2);
            this.txtOpMul.setText(cell!=null ? cell.toString() : null);
/*
            // 演算数(−)
            cell = modelOperand.getValueAt(selectedrow, 3);
            this.txtOpSub.setText(cell!=null ? cell.toString() : null);


            // 演算数(/)
            cell = modelOperand.getValueAt(selectedrow, 4);
            this.txtOpDiv.setText(cell!=null ? cell.toString() : null);
*/
        }

        return;
    }

    /**
     * 演算カウントリストモデルの行データの作成を行う。
     * @param opc		キーワードデータ
     * @return			行データ
     */
    private Object[] createOperationRowData(OperationCount opc) {

        Object[] column = new Object[6];
        // 組込み関数名
        column[0] = opc.getName();
        // 演算数(+)
        column[1] = opc.getAdd();
        // 演算数(*)
        column[2] = opc.getMul();
        // 演算数(-)
        column[3] = opc.getSub();
        // 演算数(/)
        column[4] = opc.getDiv();

        return column;
    }

    /**
     * 演算カウントパネルからキーワードオブジェクトを取得する。
     * @return		演算カウントオブジェクト
     */
    private OperationCount getOperationCount() {

        OperationCount opc = new OperationCount();

        String value = null;
        // 組込み関数名
        opc.setName(this.txtName.getText());
        // 演算数(+)
        value = this.txtOpAdd.getText();
        if (value != null && !value.isEmpty() && StringUtils.isNumeric(value)) {
            opc.setAdd(Integer.parseInt(value));
        }
        else {
            opc.setAdd(null);
        }
        // 演算数(*)
        value = this.txtOpMul.getText();
        if (value != null && !value.isEmpty() && StringUtils.isNumeric(value)) {
            opc.setMul(Integer.parseInt(value));
        }
        else {
            opc.setMul(null);
        }
        /**********
        // 演算数(-)
        value = this.txtOpSub.getText();
        if (value != null && !value.isEmpty() && StringUtils.is_int_str(value)) {
            opc.setSub(Integer.parseInt(value));
        }
        else {
            opc.setSub(null);
        }
        // 演算数(/)
        value = this.txtOpDiv.getText();
        if (value != null && !value.isEmpty() && StringUtils.is_int_str(value)) {
            opc.setDiv(Integer.parseInt(value));
        }
        else {
            opc.setDiv(null);
        }
        **************/

        return opc;
    }

    /**
     * 演算カウント設定を更新する。
     * @param opc		演算カウント
     */
    private void setOperandList(OperationCount opc) {

        // 選択行
        int selectedrow = this.tblOperand.getSelectedRow();
        if (selectedrow < 0) return;

        // 行データの作成
        Object[] column = createOperationRowData(opc);
        // 行更新
        modelOperation.removeRow(selectedrow);
        modelOperation.insertRow(selectedrow, column);
        this.tblOperand.setRowSelectionInterval(selectedrow, selectedrow);
    }

    /**
     * 演算カウントを追加する。
     * @param opc		演算カウント
     */
    private void addOperandList(OperationCount opc) {
        // 行データの作成
        Object[] column = createOperationRowData(opc);
        // 行追加
        modelOperation.addRow(column);
        int selectedrow = modelOperation.getRowCount()-1;
        this.tblOperand.setRowSelectionInterval(selectedrow, selectedrow);
    }


    /**
     * 演算カウントを削除する。
     * @param opc		演算カウント
     */
    private void removeOperationList(OperationCount opc) {
        // 選択行
        int selectedrow = this.tblOperand.getSelectedRow();
        if (selectedrow < 0) return;

        // 行削除
        modelOperation.removeRow(selectedrow);

        // キーワード設定パネルをクリアする。
        clearOperation();
    }

    /**
     * 演算カウントパネルをクリアする。
     */
    private void clearOperation() {

        // 設定をクリア
        /** 組込み関数名テキストボックス */
        this.txtName.setText(null);
        /** 演算数(+)テキストボックス */
        this.txtOpAdd.setText(null);
        /** 演算数(*)テキストボックス */
        this.txtOpMul.setText(null);
        /********
        // 演算数(-)テキストボックス
        this.txtOpSub.setText(null);
        // 演算数(/)テキストボックス
        this.txtOpDiv.setText(null);
        **********/
    }

    /**
     * 入力チェックを行う。
     * @param  addflag		追加フラグ(true=演算カウント追加)
     */
    private boolean validateOperand(boolean addflag) {
        // 名前は必須
        String name = this.txtName.getText();
        // 入力チェック
        if (name == null || name.isEmpty()) {
            JOptionPane.showMessageDialog(this,
                    Message.getString("settingoperationdialog.errordialog.nameempty.message"), //組込み関数名を入力してください。
                    Message.getString("dialog.common.error"), //エラー
                                JOptionPane.ERROR_MESSAGE);
            return false;
        }

        // 組込み関数名の重複チェックを行う。
        int selectedrow = this.tblOperand.getSelectedRow();
        int count = this.modelOperation.getRowCount();
        boolean exists = false;
        for (int i=0; i<count; i++) {
            // 組込み関数名
            String cellName = (String)(this.modelOperation.getValueAt(i, 0));
            if (name.equals(cellName)) {
                if (addflag) {
                    // 追加の場合は、同じ組込み関数名は禁止
                    exists = true;
                    break;
                }
                else if (i != selectedrow) {
                    // 更新の場合は、更新行以外に同じ組込み関数名が存在したらNG
                    exists = true;
                    break;
                }
            }
        }
        if (exists) {
            JOptionPane.showMessageDialog(this,
                    Message.getString("settingoperationdialog.errordialog.duplication.message"), //重複した組込み関数名は登録できません。
                    Message.getString("dialog.common.error"),
                    JOptionPane.ERROR_MESSAGE);
            return false;
        }

        return true;
    }

}


