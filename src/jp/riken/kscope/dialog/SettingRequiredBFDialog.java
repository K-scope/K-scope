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
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JColorChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JSeparator;
import javax.swing.JTextField;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
//import javax.swing.border.LineBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.component.JColorButton;
import jp.riken.kscope.data.RequiredBF;
import jp.riken.kscope.properties.RequiredBFProperties;
import jp.riken.kscope.utils.StringUtils;

/**
 * 要求Byte/FLOP設定ダイアログクラス
 * @author RIKEN
 */
public class SettingRequiredBFDialog extends javax.swing.JDialog implements ActionListener, ChangeListener {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** キャンセルボタン */
    private JButton btnCancel;
    /** OKボタン */
    private JButton btnOk;
    /** 適用ボタン */
    private JButton btnApply;
    /** デフォルトに戻すボタン */
    private JButton btnDefault;
    /** 演算性能 */
    private JTextField txtPerformance;
    /** アクセス先名 */
    private JLabel[] lblNames;
    /** スループット算出モード:ストア有り */
    private JTextField[] txtMem_throughput_calc_mode_stores;
    /** スループット算出モード:ストア無し */
    private JTextField[] txtMem_throughput_calc_mode_nostores;
    /** 係数 */
    private JTextField[] txtCoefs;
    /** 背景色ボタン */
    private JColorButton[] btnColors;
    /** 背景色の有効、無効 */
    private JCheckBox[] chkColors;
    /** 要求Byte算出 */
    private JCheckBox[] chkRequiredbfs;
    /** ダイアログの戻り値 */
    private int result = Constant.CANCEL_DIALOG;
    /** 要求Byte/FLOP設定プロパティ */
    private RequiredBFProperties properities;
    /** アクセス先最大項目数 */
    private final int MEMORY_MAXROWS = 8;
    /** 算出単位:Byte/FLOP */
	private JRadioButton radioBFCalcTypeUnitBF;
	/** 算出単位:Byte/FLOP */
	private JRadioButton radioBFCalcTypeUnitFB;
	/** メモリスループット算出モード：自動判定 */
	private JRadioButton radioMemThroughputCalcModeAuto;
	/** メモリスループット算出モード：ストア有り */
	private JRadioButton radioMemThroughputCalcModeStore;
	/** メモリスループット算出モード：ストアなし */
	private JRadioButton radioMemThroughputCalcModeNostore;
	/** デフォルトサイズ:real */
	private JTextField txtSizeReal;
	/** デフォルトサイズ:integer */
	private JTextField txtSizeInteger;
	/** メモリ性能算出結果ダイアログ */
    @SuppressWarnings("unused")
	private RequiredBFDialog ownerDialog;

    /**
     * コンストラクタ
     * @param frame		親フレーム
     */
    public SettingRequiredBFDialog(JFrame frame) {
        super(frame);
        ownerDialog = null;
        initGUI();
    }

    /**
     * コンストラクタ
     * @param frame		親フレーム
     * @param modal		true=モーダルダイアログを表示する
     */
    public SettingRequiredBFDialog(Frame frame, boolean modal) {
        super(frame, modal);
        ownerDialog = null;
        initGUI();
    }

    /**
     * コンストラクタ
     * @param frame		親フレーム
     * @param modal		true=モーダルダイアログを表示する
     * @param properities	要求Byte/FLOPプロパティ
     */
    public SettingRequiredBFDialog(Frame frame, boolean modal, RequiredBFProperties properities) {
        super(frame, modal);
        this.properities = properities;
        ownerDialog = null;
        initGUI();
        setRequiredBFProperties(this.properities);
    }

    /**
     * コンストラクタ
     * @param owner		メモリ性能算出結果ダイアログ
     * @param modal		true=モーダルダイアログを表示する
     * @param properities	要求Byte/FLOPプロパティ
     */
    public SettingRequiredBFDialog(RequiredBFDialog owner, boolean modal, RequiredBFProperties properities) {
        super(owner, modal);
        this.properities = properities;
        ownerDialog = owner;
        initGUI();
        setRequiredBFProperties(this.properities);
    }


    /**
     * GUI初期化を行う。
     */
    private void initGUI() {
    	
        try {
        	JPanel panelContent = null;
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
                    btnApply = new JButton();
                    panelButtons.add(btnApply);
                    btnApply.setText(Message.getString("dialog.common.button.apply")); //適用
                    btnApply.setPreferredSize(buttonSize);
                    btnApply.addActionListener(this);
                }
                {
                    btnOk = new JButton();
                    panelButtons.add(btnOk);
                    String text = "";
                    text = Message.getString("dialog.common.button.ok"); //OK
                    btnOk.setText(text);
                    btnOk.setPreferredSize(buttonSize);
                    btnOk.addActionListener(this);
                }
                {
                    btnDefault = new JButton();
                    panelButtons.add(btnDefault);
                    String text = Message.getString("mainmenu.view.filter.default"); //デフォルトに戻す
                    btnDefault.setText(text);       // デフォルトに戻す
                    btnDefault.setPreferredSize(buttonSize);
                    btnDefault.setMargin(new Insets(5, 5, 5, 5));
                    btnDefault.addActionListener(this);
                    if (text.getBytes().length > 14) {
	                    Font font = btnDefault.getFont();
	                    Font newFont = new Font(font.getFontName(), font.getStyle(), font.getSize() - 2);
	                    btnDefault.setFont(newFont);
	                    btnDefault.setMargin(new Insets(0, 0, 0, 0));
                    }
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
            // コンテンツパネル
            {
                panelContent = new JPanel();
                panelContent.setLayout(new BoxLayout(panelContent, BoxLayout.Y_AXIS));
                getContentPane().add(panelContent, BorderLayout.CENTER);
                Border border = new EmptyBorder(7,7,0,7);
                panelContent.setBorder(border);

                // 理論演算性能
                int TEXT_INPUT = 5;
                {
                    JPanel panelPerformance = new JPanel();
                    GridBagLayout layoutPerformance = new GridBagLayout();
                    layoutPerformance.rowWeights = new double [] {0.0, 0.0};
                    layoutPerformance.rowHeights = new int [] {24, 7};
                    layoutPerformance.columnWeights = new double [] {0.0, 0.0, 0.0, 1.0};
                    layoutPerformance.columnWidths = new int [] {80, 7, 7, 7};
                    panelPerformance.setLayout(layoutPerformance);
                    panelContent.add(panelPerformance);

                    JLabel label = new JLabel(Message.getString("settingrequiredbfdialog.label.performance")); //理論演算性能
                    panelPerformance.add(label, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(7, 4, 7, 4), 0, 0));
                    this.txtPerformance = new JTextField(TEXT_INPUT);
                	this.txtPerformance.setHorizontalAlignment(JTextField.RIGHT);
                	panelPerformance.add(this.txtPerformance, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 4, 0, 4), 0, 0));
                	panelPerformance.add(new JLabel("GFLOPS"), new GridBagConstraints(2, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 4, 0, 4), 0, 0));

                }

                // 要求Byte/FLOP設定リスト
                {
                    JPanel panelList = new JPanel();
                    BorderLayout panelListLayout = new BorderLayout();
                    panelList.setLayout(panelListLayout);
                    panelContent.add(panelList, BorderLayout.CENTER);
                    String subtitle = Message.getString("settingrequiredbfdialog.label.accesslist"); //アクセス先リスト
                    TitledBorder titleBorder = new TitledBorder(BorderFactory.createEtchedBorder(), subtitle);
                    panelList.setBorder(titleBorder);
                    {
                        JPanel panelRequiredBF = new JPanel();
                        panelList.add(panelRequiredBF, BorderLayout.CENTER);
                        GridBagLayout layoutMemory = new GridBagLayout();
                        layoutMemory.rowWeights = new double [] {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0};
                        layoutMemory.rowHeights = new int [] {7, 7, 4, 4, 4, 4, 4, 4, 1};
                        layoutMemory.columnWeights = new double [] {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0};
                        layoutMemory.columnWidths = new int [] {50, 4, 64, 64, 4, 64, 4, 50, 50, 4, 50, 7};
                        panelRequiredBF.setLayout(layoutMemory);
                        // ヘッダー
                        int col = 0;
                        // アクセス先
                        {
                        	JLabel header = new JLabel(Message.getString("settingrequiredbfdialog.label.access")); //アクセス先
                        	panelRequiredBF.add(header, new GridBagConstraints(col, 0, 1, 2, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.BOTH, new Insets(0, 4, 0, 4), 0, 0));
                        }
                        // 縦罫線
                        col++;
                        {
                        	JSeparator spc = new JSeparator(JSeparator.VERTICAL);
                        	panelRequiredBF.add(spc, new GridBagConstraints(col, 0, 1, layoutMemory.rowHeights.length, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.VERTICAL, new Insets(0, 4, 0, 4), 0, 0));
                        }
                        // メモリスループット
                        col++;
                        {
                        	JLabel header = new JLabel(Message.getString("settingrequiredbfdialog.label.throughput")); //メモリスループット(GB/s)
                        	header.setHorizontalAlignment(JLabel.CENTER);
                        	panelRequiredBF.add(header, new GridBagConstraints(col, 0, 2, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 4, 0, 4), 0, 0));
                        }
                        // ストア有り
                        {
                        	JLabel header = new JLabel(Message.getString("settingrequiredbfdialog.label.store")); //ストア有り
                        	header.setHorizontalAlignment(JLabel.CENTER);
                        	panelRequiredBF.add(header, new GridBagConstraints(col, 1, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 4, 0, 4), 0, 0));
                        }
                        // ストアなし
                        col++;
                        {
                        	JLabel header = new JLabel(Message.getString("settingrequiredbfdialog.label.nostore")); //ストア無し
                        	header.setHorizontalAlignment(JLabel.CENTER);
                        	panelRequiredBF.add(header, new GridBagConstraints(col, 1, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 4, 0, 4), 0, 0));
                        }
                        // 縦罫線
                        col++;
                        {
                        	JSeparator spc = new JSeparator(JSeparator.VERTICAL);
                        	panelRequiredBF.add(spc, new GridBagConstraints(col, 0, 1, layoutMemory.rowHeights.length, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.VERTICAL, new Insets(0, 4, 0, 4), 0, 0));
                        }
                        // 係数
                        col++;
                        {
                        	JLabel header = new JLabel(Message.getString("settingrequiredbfdialog.label.coef")); //係数
                        	header.setHorizontalAlignment(JLabel.CENTER);
                        	panelRequiredBF.add(header, new GridBagConstraints(col, 0, 1, 2, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 4, 0, 4), 0, 0));
                        }
                        // 縦罫線
                        col++;
                        {
                        	JSeparator spc = new JSeparator(JSeparator.VERTICAL);
                        	panelRequiredBF.add(spc, new GridBagConstraints(col, 0, 1, layoutMemory.rowHeights.length, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.VERTICAL, new Insets(0, 4, 0, 4), 0, 0));
                        }
                        // 背景色
                        col++;
                        {
                        	JLabel header = new JLabel(Message.getString("settingrequiredbfdialog.label.bgcolor")); //背景色
                        	header.setHorizontalAlignment(JLabel.CENTER);
                        	panelRequiredBF.add(header, new GridBagConstraints(col, 0, 2, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 4, 0, 4), 0, 0));
                        }
                        // 色選択
                        {
                        	JLabel header = new JLabel(Message.getString("settingrequiredbfdialog.label.bgcolor.select")); //色選択
                        	header.setHorizontalAlignment(JLabel.CENTER);
                        	panelRequiredBF.add(header, new GridBagConstraints(col, 1, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 4, 0, 4), 0, 0));
                        }
                        // 無効
                        col++;
                        {
                        	JLabel header = new JLabel(Message.getString("settingrequiredbfdialog.label.bgcolor.enabled")); //無効
                        	header.setHorizontalAlignment(JLabel.CENTER);
                        	panelRequiredBF.add(header, new GridBagConstraints(col, 1, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 4, 0, 4), 0, 0));
                        }
                        // 縦罫線
                        col++;
                        {
                        	JSeparator spc = new JSeparator(JSeparator.VERTICAL);
                        	panelRequiredBF.add(spc, new GridBagConstraints(col, 0, 1, layoutMemory.rowHeights.length, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.VERTICAL, new Insets(0, 4, 0, 4), 0, 0));
                        }
                        // 要求BF算出
                        col++;
                        {
                        	JLabel header = new JLabel(Message.getString("settingrequiredbfdialog.label.reqbf")); //要求BF算出対象
                        	header.setHorizontalAlignment(JLabel.CENTER);
                        	panelRequiredBF.add(header, new GridBagConstraints(col, 0, 1, 2, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 4, 0, 4), 0, 0));
                        }
                        // 横罫線
                        {
                        	JSeparator spc = new JSeparator(JSeparator.HORIZONTAL);
                        	panelRequiredBF.add(spc, new GridBagConstraints(0, 2, layoutMemory.columnWidths.length, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(0, 4, 0, 4), 0, 0));
                        }

                        // テーブル
                        // アクセス先名
                        this.lblNames = new JLabel[MEMORY_MAXROWS];
                        // メモリスループット（ストア有り）
                        this.txtMem_throughput_calc_mode_stores = new JTextField[MEMORY_MAXROWS];
                        // メモリスループット（ストア無し）
                        this.txtMem_throughput_calc_mode_nostores = new JTextField[MEMORY_MAXROWS];
                        // 係数
                        this.txtCoefs = new JTextField[MEMORY_MAXROWS];
                        // 背景色ボタン
                        this.btnColors = new JColorButton[MEMORY_MAXROWS];
                        // 背景色の有効、無効
                        this.chkColors = new JCheckBox[MEMORY_MAXROWS];
                        // 要求BF算出
                        this.chkRequiredbfs  = new JCheckBox[MEMORY_MAXROWS];
                        int row = 3;
                        for (int i=0; i<MEMORY_MAXROWS; i++) {
                        	col = 0;
                            // アクセス先名
                        	this.lblNames[i] = new JLabel();
                        	panelRequiredBF.add(this.lblNames[i], new GridBagConstraints(0, row, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(2, 0, 2, 0), 0, 0));
                            // メモリスループット:ストア有り
                            col+=2;
                        	this.txtMem_throughput_calc_mode_stores[i] = new JTextField(TEXT_INPUT);
                        	this.txtMem_throughput_calc_mode_stores[i].setHorizontalAlignment(JTextField.RIGHT);
                        	panelRequiredBF.add(this.txtMem_throughput_calc_mode_stores[i], new GridBagConstraints(col, row, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(2, 0, 2, 0), 0, 0));
                            // メモリスループット:ストア無し
                            col++;
                        	this.txtMem_throughput_calc_mode_nostores[i] = new JTextField(TEXT_INPUT);
                        	this.txtMem_throughput_calc_mode_nostores[i].setHorizontalAlignment(JTextField.RIGHT);
                        	panelRequiredBF.add(this.txtMem_throughput_calc_mode_nostores[i], new GridBagConstraints(col, row, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(2, 0, 2, 0), 0, 0));
                        	// 係数
                            col+=2;
                        	this.txtCoefs[i] = new JTextField(TEXT_INPUT);
                        	this.txtCoefs[i].setHorizontalAlignment(JTextField.RIGHT);
                        	panelRequiredBF.add(this.txtCoefs[i], new GridBagConstraints(col, row, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(2, 0, 2, 0), 0, 0));
                        	// 背景色ボタン
                            col+=2;
                        	this.btnColors[i] = new JColorButton();
                        	panelRequiredBF.add(this.btnColors[i], new GridBagConstraints(col, row, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(2, 0, 2, 0), 0, 0));
                            // 背景色の有効、無効
                            col++;
                        	this.chkColors[i] = new JCheckBox();
                         	panelRequiredBF.add(this.chkColors[i], new GridBagConstraints(col, row, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(2, 0, 2, 0), 0, 0));
                         	// イベント
                         	this.chkColors[i].addChangeListener(this);
                         	// 要求BF算出
                            col+=2;
                        	this.chkRequiredbfs[i] = new JCheckBox();
                         	panelRequiredBF.add(this.chkRequiredbfs[i], new GridBagConstraints(col, row, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(2, 0, 2, 0), 0, 0));

                        	row++;
                        }
                    }

                    // オプション
                    {
                        JPanel panelOptions = new JPanel();
                        GridBagLayout layoutOption = new GridBagLayout();
                        layoutOption.rowWeights = new double [] {0.0, 0.0, 0.0, 1.0};
                        layoutOption.rowHeights = new int [] {7, 7, 7, 3};
                        layoutOption.columnWeights = new double [] {0.0, 1.0, 0.0};
                        layoutOption.columnWidths = new int [] {7, 240, 7};
                        panelOptions.setLayout(layoutOption);
                        panelContent.add(panelOptions);

                        String titleOptions = Message.getString("settingrequiredbfdialog.title.option"); //オプション
                        TitledBorder titleOption = new TitledBorder(BorderFactory.createEtchedBorder(), titleOptions);
                        panelOptions.setBorder(titleOption);

                    	// メモリスループット設定（ストア設定）
                    	JLabel labelMemThroughputCalcMode = new JLabel(Message.getString("settingrequiredbfdialog.label.storemode")); //スループットストア設定
                    	panelOptions.add(labelMemThroughputCalcMode, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 4, 0, 4), 0, 0));
                    	radioMemThroughputCalcModeAuto = new JRadioButton(Message.getString("settingrequiredbfdialog.option.storemode.auto")); //自動判定
                    	radioMemThroughputCalcModeStore = new JRadioButton(Message.getString("settingrequiredbfdialog.label.store")); //ストア有り
                    	radioMemThroughputCalcModeNostore = new JRadioButton(Message.getString("settingrequiredbfdialog.label.nostore")); //ストア無し
                    	ButtonGroup groupStore = new ButtonGroup();
                    	groupStore.add(radioMemThroughputCalcModeAuto);
                    	groupStore.add(radioMemThroughputCalcModeStore);
                    	groupStore.add(radioMemThroughputCalcModeNostore);
                    	JPanel panelStore = new JPanel();
                    	panelStore.setLayout(new BoxLayout(panelStore, BoxLayout.X_AXIS));
                    	panelStore.add(radioMemThroughputCalcModeAuto);
                    	panelStore.add(Box.createRigidArea(new Dimension(7,1)));
                    	panelStore.add(radioMemThroughputCalcModeStore);
                    	panelStore.add(Box.createRigidArea(new Dimension(7,1)));
                    	panelStore.add(radioMemThroughputCalcModeNostore);
                    	panelOptions.add(panelStore, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 4, 0, 4), 0, 0));

                        // BF算出単位
                        JLabel labelBFCalcType = new JLabel(Message.getString("settingrequiredbfdialog.label.unit")); //BF算出単位
                        panelOptions.add(labelBFCalcType, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 4, 0, 4), 0, 0));
                        this.radioBFCalcTypeUnitBF = new JRadioButton(Message.getString("settingrequiredbfdialog.check.unit.bf")); //Byte/FLOP
                        this.radioBFCalcTypeUnitFB = new JRadioButton(Message.getString("settingrequiredbfdialog.check.unit.fb")); //FLOP/Byte

                    	ButtonGroup groupUnit = new ButtonGroup();
                    	groupUnit.add(this.radioBFCalcTypeUnitBF);
                    	groupUnit.add(this.radioBFCalcTypeUnitFB);
                    	JPanel panelUnit = new JPanel();
                    	panelUnit.setLayout(new BoxLayout(panelUnit, BoxLayout.X_AXIS));
                    	panelUnit.add(this.radioBFCalcTypeUnitBF);
                    	panelUnit.add(Box.createRigidArea(new Dimension(7,1)));
                    	panelUnit.add(this.radioBFCalcTypeUnitFB);
                    	panelOptions.add(panelUnit, new GridBagConstraints(1, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 4, 0, 4), 0, 0));

                    	// データ型デフォルトサイズ
                    	int TEXT_SIZE = 3;
                    	JLabel labelSize = new JLabel(Message.getString("settingrequiredbfdialog.option.defaultsize")); //デフォルトサイズ
                    	JLabel labelReal = new JLabel("real");
                    	JLabel labelInteger = new JLabel("integer");
                    	this.txtSizeReal = new JTextField(TEXT_SIZE);
                    	this.txtSizeReal.setHorizontalAlignment(JTextField.RIGHT);
                    	this.txtSizeInteger = new JTextField(TEXT_SIZE);
                    	this.txtSizeInteger.setHorizontalAlignment(JTextField.RIGHT);
                    	JLabel labelByte = new JLabel("(Byte)");
                    	panelOptions.add(labelSize, new GridBagConstraints(0, 2, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 4, 0, 4), 0, 0));
                    	JPanel panelSize = new JPanel();
                    	panelSize.setLayout(new BoxLayout(panelSize, BoxLayout.X_AXIS));
                    	panelSize.add(labelReal);
                    	panelSize.add(Box.createRigidArea(new Dimension(7,1)));
                    	panelSize.add(this.txtSizeReal);
                    	panelSize.add(Box.createRigidArea(new Dimension(24,1)));
                    	panelSize.add(labelInteger);
                    	panelSize.add(Box.createRigidArea(new Dimension(7,1)));
                    	panelSize.add(this.txtSizeInteger);
                    	panelSize.add(Box.createRigidArea(new Dimension(14,1)));
                    	panelSize.add(labelByte);
                    	panelOptions.add(panelSize, new GridBagConstraints(1, 2, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 4, 0, 4), 0, 0));
                    }
                }
            }

            setTitle(Message.getString("settingrequiredbfdialog.title")); // 要求Byte/FLOP設定
            this.setResizable(false);  // サイズ変更不可
            this.pack();

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
     * 要求Byte/FLOP設定を設定する。
     * @param properities		要求Byte/FLOP設定プロパティ
     */
    public void setRequiredBFProperties(RequiredBFProperties properities) {
    	if (properities == null) return;

        // 浮動小数点演算演算性能
        this.txtPerformance.setText(String.valueOf(properities.getFlopPerformance()));

    	// オプション：メモリスループット算出モード（ストアの有無の設定）
		RequiredBFProperties.MEM_THROUGHPUT_CALC_MODE mem_throughput_calc_mode = this.properities.getMemThroughputCalcMode();
		if (mem_throughput_calc_mode == RequiredBFProperties.MEM_THROUGHPUT_CALC_MODE.AUTO) {
			this.radioMemThroughputCalcModeAuto.setSelected(true);
		}
		else if (mem_throughput_calc_mode == RequiredBFProperties.MEM_THROUGHPUT_CALC_MODE.STORE) {
			this.radioMemThroughputCalcModeStore.setSelected(true);
		}
		else if (mem_throughput_calc_mode == RequiredBFProperties.MEM_THROUGHPUT_CALC_MODE.NOSTORE) {
			this.radioMemThroughputCalcModeNostore.setSelected(true);
		}
    	// オプション：BF算出単位
		RequiredBFProperties.BF_CALC_TYPE unit_type = this.properities.getBFCalcType();
		if (unit_type == RequiredBFProperties.BF_CALC_TYPE.BYTE_FLOP) {
			this.radioBFCalcTypeUnitBF.setSelected(true);
		}
		else if (unit_type == RequiredBFProperties.BF_CALC_TYPE.FLOP_BYTE) {
			this.radioBFCalcTypeUnitFB.setSelected(true);
		}
    	// オプション：デフォルトサイズ
		int sizeReal = this.properities.getDefaultSizeReal();
		this.txtSizeReal.setText(String.valueOf(sizeReal));
		int sizeInteger = this.properities.getDefaultSizeInteger();
		this.txtSizeInteger.setText(String.valueOf(sizeInteger));

        // メモリアクセス先
        int count = properities.getRequiredBFCount();
        for (int i=0; i<MEMORY_MAXROWS; i++) {
        	if (i<count) {
	        	RequiredBF bf = properities.getRequiredBF(i);
	            // 行データの作成
	            // メモリアクセス先名
	            this.lblNames[i].setText(bf.getName());
	            // メモリスループット:ストア有り
	            this.txtMem_throughput_calc_mode_stores[i].setText(String.valueOf(bf.getMemThroughputStore()));
	            // メモリスループット:ストア無し
	            this.txtMem_throughput_calc_mode_nostores[i].setText(String.valueOf(bf.getMemThroughputNostore()));
	            // 係数
	            this.txtCoefs[i].setText(String.valueOf(bf.getCoef()));
	            // 背景色ボタン
	            this.btnColors[i].setColor(bf.getBackColor());
	            this.btnColors[i].addActionListener(this);
	            // 背景色の有効、無効
	           	this.chkColors[i].setSelected((bf.getBackColor() == null));
	            // 要求BF算出
	            this.chkRequiredbfs[i].setSelected((bf.isRequiredBF()));
        	}
        	else {
	            this.lblNames[i].setVisible(false);
	            this.txtMem_throughput_calc_mode_stores[i].setVisible(false);
	            this.txtMem_throughput_calc_mode_nostores[i].setVisible(false);
	            this.txtCoefs[i].setVisible(false);
	            this.btnColors[i].setVisible(false);
	           	this.chkColors[i].setVisible(false);
	            this.chkRequiredbfs[i].setVisible(false);
        	}
        }

		// 背景色選択ボタンのイネーブルの切替を行う
		setEnabledColorButtons();

        this.pack();
    }

    /**
     * 要求Byte/FLOP設定を取得する。
     * @return		要求Byte/FLOPプロパティ
     */
    public RequiredBFProperties getRequiredBFProperties() {

        // 浮動小数点数演算性能
    	{
            float value = 0.0f;
            String cell = this.txtPerformance.getText();
            if (cell != null && !(cell.toString()).isEmpty() && StringUtils.isFloat(cell.toString())) {
            	value = Float.parseFloat(cell.toString());
            }
            this.properities.setFlopPerformance(value);
    	}
    	// オプション：スループット算出モード
    	{
    		if (this.radioMemThroughputCalcModeAuto.isSelected()) {
                this.properities.setMemThroughputCalcMode(RequiredBFProperties.MEM_THROUGHPUT_CALC_MODE.AUTO);
    		}
    		else if (this.radioMemThroughputCalcModeStore.isSelected()) {
                this.properities.setMemThroughputCalcMode(RequiredBFProperties.MEM_THROUGHPUT_CALC_MODE.STORE);
    		}
    		else if (this.radioMemThroughputCalcModeNostore.isSelected()) {
                this.properities.setMemThroughputCalcMode(RequiredBFProperties.MEM_THROUGHPUT_CALC_MODE.NOSTORE);
    		}
    	}
    	// オプション：BF算出単位
    	{
    		if (this.radioBFCalcTypeUnitBF.isSelected()) {
    			this.properities.setCalcType(RequiredBFProperties.BF_CALC_TYPE.BYTE_FLOP);
    		}
    		else if (this.radioBFCalcTypeUnitFB.isSelected()) {
    			this.properities.setCalcType(RequiredBFProperties.BF_CALC_TYPE.FLOP_BYTE);
    		}
    	}
    	// オプション：デフォルトサイズ
    	{
    		// real
            int sizeReal = this.properities.DEFUALT_DATASIZE;
            String cellReal = this.txtSizeReal.getText();
            if (cellReal != null && !(cellReal.toString()).isEmpty() && StringUtils.isNumeric(cellReal.toString())) {
            	sizeReal = Integer.parseInt(cellReal);
            }
            this.properities.setDefaultSizeReal(sizeReal);
    		// Integer
            int sizeInteger = this.properities.DEFUALT_DATASIZE;
            String cellInteger = this.txtSizeInteger.getText();
            if (cellInteger != null && !(cellInteger).isEmpty() && StringUtils.isNumeric(cellInteger.toString())) {
            	sizeInteger = Integer.parseInt(cellInteger);
            }
            this.properities.setDefaultSizeInteger(sizeInteger);
        }

        // 要求Byte/FLOP設定リストから要求Byte/FLOP設定の取得を行う
        int count = this.properities.getRequiredBFCount();
        for (int i=0; i<count; i++) {
            // メモリスループット:ストア有り
        	{
	            float value = 0.0f;
	            String cell = this.txtMem_throughput_calc_mode_stores[i].getText();
	            if (cell != null && !(cell.toString()).isEmpty() && StringUtils.isFloat(cell.toString())) {
	            	value = Float.parseFloat(cell.toString());
	            }
	            this.properities.getRequiredBF(i).setMemThroughputStore(value);
        	}
            // メモリスループット:ストアなし
        	{
	            float value = 0.0f;
	            String cell = this.txtMem_throughput_calc_mode_nostores[i].getText();
	            if (cell != null && !(cell.toString()).isEmpty() && StringUtils.isFloat(cell.toString())) {
	            	value = Float.parseFloat(cell.toString());
	            }
	            this.properities.getRequiredBF(i).setMemThroughputNostore(value);
        	}
            // 係数
        	{
	            float value = 0.0f;
	            String cell = this.txtCoefs[i].getText();
	            if (cell != null && !(cell.toString()).isEmpty() && StringUtils.isFloat(cell.toString())) {
	            	value = Float.parseFloat(cell.toString());
	            }
	            this.properities.getRequiredBF(i).setCoef(value);
        	}
            // 背景色
        	{
        		if (!this.chkColors[i].isSelected()) {
		            Color back = this.btnColors[i].getColor();
		            this.properities.getRequiredBF(i).setBackColor(back);
        		}
        		else {
		            this.properities.getRequiredBF(i).setBackColor(null);
        		}
        	}
            // 要求BF算出
        	{
        		boolean value = this.chkRequiredbfs[i].isSelected();
	            this.properities.getRequiredBF(i).setRequiredBF(value);
        	}
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
            getRequiredBFProperties();

            // 変更イベントを発生
            this.properities.firePropertyChange();

            // ダイアログを閉じる。
            dispose();
            return;
        }
        // 適用
        else if (event.getSource() == this.btnApply) {
            this.result = Constant.OK_DIALOG;

            // 変更内容を取得する。
            getRequiredBFProperties();

            // 変更イベントを発生
            this.properities.firePropertyChange();

            return;
        }
        // 閉じる
        else if (event.getSource() == this.btnCancel) {
            this.result = Constant.CANCEL_DIALOG;
            // ダイアログを閉じる。
            dispose();
            return;
        }
        // デフォルトに戻す
        else if (event.getSource() == this.btnDefault) {
        	setRequiredBFProperties(this.properities.getDefaultProperties());
            return;
        }
        // 背景色
        else if (event.getSource() instanceof JColorButton) {
            // 色選択ダイアログ
        	JColorButton button = (JColorButton)event.getSource();
            Color color = JColorChooser.showDialog(this, Message.getString("settingrequiredbfdialog.colorchooser.title"), button.getColor()); //色の選択
            if(color != null){
                // ボタンにカラーアイコンを設定する
            	button.setColor(color);
            }
            return;
        }
    }


	/**
	 * 背景色選択ボタンのイネーブルの切替を行う
	 */
	private void setEnabledColorButtons() {
		for (int i=0; i<this.chkColors.length; i++) {
			// 背景色選択ボタンのイネーブルの切替を行う
			this.btnColors[i].setEnabled(!this.chkColors[i].isSelected());
		}
	}

    /**
     * 背景色の有効/無効チェックボックスの変更イベント.
     * 背景色選択ボタンのイネーブルの切替を行う
     * @param event    イベント
     */
	@Override
	public void stateChanged(ChangeEvent event) {
		for (int i=0; i<this.chkColors.length; i++) {
			if (event.getSource() == this.chkColors[i]) {
				// 背景色選択ボタンのイネーブルの切替を行う
				setEnabledColorButtons();
				return;
			}
		}
	}
}
