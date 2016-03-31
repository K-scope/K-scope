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
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;

import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.data.RequiredBFResult;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.properties.RequiredBFProperties;
import jp.riken.kscope.properties.RequiredBFProperties.BF_CALC_TYPE;
import jp.riken.kscope.properties.VariableMemoryProperties;
import jp.riken.kscope.service.AnalysisMemoryService;

/**
 * メモリ性能算出結果ダイアログクラス
 * @author RIKEN
 */
public class RequiredBFDialog extends javax.swing.JDialog implements ActionListener {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** キャンセルボタン */
    private JButton btnCancel;
    /** OKボタン */
    private JButton btnOk;
    /** 変数アクセス先設定ボタン */
    private JButton btnVariable;
    /** 要求Byte/FLOP設定プロパティ */
    private RequiredBFProperties propertiesMemoryband;
    /** 変数アクセス先メモリ設定 */
    private VariableMemoryProperties propertiesVariable;
    /** ダイアログの戻り値 */
    private int result = Constant.CANCEL_DIALOG;
    /** 前の選択ブロックへ戻るボタン */
	private JButton btnPrev;
	/** 次の選択ブロックへ進むボタン */
	private JButton btnNext;
	/** ロード算出結果ラベル */
	private LabeledTextFeild txtLoad;
	/** ストア算出結果ラベル */
	private LabeledTextFeild txtStore;
	/** 演算数算出結果ラベル */
	private LabeledTextFeild txtFlop;
	/** 要求B/F算出結果ラベル */
	private LabeledTextFeild txtRequired;
	/** スループット算出結果ラベル */
	private LabeledTextFeild txtThroughput;
	/** 実効B/F算出結果ラベル */
	private LabeledTextFeild txtEffective;
	/** ピーク性能比算出結果ラベル */
	private LabeledTextFeild txtPeak;
	/** 要求B/F設定ボタン */
	private JButton btnSetting;
	/** 算出範囲ラベル */
	private JLabel lblBlock;
	/** 分析ビュー追加パネル */
	private JCheckBox chkAddList;
	/** ダイアログ幅設定 */
	private final int DEFAULT_WIDTH = 480;
	/** 変数アクセス先設定ダイアログ */
	private VariableAccessDialog nextDialog;
	/** 親ダイアログフラグ true=最初に呼び出されたダイアログ, false=他のダイアログから呼び出された */
	private boolean ownerDialog;
    /** 選択ブロック */
    private IBlock[] selectedblocks;
    /** 変数アクセス設定 */
	private JPanel panelVariable;
    /** 要求Byte/FLOP算出サービス */
    private AnalysisMemoryService serviceMemory;
    /** 現在表示ブロック */
    private int blockindex;
    /** 要求B/F単位 */
	private JLabel lblUnitRequired;
	/** 実効B/F単位 */
	private JLabel lblUnitEffective;
	/** Byte/FLOP単位文字列 */
	private final String UNIT_BITE_FLOP = "(Byte/FLOP)";
	/** FLOP/Byte単位文字列 */
	private final String UNIT_FLOP_BITE = "(FLOP/Byte)";

    /**
     * コンストラクタ
     * @param frame		親フレーム
     */
    public RequiredBFDialog(JFrame frame) {
        super(frame);
        nextDialog = null;
        this.ownerDialog = true;
        initGUI();
    }

    /**
     * コンストラクタ
     * @param frame		親フレーム
     * @param modal		true=モーダルダイアログを表示する
     */
    public RequiredBFDialog(Frame frame, boolean modal) {
        super(frame, modal);
        nextDialog = null;
        this.ownerDialog = true;
        initGUI();
    }

    /**
     * GUI初期化を行う。
     */
    private void initGUI() {
        try {
            // ボタンパネル
            {
                JPanel panelButtons = new JPanel();
                getContentPane().add(panelButtons, BorderLayout.SOUTH);
                GridBagLayout layout = new GridBagLayout();
                layout.rowWeights = new double [] {0.0};
                layout.rowHeights = new int [] {24};
                layout.columnWeights = new double [] {1.0, 0.0, 1.0};
                layout.columnWidths = new int [] {120, 50, 120};
                panelButtons.setLayout(layout);
                panelButtons.setPreferredSize(new java.awt.Dimension(390, 45));

                // メインボタンサイズ
                java.awt.Dimension buttonSize = new java.awt.Dimension(96, 22);
                {
                    btnOk = new JButton();
                    panelButtons.add(this.btnOk, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                    String text = Message.getString("dialog.common.button.close"); // 閉じる
                    btnOk.setText(text);
                    btnOk.setPreferredSize(buttonSize);
                    btnOk.addActionListener(this);
                }
                {
                	chkAddList = new JCheckBox();
                	panelButtons.add(this.chkAddList, new GridBagConstraints(2, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 14, 0, 0), 0, 0));
                	String text = Message.getString("requiredbfdialog.checkbox.addlist");  // リストに追加して閉じる
                    chkAddList.setText(text);
                    chkAddList.setSelected(true);
                }
            }
            // コンテンツパネル
            {
            	JPanel panelContent = new JPanel(new BorderLayout());
                getContentPane().add(panelContent, BorderLayout.CENTER);
                Border border = new EmptyBorder(7,7,0,7);
                panelContent.setBorder(border);

                // 選択ブロック
                {
                	JPanel panelHeader = new JPanel(new BorderLayout());
                	panelContent.add(panelHeader, BorderLayout.NORTH);
                    GridBagLayout layout = new GridBagLayout();
                    layout.rowWeights = new double [] {0.0, 1.0};
                    layout.rowHeights = new int [] {24, 4};
                    layout.columnWeights = new double [] {0.0, 1.0, 0.0};
                    layout.columnWidths = new int [] {50, 50, 50};
                    panelHeader.setLayout(layout);
                	JPanel panel = new JPanel();
                	panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
                	String text = Message.getString("requiredbfdialog.label.calculateatea");  // 算出範囲
                	JLabel label = new JLabel(text);
                	this.lblBlock = new JLabel("jacobi[962] do loop=1, nn");
                	panel.add(Box.createGlue());
                	panel.add(label);
                	panel.add(Box.createRigidArea(new Dimension(16,1)));
                	panel.add(this.lblBlock);
                	panel.add(Box.createGlue());
                	panelHeader.add(panel, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
                	this.btnPrev = new JButton("<<");
                	this.btnNext = new JButton(">>");
                	this.btnPrev.setMargin(new Insets(0, 3, 0, 3));
                	this.btnNext.setMargin(new Insets(0, 3, 0, 3));
                	this.btnPrev.addActionListener(this);
                	this.btnNext.addActionListener(this);
                	panelHeader.add(this.btnPrev, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                	panelHeader.add(this.btnNext, new GridBagConstraints(2, 0, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                }
                // メモリ性能算出結果
                {
                    JPanel panelList = new JPanel(new BorderLayout());
                    panelContent.add(panelList, BorderLayout.CENTER);
                    String text = Message.getString("requiredbfdialog.frame.performance");  // メモリ性能算出結果
                    TitledBorder titleBorder = new TitledBorder(BorderFactory.createEtchedBorder(), text);
                    panelList.setBorder(titleBorder);
                    {
                        JPanel panelMemory = new JPanel();
                        panelList.add(panelMemory, BorderLayout.CENTER);
                        GridBagLayout layoutMemory = new GridBagLayout();
                        layoutMemory.rowWeights = new double [] {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0};
                        layoutMemory.rowHeights = new int [] {7, 24, 24, 24, 24, 24, 24, 2};
                        layoutMemory.columnWeights = new double [] {0.0, 0.0, 0.0, 1.0};
                        layoutMemory.columnWidths = new int [] {50, 64, 64, 7};
                        panelMemory.setLayout(layoutMemory);

                        int columns = 10;
                        // Load
                        {
                        	JLabel header = new JLabel("Load");
                        	panelMemory.add(header, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 8, 0, 8), 0, 0));
                        	this.txtLoad = new LabeledTextFeild(columns);
                        	panelMemory.add(this.txtLoad, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 8, 0, 8), 0, 0));
                        	JLabel unit = new JLabel("(Byte)");
                        	panelMemory.add(unit, new GridBagConstraints(2, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 8, 0, 8), 0, 0));
                        }
                        // Store
                        {
                        	JLabel header = new JLabel("Store");
                        	panelMemory.add(header, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 8, 0, 8), 0, 0));
                        	this.txtStore = new LabeledTextFeild(columns);
                        	panelMemory.add(this.txtStore, new GridBagConstraints(1, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 8, 0, 8), 0, 0));
                        	JLabel unit = new JLabel("(Byte)");
                        	panelMemory.add(unit, new GridBagConstraints(2, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 8, 0, 8), 0, 0));
                        }
                        // FLOP
                        {
                            String textlabel = Message.getString("requiredbfdialog.label.flop");  // 演算数
                        	JLabel header = new JLabel(textlabel);
                        	panelMemory.add(header, new GridBagConstraints(0, 2, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 8, 0, 8), 0, 0));
                        	this.txtFlop = new LabeledTextFeild(columns);
                        	panelMemory.add(this.txtFlop, new GridBagConstraints(1, 2, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 8, 0, 8), 0, 0));
                        	JLabel unit = new JLabel("(FLOP)");
                        	panelMemory.add(unit, new GridBagConstraints(2, 2, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 8, 0, 8), 0, 0));
                        }
                        // 要求B/F
                        {
                        	String textlabel = Message.getString("requiredbfdialog.label.required");  // 要求B/F
                        	JLabel header = new JLabel(textlabel);
                        	panelMemory.add(header, new GridBagConstraints(0, 3, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 8, 0, 8), 0, 0));
                        	this.txtRequired = new LabeledTextFeild(columns);
                        	panelMemory.add(this.txtRequired, new GridBagConstraints(1, 3, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 8, 0, 8), 0, 0));
                        	lblUnitRequired = new JLabel(UNIT_BITE_FLOP);
                        	panelMemory.add(lblUnitRequired, new GridBagConstraints(2, 3, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 8, 0, 8), 0, 0));
                        }
                        // スループット
                        {
                        	String textlabel = Message.getString("requiredbfdialog.label.throughput");  //スループット
                        	JLabel header = new JLabel(textlabel);
                        	panelMemory.add(header, new GridBagConstraints(0, 4, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 8, 0, 8), 0, 0));
                        	this.txtThroughput = new LabeledTextFeild(columns);
                        	panelMemory.add(this.txtThroughput, new GridBagConstraints(1, 4, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 8, 0, 8), 0, 0));
                        	JLabel unit = new JLabel("(GB/s)");
                        	panelMemory.add(unit, new GridBagConstraints(2, 4, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 8, 0, 8), 0, 0));
                        }
                        // 実効B/F
                        {
                        	String textlabel = Message.getString("requiredbfdialog.label.effective");  //実効B/F
                        	JLabel header = new JLabel(textlabel);
                        	panelMemory.add(header, new GridBagConstraints(0, 5, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 8, 0, 8), 0, 0));
                        	this.txtEffective = new LabeledTextFeild(columns);
                        	panelMemory.add(this.txtEffective, new GridBagConstraints(1, 5, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 8, 0, 8), 0, 0));
                        	lblUnitEffective = new JLabel(UNIT_BITE_FLOP);
                        	panelMemory.add(lblUnitEffective, new GridBagConstraints(2, 5, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 8, 0, 8), 0, 0));
                        }
                        // ピーク性能比
                        {
                        	String textlabel = Message.getString("requiredbfdialog.label.peak");  //ピーク性能比
                        	JLabel header = new JLabel(textlabel);
                        	panelMemory.add(header, new GridBagConstraints(0, 6, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 8, 0, 8), 0, 0));
                        	this.txtPeak = new LabeledTextFeild(columns);
                        	panelMemory.add(this.txtPeak, new GridBagConstraints(1, 6, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 8, 0, 8), 0, 0));
                        	JLabel unit = new JLabel("(%)");
                        	panelMemory.add(unit, new GridBagConstraints(2, 6, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 8, 0, 8), 0, 0));
                        }
                    }
                }
                // 算出設定
                {

	                JPanel panelSettings = new JPanel();
	                panelSettings.setLayout(new BoxLayout(panelSettings, BoxLayout.Y_AXIS));
	                panelContent.add(panelSettings, BorderLayout.SOUTH);
	                panelSettings.add(Box.createRigidArea(new Dimension(1, 7)));
	                // スループット設定
	                {
	                    JPanel panel = new JPanel();
	                    FlowLayout layout = new FlowLayout(FlowLayout.RIGHT, 0, 0);
	                    layout.setHgap(24);
	                    panel.setLayout(layout);
	                    panelSettings.add(panel);
	                    String text = Message.getString("requiredbfdialog.setting.label.throughput");  //スループット設定
	                	JLabel header = new JLabel(text);
	                	panel.add(header);
	                	this.btnSetting = new JButton(Message.getString("dialog.common.button.setting"));  //設定);
	                	panel.add(this.btnSetting);
	                	this.btnSetting.setMargin(new Insets(0, 3, 0, 3));
	                	this.btnSetting.addActionListener(this);
	                }
	                panelSettings.add(Box.createRigidArea(new Dimension(1, 4)));
	                // 変数アクセス設定:変数アクセス設定ダイアログからの表示の場合は算出はなし。
	                if (this.ownerDialog) {
	                    panelVariable = new JPanel();
	                    FlowLayout layout = new FlowLayout(FlowLayout.RIGHT, 0, 0);
	                    layout.setHgap(24);
	                    panelVariable.setLayout(layout);
	                    panelSettings.add(panelVariable);
	                    String text = Message.getString("requiredbfdialog.setting.label.variable");  // 変数アクセス設定
	                	JLabel header = new JLabel(text);
	                	panelVariable.add(header);
	                    // 変数アクセス先設定ボタン
                    	btnVariable = new JButton(Message.getString("dialog.common.button.setting"));  //設定);
                    	panelVariable.add(btnVariable);
	                	this.btnVariable.setMargin(new Insets(0, 3, 0, 3));
	                	this.btnVariable.addActionListener(this);
	                }
                }
            }
            String text = Message.getString("requiredbfdialog.title");  // 要求Byte/FLOP算出
            setTitle(text);
            this.pack();

            Dimension size = new Dimension(DEFAULT_WIDTH, this.getSize().height);
            this.setSize(size);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * ダイアログを表示する。
     * @return    ダイアログの閉じた時のボタン種別
     */
    public int showDialog() {
    	// 要求Byte/FLOPの算出を行う.
		this.blockindex = 0;
		setArrowButton();
		if (this.selectedblocks != null && this.selectedblocks.length > 0) {
			calculateRequiredByteFlop(this.selectedblocks[this.blockindex]);
		}

        // 親フレーム中央に表示する。
        this.setLocationRelativeTo(this.getOwner());

        // ダイアログ表示
        this.setVisible(true);

        return this.result;
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
            if (this.chkAddList.isSelected()) {
            	// リストに追加する.
            	setAnalysisPanel();
            }
            // ダイアログを閉じる。
            dispose();
            return;
        }
        if (event.getSource() == this.btnVariable) {
            // 変数アクセス先設定ダイアログを表示する。
        	if (this.nextDialog != null) {
	            this.setVisible(false);
	            this.nextDialog.setOwnerDialog(false);
	            this.nextDialog.showDialog();

	        	// 要求Byte/FLOPの算出を行う.
	    		if (this.selectedblocks != null && this.selectedblocks.length > 0) {
	    			calculateRequiredByteFlop(this.selectedblocks[this.blockindex]);
	    		}
	            this.setVisible(true);
        	}
            return;
        }
        // 閉じる
        else if (event.getSource() == this.btnCancel) {
            this.result = Constant.CANCEL_DIALOG;
            // ダイアログを閉じる。
            dispose();
            return;
        }
        // スループット設定
        else if (event.getSource() == this.btnSetting) {
            // 要求Byte/FLOP設定ダイアログを表示する。
            SettingRequiredBFDialog dialog = new SettingRequiredBFDialog(this, true, this.propertiesMemoryband);
            dialog.showDialog();
        	// 要求Byte/FLOPの算出を行う.
    		if (this.selectedblocks != null && this.selectedblocks.length > 0) {
    			calculateRequiredByteFlop(this.selectedblocks[this.blockindex]);
    		}
            return;
        }
        // 前のブロック表示
        else if (event.getSource() == this.btnPrev) {
        	if (this.blockindex > 0) this.blockindex--;
        	setArrowButton();
        	// 要求Byte/FLOPの算出を行う.
    		if (this.selectedblocks != null && this.selectedblocks.length > 0) {
    			calculateRequiredByteFlop(this.selectedblocks[this.blockindex]);
    		}
        }
        // 次のブロック表示
        else if (event.getSource() == this.btnNext) {
        	if (this.selectedblocks != null && this.selectedblocks.length > 0) {
            	if (this.blockindex < this.selectedblocks.length-1) this.blockindex++;
        	}
        	setArrowButton();
        	// 要求Byte/FLOPの算出を行う.
    		if (this.selectedblocks != null && this.selectedblocks.length > 0) {
    			calculateRequiredByteFlop(this.selectedblocks[this.blockindex]);
    		}
        }
    }

	/**
	 * 選択ブロックを取得する
	 * @return 選択ブロック
	 */
	public IBlock[] getSelectedBlocks() {
		return this.selectedblocks;
	}

	/**
	 * 選択ブロックを設定する.
	 * @param blocks 選択ブロック
	 */
	public void setSelectedblocks(IBlock[] blocks) {
		this.selectedblocks = blocks;
		this.blockindex = 0;
		setArrowButton();
	}

    /**
     * 変数アクセス先メモリプロパティを取得する
     * @return		変数アクセス先メモリプロパティ
     */
    public VariableMemoryProperties getPropertiesVariable() {
        return propertiesVariable;
    }

    /**
     * 変数アクセス先メモリプロパティを取得する
     * @param  properties		変数アクセス先メモリプロパティ
     */
    public void setPropertiesVariable(VariableMemoryProperties properties) {
        this.propertiesVariable = properties;
    }

    /**
     * ラベル代替テキストフィールドクラス.
     * 編集不可透明なテキストボックス
     * @author RIKEN
     */
    class LabeledTextFeild extends JTextField {
    	/** シリアル番号 */
		private static final long serialVersionUID = 1L;

		/**
    	 * コンストラクタ
    	 */
    	public LabeledTextFeild() {
    		super();
    		setLabeled();
    	}

    	/**
    	 * コンストラクタ
    	 * @param    text    表示文字列
    	 */
    	public LabeledTextFeild(String text) {
    		super(text);
    		setLabeled();
    	}

    	/**
    	 * コンストラクタ
    	 * @param    text    表示文字列
    	 * @param    columns    表示幅
    	 */
    	public LabeledTextFeild(String text, int columns) {
    		super(text, columns);
    		setLabeled();
    	}

    	/**
    	 * コンストラクタ
    	 * @param    columns    表示幅
    	 */
    	public LabeledTextFeild(int columns) {
    		super(columns);
    		setLabeled();
    	}

    	/**
    	 * 表示設定を行う.
    	 * ラベルの様に編集不可とする.
    	 */
    	private void setLabeled() {
        	this.setOpaque(false);
        	this.setEditable(false);
        	this.setBorder(null);
        	this.setHorizontalAlignment(JTextField.RIGHT);
    	}
    }

    /**
     * 変数アクセス先設定ダイアログを設定する.
     * @param dialog		変数アクセス先設定ダイアログ
     */
    public void setVariableAccessDialog(VariableAccessDialog dialog) {
    	this.nextDialog = dialog;
    }

    /**
     * 親ダイアログフラグを設定する.
     * @param owner		親ダイアログフラグ
     */
    public void setOwnerDialog(boolean owner) {
    	this.ownerDialog = owner;
    	// 変数アクセス設定パネルを非表示にする
    	this.panelVariable.setVisible(this.ownerDialog);
    }

	/**
	 * 要求Byte/FLOP算出サービスを設定する.
	 * @param service 要求Byte/FLOP算出サービス
	 */
	public void setServiceMemory(AnalysisMemoryService service) {
		this.serviceMemory = service;
	}

	/**
	 * 要求Byte/FLOP設定プロパティを設定する.
	 * @param properities 要求Byte/FLOP設定プロパティ
	 */
	public void setPropertiesMemoryband(RequiredBFProperties properities) {
		this.propertiesMemoryband = properities;
	}

	/**
	 * 表示ブロックの前に、次へボタンの表示設定を行う.
	 */
	private void setArrowButton() {
		if (this.selectedblocks == null || this.selectedblocks.length <= 0) {
			this.btnPrev.setVisible(false);
			this.btnNext.setVisible(false);
			return;
		}
		else if (this.selectedblocks.length == 1) {
			this.btnPrev.setVisible(false);
			this.btnNext.setVisible(false);
			return;
		}

		this.btnPrev.setVisible(true);
		this.btnNext.setVisible(true);
		this.btnPrev.setEnabled(true);
		this.btnNext.setEnabled(true);
		if (this.blockindex == 0) {
			this.btnPrev.setEnabled(false);
		}
		else if (this.blockindex == this.selectedblocks.length-1) {
			this.btnNext.setEnabled(false);
		}
	}

	/**
	 * 要求Byte/FLOPを算出する.
	 * @param block    算出ブロック
	 */
	private void calculateRequiredByteFlop(IBlock block) {
		if (block == null) return;
		// 要求Byte/FLOPを算出する
		RequiredBFResult result = this.serviceMemory.calcRequiredBF(block);
		// 要求Byte/FLOP算出結果を表示する
		setRequiredByteFlopResult(result);
	}

	/**
	 * 要求Byte/FLOP算出結果を表示する.
	 * @param result    要求Byte/FLOP算出結果
	 */
	private void setRequiredByteFlopResult(RequiredBFResult result) {
		clearRequiredByteFlopResult();
		if (result == null) return;

		// ブロック
		this.lblBlock.setText(result.getBlock().toString());
		// ロード算出結果
		this.txtLoad.setText(String.format("%d", result.getLoad()));
		// ストア算出結果
		this.txtStore.setText(String.format("%d", result.getStore()));
		// 演算数算出結果
		this.txtFlop.setText(String.format("%d", result.getOperand()));
		// 要求B/F算出結果
		float required = result.getRequiredBF();
		if (this.propertiesMemoryband.getBFCalcType() == BF_CALC_TYPE.FLOP_BYTE) {
			required = result.getRequiredFB();
		}
		this.txtRequired.setText(String.format("%.2f", required));
		// スループット算出結果ラベル
		this.txtThroughput.setText(String.format("%.2f", result.getThroughput()));
		// 実効B/F算出結果ラベル
		float effective = result.getEffectiveBF();
		if (this.propertiesMemoryband.getBFCalcType() == BF_CALC_TYPE.FLOP_BYTE) {
			effective = result.getEffectiveFB();
		}
		this.txtEffective.setText(String.format("%.2f", effective));
		// ピーク性能比算出結果ラベル
		float peak = result.getPeakBF();
		if (this.propertiesMemoryband.getBFCalcType() == BF_CALC_TYPE.FLOP_BYTE) {
			peak = result.getPeakFB();
		}
		peak *= 100.0F;  // %表示
		this.txtPeak.setText(String.format("%.2f", peak));
		// 単位表示
		this.lblUnitRequired.setText(UNIT_BITE_FLOP);
		this.lblUnitEffective.setText(UNIT_BITE_FLOP);
		if (this.propertiesMemoryband.getBFCalcType() == BF_CALC_TYPE.FLOP_BYTE) {
			this.lblUnitRequired.setText(UNIT_FLOP_BITE);
			this.lblUnitEffective.setText(UNIT_FLOP_BITE);
		}

	}

	/**
	 * 要求Byte/FLOP算出結果をクリアする.
	 */
	private void clearRequiredByteFlopResult() {
		// ブロック
		this.lblBlock.setText("");
		// ロード算出結果
		this.txtLoad.setText("");
		// ストア算出結果
		this.txtStore.setText("");
		/** 演算数算出結果ラベル */
		this.txtFlop.setText("");
		/** 要求B/F算出結果ラベル */
		this.txtRequired.setText("");
		/** スループット算出結果ラベル */
		this.txtThroughput.setText("");
		/** 実効B/F算出結果ラベル */
		this.txtEffective.setText("");
		/** ピーク性能比算出結果ラベル */
		this.txtPeak.setText("");
	}

	/**
	 * 分析ビューに算出結果を追加する.
	 */
	private void setAnalysisPanel() {
		if (this.selectedblocks == null) return;

		List<RequiredBFResult> list = new ArrayList<RequiredBFResult>();
		for (IBlock block : this.selectedblocks) {
			// 要求Byte/FLOPを算出する
			RequiredBFResult result = this.serviceMemory.calcRequiredBF(block);
			if (result != null) {
				list.add(result);
			}
		}

		if (list.size() <= 0) return;
		this.serviceMemory.setAnalysisPanel(list.toArray(new RequiredBFResult[0]));

	}

}
