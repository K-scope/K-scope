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
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.File;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;

import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.data.Program;
import jp.riken.kscope.properties.ProgramProperties;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * 外部ツール設定ダイアログクラス
 * @author riken
 *
 */
public class SettingProgramDialog extends javax.swing.JDialog implements ActionListener, ListSelectionListener, ItemListener {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** キャンセルボタン */
    private JButton btnCancel;
    /** OKボタン */
    private JButton btnOk;
    /** 適用ボタン */
    private JButton btnApply;
    /** 拡張子名ラベル */
    private JLabel lblName;
    /** 拡張子名テキストボックス */
    private JTextField txtName;
    /** パターンラベル */
    private JLabel lblPattern;
    /** パターンテキストボックス */
    private JTextField txtPattern;
    /** 拡張子オプションボタン */
    private JRadioButton chkExt;
    /** 正規表現オプションボタン */
    private JRadioButton chkRegex;
    /** 外部ツールラベル */
    private JLabel lblProgram;
    /** 関連付け */
    private JCheckBox chkRelation;
    /** プログラムテキストボックス */
    private JTextField txtProgram;
    /** 登録ボタン */
    private JButton btnReg;
    /** 追加ボタン */
    private JButton btnAdd;
    /** 削除ボタン */
    private JButton btnDel;
    /** クリアボタン */
    private JButton btnClear;
    /** プログラム参照パス */
    private JButton btnExePath;
    /** 起動オプションラベル */
    private JLabel lblOption;
    /** 起動オプション */
    private JTextField txtOption;
    /** 外部ツールリスト */
    private JTable tblProgram;
    /** 外部ツールリストデータ */
    private DefaultTableModel modelProgram;
    /** 外部ツール設定パネル */
    private JPanel panelProgram;

    /** ダイアログの戻り値 */
    private int result = Constant.CANCEL_DIALOG;

    /** 外部ツールプロパティ */
    ProgramProperties properities;
    /** 設定リストヘッダー */
    private final String[] COLUMN_HEADER = { Message.getString("settingprogramdialog.column_header.preferencename"), //設定名
                                             Message.getString("settingprogramdialog.column_header.suffix-regular"), //拡張子・正規表現
                                             Message.getString("settingprogramdialog.column_header.kind"), //種別
                                             Message.getString("settingprogramdialog.column_header.externaltool"), //外部ツール
                                             Message.getString("settingprogramdialog.column_header.argument") }; //起動引数
    private final int[] COLUMN_MINWIDTH = { 120, 160, 80, 240, 120};
    private final int[] COLUMN_MAXWIDTH = { 0, 0, 0, 80, 80 };
    private final int COLUMN_COUNT = 5;


    /**
     * コンストラクタ
     * @param frame		親フレーム
     */
    public SettingProgramDialog(JFrame frame) {
        super(frame);
        initGUI();

        // 初期表示
        clearProgram();
    }

    /**
     * コンストラクタ
     * @param frame		親フレーム
     * @param modal		true=モーダルダイアログを表示する
     */
    public SettingProgramDialog(Frame frame, boolean modal) {
        super(frame, modal);
        initGUI();

        // 初期表示
        clearProgram();
    }

    /**
     * コンストラクタ
     * @param frame		親フレーム
     * @param modal		true=モーダルダイアログを表示する
     * @param properities	外部ツールプロパティ
     */
    public SettingProgramDialog(Frame frame, boolean modal, ProgramProperties properities) {
        super(frame, modal);
        initGUI();
        setProgramProperties(properities);

        // 初期表示
        clearProgram();
    }

    /**
     * GUI初期化を行う。
     */
    private void initGUI() {
        try {
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
            // コンテンツパネル
            {
                JPanel panelContent = new JPanel();
                BorderLayout panelContentLayout = new BorderLayout();
                getContentPane().add(panelContent, BorderLayout.CENTER);
                Border border = new EmptyBorder(7,7,0,7);
                panelContent.setBorder(border);
                panelContent.setLayout(panelContentLayout);

                // 外部ツールリスト
                {
                    JPanel panelList = new JPanel();
                    BorderLayout panelListLayout = new BorderLayout();
                    panelList.setLayout(panelListLayout);
                    panelContent.add(panelList, BorderLayout.CENTER);
                    {
                        JLabel lblList = new JLabel();
                        panelList.add(lblList, BorderLayout.NORTH);
                        lblList.setText(Message.getString("settingprogramdialog.label.externaltoollist")); //外部ツールリスト
                    }
                    {
                        JScrollPane scrollList = new JScrollPane();
                        scrollList.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
                        scrollList.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
                        panelList.add(scrollList, BorderLayout.CENTER);
                        {
                            modelProgram = new DefaultTableModel();
                            modelProgram.setColumnCount(COLUMN_COUNT);
                            // ヘッダー列名
                            String[] columns = COLUMN_HEADER;
                            modelProgram.setColumnIdentifiers(columns);
                            tblProgram = new JTable();
                            scrollList.setViewportView(tblProgram);
                            tblProgram.setModel(modelProgram);
                            tblProgram.setSelectionMode(ListSelectionModel.SINGLE_SELECTION );
                            tblProgram.getSelectionModel().addListSelectionListener(this);
                            tblProgram.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
                            tblProgram.setColumnSelectionAllowed(false);
                            tblProgram.setDefaultEditor(Object.class, null);

                            // 列幅設定
                            for (int i=0; i<tblProgram.getColumnModel().getColumnCount(); i++) {
                                TableColumn col = tblProgram.getColumnModel().getColumn(i);
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
                    {
                        JLabel lblSettings = new JLabel();
                        lblSettings.setText(Message.getString("settingkeyworddialog.label.preference")); //設定
                        panelSettings.add(lblSettings, BorderLayout.NORTH);
                    }
                    panelProgram = new JPanel();
                    GridBagLayout panelProgramLayout = new GridBagLayout();
                    panelProgramLayout.columnWidths = new int[] {80, 80, 80};
                    panelProgramLayout.rowHeights = new int[] {30, 30, 30, 30, 30, 30, 30, 30, 7};
                    panelProgramLayout.columnWeights = new double[] {0, 0.1, 0.1};
                    panelProgramLayout.rowWeights = new double[] {0, 0, 0, 0, 0, 0, 0, 0, 1};
                    panelSettings.add(panelProgram, BorderLayout.CENTER);
                    panelProgram.setLayout(panelProgramLayout);
                    panelProgram.setPreferredSize(new java.awt.Dimension(420, 260));

                    // 有効チェックボタン
                    EtchedBorder titleBorder = (EtchedBorder) BorderFactory.createEtchedBorder();
                    Border borderKeyword = new CompoundBorder( titleBorder, new EmptyBorder(7,7,0,7));
                    panelProgram.setBorder(borderKeyword);

                    // 外部ツール名
                    {
                        lblName = new JLabel();
                        panelProgram.add(lblName, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                        lblName.setText(Message.getString("settingprogramdialog.label.name")); //名前
                    }
                    {
                        txtName = new JTextField();
                        panelProgram.add(txtName, new GridBagConstraints(1, 0, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
                    }
                    // パターン
                    {
                        lblPattern = new JLabel();
                        panelProgram.add(lblPattern, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                        lblPattern.setText(Message.getString("settingprogramdialog.label.pattern")); //パターン
                    }
                    {
                        txtPattern = new JTextField();
                        panelProgram.add(txtPattern, new GridBagConstraints(1, 1, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
                    }
                    {
                        ButtonGroup group = new ButtonGroup();
                        // 拡張子オプションボタン
                        {
                            chkExt = new JRadioButton();
                            panelProgram.add(chkExt, new GridBagConstraints(1, 2, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
                            chkExt.setText(Message.getString("settingprogramdialog.label.suffix")); //拡張子
                        }
                        // 正規表現オプションボタン
                        {
                            chkRegex = new JRadioButton();
                            panelProgram.add(chkRegex, new GridBagConstraints(2, 2, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
                            chkRegex.setText(Message.getString("searchfinddialog.checkbox.regex")); //正規表現
                        }
                        group.add(chkExt);
                        group.add(chkRegex);
                    }
                    // 拡張子説明
                    {
                        JLabel label1 = new JLabel();
                        label1.setText(Message.getString("settingprogramdialog.label.suffix.desc")); //拡張子はカンマ区切りで複数設定できます。
                        JLabel label2 = new JLabel();
                        label2.setText(Message.getString("settingprogramdialog.label.suffix.ex")); //(例) LOG,TXT,DAT

                        // ボックスパネル
                        Box box = Box.createVerticalBox();
                        box.add(label1);
                        box.add(label2);
                        panelProgram.add(box, new GridBagConstraints(1, 3, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                    }
                    // 外部ツール
                    {
                        lblProgram = new JLabel();
                        panelProgram.add(lblProgram, new GridBagConstraints(0, 4, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                        lblProgram.setText(Message.getString("settingprogramdialog.column_header.externaltool")); //外部ツール
                    }
                    {
                        chkRelation = new JCheckBox();
                        panelProgram.add(chkRelation, new GridBagConstraints(1, 4, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
                        chkRelation.setText(Message.getString("settingprogramdialog.label.programassociation")); //関連付けプログラム
                        chkRelation.addItemListener(this);
                    }
                    {
                        // 外部ツールプログラムパス
                        txtProgram = new JTextField();

                        // 参照ボタン
                        btnExePath = new JButton();
                        btnExePath.setText(Message.getString("dialog.common.button.refer")); //参照
                        btnExePath.setPreferredSize(new java.awt.Dimension(48, 22));
                        btnExePath.setMaximumSize(new java.awt.Dimension(48, 22));
                        btnExePath.setMargin(new Insets(0, 3, 0, 3));
                        btnExePath.addActionListener(this);

                        // ボックスパネル
                        Box box = Box.createHorizontalBox();
                        box.add(txtProgram);
                        box.add(btnExePath);
                        panelProgram.add(box, new GridBagConstraints(1, 5, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
                    }
                    // オプション
                    {
                        lblOption = new JLabel();
                        panelProgram.add(lblOption, new GridBagConstraints(0, 6, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                        lblOption.setText(Message.getString("settingprogramdialog.column_header.argument")); //起動引数
                    }
                    {
                        txtOption = new JTextField();
                        panelProgram.add(txtOption, new GridBagConstraints(1, 6, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
                    }

                    // 起動引数説明
                    {
                    	// プログラム起動時の起動オプションを設定します。
                    	// 以下のマクロが使用可能です。(ソースファイルのみ)
                    	//    %F = ファイル名
                    	//    %L = 行番号
                    	//    (EMACS例 : +%L )
                        JLabel label = new JLabel(Message.getString("settingprogramdialog.discription.argument"));
                        panelProgram.add(label, new GridBagConstraints(1, 7, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0));
                    }

                    // 外部ツール追加削除ボタンパネル
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
                        btnClear.setText(Message.getString("dialog.common.button.clear")); //ｸﾘｱ
                        btnClear.setPreferredSize(minSize);
                        btnClear.setMargin(minInsets);
                        btnClear.addActionListener(this);
                    }
                }
            }

            setTitle(Message.getString("projectsettingtoolsaction.setup.status")); //外部ツール設定
            this.setSize(670, 460);

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
     * 外部ツール設定プロパティを設定する。
     * @param properities		外部ツール設定プロパティ
     */
    public void setProgramProperties(ProgramProperties properities) {
        this.properities = properities;

        // 外部ツールリストを作成する
        int count = properities.getProgramCount();
        for (int i=0; i<count; i++) {
            Program program = properities.getProgram(i);

            // 行データの作成
            Object[] column = createProgramRowData(program);
            // 行追加
            modelProgram.addRow(column);
        }
    }

    /**
     * 外部ツール設定を取得する。
     * @return		外部ツール設定プロパティ
     */
    public ProgramProperties getProgramProperties() {

        // 外部ツール設定リストから外部ツール設定の取得を行う
        int count = modelProgram.getRowCount();
        // 外部ツール設定をクリア
        this.properities.clearProgram();
        for (int i=0; i<count; i++) {
            Program prog = new Program();

            Object cell;
            // 名前
            cell = modelProgram.getValueAt(i, 0);
            String keyname = Message.getKey((String) cell);
            if (StringUtils.isNullOrEmpty(keyname)) {
            	// name変更により入力名前を設定する
            	prog.setName((String) cell);
            }
            else {
            	// name未変更によりnameのキーを設定する.
            	prog.setName(keyname);
            }
            // パターン
            cell = modelProgram.getValueAt(i, 1);
            prog.setPattern((String) cell);
            // 拡張子 or 正規表現
            boolean exts = false;
            cell = modelProgram.getValueAt(i, 2);
            if (Message.getString("settingprogramdialog.label.suffix").equals((String)cell)) { //拡張子
                exts = true;
            }
            prog.setExts(exts);		// 拡張子
            prog.setRegex(!exts);		// 正規表現

            // 外部プログラム
            boolean relation = false;
            cell = modelProgram.getValueAt(i, 3);
            if (Message.getString("settingprogramdialog.label.association").equals((String)cell)) { //関連付け
                relation = true;
            }
            prog.setRelation(relation);
            if (!relation) {
                prog.setExename((String)cell);
            }

            // オプション
            cell = modelProgram.getValueAt(i, 4);
            if ((String) cell != null && !((String)cell).isEmpty()) {
            	prog.setOption((String) cell);
            }
            // 外部ツールの追加
            properities.addProgram(prog);
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
            getProgramProperties();

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
            getProgramProperties();

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
        // 参照
        else if (event.getSource() == this.btnExePath) {
            // ファイル選択ダイアログを表示する。

            // ファイル選択ダイアログを表示する。
            File[] selected = SwingUtils.showOpenFileDialog(this,
                    Message.getString("settingprogramdialog.selectfiledialog.title"), null, null, false); //プログラムの選択
            if (selected == null || selected.length <= 0) return;

            // 選択プログラムの設定をする
            this.txtProgram.setText(selected[0].getAbsolutePath());

            return;
        }
        // 更新
        else if (event.getSource() == this.btnReg) {
            // 入力チェック
            if (this.validateProgram(false) == false) {
                // 入力ミス
                return;
            }
            // プログラムが設定されているかチェック
            if (!chkProgramPath()) {
            	JOptionPane.showMessageDialog(this,
            			Message.getString("settingprogramdialog.errordialog.noprogram.message"), //プログラムを入力してください。
            			Message.getString("dialog.common.error"), //エラー
            			JOptionPane.ERROR_MESSAGE);
            	return;
            }

            // 更新を行う
            Program prog = this.getProgram();
            setProgramList(prog);

        }
        // 追加
        else if (event.getSource() == this.btnAdd) {
            // 入力チェック
            if (this.validateProgram(true) == false) {
                // 入力ミス
                return;
            }
            // プログラムが設定されているかチェック
            if (!chkProgramPath()) {
            	JOptionPane.showMessageDialog(this,
            			Message.getString("settingprogramdialog.errordialog.noprogram.message"), //プログラムを入力してください。
            			Message.getString("dialog.common.error"), //エラー
            			JOptionPane.ERROR_MESSAGE);
            	return;
            }

            // 追加を行う
            Program prog = this.getProgram();
            addProgramList(prog);
        }
        // 削除
        else if (event.getSource() == this.btnDel) {
            // 選択行
            int selectedrow = this.tblProgram.getSelectedRow();
            if (selectedrow < 0) return;

            int option = JOptionPane.showConfirmDialog(this,
                    Message.getString("settingprogramdialog.confirmationdialog.delete.message"), //削除してもよろしいですか？
                    Message.getString("settingprogramdialog.confirmationdialog.delete.title"), //外部ツールの削除
                    JOptionPane.OK_CANCEL_OPTION);
            if (option == JOptionPane.OK_OPTION) {
                // 削除を行う
                Program orog = this.getProgram();
                removeProgramList(orog);
            }
        }
        // 新規
        else if (event.getSource() == this.btnClear) {
            // 設定パネルのイネーブルの切替を行う
            setSettingPanelEnabled(true);
            // 外部ツール設定パネルをクリアする。
            clearProgram();
        }

    }

    /**
     * 設定パネルのイネーブルの切替を行う
     * @param enabled		true=イネーブル
     */
    private void setSettingPanelEnabled(boolean enabled) {
        /** 設定名ラベル */
        this.lblName.setEnabled(enabled);
        /** 設定名テキストボックス */
        this.txtName.setEnabled(enabled);
        /** パターンラベル */
        this.lblPattern.setEnabled(enabled);
        /** パターンテキストボックス */
        this.txtPattern.setEnabled(enabled);
        /** 拡張子チェックボックス */
        this.chkExt.setEnabled(enabled);
        /** 正規表現チェックボックス */
        this.chkRegex.setEnabled(enabled);
        /** 外部ツールラベル */
        this.lblProgram.setEnabled(enabled);
        /** 関連付けチェックボックス */
        this.chkRelation.setEnabled(enabled);
        /** 外部ツールテキストボックス */
        this.txtProgram.setEnabled(enabled);
        /** 外部プログラム参照ボタン */
        this.btnExePath.setEnabled(enabled);
        /** オプション */
        this.txtOption.setEnabled(enabled);

    }


    /**
     * 外部ツール設定リストの変更イベント.<br/>
     * 選択行の外部ツール設定情報を設定パネルにセットする。
     * @param event		イベント情報
     */
    @Override
    public void valueChanged(ListSelectionEvent event) {

        if (event.getSource() == this.tblProgram.getSelectionModel()) {
            // 選択行を取得する。
            int selectedrow = this.tblProgram.getSelectedRow();
            if (selectedrow < 0) return;

            Object cell;
            // 名前
            cell = modelProgram.getValueAt(selectedrow, 0);
            this.txtName.setText((String) cell);
            // パターン
            cell = modelProgram.getValueAt(selectedrow, 1);
            this.txtPattern.setText((String) cell);
            // 拡張子 or 正規表現
            boolean exts = false;
            cell = modelProgram.getValueAt(selectedrow, 2);
            if (Message.getString("settingprogramdialog.label.suffix").equals((String)cell)) { //拡張子
                exts = true;
            }
            this.chkExt.setSelected(exts);   // 拡張子
            this.chkRegex.setSelected(!exts);		// 正規表現

            // 外部プログラム
            boolean relation = false;
            cell = modelProgram.getValueAt(selectedrow, 3);
            if (Message.getString("settingprogramdialog.label.association").equals((String)cell)) { //関連付け
                relation = true;
            }
            this.chkRelation.setSelected(relation);
            if (!relation) {
                this.txtProgram.setText((String)cell);
            }
            else {
                this.txtProgram.setText(null);
            }

            // 起動オプション
            cell = modelProgram.getValueAt(selectedrow, 4);
            this.txtOption.setText((String) cell);
        }
        return;
    }

    /**
     * 外部ツール設定リストモデルの行データの作成を行う。
     * @param prog		外部ツールデータ
     * @return			行データ
     */
    private Object[] createProgramRowData(Program prog) {

        Object[] column = new Object[5];
        // 設定名
        String name = prog.getName();
        if (Message.containsKey(name)) {
        	column[0] = Message.getString(name);
        }
        else {
        	column[0] = name;
        }
        // パターン
        column[1] = prog.getPattern();
        // 拡張子 OR 正規表現
        if (prog.isExts()) {
            column[2] = Message.getString("settingprogramdialog.label.suffix"); //拡張子
        }
        else if (prog.isRegex()) {
            column[2] = Message.getString("searchfinddialog.checkbox.regex"); //正規表現
        }
        // 外部プログラム
        if (prog.isRelation()) {
            column[3] = Message.getString("settingprogramdialog.label.association"); //関連付け
        }
        else {
            column[3] = prog.getExename();
        }
        // 起動引数
        column[4] = prog.getOption();

        return column;
    }

    /**
     * 外部ツール設定パネルから外部ツールオブジェクトを取得する。
     * @return		外部ツール設定オブジェクト
     */
    private Program getProgram() {

        Program prog = new Program();

        // 名前
        prog.setName(this.txtName.getText());
        // パターン
        prog.setPattern(this.txtPattern.getText());
        // 拡張子
        prog.setExts(this.chkExt.isSelected());
        // 正規表現
        prog.setRegex(this.chkRegex.isSelected());

        // 関連付け
        prog.setRelation(this.chkRelation.isSelected());
        // 外部プログラム
        prog.setExename(this.txtProgram.getText());
        // オプション
        prog.setOption(this.txtOption.getText());

        return prog;
    }

    /**
     * 外部ツール設定を更新する。
     * @param prog		外部ツール設定
     */
    private void setProgramList(Program prog) {

        // 選択行
        int selectedrow = this.tblProgram.getSelectedRow();
        if (selectedrow < 0) return;

        // 行データの作成
        Object[] column = createProgramRowData(prog);
        // 行更新
        modelProgram.removeRow(selectedrow);
        modelProgram.insertRow(selectedrow, column);
        this.tblProgram.setRowSelectionInterval(selectedrow, selectedrow);
    }

    /**
     * 外部ツール設定を追加する。
     * @param prog		外部ツール設定
     */
    private void addProgramList(Program prog) {
        // 行データの作成
        Object[] column = createProgramRowData(prog);
        // 行追加
        modelProgram.addRow(column);
        int selectedrow = modelProgram.getRowCount()-1;
        this.tblProgram.setRowSelectionInterval(selectedrow, selectedrow);
    }


    /**
     * 外部ツール設定を削除する。
     * @param prog		外部ツール情報
     */
    private void removeProgramList(Program prog) {
        // 選択行
        int selectedrow = this.tblProgram.getSelectedRow();
        if (selectedrow < 0) return;

        // 行削除
        modelProgram.removeRow(selectedrow);

        // 外部ツール設定パネルをクリアする。
        clearProgram();
    }

    /**
     * 外部ツール設定パネルをクリアする。
     */
    private void clearProgram() {

        // 設定をクリア
        /** 設定名テキストボックス */
        this.txtName.setText(null);
        /** パターンテキストボックス */
        this.txtPattern.setText(null);
        /** 拡張子チェックボックス */
        this.chkExt.setSelected(true);
        /** 正規表現チェックボックス */
        this.chkRegex.setSelected(false);
        /** 関連付けチェックボックス */
        this.chkRelation.setSelected(true);
        /** 外部ツールテキストボックス */
        this.txtProgram.setText(null);
        this.txtProgram.setEnabled(false);
        this.btnExePath.setEnabled(false);
        /** 起動オプション */
        this.txtOption.setText(null);

    }

    /**
     * 入力チェックを行う。
     * @param  addflag		追加フラグ(true=外部ツール設定追加)
     */
    private boolean validateProgram(boolean addflag) {
        // パターンは必須
        String pattern = this.txtPattern.getText();

        // 入力チェック
        if (pattern == null || pattern.isEmpty()) {
            JOptionPane.showMessageDialog(this,
                    Message.getString("settingprogramdialog.errordialog.nosuffix.message"), //拡張子を入力してください。
                    Message.getString("dialog.common.error"), //エラー
                    JOptionPane.ERROR_MESSAGE);
            return false;
        }

        // パターンの重複チェックを行う。
        int selectedrow = this.tblProgram.getSelectedRow();
        int count = this.modelProgram.getRowCount();
        boolean exists = false;
        for (int i=0; i<count; i++) {
            // 外部ツール拡張子
            String cellKeyword = (String)(this.modelProgram.getValueAt(i, 2));
            if (pattern.equals(cellKeyword)) {
                if (addflag) {
                    // 追加の場合は、同じ外部ツールは禁止
                    exists = true;
                    break;
                }
                else if (i != selectedrow) {
                    // 更新の場合は、更新行以外に同じ外部ツールが存在したらNG
                    exists = true;
                    break;
                }
            }
        }
        if (exists) {
            JOptionPane.showMessageDialog(this,
                    Message.getString("settingprogramdialog.errordialog.duplication.message"), //重複した拡張子は登録できません。
                    Message.getString("dialog.common.error"), //エラー
                    JOptionPane.ERROR_MESSAGE);
            return false;
        }

        return true;
    }

    /**
     * チェックボックスのチェックイベント
     * @param event			イベント情報
     */
    @Override
    public void itemStateChanged(ItemEvent event) {
        if (event.getSource() == this.chkRelation) {
            // 外部プログラムのイネーブルの切替
            boolean enabled = this.chkRelation.isSelected();
            this.txtProgram.setEnabled(!enabled);
            this.btnExePath.setEnabled(!enabled);
            this.txtOption.setEnabled(!enabled);
        }
    }

    /**
     * プログラムが指定されているかチェックする
     */
    private boolean chkProgramPath() {
    	if (!this.chkRelation.isSelected()) {
    		if (this.txtProgram.getText().isEmpty())
    			return false;
    	}
    	return true;
    }

}


