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
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JColorChooser;
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
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;

import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.common.KEYWORD_TYPE;
import jp.riken.kscope.component.JColorButton;
import jp.riken.kscope.component.JComponentTitledBorder;
import jp.riken.kscope.data.Keyword;
import jp.riken.kscope.properties.KeywordProperties;
import jp.riken.kscope.utils.StringUtils;

/**
 * キーワード設定ダイアログクラス
 * @author RIKEN
 *
 */
public class SettingKeywordDialog extends javax.swing.JDialog implements ActionListener, ListSelectionListener, ItemListener {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** キャンセルボタン */
    private JButton btnCancel;
    /** OKボタン */
    private JButton btnOk;
    /** 適用ボタン */
    private JButton btnApply;
    /** キーワード名ラベル */
    private JLabel lblName;
    /** キーワード名テキストボックス */
    private JTextField txtName;
    /** キーワードラベル */
    private JLabel lblKeyword;
    /** キーワードテキストボックス */
    private JTextField txtKeyword;
    /** フォント色ラベル */
    private JLabel lblColor;
    /** フォント色ボタン */
    private JColorButton btnColor;
    /** スタイルラベル */
    private JLabel lblStyle;
    /** イタリックチェックボックス */
    private JCheckBox chkItalic;
    /** ボイドチェックボックス */
    private JCheckBox chkBold;
    /** 有効チェックボックス */
    private JCheckBox chkEnabled;
    /** オプションラベル */
    private JLabel lblOption;
    /** 大文字小文字区別チェックボックス(true=大文字・小文字の区別を行う) */
    private JCheckBox chkSensitivecase;
    /** 正規表現チェックボックス */
    private JCheckBox chkRegex;
    /** 登録ボタン */
    private JButton btnReg;
    /** 追加ボタン */
    private JButton btnAdd;
    /** 削除ボタン */
    private JButton btnDel;
    /** 新規ボタン */
    private JButton btnNew;

    /** キーワードリスト */
    private JTable tblKeyword;
    /** キーワードリストデータ */
    private DefaultTableModel modelKeyword;
    /** 有効チェックボックス付きボーダ */
    private JComponentTitledBorder titleBorder;
    /** キーワード設定パネル */
    private JPanel panelKeyword;

    /** ダイアログの戻り値 */
    private int result = Constant.CANCEL_DIALOG;

    /** キーワードプロパティ */
    KeywordProperties properities;
    /** 設定リストヘッダー */
    private final String[] COLUMN_HEADER = { Message.getString("dialog.common.button.apply"), //適用
                                             Message.getString("settingkeyworddialog.columnheader.caption"), //キーワード名
                                             Message.getString("mainmenu.project.config.keyword"), //キーワード
                                             Message.getString("settingkeyworddialog.columnheader.fontcolor"), //フォント色
                                             Message.getString("jfontchooserdialog.fontpanel.label.style"), //スタイル
                                             Message.getString("settingkeyworddialog.columnheader.upper-lower"), //大文字・小文字区別
                                             Message.getString("searchfinddialog.checkbox.regex"), //正規表現
                                             Message.getString("settingkeyworddialog.columnheader.forbidden") //キーワード編集不可
                                             };
    private final int[] COLUMN_MINWIDTH = { 50, 120, 160, 80, 80, 120, 80, 0, 0 };
    private final int[] COLUMN_MAXWIDTH = { 50, 0, 0, 80, 80, 120, 80, -1, -1 };
    /** COLUMN_HEADER.length + 1 : 末尾にキーワードロック(非表示) */
    private final int COLUMN_COUNT = 8;

    /**
     * コンストラクタ
     * @param frame		親フレーム
     */
    public SettingKeywordDialog(JFrame frame) {
        super(frame);
        initGUI();
    }

    /**
     * コンストラクタ
     * @param frame		親フレーム
     * @param modal		true=モーダルダイアログを表示する
     */
    public SettingKeywordDialog(Frame frame, boolean modal) {
        super(frame, modal);
        initGUI();
    }

    /**
     * コンストラクタ
     * @param frame		親フレーム
     * @param modal		true=モーダルダイアログを表示する
     * @param properities	キーワードプロパティ
     */
    public SettingKeywordDialog(Frame frame, boolean modal, KeywordProperties properities) {
        super(frame, modal);
        initGUI();
        setKeywordProperties(properities);
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

                // キーワードリスト
                {
                    JPanel panelList = new JPanel();
                    BorderLayout panelListLayout = new BorderLayout();
                    panelList.setLayout(panelListLayout);
                    panelContent.add(panelList, BorderLayout.CENTER);
                    {
                        JLabel lblList = new JLabel();
                        panelList.add(lblList, BorderLayout.NORTH);
                        lblList.setText(Message.getString("settingkeyworddialog.label.keywordlist")); //キーワードリスト
                    }
                    {
                        JScrollPane scrollList = new JScrollPane();
                        scrollList.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
                        scrollList.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
                        panelList.add(scrollList, BorderLayout.CENTER);
                        {
                            modelKeyword = new DefaultTableModel();
                            modelKeyword.setColumnCount(COLUMN_COUNT);
                            // ヘッダー列名
                            String[] columns = COLUMN_HEADER;
                            modelKeyword.setColumnIdentifiers(columns);
                            tblKeyword = new JTable();
                            scrollList.setViewportView(tblKeyword);
                            tblKeyword.setModel(modelKeyword);
                            tblKeyword.setSelectionMode(ListSelectionModel.SINGLE_SELECTION );
                            tblKeyword.getSelectionModel().addListSelectionListener(this);
                            tblKeyword.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
                            tblKeyword.setDefaultRenderer(Object.class, new KeywordTableRenderer());
                            tblKeyword.setColumnSelectionAllowed(false);
                            tblKeyword.setDefaultEditor(Object.class, null);

                            // 列幅設定
                            for (int i=0; i<tblKeyword.getColumnModel().getColumnCount(); i++) {
                                TableColumn col = tblKeyword.getColumnModel().getColumn(i);
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
                    panelKeyword = new JPanel();
                    GridBagLayout panelKeywordLayout = new GridBagLayout();
                    panelKeywordLayout.columnWidths = new int[] {100, 80, 80};
                    panelKeywordLayout.rowHeights = new int[] {30, 30, 30, 30, 30, 30, 7};
                    panelKeywordLayout.columnWeights = new double[] {0.1, 0.1, 0.1};
                    panelKeywordLayout.rowWeights = new double[] {0, 0, 0, 0, 0, 0, 1};
                    panelSettings.add(panelKeyword, BorderLayout.CENTER);
                    panelKeyword.setLayout(panelKeywordLayout);
                    panelKeyword.setPreferredSize(new java.awt.Dimension(320, 234));

                    // 有効チェックボタン
                    chkEnabled = new JCheckBox(Message.getString("settingkeyworddialog.checkbox.enable")); //有効
                    chkEnabled.addItemListener(this);
                    titleBorder = new JComponentTitledBorder(chkEnabled, panelKeyword, BorderFactory.createEtchedBorder());
                    Border borderKeyword = new CompoundBorder( titleBorder, new EmptyBorder(7,7,0,7));
                    panelKeyword.setBorder(borderKeyword);

                    // キーワード名
                    {
                        lblName = new JLabel();
                        panelKeyword.add(lblName, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                        lblName.setText(Message.getString("settingkeyworddialog.label.keywordname")); //名前
                    }
                    {
                        txtName = new JTextField();
                        panelKeyword.add(txtName, new GridBagConstraints(1, 0, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
                    }
                    // キーワード
                    {
                        lblKeyword = new JLabel();
                        panelKeyword.add(lblKeyword, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                        lblKeyword.setText(Message.getString("mainmenu.project.config.keyword")); //キーワード
                    }
                    {
                        txtKeyword = new JTextField();
                        panelKeyword.add(txtKeyword, new GridBagConstraints(1, 1, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
                    }
                    // フォント色
                    {
                        lblColor = new JLabel();
                        panelKeyword.add(lblColor, new GridBagConstraints(0, 2, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                        lblColor.setText(Message.getString("settingkeyworddialog.columnheader.fontcolor")); //フォント色
                    }
                    {
                        btnColor = new JColorButton();
                        panelKeyword.add(btnColor, new GridBagConstraints(1, 2, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                        btnColor.addActionListener(this);
                    }
                    // スタイル
                    {
                        lblStyle = new JLabel();
                        panelKeyword.add(lblStyle, new GridBagConstraints(0, 3, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                        lblStyle.setText(Message.getString("jfontchooserdialog.fontpanel.label.style")); //スタイル
                    }
                    // Bold
                    {
                        chkBold = new JCheckBox();
                        panelKeyword.add(chkBold, new GridBagConstraints(1, 3, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                        chkBold.setText("BOLD");
                    }
                    // Italic
                    {
                        chkItalic = new JCheckBox();
                        panelKeyword.add(chkItalic, new GridBagConstraints(2, 3, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                        chkItalic.setText("ITALIC");
                    }
                    // オプション
                    {
                        lblOption = new JLabel();
                        panelKeyword.add(lblOption, new GridBagConstraints(0, 4, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                        lblOption.setText(Message.getString("settingkeyworddialog.label.options")); //オプション
                    }
                    {
                        chkSensitivecase = new JCheckBox();
                        panelKeyword.add(chkSensitivecase, new GridBagConstraints(1, 4, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
                        chkSensitivecase.setText(Message.getString("searchfinddialog.checkbox.upper-lower")); //大文字・小文字を区別する
                    }
                    {
                        chkRegex = new JCheckBox();
                        panelKeyword.add(chkRegex, new GridBagConstraints(1, 5, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
                        chkRegex.setText(Message.getString("searchfinddialog.checkbox.regex")); //正規表現
                    }

                    // キーワード追加削除ボタンパネル
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
                        btnAdd.setText(Message.getString("settingkeyworddialog.button.add")); //|追加
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
                        btnNew = new JButton();
                        panelAddButtons.add(btnNew);
                        btnNew.setText(Message.getString("informationdialog.button.clear.tooltip")); //クリア
                        btnNew.setPreferredSize(minSize);
                        btnNew.setMargin(minInsets);
                        btnNew.addActionListener(this);
                    }
                }
            }

            setTitle(Message.getString("settingkeyworddialog.dialog.title")); //キーワード設定
            this.setSize(670, 360);
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
     * キーワード設定を設定する。
     * @param properities		キーワード設定プロパティ
     */
    public void setKeywordProperties(KeywordProperties properities) {
        this.properities = properities;

        // キーワードリストを作成する
        //  "適用", "キーワード名", "キーワード", "フォント色", "スタイル"
        int count = properities.getKeywordCount();
        for (int i=0; i<count; i++) {
            Keyword keyword = properities.getKeyword(i);

            // 行データの作成
            Object[] column = createKeywordRowData(keyword);
            // 行追加
            modelKeyword.addRow(column);
        }
    }

    /**
     * キーワード設定を取得する。
     * @return		キーワード設定プロパティ
     */
    public KeywordProperties getKeywordProperties() {

        // キーワードプロパティのクリア
        properities.clearKeyword();

        // キーワードリストからキーワード設定の取得を行う
        int count = modelKeyword.getRowCount();
        for (int i=0; i<count; i++) {
            Keyword keyword = new Keyword(KEYWORD_TYPE.KEYWORD);

            Object cell;
            // 有効・無効
            cell = modelKeyword.getValueAt(i, 0);
            boolean enabled = Message.getString("settingkeyworddialog.checkbox.enable").equals((String)cell); //有効
            keyword.setEnabled(enabled);
            // 名前
            cell = modelKeyword.getValueAt(i, 1);
            String keyname = Message.getKey((String) cell);
            if (StringUtils.isNullOrEmpty(keyname)) {
            	// name変更により入力名前を設定する
            	keyword.setName((String) cell);
            }
            else {
            	// name未変更によりnameのキーを設定する.
            	keyword.setName(keyname);
            }
            // キーワード
            cell = modelKeyword.getValueAt(i, 2);
            keyword.setKeyword((String) cell);
            // フォント色
            cell = modelKeyword.getValueAt(i, 3);
            if (cell instanceof java.awt.Color) {
                keyword.setForecolor((java.awt.Color) cell);
            }
            // スタイル
            // Bold
            cell = modelKeyword.getValueAt(i, 4);
            boolean bold = false;
            if (((String)cell).indexOf("Bold") >= 0) {
                bold = true;
            }
            // Italic
            boolean italic = false;
            if (((String)cell).indexOf("Italic") >= 0) {
                italic = true;
            }
            int style = Font.PLAIN;
            if (bold) style += Font.BOLD;
            if (italic) style += Font.ITALIC;
            keyword.setStyle(style);

            // 大文字・小文字の区別
            cell = modelKeyword.getValueAt(i, 5);
            boolean sensitivecase = (Boolean)cell;
            keyword.setSensitivecase(sensitivecase);
            // 正規表現
            cell = modelKeyword.getValueAt(i, 6);
            boolean regex = (Boolean)cell;
            keyword.setRegex(regex);
            // キーワードロック
            cell = modelKeyword.getValueAt(i, 7);
            boolean keywordlock = (Boolean)cell;
            keyword.setKeywordlock(keywordlock);

            // キーワードの追加
            properities.addKeyword(keyword);
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
            getKeywordProperties();

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
            getKeywordProperties();

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
        // フォント、背景、アクティブ背景色
        else if (event.getSource() == this.btnColor) {
            // 色選択ダイアログ
            Color color = JColorChooser.showDialog(this, Message.getString("settingkeyworddialog.colorchooser.title"), this.btnColor.getColor()); //色の選択
            if(color != null){
                // ボタンにカラーアイコンを設定する
                this.btnColor.setColor(color);
            }

            return;
        }
        // 更新
        else if (event.getSource() == this.btnReg) {
            // 入力チェック
            if (this.validateKeyword(false) == false) {
                // 入力ミス
                return;
            }

            // 更新を行う
            Keyword keyword = this.getKeyword();
            setKeywordList(keyword);

        }
        // 追加
        else if (event.getSource() == this.btnAdd) {
            // 入力チェック
            if (this.validateKeyword(true) == false) {
                // 入力ミス
                return;
            }

            // 追加を行う
            Keyword keyword = this.getKeyword();

            // 追加の場合、キーワード編集不可はfalseとする
            keyword.setKeywordlock(false);

            addKeywordList(keyword);
        }
        // 削除
        else if (event.getSource() == this.btnDel) {
            // 削除を行う
        	Keyword keyword = this.getKeyword();
            removeKeywordList(keyword);

            // 設定パネルのイネーブルの切替を行う
            setSettingPanelEnabled(true, false);
            // キーワード設定パネルをクリアする
            clearKeyword();
        }
        // クリア
        else if (event.getSource() == this.btnNew) {
            // 設定パネルのイネーブルの切替を行う
            setSettingPanelEnabled(true, false);
            // キーワード設定パネルをクリアする。
            clearKeyword();
        }

    }

    /**
     * 有効・無効チェックボックス変更イベント
     * @param event		イベント情報
     */
    @Override
    public void itemStateChanged(ItemEvent event) {

        if (event.getSource() == this.chkEnabled) {
            boolean enabled = this.chkEnabled.isSelected();
            // 設定パネルのイネーブルの切替を行う
            setSettingPanelEnabled(enabled, true);
        }
    }

    /**
     * 設定パネルのイネーブルの切替を行う
     * @param enabled		true=イネーブル
     * @param keylockable		true=キーワードロック状態を判断する
     */
    private void setSettingPanelEnabled(boolean enabled, boolean keylockable) {
        /** キーワード名ラベル */
        this.lblName.setEnabled(enabled);
        /** キーワード名テキストボックス */
        this.txtName.setEnabled(enabled);
        /** キーワードラベル */
        this.lblKeyword.setEnabled(enabled);
        /** キーワードテキストボックス */
        this.txtKeyword.setEnabled(enabled);
        /** フォント色ラベル */
        this.lblColor.setEnabled(enabled);
        /** フォント色ボタン */
        this.btnColor.setEnabled(enabled);
        /** スタイルラベル */
        this.lblStyle.setEnabled(enabled);
        /** イタリックチェックボックス */
        this.chkItalic.setEnabled(enabled);
        /** ボイドチェックボックス */
        this.chkBold.setEnabled(enabled);
        /** オプションラベル */
        this.lblOption.setEnabled(enabled);
        /** 大文字小文字区別チェックボックス */
        this.chkSensitivecase.setEnabled(enabled);
        /** 正規表現チェックボックス */
        this.chkRegex.setEnabled(enabled);

        if (keylockable) {
            // キーワードロック状態によりEnableにはしない
            if (enabled) {
                // 選択行を取得する。
                int selectedrow = this.tblKeyword.getSelectedRow();

                // キーワードロックであれば、キーワードの編集不可
                if (selectedrow >= 0) {
                    // キーワードロック
                    Object cell = modelKeyword.getValueAt(selectedrow, 7);
                    boolean keywordlock = (Boolean)cell;
                    this.txtKeyword.setEnabled(!keywordlock);
                    this.chkRegex.setEnabled(!keywordlock);
                    this.chkSensitivecase.setEnabled(!keywordlock);
                }
            }
        }
    }


    /**
     * キーワードリストの変更イベント.<br/>
     * 選択行のキーワード情報を設定パネルにセットする。
     * @param event		イベント情報
     */
    @Override
    public void valueChanged(ListSelectionEvent event) {

        if (event.getSource() == this.tblKeyword.getSelectionModel()) {
            // 選択行を取得する。
            int selectedrow = this.tblKeyword.getSelectedRow();
            if (selectedrow < 0) return;

            Object cell;
            // 有効・無効
            cell = modelKeyword.getValueAt(selectedrow, 0);
            boolean enabled = Message.getString("settingkeyworddialog.checkbox.enable").equals((String)cell); //有効
            this.chkEnabled.setSelected(enabled);

            // 有効・無効により設定パネルのイネーブルの切替
            setSettingPanelEnabled(enabled, true);

            // 名前
            cell = modelKeyword.getValueAt(selectedrow, 1);
            this.txtName.setText((String) cell);
            // キーワード
            cell = modelKeyword.getValueAt(selectedrow, 2);
            this.txtKeyword.setText((String) cell);
            // フォント色
            cell = modelKeyword.getValueAt(selectedrow, 3);
            if (cell == null) {
                this.btnColor.setColor(null);
            }
            else if (cell instanceof java.awt.Color) {
                this.btnColor.setColor((java.awt.Color) cell);
            }
            // スタイル
            // Bold
            cell = modelKeyword.getValueAt(selectedrow, 4);
            boolean bold = false;
            if (((String)cell).indexOf("Bold") >= 0) {
                bold = true;
            }
            this.chkBold.setSelected(bold);
            // Italic
            boolean italic = false;
            if (((String)cell).indexOf("Italic") >= 0) {
                italic = true;
            }
            this.chkItalic.setSelected(italic);
            // 大文字・小文字の区別
            cell = modelKeyword.getValueAt(selectedrow, 5);
            boolean sensitivecase = (Boolean)cell;
            this.chkSensitivecase.setSelected(sensitivecase);
            // 正規表現
            cell = modelKeyword.getValueAt(selectedrow, 6);
            boolean regex = (Boolean)cell;
            this.chkRegex.setSelected(regex);
            // キーワードロック
            cell = modelKeyword.getValueAt(selectedrow, 7);
            boolean keywordlock = (Boolean)cell;
            this.txtKeyword.setEnabled(!keywordlock);
            this.chkRegex.setEnabled(!keywordlock);
            this.chkSensitivecase.setEnabled(!keywordlock);

            // 有効チェックボックスボーダの再描画
            this.chkEnabled.repaint();
            panelKeyword.repaint();
        }
        return;
    }

    /**
     * キーワードリストモデルの行データの作成を行う。
     * @param keyword		キーワードデータ
     * @return			行データ
     */
    private Object[] createKeywordRowData(Keyword keyword) {
    	// COLUMN_COUNT=8
        Object[] column = new Object[COLUMN_COUNT];
        column[0] = (keyword.isEnabled()) ? Message.getString("settingkeyworddialog.checkbox.enable") : Message.getString("settingkeyworddialog.data.disable"); //KEY13=有効 / KEY29=無効
        if (Message.containsKey(keyword.getName())) {
        	column[1] = Message.getString(keyword.getName());
        }
        else {
        	column[1] = keyword.getName();
        }
        if (keyword.getKeyword() != null) {
            column[2] = keyword.getKeyword();
        }
        else if (keyword.getClassmode() != null) {
            column[2] = keyword.getClassmode();
        }
        column[3] = keyword.getForecolor();
        if (keyword.getStyle() == Font.PLAIN) {
            column[4] = "Plain";
        }
        else if (keyword.getStyle() == Font.BOLD) {
            column[4] = "Bold";
        }
        else if (keyword.getStyle() == Font.ITALIC) {
            column[4] = "Italic";
        }
        else if (keyword.getStyle() == Font.BOLD + Font.ITALIC) {
            column[4] = "Bold Italic";
        }
        column[5] = new Boolean(keyword.isSensitivecase());
        column[6] = new Boolean(keyword.isRegex());
        column[7] = new Boolean(keyword.isKeywordlock());

        return column;
    }

    /**
     * キーワード設定パネルからキーワードオブジェクトを取得する。
     * @return		キーワードオブジェクト
     */
    private Keyword getKeyword() {

        Keyword keyword = new Keyword(KEYWORD_TYPE.KEYWORD);

        // 有効・無効
        keyword.setEnabled(this.chkEnabled.isSelected());
        // 名前
        keyword.setName(this.txtName.getText());
        // キーワード
        keyword.setKeyword(this.txtKeyword.getText());
        // フォント色
        keyword.setForecolor(this.btnColor.getColor());
        // スタイル
        // Bold
        boolean bold = this.chkBold.isSelected();
        // Italic
        boolean italic = this.chkItalic.isSelected();
        int style = Font.PLAIN;
        if (bold) style += Font.BOLD;
        if (italic) style += Font.ITALIC;
        keyword.setStyle(style);
        // 大文字・小文字の区別
        keyword.setSensitivecase(this.chkSensitivecase.isSelected());
        // 正規表現
        keyword.setRegex(this.chkRegex.isSelected());
        // キーワードロック
        keyword.setKeywordlock(!this.txtKeyword.isEnabled());

        return keyword;
    }

    /**
     * キーワードを更新する。
     * @param keyword		キーワード情報
     */
    private void setKeywordList(Keyword keyword) {

        // 選択行
        int selectedrow = this.tblKeyword.getSelectedRow();
        if (selectedrow < 0) {
        	JOptionPane.showMessageDialog(this,
        			Message.getString("settingkeyworddialog.informationdialog.update.message"), //リストから更新対象のキーワードを選択してください。
        			Message.getString("settingkeyworddialog.informationdialog.update.title"), //キーワードの更新
        			JOptionPane.INFORMATION_MESSAGE);
        	return;
        }

        // 行データの作成
        Object[] column = createKeywordRowData(keyword);
        // 行更新
        modelKeyword.removeRow(selectedrow);
        modelKeyword.insertRow(selectedrow, column);
        this.tblKeyword.setRowSelectionInterval(selectedrow, selectedrow);
    }

    /**
     * キーワードを追加する。
     * @param keyword		キーワード情報
     */
    private void addKeywordList(Keyword keyword) {
        // 行データの作成
        Object[] column = createKeywordRowData(keyword);
        // 行追加
        modelKeyword.addRow(column);
        int selectedrow = modelKeyword.getRowCount()-1;
        this.tblKeyword.setRowSelectionInterval(selectedrow, selectedrow);
    }


    /**
     * キーワードを削除する。
     * @param keyword		キーワード情報
     */
    private void removeKeywordList(Keyword keyword) {
        // 選択行
        int selectedrow = this.tblKeyword.getSelectedRow();
        if (selectedrow < 0) return;
        int option = JOptionPane.showConfirmDialog(this,
        		Message.getString("settingkeyworddialog.confirmdialog.delete.message"), //削除してもよろしいですか？
                Message.getString("settingkeyworddialog.confirmdialog.delete.title"),  //キーワードの削除
                JOptionPane.OK_CANCEL_OPTION);
        if (option == JOptionPane.OK_OPTION) {
        // 行削除
        modelKeyword.removeRow(selectedrow);

        // キーワード設定パネルをクリアする。
        clearKeyword();
        }
    }

    /**
     * キーワード設定パネルをクリアする。
     */
    private void clearKeyword() {

        // 設定をクリア
        this.chkEnabled.setSelected(true);
        /** キーワード名テキストボックス */
        this.txtName.setText(null);
        /** キーワードテキストボックス */
        this.txtKeyword.setText(null);
        /** フォント色ボタン */
        this.btnColor.setColor(null);
        /** イタリックチェックボックス */
        this.chkItalic.setSelected(false);
        /** ボイドチェックボックス */
        this.chkBold.setSelected(false);
        /** 大文字小文字区別チェックボックス */
        this.chkSensitivecase.setSelected(false);
        /** 正規表現チェックボックス */
        this.chkRegex.setSelected(false);

    }

    /**
     * 入力チェックを行う。
     * @param  addflag		追加フラグ(true=キーワード追加)
     */
    private boolean validateKeyword(boolean addflag) {
        // キーワードは必須
        String keyword = this.txtKeyword.getText();

        // 入力チェック
        if (keyword == null || keyword.isEmpty()) {
            JOptionPane.showMessageDialog(this,
            		Message.getString("settingkeyworddialog.errordialog.empty.message"), //キーワードを入力してください。
                    Message.getString("dialog.common.error"), //エラー
                                JOptionPane.ERROR_MESSAGE);
            return false;
        }

        // キーワードの重複チェックを行う。
        int selectedrow = this.tblKeyword.getSelectedRow();
        int count = this.modelKeyword.getRowCount();
        boolean exists = false;
        for (int i=0; i<count; i++) {
            // キーワード
            String cellKeyword = (String)(this.modelKeyword.getValueAt(i, 2));
            if (keyword.equals(cellKeyword)) {
                if (addflag) {
                    // 追加の場合は、同じキーワードは禁止
                    exists = true;
                    break;
                }
                else if (i != selectedrow) {
                    // 更新の場合は、更新行以外に同じキーワードが存在したらNG
                    exists = true;
                    break;
                }
            }
        }
        if (exists) {
            JOptionPane.showMessageDialog(this,
            		Message.getString("settingkeyworddialog.errordialog.exist.message"), //重複したキーワードは登録できません。
                    Message.getString("dialog.common.error"), //エラー
                    JOptionPane.ERROR_MESSAGE);
            return false;
        }

        return true;
    }

    /**
     * キーワードテーブルの色セルの描画クラス
     * @author RIKEN
     *
     */
    private class KeywordTableRenderer extends DefaultTableCellRenderer {
        /** シリアル番号 */
        private static final long serialVersionUID = 1L;

        /**
         * セルの描画コンポーネントを取得する
         * @param table			描画テーブル
         * @param value			セルデータ
         * @param isSelected	選択状態
         * @param hasFocus		フォーカス
         * @param row			行インデックス
         * @param column		列インデックス
         * @return		描画コンポーネント
         */
        @Override
        public Component getTableCellRendererComponent(JTable table, Object value,
                boolean isSelected, boolean hasFocus,
                int row, int column) {

            Object cell = null;;
            Object cellValue = value;
            Font font = table.getFont();
            java.awt.Color cellColor = isSelected?table.getSelectionBackground():table.getBackground();
            java.awt.Color foreColor = isSelected?table.getSelectionForeground():table.getForeground();
            if (value instanceof java.awt.Color) {
                cellValue = null;
                cellColor = (java.awt.Color)value;
            }
            if (column == 2) {
                java.awt.Color colorValue = (java.awt.Color)modelKeyword.getValueAt(row, 3);
                if (colorValue != null) foreColor = colorValue;

                // スタイル

                // Bold
                cell = modelKeyword.getValueAt(row, 4);
                boolean bold = false;
                if (((String)cell).indexOf("Bold") >= 0) {
                    bold = true;
                }
                // Italic
                boolean italic = false;
                if (((String)cell).indexOf("Italic") >= 0) {
                    italic = true;
                }
                int style = Font.PLAIN;
                if (bold) style += Font.BOLD;
                if (italic) style += Font.ITALIC;
                font = new Font(font.getName(), style, font.getSize());
            }


            super.getTableCellRendererComponent(table, cellValue, isSelected, hasFocus, row, column);
            this.setBackground(cellColor);
            this.setForeground(foreColor);
            this.setFont(font);

            return this;
        }
    }

}
