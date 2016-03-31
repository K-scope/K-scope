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
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JColorChooser;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;

import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.component.JColorButton;
import jp.riken.kscope.data.PropertyValue;
import jp.riken.kscope.properties.SourceProperties;

/**
 * ソースビュー設定ダイアログ
 * @author RIKEN
 */
public class SettingViewDialog extends javax.swing.JDialog implements ActionListener, ListSelectionListener {
    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** キャンセルボタン */
    private JButton btnCancel;
    /** OKボタン */
    private JButton btnOk;
    /** 適用ボタン */
    private JButton btnApply;
    /** 登録ボタン */
    private JButton btnReg;
    /** ソースビュー設定リスト */
    private JTable tblProperties;
    /** ソースビュー設定リストデータ */
    private DefaultTableModel modelProperties;
    /** ソースビュー設定パネル */
    private JPanel panelProperty;

    /** ダイアログの戻り値 */
    private int result = Constant.CANCEL_DIALOG;

    /** ソースビュープロパティ */
    SourceProperties properities;

    /** 列名 */
    private final String[] COLUMN_HEADER = {
        Message.getString("settingprojectdialog.column_header.propertyvalue"), //PROPERTYVALUE
        Message.getString("settingprojectdialog.column_header.key"), //キー
        Message.getString("settingprojectdialog.column_header.type"), //タイプ
        Message.getString("settingprogramdialog.label.name"), //名前
        Message.getString("settingprojectdialog.column_header.value"), //値
        Message.getString("settingprojectdialog.column_header.message")}; //メッセージ

    /** フォント選択ボタン */
    private JButton btnFont;
    /** 色設定ボタン */
    private JColorButton btnColor;
    /** 色有効チェックボックス */
    private JCheckBox chkEnabled;
    /** 値設定テキストボックス */
    private JTextField txtValue;
    /** フォントラベル */
    private JLabel lblFontName;

    /** 選択プロパティ */
    private PropertyValue selectedvalue;


    /**
     * コンストラクタ
     * @param frame		親フレーム
     */
    public SettingViewDialog(Frame frame) {
        super(frame);
        initGUI();
    }

    /**
     * コンストラクタ
     * @param frame		親フレーム
     * @param modal		true=モーダルダイアログを表示する
     */
    public SettingViewDialog(Frame frame, boolean modal) {
        super(frame, modal);
        initGUI();
    }

    /**
     * コンストラクタ
     * @param frame		親フレーム
     * @param modal		true=モーダルダイアログを表示する
     * @param properities		ソース設定プロパティ
     */
    public SettingViewDialog(Frame frame, boolean modal, SourceProperties properities) {
        super(frame, modal);
        initGUI();
        setSourceProperties(properities);
    }

    /**
     * ソース設定を設定する。
     * @param properities		ソース設定プロパティ
     */
    public void setSourceProperties(SourceProperties properities) {

        PropertyValue[] values = properities.getPropertyValues();

        // テーブルに追加する
        for (PropertyValue value : values) {
            // "PropertyValue", "キー", "タイプ", "名前", "値"
            Object[] rowData = new Object[6];
            rowData[0] = value;
            rowData[1] = value.getKey();
            rowData[2] = value.getType();
            rowData[3] = Message.getString(value.getName());
            rowData[4] = value.getValue();
            rowData[5] = Message.getString(value.getMessage());

            this.modelProperties.addRow(rowData);
        }

        this.properities = properities;

        return;
    }


    /**
     * GUI初期化を行う。
     */
    private void initGUI() {
        try {
            BorderLayout thisLayout = new BorderLayout();
            thisLayout.setHgap(5);
            thisLayout.setVgap(5);
            getContentPane().setLayout(thisLayout);

            // ボタンパネル
            {
                JPanel panelButtons = new JPanel();
                FlowLayout jPanel1Layout = new FlowLayout();
                jPanel1Layout.setHgap(10);
                jPanel1Layout.setVgap(10);
                panelButtons.setLayout(jPanel1Layout);
                getContentPane().add(panelButtons, BorderLayout.SOUTH);
                panelButtons.setPreferredSize(new java.awt.Dimension(390, 46));

                // メインボタンサイズ
                java.awt.Dimension buttonSize = new java.awt.Dimension(96, 22);
                {
                    btnApply = new JButton();
                    btnApply.setText(Message.getString("dialog.common.button.apply")); //適用
                    btnApply.setPreferredSize(buttonSize);
                    btnApply.addActionListener(this);
                    panelButtons.add(btnApply);
                }
                {
                    btnOk = new JButton();
                    btnOk.setText(Message.getString("dialog.common.button.ok"));//OK
                    btnOk.setPreferredSize(buttonSize);
                    btnOk.addActionListener(this);
                    panelButtons.add(btnOk);
                }
                {
                    btnCancel = new JButton();
                    btnCancel.setText(Message.getString("dialog.common.button.cancel"));//キャンセル
                    btnCancel.setPreferredSize(buttonSize);
                    btnCancel.addActionListener(this);
                    btnCancel.setMargin(new Insets(5, 5, 5, 5));
                    panelButtons.add(btnCancel);
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

                // プロパティリスト
                {
                    JPanel panelList = new JPanel();
                    BorderLayout panelListLayout = new BorderLayout();
                    panelList.setLayout(panelListLayout);
                    panelContent.add(panelList, BorderLayout.CENTER);
                    {
                        JLabel lblList = new JLabel();
                        panelList.add(lblList, BorderLayout.NORTH);
                        lblList.setText(Message.getString("settingviewdialog.label.setupsourceviewlist"));//ソースビュー設定リスト
                    }
                    {
                        JScrollPane scrollList = new JScrollPane();
                        scrollList.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
                        scrollList.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
                        panelList.add(scrollList, BorderLayout.CENTER);
                        {
                            modelProperties = new DefaultTableModel();
                            modelProperties.setColumnCount(COLUMN_HEADER.length);
                            // ヘッダー列名
                            String[] columns = COLUMN_HEADER;
                            modelProperties.setColumnIdentifiers(columns);
                            tblProperties = new JTable();
                            scrollList.setViewportView(tblProperties);
                            tblProperties.setModel(modelProperties);
                            tblProperties.setSelectionMode(ListSelectionModel.SINGLE_SELECTION );
                            tblProperties.getSelectionModel().addListSelectionListener(this);
                            tblProperties.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
                            tblProperties.setDefaultRenderer(Object.class, new PropertiesTableRenderer());
                            tblProperties.setColumnSelectionAllowed(false);
                            tblProperties.setDefaultEditor(Object.class, null);

                            // 列幅設定
                            // 1列目:PropertyValue:非表示
                            {
                                TableColumn col = tblProperties.getColumnModel().getColumn(0);
                                col.setResizable(false);
                                col.setMinWidth(0);
                                col.setMaxWidth(0);
                            }
                            // 2列目:キー:非表示
                            {
                                TableColumn col = tblProperties.getColumnModel().getColumn(1);
                                col.setResizable(false);
                                col.setMinWidth(0);
                                col.setMaxWidth(0);
                            }
                            // 3列目:タイプ:非表示
                            {
                                TableColumn col = tblProperties.getColumnModel().getColumn(2);
                                col.setResizable(false);
                                col.setMinWidth(0);
                                col.setMaxWidth(0);
                            }
                            // 4列目:名前
                            {
                                TableColumn col = tblProperties.getColumnModel().getColumn(3);
                                col.setResizable(true);
                                col.setMinWidth(160);
                            }
                            // 5列目:値
                            {
                                TableColumn col = tblProperties.getColumnModel().getColumn(4);
                                col.setResizable(true);
                                col.setMinWidth(80);
                            }
                            // 6列目:メッセージ
                            {
                                TableColumn col = tblProperties.getColumnModel().getColumn(5);
                                col.setResizable(false);
                                col.setMinWidth(0);
                                col.setMaxWidth(0);
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
                        lblSettings.setText(Message.getString("mainmenu.project.config")); //設定
                        panelSettings.add(lblSettings, BorderLayout.NORTH);
                    }
                    this.panelProperty = new JPanel();
                    GridBagLayout panelPropertyLayout = new GridBagLayout();
                    panelPropertyLayout.columnWidths = new int[] {80, 100, 80};
                    panelPropertyLayout.rowHeights = new int[] {30, 30, 30, 3, 3, 3, 3};
                    panelPropertyLayout.columnWeights = new double[] {0.1, 0.1, 0.1};
                    panelPropertyLayout.rowWeights = new double[] {0, 0, 0, 0, 0, 0, 1};
                    panelSettings.add(this.panelProperty, BorderLayout.CENTER);
                    this.panelProperty.setLayout(panelPropertyLayout);
                    this.panelProperty.setPreferredSize(new java.awt.Dimension(320, 234));

                    // 設定パネル枠
                    EtchedBorder titleBorder = (EtchedBorder) BorderFactory.createEtchedBorder();
                    Border borderKeyword = new CompoundBorder( titleBorder, new EmptyBorder(17,7,0,7));
                    panelProperty.setBorder(borderKeyword);

                    // プロパティ名
                    {
                        JLabel lblName = new JLabel();
                        this.panelProperty.add(lblName, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                        lblName.setText(Message.getString("settingprogramdialog.label.name")); //名前
                    }
                    {
                        JLabel txtName = new JLabel();
                        this.panelProperty.add(txtName, new GridBagConstraints(1, 0, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
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
                        btnReg = new JButton();
                        panelAddButtons.add(btnReg);
                        btnReg.setText(Message.getString("dialog.common.button.update")); //更新
                        btnReg.setPreferredSize(minSize);
                        btnReg.setMargin(minInsets);
                        btnReg.addActionListener(this);
                    }
                }
            }
            setTitle(Message.getString("settingviewdialog.dialog.title")); //ソースビュー設定
            setSize(600, 300);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * プロパティ設定パネルを設定する
     * @param value		設定プロパティ
     */
    private void setPropertyPanel(PropertyValue value) {
        if (value == null) return;

        if ("font".equalsIgnoreCase(value.getType())) {
            // フォント設定
            setFontPanel(value);
        }
        else if ("color".equalsIgnoreCase(value.getType())) {
            // 色設定
            setColorPanel(value);
        }
        else if ("integer".equalsIgnoreCase(value.getType())) {
            // 値設定
            setValuePanel(value);
        }
        this.panelProperty.revalidate();
        this.panelProperty.repaint();
    }


    /**
     * フォント設定パネルを設定する
     * @param value		フォント設定プロパティ
     */
    private void setFontPanel(PropertyValue value) {

        if (!("font".equalsIgnoreCase(value.getType()))) return;

        // 設定フォント
        Font font = (Font)value.getValue();
        this.panelProperty.removeAll();

        // プロパティ名
        {
            JLabel label = new JLabel();
            this.panelProperty.add(label, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
            label.setText(Message.getString("settingprogramdialog.label.name"));//名前
        }
        {
            JLabel lblName = new JLabel();
            this.panelProperty.add(lblName, new GridBagConstraints(1, 0, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
            lblName.setText(value.getName());
        }

        // フォント
        {
            JLabel lblFont = new JLabel();
            this.panelProperty.add(lblFont, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
            lblFont.setText(Message.getString("jfontchooserdialog.fontpanel.title"));//フォント
        }
        {
            lblFontName = new JLabel();
            this.panelProperty.add(lblFontName, new GridBagConstraints(1, 1, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
            setLabelFont(font, lblFontName);
            lblFontName.setVerticalAlignment(JLabel.TOP);
        }
        {
            Dimension sizeButton = new Dimension(46, 22);
            btnFont = new JButton();
            this.panelProperty.add(btnFont, new GridBagConstraints(1, 2, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
            btnFont.setText(Message.getString("settingviewdialog.label.select"));//選択
            btnFont.setMargin(new Insets(5, 5, 5, 5));
            btnFont.addActionListener(this);
            btnFont.setMaximumSize(sizeButton);
            btnFont.setMinimumSize(sizeButton);
            btnFont.setPreferredSize(sizeButton);
//            btnFont.setSize(sizeButton);
        }

        // メッセージ
        {
            JComponent lblMassage = createMessageLabel(value.getMessage());
            if (lblMassage != null) {
                this.panelProperty.add(lblMassage, new GridBagConstraints(1, 2, 2, 2, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0));
            }
        }
    }

    /**
     * 色設定パネルを設定する
     * @param value		色設定プロパティ
     */
    private void setColorPanel(PropertyValue value) {

        if (!("color".equalsIgnoreCase(value.getType()))) return;

        // 設定色
        Color color = (Color)value.getValue();

        this.panelProperty.removeAll();

        // プロパティ名
        {
            JLabel label = new JLabel();
            this.panelProperty.add(label, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
            label.setText(Message.getString("settingprogramdialog.label.name"));//名前
        }
        {
            JLabel lblName = new JLabel();
            this.panelProperty.add(lblName, new GridBagConstraints(1, 0, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
            lblName.setText(value.getName());
        }


        // 色
        {
            JLabel lblColor = new JLabel();
            this.panelProperty.add(lblColor, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
            lblColor.setText(Message.getString("settingviewdialog.label.color"));//色設定
        }
        {
            btnColor = new JColorButton();
            this.panelProperty.add(btnColor, new GridBagConstraints(1, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
            btnColor.addActionListener(this);
            btnColor.setColor(color);
        }
        {
            chkEnabled = new JCheckBox();
            this.panelProperty.add(chkEnabled, new GridBagConstraints(2, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
            chkEnabled.setText(Message.getString("settingkeyworddialog.checkbox.enable"));//有効
            chkEnabled.addActionListener(this);
        }

        // メッセージ
        {
            JComponent lblMassage = createMessageLabel(value.getMessage());
            if (lblMassage != null) {
                this.panelProperty.add(lblMassage, new GridBagConstraints(1, 2, 2, 2, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0));
            }
        }

        // イネーブル設定
        boolean enabled = (color!=null);
        this.btnColor.setEnabled(enabled);
        chkEnabled.setSelected(enabled);
    }


    /**
     * 値設定パネルを設定する
     * @param value		値設定プロパティ
     */
    private void setValuePanel(PropertyValue value) {

        if (!("integer".equalsIgnoreCase(value.getType()))) return;

        // 設定値
        Integer intValue = (Integer)value.getValue();
        this.panelProperty.removeAll();

        // プロパティ名
        {
            JLabel label = new JLabel();
            this.panelProperty.add(label, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
            label.setText(Message.getString("settingprogramdialog.label.name"));//名前
        }
        {
            JLabel lblName = new JLabel();
            this.panelProperty.add(lblName, new GridBagConstraints(1, 0, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
            lblName.setText(value.getName());
        }

        // 値
        {
            JLabel lblColor = new JLabel();
            this.panelProperty.add(lblColor, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
            lblColor.setText(Message.getString("settingprojectdialog.column_header.value"));//値
        }
        {
            txtValue = new JTextField();
            this.panelProperty.add(txtValue, new GridBagConstraints(1, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
            txtValue.setText(String.valueOf(intValue));
            txtValue.setHorizontalAlignment(JTextField.RIGHT);
        }
        // メッセージ
        {
            JComponent lblMassage = createMessageLabel(value.getMessage());
            if (lblMassage != null) {
                this.panelProperty.add(lblMassage, new GridBagConstraints(1, 2, 2, 2, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0));
            }
        }
    }

    /**
     * メッセージラベルコンポーネントを作成する
     * @param  message   表示メッセージ
     * @return			メッセージラベルコンポーネント
     */
    private JComponent createMessageLabel(String  message) {
        if (message == null) return null;

        JTextPane lblMassage = new JTextPane();
        SimpleAttributeSet attr = new SimpleAttributeSet();
        StyleConstants.setLineSpacing(attr, -0.2f);
        lblMassage.setParagraphAttributes(attr, true);
        lblMassage.setForeground(UIManager.getColor("Label.foreground"));
        lblMassage.setOpaque(false);
        lblMassage.setEditable(false);
        lblMassage.setFocusable(false);
        lblMassage.setText(message);

        return lblMassage;
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
     * ボタンクリックイベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {

        // 登録
        if (event.getSource() == this.btnOk) {
            this.result = Constant.OK_DIALOG;

            // 変更内容をソースプロパティに更新する。
            setProperties();

            // 変更イベントを発生
            this.properities.firePropertyChange();

            // ダイアログを閉じる。
            dispose();
            return;
        }
        // 適用
        if (event.getSource() == this.btnApply) {
            this.result = Constant.OK_DIALOG;

            // 変更内容をソースプロパティに更新する。
            setProperties();

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
            JColorButton button = (JColorButton) event.getSource();
            // 色選択ダイアログ
            Color color = JColorChooser.showDialog(this,
                    Message.getString("settingkeyworddialog.colorchooser.title"), //色の選択
                    button.getColor());
            if(color != null){
                // ボタンにカラーを設定する
                button.setColor(color);
            }

            return;
        }
        // ソース表示フォント
        else if (event.getSource() == this.btnFont ) {
            Font deffont = this.lblFontName.getFont();
            JFontChooserDialog dialog = new JFontChooserDialog(this, true, deffont);
            int result = dialog.showDialog();
            if (result == Constant.OK_DIALOG) {
                Font font = dialog.getSelectedFont();
                if (font != null) {
                    setLabelFont(font, this.lblFontName);
                }
            }
        }
        // 更新
        else if (event.getSource() == this.btnReg ) {
            // 設定値をテーブルに設定する
            // 選択行を取得する。
            int selectedrow = this.tblProperties.getSelectedRow();
            if (selectedrow < 0) return;
            if (selectedvalue == null) return;

            int col = 4;

            if ("font".equalsIgnoreCase(selectedvalue.getType())) {
                // フォント設定
                this.modelProperties.setValueAt(this.lblFontName.getFont() ,selectedrow, col);
            }
            else if ("color".equalsIgnoreCase(selectedvalue.getType())) {
                // 色設定
                if (this.chkEnabled.isSelected()) {
                    this.modelProperties.setValueAt(this.btnColor.getColor(), selectedrow, col);
                }
                else {
                    this.modelProperties.setValueAt(null, selectedrow, col);
                }
            }
            else if ("integer".equalsIgnoreCase(selectedvalue.getType())) {
                // 値設定
                try {
                    Integer value = Integer.parseInt(this.txtValue.getText());
                    if (value < 0) {
                    	JOptionPane.showMessageDialog(this,
                    			Message.getString("settingviewdialog.infodialog.word-erapping.message"), //折り返し文字数には０以上の値を設定してください。
                    			Message.getString("settingviewdialog.dialog.title"), //ソースビュー設定
                    			JOptionPane.INFORMATION_MESSAGE);
                    	return;
                    }
                    this.modelProperties.setValueAt(value ,selectedrow, col);
                } catch (NumberFormatException e) { }
            }
        }
        // 有効チェック
        else if (event.getSource() == this.chkEnabled ) {
            boolean enabled = this.chkEnabled.isSelected();
            this.btnColor.setEnabled(enabled);
        }
    }

    /**
     * フォントラベルを設定する
     * @param font		設定フォント
     * @param label		設定ラベル
     */
    private void setLabelFont(Font font, JLabel label) {
        if (font == null) return;
        if (label == null) return;
        label.setFont(font);
        String name = toStringFont(font);
        label.setText(name);
        return;
    }

    /**
     * フォントの文字列表現を取得する
     * @param font		設定フォント
     * @return   フォントの文字列表現
     */
    private String toStringFont(Font font) {
    	if (font == null) return "";
        String name = font.getName();
        name += " " + font.getSize() + " ";
        int style = font.getStyle();
        if ((style & Font.PLAIN) != 0 ) {
            name += "PLAIN ";
        }
        if ((style & Font.ITALIC) != 0 ) {
            name += "ITALIC ";
        }
        if ((style & Font.BOLD) != 0 ) {
            name += "BOLD ";
        }

        return name;
    }

    /**
     * ソースビュープロパティを設定する。
     */
    private void setProperties() {

        int rows = this.modelProperties.getRowCount();

        // "PropertyValue", "キー", "タイプ", "名前", "値", "メッセージ"}
        for (int i=0; i<rows; i++) {
            PropertyValue value = (PropertyValue) this.modelProperties.getValueAt(i, 0);
            value.setValue(this.modelProperties.getValueAt(i, 4));
        }

    }


    /**
     * 選択プロパティを表示する
     * @param event		イベント情報
     */
    @Override
    public void valueChanged(ListSelectionEvent event) {

        if (event.getSource() == this.tblProperties.getSelectionModel()) {
            // 選択行を取得する。
            int selectedrow = this.tblProperties.getSelectedRow();
            if (selectedrow < 0) return;

            // "PropertyValue", "キー", "タイプ", "名前", "値", "メッセージ"
            PropertyValue value = new PropertyValue(
                        (String)this.modelProperties.getValueAt(selectedrow, 1),
                        (String)this.modelProperties.getValueAt(selectedrow, 3),
                        (String)this.modelProperties.getValueAt(selectedrow, 2),
                        this.modelProperties.getValueAt(selectedrow, 4),
                        (String)this.modelProperties.getValueAt(selectedrow, 5)
                    );
            // 選択プロパティ
            this.selectedvalue = value;

            // 設定パネルに表示する
            this.setPropertyPanel(value);
        }
    }


    /**
     * プロパティテーブルの色セルの描画クラス
     * @author RIKEN
     *
     */
    private class PropertiesTableRenderer extends DefaultTableCellRenderer {
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

            Object cellValue = value;
            Font font = table.getFont();
            java.awt.Color cellColor = isSelected?table.getSelectionBackground():table.getBackground();
            java.awt.Color foreColor = isSelected?table.getSelectionForeground():table.getForeground();
            if (value instanceof java.awt.Color) {
                cellValue = null;
                cellColor = (java.awt.Color)value;
            }
            else if (value instanceof java.awt.Font) {
                // フォント名
                cellValue = toStringFont((Font)value);
                font = (Font)value;
            }

            super.getTableCellRendererComponent(table, cellValue, isSelected, hasFocus, row, column);
            this.setBackground(cellColor);
            this.setForeground(foreColor);
            this.setFont(font);

            return this;
        }
    }

}

