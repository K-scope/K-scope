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
import java.awt.Font;
import java.awt.Frame;
import java.awt.GraphicsEnvironment;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ListModel;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;

/**
 * フォント選択ダイアログ
 * @author riken
 */
public class JFontChooserDialog extends javax.swing.JDialog implements ActionListener, ListSelectionListener, DocumentListener {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;
    /** フォントプレビュー */
    private JTextField txtPreview;
    /** フォントサイズリスト */
    private JList lstSize;
    /** フォントスタイルリスト */
    private JList lstStyle;
    /** フォントリスト */
    private JList lstFont;
    /** フォントサイズテキストボックス */
    private JTextField txtSize;
    /** キャンセルボタン */
    private JButton btnCancel;
    /** OKボタン */
    private JButton btnOk;

    /** ダイアログの戻り値 */
    private int result = Constant.CANCEL_DIALOG;

    /** フォントスタイル */
    private final String FONTSTYLE_PLAIN = "Plain";
    private final String FONTSTYLE_BOLD = "Bold";
    private final String FONTSTYLE_ITALIC = "Italic";
    private final String FONTSTYLE_BOLDITALIC = "Bold Italic";


    /**
     * コンストラクタ
     */
    public JFontChooserDialog() {
        super();
        initGUI();
        setDefualtFont(null);
    }


    /**
     * コンストラクタ
     * @param owner		親フレーム
     */
    public JFontChooserDialog(Frame owner) {
        super(owner);
        initGUI();
        setDefualtFont(null);
    }

    /**
     * コンストラクタ
     * @param owner		親ダイアログ
     * @param modal		true=モーダルダイアログを表示する
     */
    public JFontChooserDialog(Frame owner, boolean modal) {
        super(owner, modal);
        initGUI();
        setDefualtFont(null);
    }

    /**
     * コンストラクタ
     * @param owner		親ダイアログ
     * @param modal		true=モーダルダイアログを表示する
     * @param deffont		デフォルトフォント
     */
    public JFontChooserDialog(Frame owner, boolean modal, Font deffont) {
        super(owner, modal);
        initGUI();
        setDefualtFont(deffont);
    }

    /**
     * コンストラクタ
     * @param owner		親ダイアログ
     */
    public JFontChooserDialog(JDialog owner) {
        super(owner);
        initGUI();
        setDefualtFont(null);
    }

    /**
     * コンストラクタ
     * @param owner		親ダイアログ
     * @param modal		true=モーダルダイアログを表示する
     */
    public JFontChooserDialog(JDialog owner, boolean modal) {
        super(owner, modal);
        initGUI();
        setDefualtFont(null);
    }

    /**
     * コンストラクタ
     * @param owner		親ダイアログ
     * @param modal		true=モーダルダイアログを表示する
     * @param deffont		デフォルトフォント
     */
    public JFontChooserDialog(JDialog owner, boolean modal, Font deffont) {
        super(owner, modal);
        initGUI();
        setDefualtFont(deffont);
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
     * 選択されたフォントを返す.<br/>
     * キャンセルボタンがクリックされた場合は、nullを返す。
     * @return		選択フォント
     */
    public Font getSelectedFont() {
        if (this.result == Constant.CANCEL_DIALOG) return null;

        return this.txtPreview.getFont();
    }


    /**
     * GUI初期化を行う。
     */
    private void initGUI() {
        try {
            // ボタンパネル
            {
                JPanel panelButton = new JPanel();
                FlowLayout panelButtonLayout = new FlowLayout();
                panelButtonLayout.setHgap(10);
                panelButtonLayout.setVgap(10);
                panelButton.setLayout(panelButtonLayout);
                getContentPane().add(panelButton, BorderLayout.SOUTH);
                panelButton.setPreferredSize(new java.awt.Dimension(423, 44));

                // メインボタンサイズ
                java.awt.Dimension buttonSize = new java.awt.Dimension(96, 22);
                {
                    btnOk = new JButton();
                    panelButton.add(btnOk);
                    btnOk.setText(Message.getString("dialog.common.button.ok")); //OK
                    btnOk.setPreferredSize(buttonSize);
                    btnOk.addActionListener(this);
                }
                {
                    btnCancel = new JButton();
                    panelButton.add(btnCancel);
                    btnCancel.setText(Message.getString("dialog.common.button.cancel")); //キャンセル
                    btnCancel.setPreferredSize(buttonSize);
                    btnCancel.addActionListener(this);
                    btnCancel.setMargin(new Insets(5, 5, 5, 5));
                }
            }
            // コンテンツパネル
            {
                JPanel panelContent = new JPanel();
                GridBagLayout panelContentLayout = new GridBagLayout();
                panelContentLayout.rowWeights = new double[] {1.0, 0.3};
                panelContentLayout.rowHeights = new int[] {7, 64};
                panelContentLayout.columnWeights = new double[] {0.1};
                panelContentLayout.columnWidths = new int[] {7};
                panelContent.setLayout(panelContentLayout);
                getContentPane().add(panelContent, BorderLayout.CENTER);
                panelContent.setPreferredSize(new java.awt.Dimension(423, 144));
                // フォントパネル
                {
                    JPanel panelFont = new JPanel();
                    TitledBorder titleBorder = new TitledBorder(new  EtchedBorder(EtchedBorder.LOWERED), Message.getString("jfontchooserdialog.fontpanel.title")); //フォント
                    Border border = new CompoundBorder( new EmptyBorder(7,7,0,7), titleBorder);
                    panelFont.setBorder(border);

                    GridBagLayout panelFontLayout = new GridBagLayout();
                    panelFontLayout.columnWidths = new int[] {7, 96, 74};
                    panelFontLayout.rowHeights = new int[] {7, 7, 7};
                    panelFontLayout.columnWeights = new double[] {1.0, 0, 0};
                    panelFontLayout.rowWeights = new double[] {0.0, 0, 1.0};
                    panelContent.add(panelFont, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0));
                    panelFont.setLayout(panelFontLayout);
                    // フォント名
                    {
                        JLabel lblName = new JLabel();
                        panelFont.add(lblName, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 10, 0, 0), 0, 0));
                        lblName.setText(Message.getString("jfontchooserdialog.fontpanel.title")); //フォント
                    }
                    // フォントリスト
                    {
                        JScrollPane scrollFont = new JScrollPane();
                        panelFont.add(scrollFont, new GridBagConstraints(0, 1, 1, 2, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 10, 10, 0), 0, 0));
                        {
                            ListModel llstFontModel =
                                    new DefaultComboBoxModel(
                                            new String[] { "Item One", "Item Two" });
                            lstFont = new JList();
                            scrollFont.setViewportView(lstFont);
                            lstFont.setModel(llstFontModel);
                            lstFont.addListSelectionListener(this);
                            lstFont.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
                        }
                    }
                    // スタイル
                    {
                        JLabel lblStyle = new JLabel();
                        panelFont.add(lblStyle, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 10, 0, 0), 0, 0));
                        lblStyle.setText(Message.getString("jfontchooserdialog.fontpanel.label.style")); //スタイル
                    }
                    // スタイルリスト
                    {
                        JScrollPane scrollStyle = new JScrollPane();
                        panelFont.add(scrollStyle, new GridBagConstraints(1, 1, 1, 2, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 10, 10, 0), 0, 0));
                        {
                            ListModel lstStyleModel =
                                    new DefaultComboBoxModel(
                                            new String[] { FONTSTYLE_PLAIN, FONTSTYLE_BOLD, FONTSTYLE_ITALIC, FONTSTYLE_BOLDITALIC });
                            lstStyle = new JList();
                            scrollStyle.setViewportView(lstStyle);
                            lstStyle.setModel(lstStyleModel);
                            lstStyle.addListSelectionListener(this);
                            lstStyle.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
                        }
                    }
                    // サイズ
                    {
                        JLabel lblSize = new JLabel();
                        panelFont.add(lblSize, new GridBagConstraints(2, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 10, 0, 0), 0, 0));
                        lblSize.setText(Message.getString("jfontchooserdialog.fontpanel.label.size")); //サイズ
                    }
                    // サイズリスト
                    {
                        JScrollPane scrollSize = new JScrollPane();
                        panelFont.add(scrollSize, new GridBagConstraints(2, 2, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 10, 10, 10), 0, 0));
                        {
                            ListModel lstSizeModel =
                                    new DefaultComboBoxModel(
                                            new String[] { "8", "9", "10", "11", "12", "13", "14", "16", "18", "20", "22", "24", "26", "28", "32", "36", "40", "48", "56", "64", "72" });
                            lstSize = new JList();
                            scrollSize.setViewportView(lstSize);
                            lstSize.setModel(lstSizeModel);
                            lstSize.addListSelectionListener(this);
                            lstSize.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
                        }
                    }
                    // サイズテキストボックス
                    {
                        txtSize = new JTextField();
                        panelFont.add(txtSize, new GridBagConstraints(2, 1, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(0, 10, 0, 10), 0, 0));
                        txtSize.setText("10");
                        txtSize.getDocument().addDocumentListener(this);
                    }
                }
                // フォントプレビュー
                {
                    JPanel panelPreview = new JPanel();
                    TitledBorder titleBorder = new TitledBorder(new  EtchedBorder(EtchedBorder.LOWERED),
                                                                Message.getString("jfontchooserdialog.button.label.preview")); //プレビュー
                    Border margin = new EmptyBorder(0, 7, 7, 7);
                    Border inBorder = new CompoundBorder( titleBorder, margin);
                    Border border = new CompoundBorder( new EmptyBorder(7,7,0,7), inBorder);
                    panelPreview.setBorder(border);
                    Dimension previewSize = new Dimension(433, 80);
                    panelPreview.setPreferredSize(previewSize);
                    panelPreview.setMinimumSize(previewSize);

                    BorderLayout panelPreviewLayout = new BorderLayout();
                    panelContent.add(panelPreview, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0));
                    panelPreview.setLayout(panelPreviewLayout);
                    {
                        txtPreview = new JTextField();
                        panelPreview.add(txtPreview, BorderLayout.CENTER);
                        txtPreview.setText(Message.getString("jfontchooserdialog.button.label.sample"));
                    }
                }
            }
            this.setTitle(Message.getString("jfontchooserdialog.dialog.title")); //フォント選択
            this.setSize(433, 342);

            // フォント一覧を設定する
            setEnvironmentFontList();

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * フォント一覧を設定する。
     */
    private void setEnvironmentFontList() {
        // フォント名の取得
        GraphicsEnvironment env = GraphicsEnvironment.getLocalGraphicsEnvironment();
        String[] fontNames = env.getAvailableFontFamilyNames();

        // リストモデルの作成
        DefaultListModel model = new DefaultListModel();
        if (fontNames != null && fontNames.length > 0) {
            for(int i=0;i<fontNames.length;i++) {
                model.addElement(fontNames[i]);
            }
        }
        this.lstFont.setModel(model);
    }



    /**
     * デフォルトフォントを設定する
     * @param deffont		デフォルトフォント
     */
    private void setDefualtFont(Font deffont) {

        if (deffont == null) {
            deffont = this.txtPreview.getFont();
        }

        // フォント名
        this.lstFont.setSelectedValue(deffont.getFamily(), true);

        // スタイル
        int style = deffont.getStyle();
        if (style == Font.BOLD) {
            this.lstStyle.setSelectedValue(this.FONTSTYLE_BOLD, true);
        }
        else if (style == Font.ITALIC) {
            this.lstStyle.setSelectedValue(this.FONTSTYLE_ITALIC, true);
        }
        else if (style == Font.BOLD + Font.ITALIC) {
            this.lstStyle.setSelectedValue(this.FONTSTYLE_BOLDITALIC, true);
        }
        else {
            this.lstStyle.setSelectedValue(this.FONTSTYLE_PLAIN, true);
        }

        // サイズ
        int size = deffont.getSize();
        this.lstSize.setSelectedValue(String.valueOf(size), true);

    }


    /**
     * フォントをプレビューする。
     */
    private void previewFont() {
        // フォント名
        String name = (String)this.lstFont.getSelectedValue();
        if (name == null || name.isEmpty()) return;

        // スタイル
        String stylevalue = (String)this.lstStyle.getSelectedValue();
        int style = Font.PLAIN;
        if (stylevalue != null) {
            if (stylevalue.equalsIgnoreCase(this.FONTSTYLE_BOLD)) {
                style = Font.BOLD;
            }
            else if (stylevalue.equalsIgnoreCase(this.FONTSTYLE_ITALIC)) {
                style = Font.ITALIC;
            }
            else if (stylevalue.equalsIgnoreCase(this.FONTSTYLE_BOLDITALIC)) {
                style = Font.BOLD + Font.ITALIC;
            }
        }

        // サイズ
        int size = 0;
        String sizevalue = this.txtSize.getText();
        try {
            size = Integer.parseInt(sizevalue);
        } catch (Exception e) {

        }
        if (size ==0) return;

        // フォント設定
        Font font = new Font(name, style, size);
        this.txtPreview.setFont(font);

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
            // ダイアログを閉じる。
            dispose();
            return;
        }
        // キャンセル
        else if (event.getSource() == this.btnCancel) {
            this.result = Constant.CANCEL_DIALOG;
            // ダイアログを閉じる。
            dispose();
            return;
        }
    }


    /**
     * リストの変更イベント
     * @param event			イベント情報
     */
    @Override
    public void valueChanged(ListSelectionEvent event) {

        if (event.getSource() == this.lstSize) {
            String value = (String)this.lstSize.getSelectedValue();
            this.txtSize.setText(value);
        }

        // フォントをプレビューする。
        previewFont();
    }


    /**
     * サイズテキストボックスの挿入イベント
     * @param event		イベント情報
     */
    @Override
    public void insertUpdate(DocumentEvent event) { }


    /**
     * サイズテキストボックスの削除イベント
     * @param event		イベント情報
     */
    @Override
    public void removeUpdate(DocumentEvent event) { }


    /**
     * サイズテキストボックスの変更イベント
     * @param event		イベント情報
     */
    @Override
    public void changedUpdate(DocumentEvent event) {
        // フォントをプレビューする。
        previewFont();
    }

}
