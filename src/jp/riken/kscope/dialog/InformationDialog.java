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

import java.io.File;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.text.AbstractDocument;
import javax.swing.text.BadLocationException;
import javax.swing.text.BoxView;
import javax.swing.text.ComponentView;
import javax.swing.text.Element;
import javax.swing.text.IconView;
import javax.swing.text.LabelView;
import javax.swing.text.ParagraphView;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledEditorKit;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;
import javax.swing.border.EtchedBorder;
import javax.swing.BoxLayout;

import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.utils.FileUtils;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.utils.SwingUtils;


/**
 * 付加情報編集ダイアログ
 * @author riken
 *
 */
public class InformationDialog extends javax.swing.JDialog implements ActionListener {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;
    /** 付加情報エディトパイン */
    private JEditorPane editorInformation;
    /** OKボタン */
    private JButton btnOk;
    /** キャンセルボタン */
    private JButton btnCancel;
    /** 参照ボタン */
    private JButton btnRef;
    /** クリアボタン */
    private JButton btnClear;
    /** 削除ボタン */
    private JButton btnDelete;
    /** ダイアログの戻り値 */
    private int result = Constant.CANCEL_DIALOG;
    /** プロジェクトフォルダ */
    private File projectFolder;
    /** 付加情報 */
    private String information;
    /** カットボタン */
    private JButton btnCut;
    /** コピーボタン */
    private JButton btnCopy;
    /** ペーストボタン */
    private JButton btnPaste;
    /** 付加情報:ブロック */
    private JLabel labelBlock;
    /** 編集可否 */
    private boolean editable;

    /**
     * コンストラクタ
     * @param owner		親フレーム
     * @param modal		true=モーダルダイアログを表示する
     */
    public InformationDialog(Frame owner, boolean modal) {
        super(owner, modal);
        initGUI();
    }

    /**
     * GUI初期化を行う。
     */
    private void initGUI() {
        try {
            BorderLayout thisLayout = new BorderLayout();
            getContentPane().setLayout(thisLayout);

            {
                // ボタンパネル
                {
                    JPanel panelButtons = new JPanel();
                    FlowLayout layoutButtons = new FlowLayout();
                    layoutButtons.setHgap(10);
                    layoutButtons.setVgap(10);
                    panelButtons.setLayout(layoutButtons);
                    getContentPane().add(panelButtons, BorderLayout.SOUTH);
                    panelButtons.setPreferredSize(new java.awt.Dimension(390, 48));

                    java.awt.Dimension buttonSize = new java.awt.Dimension(112, 22);
                    {
                        btnOk = new JButton();
                        btnOk.setPreferredSize(buttonSize);
                        btnOk.setText(Message.getString("dialog.common.button.ok")); //OK
                        btnOk.addActionListener(this);
                        panelButtons.add(btnOk);
                    }
                    {
                        btnDelete = new JButton();
                        btnDelete.setPreferredSize(buttonSize);
                        btnDelete.setText(Message.getString("dialog.common.button.delete")); //削除
                        btnDelete.addActionListener(this);
                        panelButtons.add(btnDelete);
                    }
                    {
                        btnCancel = new JButton();
                        btnCancel.setPreferredSize(buttonSize);
                        btnCancel.setText(Message.getString("dialog.common.button.cancel")); //キャンセル
                        btnCancel.addActionListener(this);
                        panelButtons.add(btnCancel);
                    }
                }
                // 付加情報
                {
                    JPanel panelContent = new JPanel();
                    GridBagLayout panelContentLayout = new GridBagLayout();
                    panelContentLayout.columnWidths = new int[] {10, 64, 10, 10};
                    panelContentLayout.rowHeights = new int[] {32, 20, 10, 10};
                    panelContentLayout.columnWeights = new double[] {0.0, 0.0, 1.0, 0.0};
                    panelContentLayout.rowWeights = new double[] {0.0, 0.0, 1.0, 0.0};
                    getContentPane().add(panelContent, BorderLayout.CENTER);
                    panelContent.setLayout(panelContentLayout);
                    panelContent.setPreferredSize(new java.awt.Dimension(390, 230));
                    // ラベル
                    {
                        JLabel label = new JLabel();
                        panelContent.add(label, new GridBagConstraints(1, 0, 2, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                        label.setText(Message.getString("mainmenu.edit.info")); //付加情報編集
                    }
                    // 付加情報:ラベル
                    {
                        JLabel label = new JLabel();
                        panelContent.add(label, new GridBagConstraints(1, 1, 1, 1, 0.0, 0.0, GridBagConstraints.NORTHEAST, GridBagConstraints.NONE, new Insets(0, 0, 0, 7), 0, 0));
                        label.setText(Message.getString("informationdialog.label.information")); //付加情報
                    }
                    // 付加情報:ブロック
                    {
                        labelBlock = new JLabel();
                        panelContent.add(labelBlock, new GridBagConstraints(2, 1, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 7), 0, 0));
                        labelBlock.setText(Message.getString("informationdialog.label.block")); //BLOCK
                    }
                    // 付加情報テキストボックス
                    {
                        editorInformation = new JEditorPane();
                        editorInformation.setEditorKit(new NoWrapEditorKit());

                        JScrollPane scrollEditor = new JScrollPane(editorInformation);
                        // タブサイズ = 4
                        SwingUtils.setTabSize(editorInformation, 4);
                        editorInformation.putClientProperty(JEditorPane.HONOR_DISPLAY_PROPERTIES, Boolean.TRUE);
                        scrollEditor.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
                        scrollEditor.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
                        panelContent.add(scrollEditor, new GridBagConstraints(2, 2, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0));
                    }

                    // ボタンパネル
                    {
                        JPanel panelSubButtons = new JPanel();
                        //FlowLayout panelSubButtonsLayout = new FlowLayout();
                        //panelSubButtonsLayout.setAlignment(FlowLayout.LEADING);
                        //panelSubButtonsLayout.setHgap(10);
                        //panelSubButtonsLayout.setVgap(0);
                        //panelSubButtons.setLayout(panelSubButtonsLayout);
                        //panelContent.add(panelSubButtons, new GridBagConstraints(2, 0, 1, 1, 0.0, 0.0, GridBagConstraints.SOUTHEAST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));

                        panelSubButtons.setLayout(new BoxLayout(panelSubButtons, BoxLayout.X_AXIS));

                        EtchedBorder titleBorder = (EtchedBorder) BorderFactory.createEtchedBorder();
                        Border borderKeyword = new CompoundBorder(titleBorder, new EmptyBorder(0,0,0,0));
                        panelSubButtons.setBorder(borderKeyword);

                        getContentPane().add(panelSubButtons, BorderLayout.NORTH);

                        java.awt.Dimension minSize = new java.awt.Dimension(64, 64);
                        {
                            Icon icon = ResourceUtils.getIcon("button_copy.png");
                            btnCopy = new JButton(icon);
                            panelSubButtons.add(btnCopy);
                            btnCopy.setPreferredSize(minSize);
                            btnCopy.addActionListener(this);
                            btnCopy.setContentAreaFilled(false);
                            btnCopy.setBorderPainted(false);
                        }
                        {
                            Icon icon = ResourceUtils.getIcon("button_paste.png");
                            btnPaste = new JButton(icon);
                            panelSubButtons.add(btnPaste);
                            btnPaste.setPreferredSize(minSize);
                            btnPaste.addActionListener(this);
                            btnPaste.setContentAreaFilled(false);
                            btnPaste.setBorderPainted(false);
                        }
                        {
                            Icon icon = ResourceUtils.getIcon("button_cut.png");
                            btnCut = new JButton(icon);
                            panelSubButtons.add(btnCut);
                            btnCut.setPreferredSize(minSize);
                            btnCut.addActionListener(this);
                            btnCut.setContentAreaFilled(false);
                            btnCut.setBorderPainted(false);
                        }
                        {
                            Icon icon = ResourceUtils.getIcon("button_filepaste.png");
                            btnRef = new JButton(icon);
                            panelSubButtons.add(btnRef);
                            btnRef.setPreferredSize(minSize);
                            btnRef.addActionListener(this);
                            btnRef.setContentAreaFilled(false);
                            btnRef.setBorderPainted(false);
                        }
                        {
                            Icon icon = ResourceUtils.getIcon("button_clear.png");
                            btnClear = new JButton(icon);
                            panelSubButtons.add(btnClear);
                            btnClear.setPreferredSize(minSize);
                            btnClear.addActionListener(this);
                            btnClear.setContentAreaFilled(false);
                            btnClear.setBorderPainted(false);
                        }

                        // ツールチップ設定
                        btnRef.setToolTipText(Message.getString("informationdialog.button.filepaste.tooltip")); //ファイル貼り付け
                        btnCut.setToolTipText(Message.getString("informationdialog.button.cut.tooltip")); //切り取り
                        btnCopy.setToolTipText(Message.getString("informationdialog.button.copy.tooltip")); //コピー
                        btnPaste.setToolTipText(Message.getString("informationdialog.button.paste.tooltip")); //貼り付け
                        btnClear.setToolTipText(Message.getString("informationdialog.button.clear.tooltip")); //クリア
                    }
                }
            }
            this.setTitle(Message.getString("mainmenu.edit.info")); //付加情報編集
            this.setSize(640, 400);

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
     * ボタンクリックイベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {

        // 削除
        if (event.getSource() == this.btnDelete) {
            // 削除確認
            int option = JOptionPane.showConfirmDialog(this,
                    Message.getString("informationdialog.confirmdialog.delete.message"), //付加情報を削除してもよろしいですか？
                    Message.getString("informationdialog.confirmdialog.delete.title"), //削除確認
                    JOptionPane.OK_CANCEL_OPTION);
            if (option != JOptionPane.OK_OPTION) return;

            this.setInformation("");
            this.result = Constant.DELETE_DIALOG;

            // ダイアログを閉じる。
            dispose();
            return;
        }
        // OK
        else if (event.getSource() == this.btnOk) {
            // 付加情報の登録チェック
            if (validateInformation()) {
                this.setInformation(this.editorInformation.getText());
                this.result = Constant.OK_DIALOG;
                // ダイアログを閉じる。
                dispose();
                return;
            }
        }
        // キャンセル
        else if (event.getSource() == this.btnCancel) {
            this.result = Constant.CANCEL_DIALOG;
            // ダイアログを閉じる。
            dispose();
            return;
        }
        // 参照ボタン
        else if (event.getSource() == this.btnRef) {
            // ファイル選択ダイアログを表示する。
            File[] selected = SwingUtils.showOpenFileDialog(this,
            		Message.getString("informationdialog.selectfiledialog.title"), //ファイルの選択
                    projectFolder.getAbsolutePath(), null, true);
            if (selected == null || selected.length <= 0) return;

            try {
                StringBuffer buf = new StringBuffer();
                int offset = this.editorInformation.getCaretPosition();
                if (offset > 0) {
                    // キャレット位置が行途中ならば、改行を挿入する。
                    String prev_char = editorInformation.getDocument().getText(offset-1, 1);
                    if (!("\n".equals(prev_char))) {
                        buf.append("\n");
                    }
                }
                for (int i=0; i<selected.length; i++) {
                    // 現在キャレット位置にファイルパスを追加する
                    String path = FileUtils.getRelativePath(selected[i], this.projectFolder);
                    if (path == null) {
                    	JOptionPane.showMessageDialog(this,
                    			Message.getString("informationdialog.errordialog.notexist.message", selected[i]), // [file] は存在しません。
                    			Message.getString("dialog.common.error"), //エラー
                    			JOptionPane.ERROR_MESSAGE);
                    	continue;
                    }
                    buf.append(path);
                    buf.append("\n");
                }
                SimpleAttributeSet attr = new SimpleAttributeSet();
                editorInformation.getDocument().insertString(offset, buf.toString(), attr);
                this.editorInformation.setCaretPosition(offset+buf.length());

                // 付加情報テキストボックスにフォーカス移動
                this.editorInformation.requestFocus();

            } catch (BadLocationException ex) {
                ex.printStackTrace();
            }
        }
        // 切り取りボタン
        else if (event.getSource() == this.btnCut) {
            this.editorInformation.cut();
        }
        // コピーボタン
        else if (event.getSource() == this.btnCopy) {
            this.editorInformation.copy();
        }
        // 貼り付けボタン
        else if (event.getSource() == this.btnPaste) {
            this.editorInformation.paste();
        }
        // クリア
        else if (event.getSource() == this.btnClear) {
            // 情報をクリアする。
            this.editorInformation.setText("");
        }

        return;
    }

    /**
     * 入力チェックを行う。
     * @return        成否
     */
    private boolean validateInformation() {

        // 情報
        String content = this.editorInformation.getText();
        content = StringUtils.trim(content);
        if (content == null || content.isEmpty()) {
            JOptionPane.showMessageDialog(this,
            		Message.getString("informationdialog.errordialog.informationempty.message"), //付加情報を入力してください。
                    Message.getString("dialog.common.error"), //エラー
                                                JOptionPane.ERROR_MESSAGE);
            return false;
        }

        return true;
    }


    /**
     * プロジェクトフォルダを設定する
     * @param  folder  プロジェクトフォルダ
     */
    public void setProjectFolder(File folder) {
        this.projectFolder = folder;
    }

    /**
     * 付加情報を取得する
     * @return 		付加情報
     */
    public String getInformation() {
        return this.information;
    }

    /**
     * 付加情報ブロックを設定する。
     * @param block 	付加情報ブロック
     */
    public void setBlockName(String block) {
        this.labelBlock.setText(block);
    }

    /**
     * 付加情報を設定する。
     * @param information 	付加情報
     */
    public void setInformation(String information) {
        this.information = information;
        this.editorInformation.setText(information);
    }

    /**
     * 編集可否を取得する.
	 * @return 編集可否
	 */
	public boolean isEditable() {
		return editable;
	}

	/**
	 * 編集可否を設定する.
	 * @param editable    編集可否
	 */
	public void setEditable(boolean editable) {
		this.editable = editable;
		if (this.btnOk != null) {
			this.btnOk.setEnabled(this.editable);
		}
		if (this.btnDelete != null) {
			this.btnDelete.setEnabled(this.editable);
		}
	}

	/**
     * 行折り返し段落のビュークラス
     * @author riken
     */
    private class NoWrapParagraphView extends ParagraphView {
        /**
         * コンストラクタ
         * @param elem		このビューが扱う要素
         */
        public NoWrapParagraphView(Element elem) {
            super(elem);
        }

        /**
         * 行の幅のサイズ要件を計算します.<br/>
         * １行の折り返しサイズを設定する。
         * @param axis			行位置
         * @param r				コンポーネントのサイズと位置オブジェクト
         * @return				コンポーネントのサイズと位置オブジェクト
         */
        @Override
        protected SizeRequirements calculateMinorAxisRequirements(int axis, SizeRequirements r) {
            SizeRequirements req = super.calculateMinorAxisRequirements(axis, r);
            req.minimum = req.preferred;
            return req;
        }

        /**
         * 指定された子のインデックスに反してフローする制約スパンを取り出します。
         * @param index		照会されるビューのインデックス
         * @return			ビューの制約スパン
         */
        @Override
		public int getFlowSpan(int index) {
            return Integer.MAX_VALUE;
        }
    }

    /**
     * ビューの作成クラス
     * @author riken
     */
    class NoWrapViewFactory implements ViewFactory {
        /**
         * 要素に基づいてビューを作成します。
         * @param elem		作成対象要素
         * @return			ビュー
         */
        @Override
		public View create(Element elem) {
            String kind = elem.getName();
            if(kind != null) {
                if(kind.equals(AbstractDocument.ContentElementName)) {
                    return new LabelView(elem);
                }else if(kind.equals(AbstractDocument.ParagraphElementName)) {
                    return new NoWrapParagraphView(elem);
                }else if(kind.equals(AbstractDocument.SectionElementName)) {
                    return new BoxView(elem, View.Y_AXIS);
                }else if(kind.equals(StyleConstants.ComponentElementName)) {
                    return new ComponentView(elem);
                }else if(kind.equals(StyleConstants.IconElementName)) {
                    return new IconView(elem);
                }
            }
            return new LabelView(elem);
        }
    }

    /**
     * 書式付きテキストスタイル
     * @author riken
     */
    @SuppressWarnings("serial")
    class NoWrapEditorKit extends StyledEditorKit {

        /**
         * ビュー作成クラスを取得する。
         * @return		ビュー作成クラス
         */
        @Override
		public ViewFactory getViewFactory() {
            return new NoWrapViewFactory();
        }
    }


}
