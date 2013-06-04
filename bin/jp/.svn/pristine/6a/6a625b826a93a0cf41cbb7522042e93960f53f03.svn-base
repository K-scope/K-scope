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

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.properties.ProfilerProperties;
import jp.riken.kscope.utils.StringUtils;

/**
 * 詳細プロファイラ測定区間設定ダイアログクラス
 * @author riken
 */
public class EprofStatementDialog extends javax.swing.JDialog implements ActionListener {

    /** デフォルトシリアル番号 */
    private static final long serialVersionUID = 1L;
    /** OKボタン */
    private JButton btnOk;
    /** CANCELボタン */
    private JButton btnCancel;
    /** グループ名テキストボックス */
    private JTextField txtGroupname;
    /** 詳細番号テキストボックス */
    private JTextField txtNumber;
    /** プライオリティレベル */
    private JTextField txtLevel;
    /** プロファイラプロパティ */
    private ProfilerProperties properties;
    /** ダイアログの戻り値 */
    private int result = Constant.CANCEL_DIALOG;
    /** グループ名ラベル */
    private JLabel labelName;
    /** 詳細番号ラベル */
    private JLabel labelNumber;
    /** プライオリティラベル */
    private JLabel labelLevel;

    /**
     * コンストラクタ
     * @param owner		親フレーム
     * @param modal		true=モーダルダイアログを表示する
     */
    public EprofStatementDialog(Frame owner, boolean modal) {
        super(owner, modal);
        initGUI();
    }

    /**
     * 画面の初期化を行う
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
                        btnCancel = new JButton();
                        btnCancel.setPreferredSize(buttonSize);
                        btnCancel.setText(Message.getString("dialog.common.button.cancel")); //キャンセル
                        btnCancel.addActionListener(this);
                        panelButtons.add(btnCancel);
                    }
                }
                // プロファイラ
                {
                    JPanel panelContent = new JPanel();
                    GridBagLayout panelContentLayout = new GridBagLayout();
                    panelContentLayout.columnWidths = new int[] {120, 160};
                    panelContentLayout.rowHeights = new int[] {25, 25, 25};
                    panelContentLayout.columnWeights = new double[] {1.0, 1.0};
                    panelContentLayout.rowWeights = new double[] {1.0, 1.0, 1.0};
                    getContentPane().add(panelContent, BorderLayout.CENTER);
                    panelContent.setLayout(panelContentLayout);
                    panelContent.setPreferredSize(new java.awt.Dimension(390, 230));
                    // グループ名
                    {
                        labelName = new JLabel();
                        panelContent.add(labelName, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 0, 5), 0, 0));
                        labelName.setText(Message.getString("eprofstatementdialog.groupname.title")); //グループ名
                    }
                    {
                        txtGroupname = new JTextField();
                        panelContent.add(txtGroupname, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 5, 0, 10), 0, 0));
                    }
                    // 詳細番号
                    {
                        labelNumber = new JLabel();
                        panelContent.add(labelNumber, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 0, 5), 0, 0));
                        labelNumber.setText(Message.getString("eprofstatementdialog.detailnum.title")); //詳細番号
                    }
                    {
                        txtNumber = new JTextField();
                        panelContent.add(txtNumber, new GridBagConstraints(1, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 5, 0, 10), 0, 0));
                    }
                    // プライオリティレベル
                    {
                        labelLevel = new JLabel();
                        panelContent.add(labelLevel, new GridBagConstraints(0, 2, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 0, 5), 0, 0));
                        labelLevel.setText(Message.getString("eprofstatementdialog.priority.title")); //プライオリティレベル
                    }
                    {
                        txtLevel = new JTextField();
                        panelContent.add(txtLevel, new GridBagConstraints(1, 2, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 5, 0, 10), 0, 0));
                    }
                }
            }
            // テキストボックスのイネーブルの設定を行う。
            setEnabledText();

            this.setTitle(Message.getString("eprofstatementdialog.dialog.desc")); //測定区間設定
            this.setSize(320, 160);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * テキストボックスのイネーブルの設定を行う。
     */
    private void setEnabledText() {
        if (this.properties == null) return;

        // グループ名
        boolean enabledName = this.properties.existsMacroErofName();
        labelName.setEnabled(enabledName);
        txtGroupname.setEnabled(enabledName);

        // 詳細番号
        boolean enabledNumber = this.properties.existsMacroErofNumber();
        labelNumber.setEnabled(enabledNumber);
        txtNumber.setEnabled(enabledNumber);

        // プライオリティレベル
        boolean enabledLevel = this.properties.existsMacroErofNumber();
        labelLevel.setEnabled(enabledLevel);
        txtLevel.setEnabled(enabledLevel);
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
     * ボタンのクリックイベント
     * @param event			イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {
        // OK
        if (event.getSource() == this.btnOk) {
            // 測定区間設定チェック
            if (validateEprof()) {
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
    }

    /**
     * 測定区間設定の入力チェック
     * @return		true=正常
     */
    private boolean validateEprof() {
        String error = "";
        if (this.txtGroupname.isEnabled()) {
            if (this.txtGroupname.getText() == null || this.txtGroupname.getText().isEmpty()) {
                error += Message.getString("eprofstatementdialog.groupname.error.message"); //グループ名を入力してください。\n
            }
        }
        if (this.txtNumber.isEnabled()) {
            if (this.txtNumber.getText() == null || this.txtNumber.getText().isEmpty()) {
                error += Message.getString("eprofstatementdialog.detailnum.error.message"); //詳細番号を入力してください。\n
            }
//            else if (!StringUtils.isNumeric(this.txtNumber.getText())){
//                error += "詳細番号は数値で入力してください。\n";
//            }
        }
        if (this.txtLevel.isEnabled()) {
            if (this.txtLevel.getText() == null || this.txtLevel.getText().isEmpty()) {
                error += Message.getString("eprofstatementdialog.priority.error.message"); //プライオリティレベルを入力してください。\n
            }
//            else if (!StringUtils.isNumeric(this.txtLevel.getText())){
//                error += "プライオリティレベルは数値で入力してください。\n";
//            }
        }
        if (error != null && !error.isEmpty()) {
            JOptionPane.showMessageDialog(this, error.trim(), 
            		Message.getString("dialog.common.error"), //エラー
            		JOptionPane.ERROR_MESSAGE);
            return false;
        }

        return true;
    }

    /**
     * グループ名を取得する
     * @return		グループ名
     */
    public String getGroupname() {
        if (!this.txtGroupname.isEditable()) {
            return null;
        }
        return this.txtGroupname.getText();
    }

    /**
     * 詳細番号を取得する
     * @return		詳細番号
     */
    public String getNumber() {
        if (!this.txtNumber.isEnabled()) {
            return null;
        }
        return this.txtNumber.getText();
    }

    /**
     * プライオリティレベルを取得する
     * @return		プライオリティレベル
     */
    public String getLevel() {
        if (!this.txtLevel.isEnabled()) {
            return null;
        }
        return this.txtLevel.getText();
    }

    /**
     * プロファイラプロパティを設定する
     * @param properties プロファイラプロパティ
     */
    public void setProperties(ProfilerProperties properties) {
        this.properties = properties;
        // テキストボックスのイネーブルの設定を行う。
        setEnabledText();
    }
}
