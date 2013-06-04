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
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JCheckBox;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;

import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.common.EXPLORE_PANEL;

/**
 * 検索ダイアログクラス
 * @author riken
 */
public class SearchGrepDialog extends javax.swing.JDialog implements ActionListener {
    /** シリアル番号 */
    private static final long serialVersionUID = 1L;
    /** 検索ボタン */
    private JButton btnOk;
    /** 閉じるボタン */
    private JButton btnClose;
    /** 単語検索ボタン */
    private JCheckBox chkWord;
    /** 正規表現検索 */
    private JCheckBox chkRegex;
    /** 大文字・小文字区別なし検索 */
    private JCheckBox chkSensitivecase;
    /** 検索文字列 */
    private JTextField txtSearch;
    /** 検索フォルダ */
    private JTextField txtFolder;
    /** フォルダ参照ボタン */
    private JButton btnFolder;
    /** 参照ツリーモデル */
    private TreeModel modelTree;
    /** 選択ツリーパス */
    private TreeNode[] selectedNodes;

    /** ダイアログの戻り値 */
    private int result = Constant.CANCEL_DIALOG;

    /**
     * コンストラクタ
     * @param owner		親フレーム
     * @param modal		true=モーダルダイアログを表示する
     */
    public SearchGrepDialog(Frame owner, boolean modal) {
        super(owner, modal);
        initGUI();
    }

    /**
     * コンストラクタ
     * @param frame    親フレーム
     */
    public SearchGrepDialog(JFrame frame) {
        super(frame);
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
                FlowLayout panelButtonsLayout = new FlowLayout();
                panelButtonsLayout.setHgap(10);
                panelButtonsLayout.setVgap(10);
                panelButtons.setLayout(panelButtonsLayout);
                getContentPane().add(panelButtons, BorderLayout.SOUTH);
                panelButtons.setPreferredSize(new java.awt.Dimension(390, 41));

                // 検索ボタン
                java.awt.Dimension buttonSize = new java.awt.Dimension(96, 22);
                {
                    btnOk = new JButton();
                    btnOk.setText(Message.getString("mainmenu.search")); //検索 
                    btnOk.setPreferredSize(buttonSize);
                    btnOk.addActionListener(this);
                    panelButtons.add(btnOk);
                }
                // 閉じるボタン
                {
                    btnClose = new JButton();
                    btnClose.setText(Message.getString("dialog.common.button.cancel")); //キャンセル
                    btnClose.setMargin(new Insets(0, 5, 0, 5));
                    btnClose.setPreferredSize(buttonSize);
                    btnClose.addActionListener(this);
                    panelButtons.add(btnClose);
                }
            }

            // 検索コンテンツ
            {
                JPanel panelContent = new JPanel();
                GridBagLayout panelContentLayout = new GridBagLayout();
                panelContentLayout.columnWidths = new int[] {10, 100, 10, 10};
                panelContentLayout.rowHeights = new int[] {7, 7, 7, 7, 7, 7, 7};
                panelContentLayout.columnWeights = new double[] {0.0, 0.0, 1.0, 0.0};
                panelContentLayout.rowWeights = new double[] {0.0, 0.1, 0.1, 0.1, 0.1, 0.1, 0.0};
                getContentPane().add(panelContent, BorderLayout.CENTER);
                panelContent.setLayout(panelContentLayout);

                // 説明文ラベル
                {
                    JLabel label = new JLabel();
                    panelContent.add(label, new GridBagConstraints(1, 0, 2, 1, 0.0, 0.0, GridBagConstraints.NORTHWEST, GridBagConstraints.NONE, new Insets(0, 0, 7, 0), 0, 0));
                    label.setText(Message.getString("searchgrepdialog.label.desc")); //プロジェクトのソースファイルからテキスト検索を行います。
                    //label.setForeground(Color.BLUE);
                }
                // 検索ラベル
                {
                    JLabel lblSearch = new JLabel();
                    panelContent.add(lblSearch, new GridBagConstraints(1, 1, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                    lblSearch.setText(Message.getString("searchfinddialog.label.searchword")); //検索文字：
                }
                // 検索テキストボックス
                {
                    txtSearch = new JTextField();
                    panelContent.add(txtSearch, new GridBagConstraints(2, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
                }
                // オプションラベル
                {
                    JLabel lblOption = new JLabel();
                    panelContent.add(lblOption, new GridBagConstraints(1, 3, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                    lblOption.setText(Message.getString("searchfinddialog.label.options")); //オプション：
                }
                // オプション：大文字・小文字を区別する
                {
                    chkSensitivecase = new JCheckBox();
                    panelContent.add(chkSensitivecase, new GridBagConstraints(2, 3, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                    chkSensitivecase.setText(Message.getString("searchfinddialog.checkbox.upper-lower")); //大文字・小文字を区別する
                    chkSensitivecase.setMargin(new java.awt.Insets(2, 10, 2, 1));
                }
                // オプション：単語検索
                {
                    chkWord = new JCheckBox();
                    panelContent.add(chkWord, new GridBagConstraints(2, 4, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                    chkWord.setText(Message.getString("searchfinddialog.checkbox.word")); //単語検索
                    chkWord.setMargin(new java.awt.Insets(2, 10, 2, 1));
                }
                // オプション：正規表現
                {
                    chkRegex = new JCheckBox();
                    panelContent.add(chkRegex, new GridBagConstraints(2, 5, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                    chkRegex.setText(Message.getString("searchfinddialog.checkbox.regex")); //正規表現
                    chkRegex.setMargin(new java.awt.Insets(2, 10, 2, 1));
                }
                {
                    JLabel lblFolder = new JLabel();
                    panelContent.add(lblFolder, new GridBagConstraints(1, 2, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                    lblFolder.setText(Message.getString("searchgrepdialog.label.searchfolder")); //検索フォルダ：
                }
                {
                    JPanel panelFolder = new JPanel();
                    GridBagLayout layoutFolder = new GridBagLayout();
                    panelContent.add(panelFolder, new GridBagConstraints(2, 2, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0));
                    //panelFolder.setBorder(new LineBorder(new java.awt.Color(0,0,0), 1, false));
                    layoutFolder.rowWeights = new double[] {0.1};
                    layoutFolder.rowHeights = new int[] {7};
                    layoutFolder.columnWeights = new double[] {1.0, 0.0};
                    layoutFolder.columnWidths = new int[] {7, 7};
                    panelFolder.setLayout(layoutFolder);
                    {
                        txtFolder = new JTextField();
                        txtFolder.setEditable(false);
                        txtFolder.setText(Message.getString("searchgrepdialog.text.allfolders")); //すべてのフォルダ
                        panelFolder.add(txtFolder, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
                    }
                    {
                        btnFolder = new JButton();
                        panelFolder.add(btnFolder, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                        btnFolder.setText(Message.getString("dialog.common.button.refer")); //参照
                        btnFolder.setPreferredSize(new java.awt.Dimension(80, 22));
                        btnFolder.addActionListener(this);
                    }
                }
            }
            this.setTitle(Message.getString("mainmenu.search.file")); //ファイル検索
            this.setSize(435, 240);
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

        // 登録
        if (event.getSource() == this.btnOk) {
            // 入力チェック
            if (!validateSearch()) {
                // エラー
                return;
            }
            this.result = Constant.OK_DIALOG;
            // ダイアログを閉じる。
            dispose();
            return;
        }
        // 閉じる
        else if (event.getSource() == this.btnClose) {
            this.result = Constant.CANCEL_DIALOG;
            // ダイアログを閉じる。
            dispose();
            return;
        }
        // 参照
        else if (event.getSource() == this.btnFolder) {
            TreeChooserDialog dialog = new TreeChooserDialog(this, true, EXPLORE_PANEL.SOURCE, this.modelTree);
            dialog.setSelectedTreeNodes(this.selectedNodes);
            int result = dialog.showDialog();
            if (result == Constant.CANCEL_DIALOG) return;

            // 選択ツリーパス
            setSelectedTreeNodes(dialog.getSelectedTreeNodes());
            return;
        }
    }

    /**
     * 大文字・小文字の区別を取得する
     * @return		大文字・小文字の区別
     */
    public boolean isSensitivecase() {
        return this.chkSensitivecase.isSelected();
    }

    /**
     * 正規表現を取得する
     * @return		正規表現
     */
    public boolean isRegex() {
        return this.chkRegex.isSelected();
    }

    /**
     * 単語検索を取得する
     * @return		単語検索
     */
    public boolean isWord() {
        return this.chkWord.isSelected();
    }

    /**
     * 検索文字列を取得する
     * @return		検索文字列
     */
    public String getSearchText() {
        return this.txtSearch.getText();
    }

    /**
     * 検索文字列を設定する
     * @param  text		検索文字列
     */
    public void setSearchText(String text) {
        this.txtSearch.setText(text);
    }

    /**
     * 参照ツリーモデルを設定する
     * @param model		ツリーモデル
     */
    public void setReferenceTreeModel(TreeModel model) {
        this.modelTree = model;
    }

    /**
     * 選択ノードを取得する
     * @return 		選択ノード
     */
    public TreeNode[] getSelectedTreeNodes() {
        return this.selectedNodes;
    }

    /**
     * 選択ノードを設定する。
     * @param nodes 	選択ノード
     */
    public void setSelectedTreeNodes(TreeNode[] nodes) {
        this.selectedNodes = nodes;
        String selected = "";
        if (nodes != null) {
            for (int i=0; i<nodes.length; i++) {
                selected += nodes[i].toString();
                selected += ", ";
            }
            selected = selected.trim();
            selected = selected.substring(0, selected.length() - 1);
        }
        else {
            selected = Message.getString("searchgrepdialog.text.allfolders"); //すべてのフォルダ;
        }
        this.txtFolder.setText(selected);
    }

    /**
     * 入力チェックを行う
     * @return			true=入力チェックOK
     */
    private boolean validateSearch() {
        String text = this.txtSearch.getText();
        if (text != null) text = text.trim();
        if (text == null || text.isEmpty()) {
            JOptionPane.showMessageDialog(this,
                                          Message.getString("searchfinddialog.errordialog.empty.message"), //検索文字列を入力してください。
                                          Message.getString("dialog.common.error"), //エラー
                                          JOptionPane.ERROR_MESSAGE);
            return false;
        }

        return true;
    }
}
