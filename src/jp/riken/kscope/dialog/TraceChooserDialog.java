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
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;

import jp.riken.kscope.Message;
import jp.riken.kscope.action.ViewOpenAnalysisLineAction;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.component.JStripeTable;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.model.TraceResultModel;
import jp.riken.kscope.utils.ResourceUtils;


/**
 * トレース選択ダイアログ
 * @author riken
 *
 */
public class TraceChooserDialog extends javax.swing.JDialog implements ActionListener, MouseListener {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;
    /** トレーステーブル */
    private JTable tableTrace;
    /** OKボタン */
    private JButton btnOk;
    /** キャンセルボタン */
    private JButton btnCancel;
    /** ファイルを開く */
    private JButton btnOpenFile;
    /** 該当個所を開くアクション */
    ViewOpenAnalysisLineAction actionOpen;

    /** ダイアログの戻り値 */
    private int result = Constant.CANCEL_DIALOG;

    /**
     * コンストラクタ
     * @param owner		親フレーム
     * @param modal		true=モーダルダイアログを表示する
     */
    public TraceChooserDialog(Frame owner, boolean modal) {
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
                        btnCancel = new JButton();
                        btnCancel.setPreferredSize(buttonSize);
                        btnCancel.setText(Message.getString("dialog.common.button.cancel")); //キャンセル
                        btnCancel.addActionListener(this);
                        panelButtons.add(btnCancel);
                    }
                }
                // トレース
                {
                    JPanel panelContent = new JPanel();
                    GridBagLayout panelContentLayout = new GridBagLayout();
                    panelContentLayout.columnWidths = new int[] {10, 64, 10, 10};
                    panelContentLayout.rowHeights = new int[] {32, 32, 10, 10};
                    panelContentLayout.columnWeights = new double[] {0.0, 0.0, 1.0, 0.0};
                    panelContentLayout.rowWeights = new double[] {0.0, 0.0, 1.0, 0.0};
                    getContentPane().add(panelContent, BorderLayout.CENTER);
                    panelContent.setLayout(panelContentLayout);
                    panelContent.setPreferredSize(new java.awt.Dimension(390, 230));
                    // ラベル
                    {
                        JLabel label = new JLabel();
                        panelContent.add(label, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
                        label.setText(Message.getString("tracechooserdialog.label.next")); //トレース先選択
                    }

                    // トレース先テーブル
                    {
                        tableTrace = new JStripeTable();
                        String[] HEADER_COLUMNS = {"", Message.getString("settingprojectdialog.column_header.message")}; //メッセージ

                        DefaultTableModel tableModel = new DefaultTableModel();
                        tableModel.setColumnIdentifiers(HEADER_COLUMNS);
                        tableTrace.setModel(tableModel);
                        JScrollPane scrollTrace = new JScrollPane(tableTrace);
                        tableTrace.setAutoCreateColumnsFromModel(false);
                        tableTrace.setSelectionMode(ListSelectionModel.SINGLE_SELECTION );
                        tableTrace.setColumnSelectionAllowed(false);

                        // テーブル列モデル
                        DefaultTableColumnModel columnModel = (DefaultTableColumnModel)tableTrace.getColumnModel();
                        TableColumn column = null;

                        // 1列目はモデル情報：非表示
                        column = columnModel.getColumn(0);
                        column.setPreferredWidth(0);
                        column.setMinWidth(0);
                        column.setMaxWidth(0);
                        column.setResizable(false);

                        // ステータス列
                        column = columnModel.getColumn(1);
                        column.setPreferredWidth(445);
                        column.setMinWidth(160);

                        scrollTrace.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
                        scrollTrace.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
                        panelContent.add(scrollTrace, new GridBagConstraints(1, 1, 2, 2, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 0, 0, 0), 0, 0));
                    }
                    // ボタンパネル
                    {
                        JPanel panelSubButtons = new JPanel();
                        FlowLayout panelSubButtonsLayout = new FlowLayout();
                        panelSubButtonsLayout.setAlignment(FlowLayout.RIGHT);
                        panelSubButtonsLayout.setHgap(10);
                        panelSubButtonsLayout.setVgap(0);
                        panelSubButtons.setLayout(panelSubButtonsLayout);
                        panelContent.add(panelSubButtons, new GridBagConstraints(2, 0, 1, 1, 0.0, 0.0, GridBagConstraints.SOUTHEAST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));

                        java.awt.Dimension minSize = new java.awt.Dimension(18, 18);
                        {
                            Icon icon = ResourceUtils.getIcon("openfile.gif");
                            btnOpenFile = new JButton(icon);
                            panelSubButtons.add(btnOpenFile);
                            btnOpenFile.setPreferredSize(minSize);
                            btnOpenFile.addActionListener(this);
                            btnOpenFile.setContentAreaFilled(false);
                            btnOpenFile.setBorderPainted(false);
                        }

                        // ツールチップ設定
                        btnOpenFile.setToolTipText(Message.getString("tracechooserdialog.tooltip.open")); //該当個所を開く

                        // イベント登録
                        btnOpenFile.addActionListener(this);
                        tableTrace.addMouseListener(this);
                    }
                }
            }
            this.setTitle(Message.getString("tracechooserdialog.label.next")); //トレース先選択
            this.setSize(480, 280);

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
        // 該当箇所を開く
        else if (event.getSource() == this.btnOpenFile) {
            // 該当個所を開く
            viewSelectedBlock();

            return;
        }

        return;
    }

    /**
     * トレース先モデルを取得する
     * @return 		トレース先モデル
     */
    public TraceResultModel getTraceResultModel() {

        int selectedrow = this.tableTrace.getSelectedRow();
        if (selectedrow < 0) return null;
        Object cell = this.tableTrace.getModel().getValueAt(selectedrow, 0);
        if (cell == null) return null;
        if (cell instanceof TraceResultModel) {
            return (TraceResultModel)cell;
        }
        return null;
    }

    /**
     * レース先モデルを設定する。
     * @param traces 	レース先モデル
     */
    public void setTraceResultModel(TraceResultModel[] traces) {
        if (traces == null) return;

        DefaultTableModel tableModel = new DefaultTableModel();

        String[] HEADER_COLUMNS = {Message.getString("tracechooserdialog.header_column.codeline"), //CODELINE
                                   Message.getString("settingprojectdialog.column_header.message")}; //メッセージ
        tableModel.setColumnIdentifiers(HEADER_COLUMNS);
        for (TraceResultModel model : traces) {
            Object[] rows = new Object[2];
            rows[0] = model;
            rows[1] = model.getBlocklabel();
            tableModel.addRow(rows);
        }
        this.tableTrace.setModel(tableModel);

        // 先頭行を選択状態とする
        this.tableTrace.setRowSelectionInterval(0, 0);
    }

    /**
     * 選択箇所を開く
     */
    private void viewSelectedBlock() {
        if (this.actionOpen == null) return;

        TraceResultModel selectedModel = getTraceResultModel();
        IBlock block = selectedModel.getSelectedBlock();

        // 該当箇所を開く
        this.actionOpen.viewSelectedBlock(block);

        return;
    }


    /**
     * マウスクリックイベント
     * @param event		マウスイベント情報
     */
    @Override
    public void mouseClicked(MouseEvent event) {
        // ダブルクリックチェック
        if (SwingUtilities.isLeftMouseButton(event) && event.getClickCount() == 2) {
            if (event.getSource() == this.tableTrace) {
                // 該当個所を開く
                viewSelectedBlock();
            }
        }
    }


    /**
     * マウスボタンダウンイベント
     * @param e		マウスイベント情報
     */
    @Override
    public void mousePressed(MouseEvent e) { }

    /**
     * マウスボタンアップイベント
     * @param e		マウスイベント情報
     */
    @Override
    public void mouseReleased(MouseEvent e) {}

    /**
     * マウスオーバーイベント
     * @param e		マウスイベント情報
     */
    @Override
    public void mouseEntered(MouseEvent e) {}

    /**
     * マウスアウトイベント
     * @param e		マウスイベント情報
     */
    @Override
    public void mouseExited(MouseEvent e) {}


    /**
     * 該当個所を開くアクションを設定する。
     * @param action		該当個所を開くアクション
     */
    public void setViewOpenAnalysisLineAction(ViewOpenAnalysisLineAction action) {
        this.actionOpen = action;
    }

}
