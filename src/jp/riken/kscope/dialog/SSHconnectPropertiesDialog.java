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
import java.awt.Frame;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Comparator;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.JViewport;
import javax.swing.RowFilter;
import javax.swing.RowSorter;
import javax.swing.RowSorter.SortKey;
import javax.swing.ScrollPaneConstants;
import javax.swing.SortOrder;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;
import javax.swing.table.TableRowSorter;

import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.properties.SSHconnectProperties;

public class SSHconnectPropertiesDialog  extends javax.swing.JDialog implements ActionListener  {

    private SSHconnectProperties sshproperties;
	
    private static final long serialVersionUID = -8218498915763496914L;
    /** キャンセルボタン */
    private JButton btnCancel;
    /** OKボタン */
    private JButton btnOk;
    /** 適用ボタン */
    private JButton btnApply;
    /** プロジェクト設定リスト */
    private JTable tblProperties;
    /** プロジェクト設定リストデータ */
    private DefaultTableModel modelProperties;
    /** 列名 */
    private final String[] COLUMN_HEADERS = {
        Message.getString("sshconnectsettingdialog.parameter.order"),
        Message.getString("sshconnectsettingdialog.parameter.name"),
        Message.getString("sshconnectsettingdialog.parameter.value"),
        Message.getString("sshconnectsettingdialog.parameter.description")
    };
    private String message = null;

    /** ダイアログの戻り値 */
    private int result = Constant.CANCEL_DIALOG;

    public SSHconnectPropertiesDialog(Frame frame, SSHconnectProperties settings) {
        super(frame);
        this.sshproperties = settings;
        initGUI();
    }
	
    /**
     * @wbp.parser.constructor
     */
    public SSHconnectPropertiesDialog(Frame frame, SSHconnectProperties settings, String message) {
        super(frame);
        this.sshproperties = settings;
        this.message = message;
        initGUI();
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

            // Message panel
            if (this.message != null) {
                JPanel panel_message = new JPanel();
                BorderLayout jPanelLayout = new BorderLayout(0, 0);
                panel_message.setLayout(jPanelLayout);
                getContentPane().add(panel_message, BorderLayout.NORTH);
                panel_message.setPreferredSize(new java.awt.Dimension(390, 30));

                JTextArea text = new JTextArea(message);
                text.setLineWrap(true);
                text.setWrapStyleWord(false);
                text.setOpaque(true);
                text.setEditable(false);
                text.setBackground(Color.white);
                text.setBorder(new EmptyBorder(3, 15, 3, 15));
                panel_message.add(text, BorderLayout.CENTER);
            }
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
                Border border = new EmptyBorder(7, 7, 0, 7);
                panelContent.setBorder(border);
                panelContent.setLayout(panelContentLayout);


                // Connection to data in SSHconnectProperties class (instance settings)
                modelProperties = new DefaultTableModel() {

                    private static final long serialVersionUID = -6996565435968749645L;

                    public int getColumnCount() {
                        return COLUMN_HEADERS.length;
                    }

                    public int getRowCount() {
                        return sshproperties.count();
                    }

                    public Object getValueAt(int row, int column) {
                        if (column > COLUMN_HEADERS.length) {
                            System.err.println("Table has " + COLUMN_HEADERS.length + " columns. You asked for column number" + column);
                            return null;
                        }

                        if (column == 0) {
                            return sshproperties.getOrder(row);
                        } else if (column == 1) {
                            return sshproperties.getKey(row);
                        } else if (column == 2) {
                            return sshproperties.getValue(row);
                        } else if (column == 3) {
                            return Message.getString(sshproperties.getDescription(row));
                        }
                        return null;
                    }

                    public boolean isCellEditable(int row, int column) {
                        if (column == 2) {
                            return true;
                        }
                        return false;
                    }

                    public void setValueAt(Object value, int row, int column) {
                        if (column > COLUMN_HEADERS.length) {
                            System.err.println("Table has " + COLUMN_HEADERS.length + " columns. You asked for column number" + column);
                            return;
                        }
                        if (column == 2) {
                            sshproperties.setValue(row, value.toString());
                        }
                        fireTableCellUpdated(row, column);
                    }
                };

                modelProperties.setColumnIdentifiers(COLUMN_HEADERS);
                tblProperties = new JTable(modelProperties);
                tblProperties.getColumnModel().getColumn(0).setMaxWidth(25);
                tblProperties.getColumnModel().getColumn(1).setMinWidth(120);
                tblProperties.getColumnModel().getColumn(1).setMaxWidth(140);
                tblProperties.getColumnModel().getColumn(2).setMinWidth(200);
                tblProperties.getColumnModel().getColumn(2).setMaxWidth(400);
                tblProperties.setRowMargin(5);
                tblProperties.setRowHeight(20);
                DefaultTableCellRenderer num_cell_renderer = new DefaultTableCellRenderer();
                num_cell_renderer.setHorizontalAlignment(JLabel.CENTER);
                num_cell_renderer.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 5));
                tblProperties.getColumnModel().getColumn(0).setCellRenderer(num_cell_renderer);

                TableRowSorter<TableModel> sorter;
                sorter = new TableRowSorter<TableModel>(modelProperties);

                String filter_expr = "^((?!" + SSHconnectProperties.BUILD_COMMAND + ").)*$";
                sorter.setRowFilter(RowFilter.regexFilter(filter_expr, 1));

                sorter.setComparator(0, new Comparator<Integer>() {
                    @Override
                    public int compare(Integer o1, Integer o2) {
                        return o1 - o2;
                    }
                });

                ArrayList<SortKey> list = new ArrayList<SortKey>();
                list.add(new RowSorter.SortKey(0, SortOrder.ASCENDING));
                sorter.setSortKeys(list);
                tblProperties.setRowSorter(sorter);

                JScrollPane scrollList = new JScrollPane(tblProperties);
                scrollList.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
                scrollList.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
                scrollList.setColumnHeader(new JViewport() {
                    @Override
                    public Dimension getPreferredSize() {
                        Dimension d = super.getPreferredSize();
                        d.height = 30;
                        return d;
                    }
                });
                panelContent.add(scrollList, BorderLayout.CENTER);
            }
            setTitle(Message.getString("sshconnectsettingdialog.title"));
            setSize(800, 350);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    /**
     * ダイアログを表示する。
     *
     * @return ダイアログの閉じた時のボタン種別
     */
    public int showDialog() {

        // 親フレーム中央に表示する。
        this.setLocationRelativeTo(this.getOwner());

        // ダイアログ表示
        this.setVisible(true);
        this.requestFocus();
        return this.result;
    }

    @Override
    public void actionPerformed(ActionEvent event) {
        // OK
        // 登録
        if (event.getSource() == this.btnOk) {
            this.result = Constant.OK_DIALOG;

            // 変更イベントを発生
            //this.sshproperties.firePropertyChange();

            // ダイアログを閉じる。
            dispose();
            return;
        }
        // 適用
        if (event.getSource() == this.btnApply) {
            this.result = Constant.OK_DIALOG;

            // 変更イベントを発生
            //this.sshproperties.firePropertyChange();

            return;
        } // 閉じる
        else if (event.getSource() == this.btnCancel) {
            this.result = Constant.CANCEL_DIALOG;
            // ダイアログを閉じる。
            dispose();
            return;
        }
    }
}
