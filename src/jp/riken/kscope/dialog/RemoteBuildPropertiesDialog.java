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
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableModel;
import javax.swing.table.TableRowSorter;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.properties.ProjectProperties;
import jp.riken.kscope.properties.RemoteBuildProperties;

public class RemoteBuildPropertiesDialog extends javax.swing.JDialog implements ActionListener {

  private RemoteBuildProperties rb_properties;

  private static final long serialVersionUID = -8218498915763496914L;
  /** Cancel button */
  private JButton btnCancel;
  /** OK button */
  private JButton btnOk;
  /** Apply button */
  private JButton btnApply;
  /** Project setting list */
  private JTable tblProperties;
  /** Project setting list data */
  private DefaultTableModel modelProperties;
  /** Column name */
  private final String[] COLUMN_HEADERS = {
    Message.getString("remotebuildsettingdialog.parameter.order"),
    Message.getString("remotebuildsettingdialog.parameter.name"),
    Message.getString("remotebuildsettingdialog.parameter.value"),
    Message.getString("remotebuildsettingdialog.parameter.description")
  };

  private String message = null;

  /** Dialog return value */
  private int result = Constant.CANCEL_DIALOG;

  public RemoteBuildPropertiesDialog(Frame frame, RemoteBuildProperties settings) {
    super(frame);
    this.rb_properties = settings;
    initGUI();
  }

  /** @wbp.parser.constructor */
  public RemoteBuildPropertiesDialog(Frame frame, RemoteBuildProperties settings, String message) {
    super(frame);
    this.rb_properties = settings;
    this.message = message;
    initGUI();
  }

  /** Initialize the GUI. */
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
      // Button panel
      {
        JPanel panelButtons = new JPanel();
        FlowLayout jPanel1Layout = new FlowLayout();
        jPanel1Layout.setHgap(10);
        jPanel1Layout.setVgap(10);
        panelButtons.setLayout(jPanel1Layout);
        getContentPane().add(panelButtons, BorderLayout.SOUTH);
        panelButtons.setPreferredSize(new java.awt.Dimension(390, 46));

        // Main button size
        java.awt.Dimension buttonSize = new java.awt.Dimension(96, 22);
        {
          btnApply = new JButton();
          btnApply.setText(Message.getString("dialog.common.button.apply")); // Apply
          btnApply.setPreferredSize(buttonSize);
          btnApply.addActionListener(this);
          panelButtons.add(btnApply);
        }
        {
          btnOk = new JButton();
          btnOk.setText(Message.getString("dialog.common.button.ok")); // OK
          btnOk.setPreferredSize(buttonSize);
          btnOk.addActionListener(this);
          panelButtons.add(btnOk);
        }
        {
          btnCancel = new JButton();
          btnCancel.setText(Message.getString("dialog.common.button.cancel")); // Cancel
          btnCancel.setPreferredSize(buttonSize);
          btnCancel.addActionListener(this);
          btnCancel.setMargin(new Insets(5, 5, 5, 5));
          panelButtons.add(btnCancel);
        }
      }

      // Content panel
      {
        JPanel panelContent = new JPanel();
        BorderLayout panelContentLayout = new BorderLayout();
        getContentPane().add(panelContent, BorderLayout.CENTER);
        Border border = new EmptyBorder(7, 7, 0, 7);
        panelContent.setBorder(border);
        panelContent.setLayout(panelContentLayout);

        // Connection to data in RemoteBuildProperties class
        modelProperties =
            new DefaultTableModel() {
              private static final long serialVersionUID = -6996565435968749645L;

              public int getColumnCount() {
                return COLUMN_HEADERS.length;
              }

              public int getRowCount() {
                return rb_properties.count();
              }

              public Object getValueAt(int row, int column) {
                if (column > COLUMN_HEADERS.length) {
                  System.err.println(
                      "Table has "
                          + COLUMN_HEADERS.length
                          + " columns. You asked for column number"
                          + column);
                  return null;
                }

                if (column == 0) {
                  return rb_properties.getOrder(row);
                } else if (column == 1) {
                  return rb_properties.getKey(row);
                } else if (column == 2) {
                  return rb_properties.getValue(row);
                } else if (column == 3) {
                  return Message.getString(rb_properties.getDescription(row));
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
                  System.err.println(
                      "Table has "
                          + COLUMN_HEADERS.length
                          + " columns. You asked for column number"
                          + column);
                  return;
                }
                if (column == 2) {
                  rb_properties.setValue(row, value.toString());
                }
                fireTableCellUpdated(row, column);
              }
            };

        modelProperties.setColumnIdentifiers(COLUMN_HEADERS);
        final CustomCellRenderer ccr = new CustomCellRenderer();
        tblProperties =
            new JTable(modelProperties) {
              /**
               * JTabe class with customizable CellRenderer for hiding passwords. Hides cell in
               * column 2 if value in column 1 contains string "pass".
               */
              private static final long serialVersionUID = 1L;

              public TableCellRenderer getCellRenderer(int row, int column) {
                String value = (String) this.getValueAt(row, 1);
                if (column == 2 && value.indexOf("pass") >= 0) {
                  return ccr;
                }
                return super.getCellRenderer(row, column);
              }
            };
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

        String filter_expr = "^((?!" + ProjectProperties.BUILD_COMMAND + ").)*$";
        sorter.setRowFilter(RowFilter.regexFilter(filter_expr, 1));

        sorter.setComparator(
            0,
            new Comparator<Integer>() {
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
        scrollList.setColumnHeader(
            new JViewport() {
              private static final long serialVersionUID = -8778306342340592940L;

              @Override
              public Dimension getPreferredSize() {
                Dimension d = super.getPreferredSize();
                d.height = 30;
                return d;
              }
            });
        panelContent.add(scrollList, BorderLayout.CENTER);
      }
      setTitle(Message.getString("remotebuildsettingdialog.title"));
      setSize(800, 350);

    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  static class CustomCellRenderer extends DefaultTableCellRenderer {

    private static final long serialVersionUID = -7528172127524209908L;
    private static final String ASTERISKS = "************************";

    @Override
    public Component getTableCellRendererComponent(
        JTable arg0, Object arg1, boolean arg2, boolean arg3, int arg4, int arg5) {
      int length = 0;
      if (arg1 instanceof String) {
        length = ((String) arg1).length();
      } else if (arg1 instanceof char[]) {
        length = ((char[]) arg1).length;
      }
      setText(asterisks(length));
      return this;
    }

    private String asterisks(int length) {
      if (length > ASTERISKS.length()) {
        StringBuilder sb = new StringBuilder(length);
        for (int i = 0; i < length; i++) {
          sb.append('*');
        }
        return sb.toString();
      } else {
        return ASTERISKS.substring(0, length);
      }
    }
  }

  /**
   * Display a dialog.
   *
   * @return Button type when the dialog is closed
   */
  public int showDialog() {

    // Display in the center of the parent frame.
    this.setLocationRelativeTo(this.getOwner());

    // Dialog display
    this.setVisible(true);
    this.requestFocus();
    return this.result;
  }

  @Override
  public void actionPerformed(ActionEvent event) {
    // OK
    // Registration
    if (event.getSource() == this.btnOk) {
      this.result = Constant.OK_DIALOG;

      // Fire a change event
      // this.sshproperties.firePropertyChange();

      // Close the dialog.
      dispose();
      return;
    }
    // Apply
    if (event.getSource() == this.btnApply) {
      this.result = Constant.OK_DIALOG;

      // Fire a change event
      // this.sshproperties.firePropertyChange();

      return;
    } // close
    else if (event.getSource() == this.btnCancel) {
      this.result = Constant.CANCEL_DIALOG;
      // Close the dialog.
      dispose();
      return;
    }
  }
}
