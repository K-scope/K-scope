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
 * Trace selection dialog
 *
 * @author RIKEN
 */
public class TraceChooserDialog extends javax.swing.JDialog
    implements ActionListener, MouseListener {

  /** Serial number */
  private static final long serialVersionUID = 1L;
  /** Trace table */
  private JTable tableTrace;
  /** OK button */
  private JButton btnOk;
  /** Cancel button */
  private JButton btnCancel;
  /** Open file */
  private JButton btnOpenFile;
  /** Action to open the relevant part */
  ViewOpenAnalysisLineAction actionOpen;

  /** Dialog return value */
  private int result = Constant.CANCEL_DIALOG;

  /**
   * Constructor
   *
   * @param owner parent frame
   * @param modal true = Show modal dialog
   */
  public TraceChooserDialog(Frame owner, boolean modal) {
    super(owner, modal);
    initGUI();
  }

  /** Initialize the GUI. */
  private void initGUI() {
    try {
      BorderLayout thisLayout = new BorderLayout();
      getContentPane().setLayout(thisLayout);

      {
        // Button panel
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
            btnOk.setText(Message.getString("dialog.common.button.ok")); // OK
            btnOk.addActionListener(this);
            panelButtons.add(btnOk);
          }
          {
            btnCancel = new JButton();
            btnCancel.setPreferredSize(buttonSize);
            btnCancel.setText(Message.getString("dialog.common.button.cancel")); // Cancel
            btnCancel.addActionListener(this);
            panelButtons.add(btnCancel);
          }
        }
        // Trace
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
          // Label
          {
            JLabel label = new JLabel();
            panelContent.add(
                label,
                new GridBagConstraints(
                    1,
                    0,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.WEST,
                    GridBagConstraints.NONE,
                    new Insets(0, 0, 0, 0),
                    0,
                    0));
            label.setText(
                Message.getString("tracechooserdialog.label.next")); // Select trace destination
          }

          // Trace destination table
          {
            tableTrace = new JStripeTable();
            String[] HEADER_COLUMNS = {
              "", Message.getString("settingprojectdialog.column_header.message")
            }; // message

            DefaultTableModel tableModel = new DefaultTableModel();
            tableModel.setColumnIdentifiers(HEADER_COLUMNS);
            tableTrace.setModel(tableModel);
            JScrollPane scrollTrace = new JScrollPane(tableTrace);
            tableTrace.setAutoCreateColumnsFromModel(false);
            tableTrace.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
            tableTrace.setColumnSelectionAllowed(false);

            // Table column model
            DefaultTableColumnModel columnModel =
                (DefaultTableColumnModel) tableTrace.getColumnModel();
            TableColumn column = null;

            // First column is model information: hidden
            column = columnModel.getColumn(0);
            column.setPreferredWidth(0);
            column.setMinWidth(0);
            column.setMaxWidth(0);
            column.setResizable(false);

            // Status column
            column = columnModel.getColumn(1);
            column.setPreferredWidth(445);
            column.setMinWidth(160);

            scrollTrace.setHorizontalScrollBarPolicy(
                ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            scrollTrace.setVerticalScrollBarPolicy(
                ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
            panelContent.add(
                scrollTrace,
                new GridBagConstraints(
                    1,
                    1,
                    2,
                    2,
                    0.0,
                    0.0,
                    GridBagConstraints.CENTER,
                    GridBagConstraints.BOTH,
                    new Insets(0, 0, 0, 0),
                    0,
                    0));
          }
          // Button panel
          {
            JPanel panelSubButtons = new JPanel();
            FlowLayout panelSubButtonsLayout = new FlowLayout();
            panelSubButtonsLayout.setAlignment(FlowLayout.RIGHT);
            panelSubButtonsLayout.setHgap(10);
            panelSubButtonsLayout.setVgap(0);
            panelSubButtons.setLayout(panelSubButtonsLayout);
            panelContent.add(
                panelSubButtons,
                new GridBagConstraints(
                    2,
                    0,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.SOUTHEAST,
                    GridBagConstraints.HORIZONTAL,
                    new Insets(0, 0, 0, 0),
                    0,
                    0));

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

            // Tooltip settings
            btnOpenFile.setToolTipText(
                Message.getString("tracechooserdialog.tooltip.open")); // Open the relevant part

            // Event registration
            btnOpenFile.addActionListener(this);
            tableTrace.addMouseListener(this);
          }
        }
      }
      this.setTitle(Message.getString("tracechooserdialog.label.next")); // Select trace destination
      this.setSize(480, 280);

    } catch (Exception e) {
      e.printStackTrace();
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

    return this.result;
  }

  /**
   * Button click event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {

    // OK
    if (event.getSource() == this.btnOk) {
      this.result = Constant.OK_DIALOG;
      // Close the dialog.
      dispose();
      return;
    }
    // Cancel
    else if (event.getSource() == this.btnCancel) {
      this.result = Constant.CANCEL_DIALOG;
      // Close the dialog.
      dispose();
      return;
    }
    // Open the relevant part
    else if (event.getSource() == this.btnOpenFile) {
      // Open the relevant part
      viewSelectedBlock();

      return;
    }

    return;
  }

  /**
   * Get the trace destination model
   *
   * @return Trace destination model
   */
  public TraceResultModel getTraceResultModel() {

    int selectedrow = this.tableTrace.getSelectedRow();
    if (selectedrow < 0) return null;
    Object cell = this.tableTrace.getModel().getValueAt(selectedrow, 0);
    if (cell == null) return null;
    if (cell instanceof TraceResultModel) {
      return (TraceResultModel) cell;
    }
    return null;
  }

  /**
   * Set the race destination model.
   *
   * @param traces Race destination model
   */
  public void setTraceResultModel(TraceResultModel[] traces) {
    if (traces == null) return;

    DefaultTableModel tableModel = new DefaultTableModel();

    String[] HEADER_COLUMNS = {
      Message.getString("tracechooserdialog.header_column.codeline"), // CODELINE
      Message.getString("settingprojectdialog.column_header.message")
    }; // message
    tableModel.setColumnIdentifiers(HEADER_COLUMNS);
    for (TraceResultModel model : traces) {
      Object[] rows = new Object[2];
      rows[0] = model;
      rows[1] = model.getBlocklabel();
      tableModel.addRow(rows);
    }
    this.tableTrace.setModel(tableModel);

    // Select the first line
    this.tableTrace.setRowSelectionInterval(0, 0);
  }

  /** Open selection */
  private void viewSelectedBlock() {
    if (this.actionOpen == null) return;

    TraceResultModel selectedModel = getTraceResultModel();
    IBlock block = selectedModel.getSelectedBlock();

    // Open the relevant part
    this.actionOpen.viewSelectedBlock(block);

    return;
  }

  /**
   * Mouse click event
   *
   * @param event Mouse event information
   */
  @Override
  public void mouseClicked(MouseEvent event) {
    // Double click check
    if (SwingUtilities.isLeftMouseButton(event) && event.getClickCount() == 2) {
      if (event.getSource() == this.tableTrace) {
        // Open the relevant part
        viewSelectedBlock();
      }
    }
  }

  /**
   * Mouse button down event
   *
   * @param e Mouse event information
   */
  @Override
  public void mousePressed(MouseEvent e) {}

  /**
   * Mouse button up event
   *
   * @param e Mouse event information
   */
  @Override
  public void mouseReleased(MouseEvent e) {}

  /**
   * Mouseover event
   *
   * @param e Mouse event information
   */
  @Override
  public void mouseEntered(MouseEvent e) {}

  /**
   * Mouse out event
   *
   * @param e Mouse event information
   */
  @Override
  public void mouseExited(MouseEvent e) {}

  /**
   * Set the action to open the relevant part.
   *
   * @param action Action to open the corresponding part
   */
  public void setViewOpenAnalysisLineAction(ViewOpenAnalysisLineAction action) {
    this.actionOpen = action;
  }
}
