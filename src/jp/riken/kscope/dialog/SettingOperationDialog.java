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
// import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BorderFactory;
import javax.swing.JButton;
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
// import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.data.OperationCount;
import jp.riken.kscope.properties.OperationProperties;
import jp.riken.kscope.utils.StringUtils;

/**
 * Calculation count dialog class
 *
 * @author RIKEN
 */
public class SettingOperationDialog extends javax.swing.JDialog
    implements ActionListener, ListSelectionListener {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Cancel button */
  private JButton btnCancel;
  /** OK button */
  private JButton btnOk;
  /** Built-in function label */
  private JLabel lblName;
  /** Built-in function name text box */
  private JTextField txtName;
  /** Op (+) text box */
  private JTextField txtOpAdd;
  /** Op (*) text box */
  private JTextField txtOpMul;
  /** Registration button */
  private JButton btnReg;
  /** Add button */
  private JButton btnAdd;
  /** Delete button */
  private JButton btnDel;
  /** Clear button */
  private JButton btnClear;

  /** Arithmetic count list */
  private JTable tblOperand;
  /** Calculation count list data */
  private DefaultTableModel modelOperation;
  /** Calculation count setting panel */
  private JPanel panelOperand;
  /** Four arithmetic FLOP settings: + */
  private JTextField txtAdd;
  /** Four arithmetic FLOP settings: * */
  private JTextField txtMul;
  /** Four arithmetic FLOP settings:- */
  private JTextField txtSub;
  /** Four arithmetic FLOP settings: / */
  private JTextField txtDiv;
  /** Dialog return value */
  private int result = Constant.CANCEL_DIALOG;

  /** Arithmetic count property */
  OperationProperties properities;

  private final String[] COLUMN_HEADER = {
    Message.getString("settingoperationdialog.columnheader.instinsic"),
    "op(+)",
    "op(*)",
    "op(-)",
    "op(/)"
  }; // Built-in function
  private final int[] COLUMN_MINWIDTH = {110, 48, 48, -1, -1};
  private final int[] COLUMN_MAXWIDTH = {0, 48, 48, -1, -1};
  private final int COLUMN_COUNT = 6;

  /**
   * Constructor
   *
   * @param frame Parent frame
   */
  public SettingOperationDialog(JFrame frame) {
    super(frame);
    initGUI();

    // Initial display
    clearOperation();
  }

  /**
   * Constructor
   *
   * @param frame Parent frame
   * @param modal true = Show modal dialog
   */
  public SettingOperationDialog(Frame frame, boolean modal) {
    super(frame, modal);
    initGUI();

    // Initial display
    clearOperation();
  }

  /**
   * Constructor
   *
   * @param frame Parent frame
   * @param modal true = Show modal dialog
   * @param properities External tool properties
   */
  public SettingOperationDialog(Frame frame, boolean modal, OperationProperties properities) {
    super(frame, modal);
    initGUI();
    setOperandProperties(properities);

    // Initial display
    clearOperation();
  }

  /** Initialize the GUI. */
  private void initGUI() {
    try {

      getContentPane().setLayout(new BorderLayout());

      // Button panel
      {
        JPanel panelButtons = new JPanel();
        FlowLayout jPanel1Layout = new FlowLayout();
        jPanel1Layout.setHgap(10);
        jPanel1Layout.setVgap(10);
        panelButtons.setLayout(jPanel1Layout);
        getContentPane().add(panelButtons, BorderLayout.SOUTH);
        panelButtons.setPreferredSize(new java.awt.Dimension(390, 45));

        // Main button size
        java.awt.Dimension buttonSize = new java.awt.Dimension(96, 22);
        {
          btnOk = new JButton();
          panelButtons.add(btnOk);
          btnOk.setText(Message.getString("dialog.common.button.ok")); // OK
          btnOk.setPreferredSize(buttonSize);
          btnOk.addActionListener(this);
        }
        {
          btnCancel = new JButton();
          panelButtons.add(btnCancel);
          btnCancel.setText(Message.getString("dialog.common.button.cancel")); // Cancel
          btnCancel.setPreferredSize(buttonSize);
          btnCancel.setMargin(new Insets(5, 5, 5, 5));
          btnCancel.addActionListener(this);
        }
      }
      // Four arithmetic FLOP count
      {
        JPanel panelFlop = new JPanel();
        getContentPane().add(panelFlop, BorderLayout.NORTH);
        // FLOP setting of four arithmetic operations
        String title =
            Message.getString(
                "settingoperationdialog.panelflop.title"); // Set the number of arithmetic
        // operations
        TitledBorder borderTitle = new TitledBorder(BorderFactory.createEtchedBorder(), title);
        Border border = new CompoundBorder(new EmptyBorder(7, 7, 7, 7), borderTitle);
        panelFlop.setBorder(border);
        GridBagLayout layout = new GridBagLayout();
        layout.columnWidths = new int[] {24, 32, 24, 32, 24, 32, 24, 32, 24};
        layout.rowHeights = new int[] {24};
        layout.columnWeights = new double[] {0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0};
        layout.rowWeights = new double[] {0.0};
        panelFlop.setLayout(layout);

        int col = 0;
        // +
        JLabel lblAdd = new JLabel("(+) :");
        this.txtAdd = new JTextField();
        panelFlop.add(
            lblAdd,
            new GridBagConstraints(
                col++,
                0,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(0, 24, 0, 0),
                0,
                0));
        panelFlop.add(
            txtAdd,
            new GridBagConstraints(
                col++,
                0,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.HORIZONTAL,
                new Insets(0, 7, 0, 0),
                0,
                0));
        // *
        JLabel lblMul = new JLabel("(*) :");
        this.txtMul = new JTextField();
        panelFlop.add(
            lblMul,
            new GridBagConstraints(
                col++,
                0,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(0, 24, 0, 0),
                0,
                0));
        panelFlop.add(
            txtMul,
            new GridBagConstraints(
                col++,
                0,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.HORIZONTAL,
                new Insets(0, 7, 0, 0),
                0,
                0));
        // -
        JLabel lblSub = new JLabel("(-) :");
        this.txtSub = new JTextField();
        panelFlop.add(
            lblSub,
            new GridBagConstraints(
                col++,
                0,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(0, 24, 0, 0),
                0,
                0));
        panelFlop.add(
            txtSub,
            new GridBagConstraints(
                col++,
                0,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.HORIZONTAL,
                new Insets(0, 7, 0, 0),
                0,
                0));
        // /
        JLabel lblDiv = new JLabel("(/) :");
        this.txtDiv = new JTextField();
        panelFlop.add(
            lblDiv,
            new GridBagConstraints(
                col++,
                0,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(0, 24, 0, 0),
                0,
                0));
        panelFlop.add(
            txtDiv,
            new GridBagConstraints(
                col++,
                0,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.HORIZONTAL,
                new Insets(0, 7, 0, 0),
                0,
                0));
        // (FLOP)
        JLabel lblUnit = new JLabel("(FLOP)");
        panelFlop.add(
            lblUnit,
            new GridBagConstraints(
                col++,
                0,
                1,
                1,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(0, 24, 0, 24),
                0,
                0));
      }
      // Content panel
      {
        JPanel panelContent = new JPanel();
        BorderLayout panelContentLayout = new BorderLayout();
        getContentPane().add(panelContent, BorderLayout.CENTER);
        String title =
            Message.getString(
                "settingoperationdialog.contentspanel.title"); // Set the number of operations of
        // the built-in function
        TitledBorder titleOp = new TitledBorder(BorderFactory.createEtchedBorder(), title);
        Border borderOut = new CompoundBorder(new EmptyBorder(7, 7, 7, 7), titleOp);
        Border border = new CompoundBorder(borderOut, new EmptyBorder(7, 7, 7, 7));
        panelContent.setBorder(border);
        panelContent.setLayout(panelContentLayout);

        // Calculation count list
        {
          JPanel panelList = new JPanel();
          BorderLayout panelListLayout = new BorderLayout();
          panelList.setLayout(panelListLayout);
          panelContent.add(panelList, BorderLayout.CENTER);
          {
            JScrollPane scrollList = new JScrollPane();
            scrollList.setHorizontalScrollBarPolicy(
                ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            scrollList.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
            panelList.add(scrollList, BorderLayout.CENTER);
            {
              modelOperation = new DefaultTableModel();
              modelOperation.setColumnCount(COLUMN_COUNT);
              // Header column name
              String[] columns = COLUMN_HEADER;
              modelOperation.setColumnIdentifiers(columns);
              tblOperand = new JTable();
              scrollList.setViewportView(tblOperand);
              tblOperand.setModel(modelOperation);
              tblOperand.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
              tblOperand.getSelectionModel().addListSelectionListener(this);
              tblOperand.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
              tblOperand.setColumnSelectionAllowed(false);
              tblOperand.setDefaultEditor(Object.class, null);

              // Column width setting
              for (int i = 0; i < tblOperand.getColumnModel().getColumnCount(); i++) {
                TableColumn col = tblOperand.getColumnModel().getColumn(i);
                // Set the column width.
                if (i < COLUMN_MINWIDTH.length) {
                  col.setMinWidth(COLUMN_MINWIDTH[i]);
                  // If the maximum column width is set (> 0), set the maximum column width to make
                  // it a fixed column width.
                  if (COLUMN_MAXWIDTH[i] > 0) {
                    col.setMaxWidth(COLUMN_MAXWIDTH[i]);
                    col.setResizable(false);
                  }
                  // If the maximum column width is set to -1, set it as a hidden column (= 0).
                  else if (COLUMN_MAXWIDTH[i] < 0) {
                    col.setMaxWidth(0);
                    col.setResizable(false);
                  }
                }
                // Hide columns for which no column width is set.
                else {
                  col.setMinWidth(0);
                  col.setMaxWidth(0);
                }
              }
            }
          }
        }

        // Settings panel
        {
          JPanel panelSettings = new JPanel();
          BorderLayout panelSettingsLayout = new BorderLayout();
          panelContent.add(panelSettings, BorderLayout.EAST);
          Border borderSettings = new EmptyBorder(0, 7, 0, 0);
          panelSettings.setBorder(borderSettings);
          panelSettings.setLayout(panelSettingsLayout);

          panelOperand = new JPanel();
          GridBagLayout panelKeywordLayout = new GridBagLayout();
          panelKeywordLayout.columnWidths = new int[] {100, 100, 7};
          panelKeywordLayout.rowHeights = new int[] {30, 30, 30, 30, 30, 30, 7};
          panelKeywordLayout.columnWeights = new double[] {0, 0, 0};
          panelKeywordLayout.rowWeights = new double[] {0, 0, 0, 0, 0, 0, 1};
          panelSettings.add(panelOperand, BorderLayout.CENTER);
          panelOperand.setLayout(panelKeywordLayout);
          panelOperand.setPreferredSize(new java.awt.Dimension(230, 234));

          // Calculation count frame

          String titleOperand =
              Message.getString("settingkeyworddialog.label.preference"); // Configuration
          TitledBorder borderOutOperand =
              new TitledBorder(BorderFactory.createEtchedBorder(), titleOperand);
          Border borderOperand = new CompoundBorder(borderOutOperand, new EmptyBorder(0, 7, 0, 7));
          panelOperand.setBorder(borderOperand);

          // Built-in function name
          {
            lblName = new JLabel();
            panelOperand.add(
                lblName,
                new GridBagConstraints(
                    0,
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
            lblName.setText(
                Message.getString(
                    "settingoperationdialog.label.intrinsicname")); // Built-in functions
          }
          {
            txtName = new JTextField();
            panelOperand.add(
                txtName,
                new GridBagConstraints(
                    1,
                    0,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.WEST,
                    GridBagConstraints.HORIZONTAL,
                    new Insets(0, 0, 0, 0),
                    0,
                    0));
          }
          // Operator: +
          {
            JLabel lblOpPlus = new JLabel();
            panelOperand.add(
                lblOpPlus,
                new GridBagConstraints(
                    0,
                    1,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.WEST,
                    GridBagConstraints.NONE,
                    new Insets(0, 0, 0, 0),
                    0,
                    0));
            lblOpPlus.setText(
                Message.getString("settingoperationdialog.label.plus")); // Number of operations (+)
          }
          {
            txtOpAdd = new JTextField();
            panelOperand.add(
                txtOpAdd,
                new GridBagConstraints(
                    1,
                    1,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.WEST,
                    GridBagConstraints.HORIZONTAL,
                    new Insets(0, 0, 0, 0),
                    0,
                    0));
          }
          // Operator: *
          {
            JLabel lblOpProduct = new JLabel();
            panelOperand.add(
                lblOpProduct,
                new GridBagConstraints(
                    0,
                    2,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.WEST,
                    GridBagConstraints.NONE,
                    new Insets(0, 0, 0, 0),
                    0,
                    0));
            lblOpProduct.setText(
                Message.getString(
                    "settingoperationdialog.label.product")); // Number of operations (*)
          }
          {
            txtOpMul = new JTextField();
            panelOperand.add(
                txtOpMul,
                new GridBagConstraints(
                    1,
                    2,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.WEST,
                    GridBagConstraints.HORIZONTAL,
                    new Insets(0, 0, 0, 0),
                    0,
                    0));
          }
          /***********
           * // Operator:-
           * {
           * JLabel lblOpMinus = new JLabel ();
           * panelOperand.add (lblOpMinus, new GridBagConstraints (0, 3, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets (0, 0, 0, 0), 0, 0));
           * lblOpMinus.setText ("Number of operations (-)");
           * }
           * {
           * txtOpSub = new JTextField ();
           * panelOperand.add (txtOpSub, new GridBagConstraints (1, 3, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets (0, 0, 0, 0), 0, 0));
           * }
           * // Operator: /
           * {
           * JLabel lblOpQuotient = new JLabel ();
           * panelOperand.add (lblOpQuotient, new GridBagConstraints (0, 4, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets (0, 0, 0, 0), 0, 0));
           * lblOpQuotient.setText ("Number of operations (/)");
           * }
           * {
           * txtOpDiv = new JTextField ();
           * panelOperand.add (txtOpDiv, new GridBagConstraints (1, 4, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets (0, 0, 0, 0), 0, 0));
           * }
           ***************/

          // Calculation count addition / deletion button panel
          JPanel panelAddButtons = new JPanel();
          FlowLayout panelAddButtonsLayout = new FlowLayout(FlowLayout.RIGHT);
          Border borderAddButtons = new EmptyBorder(0, 7, 0, 0);
          panelAddButtons.setBorder(borderAddButtons);
          panelAddButtons.setLayout(panelAddButtonsLayout);
          panelSettings.add(panelAddButtons, BorderLayout.SOUTH);

          java.awt.Dimension minSize = new java.awt.Dimension(60, 22);
          Insets minInsets = new Insets(3, 3, 3, 3);
          {
            btnAdd = new JButton();
            panelAddButtons.add(btnAdd);
            btnAdd.setText(Message.getString("dialog.common.button.add")); // add to
            btnAdd.setPreferredSize(minSize);
            btnAdd.setMargin(minInsets);
            btnAdd.addActionListener(this);
          }
          {
            btnReg = new JButton();
            panelAddButtons.add(btnReg);
            btnReg.setText(Message.getString("dialog.common.button.update")); // update
            btnReg.setPreferredSize(minSize);
            btnReg.setMargin(minInsets);
            btnReg.addActionListener(this);
          }
          {
            btnDel = new JButton();
            panelAddButtons.add(btnDel);
            btnDel.setText(Message.getString("dialog.common.button.delete")); // Delete
            btnDel.setPreferredSize(minSize);
            btnDel.setMargin(minInsets);
            btnDel.addActionListener(this);
          }
          {
            btnClear = new JButton();
            panelAddButtons.add(btnClear);
            btnClear.setText(Message.getString("dialog.common.button.clear")); // clear
            btnClear.setPreferredSize(minSize);
            btnClear.setMargin(minInsets);
            btnClear.addActionListener(this);
          }
        }
      }

      setTitle(
          Message.getString(
              "settingoperationdialog.dialog.title")); // Count the number of operations
      this.setSize(560, 420);

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
   * Set the operation count property.
   *
   * @param properities Arithmetic count properties
   */
  public void setOperandProperties(OperationProperties properities) {
    this.properities = properities;

    // Sort by built-in function (key) name
    Object[] keyList = properities.keySet().toArray();
    java.util.Arrays.sort(keyList);

    // Create an arithmetic count list
    for (Object key : keyList) {
      OperationCount value = (OperationCount) properities.get(key);

      // Create row data
      Object[] column = createOperationRowData(value);
      // Add line
      modelOperation.addRow(column);
    }

    // Four arithmetic FLOP settings
    this.txtAdd.setText(String.valueOf(properities.getFlopAdd()));
    this.txtMul.setText(String.valueOf(properities.getFlopMul()));
    this.txtSub.setText(String.valueOf(properities.getFlopSub()));
    this.txtDiv.setText(String.valueOf(properities.getFlopDiv()));
  }

  /**
   * Get the operation count.
   *
   * @return operation count property
   */
  public OperationProperties getOperandProperties() {

    // Clear operation count property
    properities.clear();

    // Get the operation count from the operation count list
    int count = modelOperation.getRowCount();
    for (int i = 0; i < count; i++) {
      OperationCount opc = new OperationCount();

      Object cell;
      String name = null;
      // name
      cell = modelOperation.getValueAt(i, 0);
      if (cell != null && !((String) cell).isEmpty()) {
        opc.setName((String) cell);
        name = (String) cell;
      }
      // Number of operations (+)
      cell = modelOperation.getValueAt(i, 1);
      if (cell != null && !cell.toString().isEmpty() && StringUtils.isNumeric(cell.toString())) {
        opc.setAdd(Integer.parseInt(cell.toString()));
      }
      // Number of operations (*)
      cell = modelOperation.getValueAt(i, 2);
      if (cell != null && !(cell.toString()).isEmpty() && StringUtils.isNumeric(cell.toString())) {
        opc.setMul(Integer.parseInt(cell.toString()));
      }
      // Number of operations (-)
      cell = modelOperation.getValueAt(i, 3);
      if (cell != null && !(cell.toString()).isEmpty() && StringUtils.isNumeric(cell.toString())) {
        opc.setSub(Integer.parseInt(cell.toString()));
      }
      // Number of operations (/)
      cell = modelOperation.getValueAt(i, 4);
      if (cell != null && !(cell.toString()).isEmpty() && StringUtils.isNumeric(cell.toString())) {
        opc.setDiv(Integer.parseInt(cell.toString()));
      }

      // Add operation count
      if (name != null && !name.isEmpty()) {
        properities.addOperationProperty(name, opc);
      }
    }

    // Four arithmetic FLOP settings
    {
      int value = 0;
      if (StringUtils.isNumeric(this.txtAdd.getText())) {
        value = Integer.parseInt(this.txtAdd.getText());
      }
      properities.setFlopAdd(value);
    }
    {
      int value = 0;
      if (StringUtils.isNumeric(this.txtMul.getText())) {
        value = Integer.parseInt(this.txtMul.getText());
      }
      properities.setFlopMul(value);
    }
    {
      int value = 0;
      if (StringUtils.isNumeric(this.txtSub.getText())) {
        value = Integer.parseInt(this.txtSub.getText());
      }
      properities.setFlopSub(value);
    }
    {
      int value = 0;
      if (StringUtils.isNumeric(this.txtDiv.getText())) {
        value = Integer.parseInt(this.txtDiv.getText());
      }
      properities.setFlopDiv(value);
    }

    return properities;
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

      // Get the changes.
      getOperandProperties();

      // Fire a change event
      this.properities.firePropertyChange();

      // Close the dialog.
      dispose();
      return;
    }
    // close
    else if (event.getSource() == this.btnCancel) {
      this.result = Constant.CANCEL_DIALOG;
      // Close the dialog.
      dispose();
      return;
    }
    // update
    else if (event.getSource() == this.btnReg) {
      // Check the input
      if (this.validateOperand(false) == false) {
        // Typing error
        return;
      }

      // Update
      OperationCount opc = this.getOperationCount();
      setOperandList(opc);

    }
    // add to
    else if (event.getSource() == this.btnAdd) {
      // Check the input
      if (this.validateOperand(true) == false) {
        // Typing error
        return;
      }

      // make an addition
      OperationCount opc = this.getOperationCount();
      addOperandList(opc);
    }
    // Delete
    else if (event.getSource() == this.btnDel) {
      // Selected line
      int selectedrow = this.tblOperand.getSelectedRow();
      if (selectedrow < 0) return;

      int option =
          JOptionPane.showConfirmDialog(
              this,
              Message.getString(
                  "settingoperationdialog.confirmdialog.delete.message"), // Delete Are you sure you
              // want to?
              Message.getString("settingoperationdialog.confirmdialog.delete.title"),
              JOptionPane.OK_CANCEL_OPTION); // Delete operation count
      if (option == JOptionPane.OK_OPTION) {
        // Delete
        OperationCount opc = this.getOperationCount();
        removeOperationList(opc);
      }
    }
    // new
    else if (event.getSource() == this.btnClear) {
      // Clear the calculation count panel.
      clearOperation();
    }
  }

  /**
   * Operation count list change event. <br>
   * Set the calculation count information of the selected line in the setting panel.
   *
   * @param event Event information
   */
  @Override
  public void valueChanged(ListSelectionEvent event) {

    if (event.getSource() == this.tblOperand.getSelectionModel()) {
      // Get the selected row.
      int selectedrow = this.tblOperand.getSelectedRow();
      if (selectedrow < 0) return;

      Object cell;
      // name
      cell = modelOperation.getValueAt(selectedrow, 0);
      this.txtName.setText((String) cell);

      // Number of operations (+)
      cell = modelOperation.getValueAt(selectedrow, 1);
      this.txtOpAdd.setText(cell != null ? cell.toString() : null);

      // Number of operations (*)
      cell = modelOperation.getValueAt(selectedrow, 2);
      this.txtOpMul.setText(cell != null ? cell.toString() : null);
      /*
                  // Number of operations (-)
                  cell = modelOperand.getValueAt (selectedrow, 3);
                  this.txtOpSub.setText (cell! = null? cell.toString (): null);


                  // Number of operations (/)
                  cell = modelOperand.getValueAt (selectedrow, 4);
                  this.txtOpDiv.setText (cell! = null? cell.toString (): null);
      */
    }

    return;
  }

  /**
   * Create row data for the arithmetic count list model.
   *
   * @param opc Keyword data
   * @return line data
   */
  private Object[] createOperationRowData(OperationCount opc) {

    Object[] column = new Object[6];
    // Built-in function name
    column[0] = opc.getName();
    // Number of operations (+)
    column[1] = opc.getAdd();
    // Number of operations (*)
    column[2] = opc.getMul();
    // Number of operations (-)
    column[3] = opc.getSub();
    // Number of operations (/)
    column[4] = opc.getDiv();

    return column;
  }

  /**
   * Get the keyword object from the calculation count panel.
   *
   * @return Operation count object
   */
  private OperationCount getOperationCount() {

    OperationCount opc = new OperationCount();

    String value = null;
    // Built-in function name
    opc.setName(this.txtName.getText());
    // Number of operations (+)
    value = this.txtOpAdd.getText();
    if (value != null && !value.isEmpty() && StringUtils.isNumeric(value)) {
      opc.setAdd(Integer.parseInt(value));
    } else {
      opc.setAdd(null);
    }
    // Number of operations (*)
    value = this.txtOpMul.getText();
    if (value != null && !value.isEmpty() && StringUtils.isNumeric(value)) {
      opc.setMul(Integer.parseInt(value));
    } else {
      opc.setMul(null);
    }
    /**********
     * // Number of operations (-)
     * value = this.txtOpSub.getText ();
     * if (value! = null &&! value.isEmpty () && StringUtils.is_int_str (value)) {
     * opc.setSub (Integer.parseInt (value));
     * }
     * else {
     * opc.setSub (null);
     * }
     * // Number of operations (/)
     * value = this.txtOpDiv.getText ();
     * if (value! = null &&! value.isEmpty () && StringUtils.is_int_str (value)) {
     * opc.setDiv (Integer.parseInt (value));
     * }
     * else {
     * opc.setDiv (null);
     * }
     **************/

    return opc;
  }

  /**
   * Update the calculation count setting.
   *
   * @param opc Operation count
   */
  private void setOperandList(OperationCount opc) {

    // Selected line
    int selectedrow = this.tblOperand.getSelectedRow();
    if (selectedrow < 0) return;

    // Create row data
    Object[] column = createOperationRowData(opc);
    // Line update
    modelOperation.removeRow(selectedrow);
    modelOperation.insertRow(selectedrow, column);
    this.tblOperand.setRowSelectionInterval(selectedrow, selectedrow);
  }

  /**
   * Add an arithmetic count.
   *
   * @param opc Operation count
   */
  private void addOperandList(OperationCount opc) {
    // Create row data
    Object[] column = createOperationRowData(opc);
    // Add line
    modelOperation.addRow(column);
    int selectedrow = modelOperation.getRowCount() - 1;
    this.tblOperand.setRowSelectionInterval(selectedrow, selectedrow);
  }

  /**
   * Delete the operation count.
   *
   * @param opc Operation count
   */
  private void removeOperationList(OperationCount opc) {
    // Selected line
    int selectedrow = this.tblOperand.getSelectedRow();
    if (selectedrow < 0) return;

    // Delete line
    modelOperation.removeRow(selectedrow);

    // Clear the keyword setting panel.
    clearOperation();
  }

  /** Clear the calculation count panel. */
  private void clearOperation() {

    // Clear settings
    /** Built-in function name text box */
    this.txtName.setText(null);
    /** Number of operations (+) text box */
    this.txtOpAdd.setText(null);
    /** Number of operations (*) Text box */
    this.txtOpMul.setText(null);
    /********
     * // Number of operations (-) text box
     * this.txtOpSub.setText (null);
     * // Number of operations (/) Text box
     * this.txtOpDiv.setText (null);
     **********/
  }

  /**
   * Check the input.
   *
   * @param addflag Add flag (true = add operation count)
   */
  private boolean validateOperand(boolean addflag) {
    // Name required
    String name = this.txtName.getText();
    // Check the input
    if (name == null || name.isEmpty()) {
      JOptionPane.showMessageDialog(
          this,
          Message.getString(
              "settingoperationdialog.errordialog.nameempty.message"), // Enter the built-in
          // function name.
          Message.getString("dialog.common.error"), // error
          JOptionPane.ERROR_MESSAGE);
      return false;
    }

    // Check for duplicate built-in function names.
    int selectedrow = this.tblOperand.getSelectedRow();
    int count = this.modelOperation.getRowCount();
    boolean exists = false;
    for (int i = 0; i < count; i++) {
      // Built-in function name
      String cellName = (String) (this.modelOperation.getValueAt(i, 0));
      if (name.equals(cellName)) {
        if (addflag) {
          // In case of addition, the same built-in function name is prohibited
          exists = true;
          break;
        } else if (i != selectedrow) {
          // In case of update, NG if the same built-in function name exists other than the update
          // line
          exists = true;
          break;
        }
      }
    }
    if (exists) {
      JOptionPane.showMessageDialog(
          this,
          Message.getString(
              "settingoperationdialog.errordialog.duplication.message"), // Duplicate built-in
          // function names cannot be
          // registered.
          Message.getString("dialog.common.error"),
          JOptionPane.ERROR_MESSAGE);
      return false;
    }

    return true;
  }
}
