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
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import javax.swing.BoxLayout;
import javax.swing.DefaultCellEditor;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ACCESSMEMORY_TYPE;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.component.JBackgroundComboBox;
import jp.riken.kscope.data.RequiredBF;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.properties.RequiredBFProperties;
import jp.riken.kscope.properties.VariableMemoryProperties;

/**
 * Variable access destination setting dialog
 *
 * @author RIKEN
 */
public class VariableAccessDialog extends javax.swing.JDialog implements ActionListener {

  /** Serial number */
  private static final long serialVersionUID = 1L;
  /** Cancel button */
  private JButton btnCancel;
  /** OK button */
  private JButton btnOk;
  /** Apply button */
  private JButton btnApply;
  /** Calculate button */
  private JButton btnCalc;
  /** Change button */
  private JButton btnChange;
  /** Change All: Select */
  private JCheckBox chkSelect;
  /** Change everything: array */
  private JCheckBox chkArray;
  /** All changed: scalar */
  private JCheckBox chkScalar;
  /** All settings Access memory combo box */
  private JBackgroundComboBox cmbAllAccess;
  /** Memory access list */
  private JTable tblAccess;
  /** Memory access list data */
  private DefaultTableModel modelAccess;
  /** Selection block */
  private IBlock[] selectedblocks;
  /** Variable access destination memory setting */
  private VariableMemoryProperties propertiesVariable;
  /** Request Byte / FLOP setting Property setting */
  private RequiredBFProperties propertiesMemoryband;
  /** Dialog return value */
  private int result = Constant.CANCEL_DIALOG;
  /** Memory performance calculation result dialog */
  private RequiredBFDialog nextDialog;
  /** Parent dialog flag true = first called dialog, false = called from another dialog */
  private boolean ownerDialog;
  /** Calculation panel */
  private JPanel panelCalculate;
  /** Memory access table column header setting */
  private final String[] COLUMN_HEADER = {
    "",
    Message.getString("variableaccessdialog.header.variable"), // Variable
    Message.getString("settingprojectdialog.column_header.type"), // Type
    Message.getString("variableaccessdialog.header.datatype"), // Data type
    Message.getString("settingrequiredbfdialog.label.access"), // Access destination
    ""
  };
  /** Minimum width of memory access table column */
  private final int[] COLUMN_MINWIDTH = {0, 80, 60, 240, 120, 0};
  /** Maximum width of memory access table column */
  private final int[] COLUMN_MAXWIDTH = {-1, 0, 60, 0, 120, -1};
  /** Number of memory access destination table columns */
  private final int COLUMN_COUNT = 6;
  /** Initial height of dialog */
  private final int DEFAULT_HEIGHT = 360;
  /** Memory access table row height */
  private final int TABLE_ROWHEIGHT = 22;
  /** Access memory combo box column */
  private final int MEMORYACCESS_COLUMN = 4;
  /** Type column */
  private final int ARRAYSCALAR_COLUMN = 2;
  /** Save destination column of access destination memory character string */
  private final int ACCESSMEMORYTYPE_COLUMN = 5;

  /**
   * Constructor
   *
   * @param frame Parent frame
   */
  public VariableAccessDialog(JFrame frame) {
    super(frame);
    nextDialog = null;
    this.ownerDialog = true;
    initGUI();
  }

  /**
   * Constructor
   *
   * @param frame Parent frame
   * @param modal true = Show modal dialog
   */
  public VariableAccessDialog(Frame frame, boolean modal) {
    super(frame, modal);
    nextDialog = null;
    this.ownerDialog = true;
    initGUI();
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
        // panelButtons.setPreferredSize(new java.awt.Dimension(390, 45));

        // Main button size
        java.awt.Dimension buttonSize = new java.awt.Dimension(96, 22);
        {
          btnApply = new JButton();
          panelButtons.add(btnApply);
          btnApply.setText(Message.getString("dialog.common.button.apply")); // Apply
          btnApply.setPreferredSize(buttonSize);
          btnApply.addActionListener(this);
        }
        {
          String textOk = Message.getString("dialog.common.button.ok");
          btnOk = new JButton();
          panelButtons.add(btnOk);
          btnOk.setText(textOk);
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
      // Content panel
      {
        JPanel panelContent = new JPanel();
        panelContent.setLayout(new BoxLayout(panelContent, BoxLayout.Y_AXIS));
        getContentPane().add(panelContent, BorderLayout.CENTER);

        JPanel panelTable = new JPanel();
        panelContent.add(panelTable);
        GridBagLayout layoutTable = new GridBagLayout();
        layoutTable.rowWeights = new double[] {0.0, 1.0, 0.0};
        layoutTable.rowHeights = new int[] {24, 100, 7};
        layoutTable.columnWeights = new double[] {0.0, 1.0, 0.0};
        layoutTable.columnWidths = new int[] {7, 520, 7};
        panelTable.setLayout(layoutTable);
        String title =
            Message.getString(
                "variableaccessdialog.table.title"); // Variable access destination list
        TitledBorder insideborder = new TitledBorder(new EtchedBorder(), title);
        Border outsideBorder = new EmptyBorder(7, 7, 7, 7);
        CompoundBorder border = new CompoundBorder(outsideBorder, insideborder);
        panelTable.setBorder(border);

        // All settings
        {
          JPanel panelAll = new JPanel();
          FlowLayout layout = new FlowLayout(FlowLayout.RIGHT);
          layout.setHgap(10);
          panelAll.setLayout(layout);
          panelTable.add(
              panelAll,
              new GridBagConstraints(
                  1,
                  0,
                  1,
                  1,
                  0.0,
                  0.0,
                  GridBagConstraints.EAST,
                  GridBagConstraints.HORIZONTAL,
                  new Insets(0, 0, 0, 0),
                  0,
                  0));

          // Collectively set
          String titleAll =
              Message.getString(
                  "variableaccessdialog.all.title"); // Access destination batch setting
          JLabel labelAll = new JLabel(titleAll + " : ");
          panelAll.add(labelAll);

          // select all
          String select = Message.getString("settingviewdialog.label.select"); // Choice
          this.chkSelect = new JCheckBox(select, true);
          panelAll.add(chkSelect);

          // All: Array
          this.chkArray = new JCheckBox("array");
          panelAll.add(chkArray);
          // All: Scalar
          this.chkScalar = new JCheckBox("scalar");
          panelAll.add(chkScalar);
          // Memory access destination
          this.cmbAllAccess = new JBackgroundComboBox();
          panelAll.add(this.cmbAllAccess);
          // Change button
          btnChange = new JButton();
          panelAll.add(btnChange);
          btnChange.setText(Message.getString("dialog.common.button.change")); // change button
          btnChange.setMargin(new Insets(0, 2, 2, 2));
          btnChange.addActionListener(this);
        }
        // Access variable list
        {
          JScrollPane scrollList = new JScrollPane();
          panelTable.add(
              scrollList,
              new GridBagConstraints(
                  1,
                  1,
                  1,
                  1,
                  0.0,
                  0.0,
                  GridBagConstraints.WEST,
                  GridBagConstraints.BOTH,
                  new Insets(0, 0, 0, 0),
                  0,
                  0));
          scrollList.setHorizontalScrollBarPolicy(
              ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
          scrollList.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
          {
            this.modelAccess = createVariableTableModel();
            this.tblAccess = createVariableTable(this.modelAccess);
            scrollList.setViewportView(tblAccess);
          }
        }
        // Calculation: No calculation when displaying from the memory performance calculation
        // result dialog.
        if (this.ownerDialog) {
          panelCalculate = new JPanel();
          panelContent.add(panelCalculate);
          FlowLayout layout = new FlowLayout(FlowLayout.RIGHT);
          layout.setHgap(24);
          panelCalculate.setLayout(layout);

          // Request Byte / FLOP calculation
          JLabel label =
              new JLabel(
                  Message.getString(
                      "mainmenu.analysis.calculate")); // Request Byte / FLOP calculation
          panelCalculate.add(label);
          // Calculate
          {
            btnCalc = new JButton();
            panelCalculate.add(btnCalc);
            btnCalc.setText(
                Message.getString("variableaccessdialog.calculate.button")); // calculate
            btnCalc.setMargin(new Insets(2, 2, 2, 2));
            btnCalc.addActionListener(this);
          }
        }
      }
      // Update property data
      updateProperties();

      setTitle(
          Message.getString("mainmenu.analysis.access")); // Variable access destination setting
      this.pack();

      Dimension size = new Dimension(this.getSize().width + 5, DEFAULT_HEIGHT);
      this.setSize(size);

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
   * Create all memory access combo boxes.
   *
   * @return All memory access destination combo boxes
   */
  @SuppressWarnings("unused")
  private JBackgroundComboBox createAllAccessCombobox() {
    //    	JComboBox cmb = new JComboBox(listmemory);
    //		ComboColorRenderer renderer = new ComboColorRenderer();
    //		cmb.setRenderer(renderer);
    JBackgroundComboBox cmb = new JBackgroundComboBox();
    return cmb;
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
      // Set the access destination memory to the variable.
      setVariableAccessMemory(false); // Apply access destination

      // Close the dialog.
      dispose();
      return;
    }
    // Apply
    else if (event.getSource() == this.btnApply) {
      this.result = Constant.OK_DIALOG;
      // Set the access destination memory to the variable.
      setVariableAccessMemory(false); // Apply access destination

      return;
    }
    // close
    else if (event.getSource() == this.btnCancel) {
      this.result = Constant.CANCEL_DIALOG;
      // Close the dialog.
      dispose();
      return;
    }
    // Calculate
    else if (event.getSource() == this.btnCalc) {
      if (this.nextDialog != null) {
        // Temporarily set the access destination memory to the variable.
        setVariableAccessMemory(true);
        // Display the memory performance calculation result dialog.
        this.setVisible(false);
        this.nextDialog.setOwnerDialog(false);
        this.nextDialog.showDialog();
        // Redisplay
        this.setVisible(true);
      }
      return;
    }
    // Change all access destinations
    else if (event.getSource() == this.btnChange) {
      setAllAccessMemory();
    }
  }

  /**
   * Combo box for drawing cells
   *
   * @author RIKEN
   */
  class RendererComboBox extends JBackgroundComboBox implements TableCellRenderer {
    /** Serial number */
    private static final long serialVersionUID = 1L;

    /** Constructor */
    public RendererComboBox() {
      super();
    }

    /** Returns the component used to draw the cell. */
    @Override
    public Component getTableCellRendererComponent(
        JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {

      Color selectColor = Color.white;
      ACCESSMEMORY_TYPE type = ACCESSMEMORY_TYPE.getAccessMemoryType(value.toString());
      if (type != null) {
        RequiredBF mem = VariableAccessDialog.this.propertiesMemoryband.getRequiredBF(type);
        if (mem != null) {
          selectColor = mem.getBackColor();
        }
      }
      this.removeAllItems();
      this.addItem(value.toString(), selectColor);
      setSelectedItem(value);
      return this;
    }
  }

  /**
   * Combo box cell editor
   *
   * @author RIKEN
   */
  class ComboBoxCellEditor extends DefaultCellEditor {
    /** Serial number */
    private static final long serialVersionUID = 1L;

    /** Constructor */
    public ComboBoxCellEditor() {
      super(new JBackgroundComboBox());
    }

    /** Returns the editing component of the editor. */
    @Override
    public Component getTableCellEditorComponent(
        JTable table, Object value, boolean isSelected, int row, int column) {

      if (super.getComponent() instanceof JBackgroundComboBox) {
        JBackgroundComboBox combo = (JBackgroundComboBox) super.getComponent();
        combo.removeAllItems();
        // Choices
        String cellValue = (String) table.getValueAt(row, ACCESSMEMORYTYPE_COLUMN);
        ACCESSMEMORY_TYPE[] listmemory = ACCESSMEMORY_TYPE.values();
        ACCESSMEMORY_TYPE type = null;
        try {
          type = ACCESSMEMORY_TYPE.getAccessMemoryType(cellValue);
        } catch (Exception ex) {
        }
        for (int i = 0; i < listmemory.length; i++) {
          RequiredBF mem = propertiesMemoryband.getRequiredBF(listmemory[i]);
          combo.addItem(listmemory[i].getName(), mem != null ? mem.getBackColor() : null);
        }
        if (type == null) {
          combo.addItem(cellValue, null);
        }
        combo.setSelectedItem(value);
      }
      return super.getTableCellEditorComponent(table, value, isSelected, row, column);
    }
  }

  /**
   * Create a variable access destination memory table model.
   *
   * @return Variable access destination memory table model
   */
  private DefaultTableModel createVariableTableModel() {
    DefaultTableModel model = new DefaultTableModel();
    // Header column name
    model.setColumnCount(COLUMN_COUNT);
    String[] columns = COLUMN_HEADER;
    model.setColumnIdentifiers(columns);

    // Variable data
    if (this.selectedblocks == null) return model;
    List<Variable> list = new ArrayList<Variable>();
    for (IBlock block : this.selectedblocks) {
      Set<Variable> vars = block.getAllVariables();
      if (vars != null) {
        list.addAll(vars);
      }
    }
    List<Variable> listMemory = getAccessVariables(list);
    // Variable sorting
    java.util.Collections.sort(
        listMemory,
        new java.util.Comparator<Variable>() {
          public int compare(Variable o1, Variable o2) {
            Variable src1 = (Variable) o1;
            Variable src2 = (Variable) o2;
            if (src1.getDefinition() == null) return -1;
            if (src2.getDefinition() == null) return -1;
            boolean isscalar1 = (src1.getDefinition().get_dimension_size() <= 0);
            boolean isscalar2 = (src2.getDefinition().get_dimension_size() <= 0);
            // scalar && array
            if (isscalar1 && !isscalar2) return -1;
            // array && scalar
            else if (!isscalar1 && isscalar2) return 1;
            // Variable name comparison
            String text1 = src1.getVariableString();
            String text2 = src2.getVariableString();
            if (text1 == null) return -1;
            return text1.compareTo(text2);
          }
        });

    if (listMemory == null || listMemory.size() <= 0) return model;
    Object[][] tabledata = new Object[listMemory.size()][columns.length];
    for (int row = 0; row < listMemory.size(); row++) {
      Variable var = listMemory.get(row);
      if (var == null) continue;
      if (var.getDefinition() == null) continue;
      int col = 0;
      tabledata[row][col] = getVariables(list, var); // List<Variable>
      tabledata[row][++col] = var.getVariableString();
      if (var.getDefinition().get_dimension_size() > 0) {
        tabledata[row][++col] = "array";
      } else {
        tabledata[row][++col] = "scalar";
      }
      tabledata[row][++col] = var.getDefinition().toString();
      // Selected memory access destination
      String memText = getAccessMemoryString(list, var);
      tabledata[row][++col] = memText; // For combo box cell display
      tabledata[row][++col] = memText; // For adding combo box list
    }
    model.setDataVector(tabledata, columns);
    return model;
  }

  /**
   * Get the selected block
   *
   * @return selection block
   */
  public IBlock[] getSelectedBlocks() {
    return this.selectedblocks;
  }

  /**
   * Set the selection block.
   *
   * @param blocks selection blocks
   */
  public void setSelectedblocks(IBlock[] blocks) {
    this.selectedblocks = blocks;
    this.modelAccess = createVariableTableModel();
    createVariableTable(this.modelAccess);
  }

  /**
   * Create a variable access destination memory table.
   *
   * @param model Table model
   * @return Variable access destination memory table
   */
  private JTable createVariableTable(DefaultTableModel model) {
    if (this.tblAccess == null) {
      tblAccess = new JTable();
    }
    if (model == null) return this.tblAccess;
    this.tblAccess.setModel(model);
    this.tblAccess.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    this.tblAccess.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
    this.tblAccess.setColumnSelectionAllowed(false);
    this.tblAccess.setDefaultEditor(Object.class, null);
    this.tblAccess.getTableHeader().setReorderingAllowed(false);
    this.tblAccess.setAutoCreateRowSorter(true);

    // Column width setting
    for (int i = 0; i < this.tblAccess.getColumnModel().getColumnCount(); i++) {
      TableColumn col = this.tblAccess.getColumnModel().getColumn(i);
      // Set the column width.
      if (i < COLUMN_MINWIDTH.length) {
        col.setMinWidth(COLUMN_MINWIDTH[i]);
        // If the maximum column width is set (> 0), set the maximum column width to make it a fixed
        // column width.
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

    // Create and display access destination combo box
    TableColumn col = this.tblAccess.getColumnModel().getColumn(MEMORYACCESS_COLUMN);
    ComboBoxCellEditor editor = new ComboBoxCellEditor();
    RendererComboBox renderer = new RendererComboBox();
    col.setCellEditor(editor);
    col.setCellRenderer(renderer);
    this.tblAccess.setRowHeight(TABLE_ROWHEIGHT);

    return this.tblAccess;
  }

  /**
   * Get the memory access destination variable list
   *
   * @param listVariable Variable list
   * @return Memory access destination variable list
   */
  private List<Variable> getAccessVariables(List<Variable> listVariable) {
    List<Variable> list = new ArrayList<Variable>();
    for (Variable var : listVariable) {
      // Real number, array
      if (!var.isMemoryAccess()) continue;
      if (!containsVariables(list, var)) {
        list.add(var);
      }
    }

    return list;
  }

  /**
   * Check if the variable list contains variables.
   *
   * @param list Variable list
   * @param value variable
   * @return true = variable list
   */
  private boolean containsVariables(List<Variable> list, Variable value) {
    List<Variable> vars = getVariables(list, value);
    return (vars != null && vars.size() > 0);
  }

  /**
   * Get the same variable list as the variable from the variable list
   *
   * @param listVar Variable list
   * @param value variable
   * @return true = variable list
   */
  private List<Variable> getVariables(List<Variable> listVar, Variable value) {
    if (listVar == null || listVar.size() <= 0) return null;
    if (value == null) return null;
    if (value.getVariableString() == null) return null;
    List<Variable> listEquals = new ArrayList<Variable>();
    for (Variable var : listVar) {
      if (var == null) continue;
      // The definition is the same
      if (value.getDefinition() != var.getDefinition()) continue;
      if (value.equalsVariable(var)) {
        listEquals.add(var);
      }
    }
    if (listEquals.size() > 0) {
      return listEquals;
    }

    return null;
  }

  /**
   * Get the access destination memory string.
   *
   * @param listSelected Variable list
   * @param var variable
   * @return Access memory string
   */
  private String getAccessMemoryString(List<Variable> listSelected, Variable var) {
    if (var == null || var.getDefinition() == null)
      return ACCESSMEMORY_TYPE.getDefaultType().getName();
    ACCESSMEMORY_TYPE defaulttype = ACCESSMEMORY_TYPE.getDefaultType(var.getDefinition());

    // Get the list of other access destination settings in the selection.
    List<Variable> varsSelected = getVariables(listSelected, var);
    if (varsSelected == null || varsSelected.size() <= 0) return defaulttype.getName();
    List<ACCESSMEMORY_TYPE> listMemSelected = getAccessMemory(varsSelected);
    String buf = getAccessMemoryString(listMemSelected);
    if (buf != null && !buf.isEmpty()) {
      return buf;
    }

    // It doesn't exist, so get it from the configured list.
    List<Variable> varsSave = getVariables(this.propertiesVariable.getListVariable(), var);
    if (varsSave == null || varsSave.size() <= 0) return defaulttype.getName();
    List<ACCESSMEMORY_TYPE> listMemSave = getAccessMemory(varsSave);
    buf = getAccessMemoryString(listMemSave);
    if (buf != null && !buf.isEmpty()) {
      return buf;
    }

    return buf;
  }

  /**
   * Get the access destination memory string.
   *
   * @param listMem Access memory list
   * @return Access memory string
   */
  private String getAccessMemoryString(List<ACCESSMEMORY_TYPE> listMem) {
    if (listMem == null || listMem.size() <= 0) return null;
    if (listMem.size() == 1) {
      return listMem.get(0).getName();
    }
    // Since there are multiple access destinations, combine the abbreviations separated by commas.
    String buf = "";
    for (ACCESSMEMORY_TYPE type : listMem) {
      if (!buf.isEmpty()) buf += ",";
      buf += type.getShortname();
    }
    return buf;
  }

  /**
   * Get the access destination memory list.
   *
   * @param listVar Variable list
   * @return Access memory list
   */
  private List<ACCESSMEMORY_TYPE> getAccessMemory(List<Variable> listVar) {
    if (listVar == null || listVar.size() <= 0) return null;
    List<ACCESSMEMORY_TYPE> listMem = new ArrayList<ACCESSMEMORY_TYPE>();
    for (Variable item : listVar) {
      if (item == null) continue;
      ACCESSMEMORY_TYPE type = item.getMemoryType();
      if (type == null) continue;
      if (!listMem.contains(type)) {
        listMem.add(type);
      }
    }
    if (listMem.size() <= 0) return null;
    return listMem;
  }

  /** Make batch changes to the access destination memory. */
  private void setAllAccessMemory() {
    // Access memory to apply to all
    Object selobj = this.cmbAllAccess.getSelectedItem();
    String value = null;
    if (selobj instanceof String) {
      value = (String) selobj;
    } else if (selobj instanceof RequiredBF) {
      value = ((RequiredBF) selobj).getName();
    }

    if (this.chkSelect.isSelected()) {
      int[] rows = this.tblAccess.getSelectedRows();
      for (int row : rows) {
        // Change the value of the table cell.
        setTableCellValueAt(value, row, MEMORYACCESS_COLUMN);
      }
    }
    if (this.chkArray.isSelected() || this.chkScalar.isSelected()) {
      for (int row = 0; row < this.tblAccess.getRowCount(); row++) {
        String type = (String) this.tblAccess.getValueAt(row, ARRAYSCALAR_COLUMN);
        if (this.chkArray.isSelected()) {
          if (this.chkArray.getText().equalsIgnoreCase(type)) {
            setTableCellValueAt(value, row, MEMORYACCESS_COLUMN);
            continue;
          }
        }
        if (this.chkScalar.isSelected()) {
          if (this.chkScalar.getText().equalsIgnoreCase(type)) {
            setTableCellValueAt(value, row, MEMORYACCESS_COLUMN);
            continue;
          }
        }
      }
    }
    return;
  }

  /**
   * Change the value of the table cell.
   *
   * @param value Change value
   * @param row row index
   * @param col column index
   */
  private void setTableCellValueAt(String value, int row, int col) {
    Object obj = this.tblAccess.getValueAt(row, col);
    Object objEditor = this.tblAccess.getCellEditor(row, col);
    Object objRenderer = this.tblAccess.getCellRenderer(row, col);
    if (obj instanceof JComboBox) {
      ((JComboBox<?>) obj).setSelectedItem(value);
    } else {
      this.tblAccess.setValueAt(value, row, col);
    }
    if (objEditor instanceof ComboBoxCellEditor) {
      Component cmp = ((ComboBoxCellEditor) objEditor).getComponent();
      if (cmp instanceof JComboBox) {
        ((JComboBox<?>) cmp).setSelectedItem(value);
      }
    }
    if (objRenderer instanceof JComboBox) {
      ((JComboBox<?>) objRenderer).setSelectedItem(value);
    }
  }

  /**
   * Get the value of the table cell.
   *
   * @param row row index
   * @param col column index
   * @return table cell value
   */
  private String getTableCellValueAt(int row, int col) {
    String value = null;
    Object obj = this.tblAccess.getValueAt(row, col);
    Object objEditor = this.tblAccess.getCellEditor(row, col);
    if (obj instanceof JComboBox) {
      value = (String) ((JComboBox<?>) obj).getSelectedItem();
    } else {
      value = (String) this.tblAccess.getValueAt(row, col);
    }
    return value;
  }

  /**
   * Set the access destination memory to the variable.
   *
   * @param temporary Temporary setting flag: true = Does not apply to source view.
   */
  @SuppressWarnings("unchecked")
  private void setVariableAccessMemory(boolean temporary) {
    for (int row = 0; row < this.tblAccess.getRowCount(); row++) {
      // Variable object
      Variable var = null;
      List<Variable> list = null;
      Object obj = this.tblAccess.getValueAt(row, 0);
      if (obj == null) continue;
      if (obj instanceof Variable) {
        var = (Variable) obj;
      } else if (obj instanceof List) {
        list = (List<Variable>) obj;
      }
      if (var == null && list == null) continue;
      if (list == null) {
        list = new ArrayList<Variable>();
        list.add(var);
      }
      String value = getTableCellValueAt(row, MEMORYACCESS_COLUMN);
      ACCESSMEMORY_TYPE type = ACCESSMEMORY_TYPE.getAccessMemoryType(value);
      if (type == null) continue;
      for (Variable data : list) {
        if (temporary) {
          // Temporary setting Set to access destination memory.
          data.setTemporaryMemoryType(type);
        } else if (type == ACCESSMEMORY_TYPE.DEFAULT) {
          // Cancel the access destination memory setting
          data.clearMemoryType();
          // Add a variable to the variable access destination memory property
          this.propertiesVariable.removeVariable(data);
        } else {
          // Set the access destination memory
          data.setMemoryType(type);
          // Clear the temporary access destination memory.
          data.setTemporaryMemoryType(null);
          // Add a variable to the variable access destination memory property
          this.propertiesVariable.addVariable(data);
        }
      }
    }

    if (!temporary) {
      // Apply variable access destination memory property
      this.propertiesVariable.firePropertyChange();
    }
  }

  /**
   * Get variable access destination memory property
   *
   * @return variable access destination memory property
   */
  public VariableMemoryProperties getPropertiesVariable() {
    return propertiesVariable;
  }

  /**
   * Get variable access destination memory property
   *
   * @param properties Variable access destination memory property
   */
  public void setPropertiesVariable(VariableMemoryProperties properties) {
    this.propertiesVariable = properties;
  }

  /**
   * Set the memory performance calculation result dialog.
   *
   * @param dialog Memory performance calculation result dialog
   */
  public void setMemoryPerformanceDialog(RequiredBFDialog dialog) {
    this.nextDialog = dialog;
  }

  /**
   * Set the parent dialog flag.
   *
   * @param owner Parent dialog flag
   */
  public void setOwnerDialog(boolean owner) {
    this.ownerDialog = owner;
    // Hide the calculation panel
    panelCalculate.setVisible(this.ownerDialog);
    // Change the text of the OK button
    String text = Message.getString("dialog.common.button.ok"); // OK
    if (!owner) {
      text = Message.getString("dialog.common.button.recalculate"); // Recalculation
    }
    this.btnOk.setText(text);
  }

  /**
   * Set the request Byte / FLOP setting property.
   *
   * @param properties Request Byte / FLOP configuration properties
   */
  public void setPropertiesMemoryband(RequiredBFProperties properties) {
    this.propertiesMemoryband = properties;
    updateProperties();
  }

  /** Update the access memory properties. */
  private void updateProperties() {

    if (this.propertiesMemoryband != null) {
      // Access memory
      ACCESSMEMORY_TYPE[] listmemory = ACCESSMEMORY_TYPE.values();
      if (this.propertiesMemoryband != null) {
        this.cmbAllAccess.removeAllItems();
        for (ACCESSMEMORY_TYPE type : listmemory) {
          RequiredBF mem = this.propertiesMemoryband.getRequiredBF(type);
          this.cmbAllAccess.addItem(type.getName(), mem != null ? mem.getBackColor() : null);
        }
        this.cmbAllAccess.setSelectedIndex(0);
      }
    }
  }
}
