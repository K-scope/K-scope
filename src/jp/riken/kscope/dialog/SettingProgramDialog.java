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
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.File;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.data.Program;
import jp.riken.kscope.properties.ProgramProperties;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * External tool settings dialog class
 *
 * @author RIKEN
 */
public class SettingProgramDialog extends javax.swing.JDialog
    implements ActionListener, ListSelectionListener, ItemListener {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Cancel button */
  private JButton btnCancel;
  /** OK button */
  private JButton btnOk;
  /** Apply button */
  private JButton btnApply;
  /** Extension name label */
  private JLabel lblName;
  /** Extension name text box */
  private JTextField txtName;
  /** Pattern label */
  private JLabel lblPattern;
  /** Pattern text box */
  private JTextField txtPattern;
  /** Extension option button */
  private JRadioButton chkExt;
  /** Regular expression option button */
  private JRadioButton chkRegex;
  /** External tool label */
  private JLabel lblProgram;
  /** Association */
  private JCheckBox chkRelation;
  /** Program text box */
  private JTextField txtProgram;
  /** Registration button */
  private JButton btnReg;
  /** Add button */
  private JButton btnAdd;
  /** Delete button */
  private JButton btnDel;
  /** Clear button */
  private JButton btnClear;
  /** Program reference path */
  private JButton btnExePath;
  /** Startup option label */
  private JLabel lblOption;
  /** Boot options */
  private JTextField txtOption;
  /** External tool list */
  private JTable tblProgram;
  /** External tool list data */
  private DefaultTableModel modelProgram;
  /** External tool settings panel */
  private JPanel panelProgram;

  /** Dialog return value */
  private int result = Constant.CANCEL_DIALOG;

  /** External tool properties */
  ProgramProperties properities;
  /** Settings list header */
  private final String[] COLUMN_HEADER = {
    Message.getString("settingprogramdialog.column_header.preferencename"), // Setting name
    Message.getString(
        "settingprogramdialog.column_header.suffix-regular"), // Extension / Regular expression
    Message.getString("settingprogramdialog.column_header.kind"), // Type
    Message.getString("settingprogramdialog.column_header.externaltool"), // External tools
    Message.getString("settingprogramdialog.column_header.argument")
  }; // Start argument

  private final int[] COLUMN_MINWIDTH = {120, 160, 80, 240, 120};
  private final int[] COLUMN_MAXWIDTH = {0, 0, 0, 80, 80};
  private final int COLUMN_COUNT = 5;

  /**
   * Constructor
   *
   * @param frame Parent frame
   */
  public SettingProgramDialog(JFrame frame) {
    super(frame);
    initGUI();

    // Initial display
    clearProgram();
  }

  /**
   * Constructor
   *
   * @param frame Parent frame
   * @param modal true = Show modal dialog
   */
  public SettingProgramDialog(Frame frame, boolean modal) {
    super(frame, modal);
    initGUI();

    // Initial display
    clearProgram();
  }

  /**
   * Constructor
   *
   * @param frame Parent frame
   * @param modal true = Show modal dialog
   * @param properities External tool properties
   */
  public SettingProgramDialog(Frame frame, boolean modal, ProgramProperties properities) {
    super(frame, modal);
    initGUI();
    setProgramProperties(properities);

    // Initial display
    clearProgram();
  }

  /** Initialize the GUI. */
  private void initGUI() {
    try {
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
          btnApply = new JButton();
          panelButtons.add(btnApply);
          btnApply.setText(Message.getString("dialog.common.button.apply")); // Apply
          btnApply.setPreferredSize(buttonSize);
          btnApply.addActionListener(this);
        }
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
      // Content panel
      {
        JPanel panelContent = new JPanel();
        BorderLayout panelContentLayout = new BorderLayout();
        getContentPane().add(panelContent, BorderLayout.CENTER);
        Border border = new EmptyBorder(7, 7, 0, 7);
        panelContent.setBorder(border);
        panelContent.setLayout(panelContentLayout);

        // External tool list
        {
          JPanel panelList = new JPanel();
          BorderLayout panelListLayout = new BorderLayout();
          panelList.setLayout(panelListLayout);
          panelContent.add(panelList, BorderLayout.CENTER);
          {
            JLabel lblList = new JLabel();
            panelList.add(lblList, BorderLayout.NORTH);
            lblList.setText(
                Message.getString(
                    "settingprogramdialog.label.externaltoollist")); // External tool list
          }
          {
            JScrollPane scrollList = new JScrollPane();
            scrollList.setHorizontalScrollBarPolicy(
                ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            scrollList.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
            panelList.add(scrollList, BorderLayout.CENTER);
            {
              modelProgram = new DefaultTableModel();
              modelProgram.setColumnCount(COLUMN_COUNT);
              // Header column name
              String[] columns = COLUMN_HEADER;
              modelProgram.setColumnIdentifiers(columns);
              tblProgram = new JTable();
              scrollList.setViewportView(tblProgram);
              tblProgram.setModel(modelProgram);
              tblProgram.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
              tblProgram.getSelectionModel().addListSelectionListener(this);
              tblProgram.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
              tblProgram.setColumnSelectionAllowed(false);
              tblProgram.setDefaultEditor(Object.class, null);

              // Column width setting
              for (int i = 0; i < tblProgram.getColumnModel().getColumnCount(); i++) {
                TableColumn col = tblProgram.getColumnModel().getColumn(i);
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
          {
            JLabel lblSettings = new JLabel();
            lblSettings.setText(
                Message.getString("settingkeyworddialog.label.preference")); // Configuration
            panelSettings.add(lblSettings, BorderLayout.NORTH);
          }
          panelProgram = new JPanel();
          GridBagLayout panelProgramLayout = new GridBagLayout();
          panelProgramLayout.columnWidths = new int[] {80, 80, 80};
          panelProgramLayout.rowHeights = new int[] {30, 30, 30, 30, 30, 30, 30, 30, 7};
          panelProgramLayout.columnWeights = new double[] {0, 0.1, 0.1};
          panelProgramLayout.rowWeights = new double[] {0, 0, 0, 0, 0, 0, 0, 0, 1};
          panelSettings.add(panelProgram, BorderLayout.CENTER);
          panelProgram.setLayout(panelProgramLayout);
          panelProgram.setPreferredSize(new java.awt.Dimension(420, 260));

          // Valid check button
          EtchedBorder titleBorder = (EtchedBorder) BorderFactory.createEtchedBorder();
          Border borderKeyword = new CompoundBorder(titleBorder, new EmptyBorder(7, 7, 0, 7));
          panelProgram.setBorder(borderKeyword);

          // External tool name
          {
            lblName = new JLabel();
            panelProgram.add(
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
            lblName.setText(Message.getString("settingprogramdialog.label.name")); // name
          }
          {
            txtName = new JTextField();
            panelProgram.add(
                txtName,
                new GridBagConstraints(
                    1,
                    0,
                    2,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.WEST,
                    GridBagConstraints.HORIZONTAL,
                    new Insets(0, 0, 0, 0),
                    0,
                    0));
          }
          // Pattern
          {
            lblPattern = new JLabel();
            panelProgram.add(
                lblPattern,
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
            lblPattern.setText(Message.getString("settingprogramdialog.label.pattern")); // pattern
          }
          {
            txtPattern = new JTextField();
            panelProgram.add(
                txtPattern,
                new GridBagConstraints(
                    1,
                    1,
                    2,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.WEST,
                    GridBagConstraints.HORIZONTAL,
                    new Insets(0, 0, 0, 0),
                    0,
                    0));
          }
          {
            ButtonGroup group = new ButtonGroup();
            // Extension option button
            {
              chkExt = new JRadioButton();
              panelProgram.add(
                  chkExt,
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
              chkExt.setText(Message.getString("settingprogramdialog.label.suffix")); // extension
            }
            // Regular expression option button
            {
              chkRegex = new JRadioButton();
              panelProgram.add(
                  chkRegex,
                  new GridBagConstraints(
                      2,
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
              chkRegex.setText(
                  Message.getString("searchfinddialog.checkbox.regex")); // Regular expressions
            }
            group.add(chkExt);
            group.add(chkRegex);
          }
          // Extension description
          {
            JLabel label1 = new JLabel();
            panelProgram.add(
                label1,
                new GridBagConstraints(
                    1,
                    3,
                    2,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.WEST,
                    GridBagConstraints.HORIZONTAL,
                    new Insets(0, 0, 0, 0),
                    0,
                    0));
            label1.setText(
                Message.getString(
                    "settingprogramdialog.label.suffix.desc")); // You can set multiple extensions
            // separated by commas.
            // JLabel label2 = new JLabel();
            // label2.setText (Message.getString ("settingprogramdialog.label.suffix.ex")); //
            // (Example) LOG, TXT, DAT

            // Box panel
            // Box box = Box.createVerticalBox();
            // box.add(label1);
            // box.add(label2);
            // panelProgram.add(box, new GridBagConstraints(1, 3, 2, 1, 0.0, 0.0,
            // GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0));
          }
          // External tools
          {
            lblProgram = new JLabel();
            panelProgram.add(
                lblProgram,
                new GridBagConstraints(
                    0,
                    4,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.WEST,
                    GridBagConstraints.NONE,
                    new Insets(0, 0, 0, 0),
                    0,
                    0));
            lblProgram.setText(
                Message.getString(
                    "settingprogramdialog.column_header.externaltool")); // External tools
          }
          {
            chkRelation = new JCheckBox();
            panelProgram.add(
                chkRelation,
                new GridBagConstraints(
                    1,
                    4,
                    2,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.WEST,
                    GridBagConstraints.HORIZONTAL,
                    new Insets(0, 0, 0, 0),
                    0,
                    0));
            chkRelation.setText(
                Message.getString(
                    "settingprogramdialog.label.programassociation")); // Association program
            chkRelation.addItemListener(this);
          }
          {
            // External tool program path
            txtProgram = new JTextField();

            // Browse button
            btnExePath = new JButton();
            btnExePath.setText(Message.getString("dialog.common.button.refer")); // reference
            btnExePath.setPreferredSize(new java.awt.Dimension(48, 22));
            btnExePath.setMaximumSize(new java.awt.Dimension(48, 22));
            btnExePath.setMargin(new Insets(0, 3, 0, 3));
            btnExePath.addActionListener(this);

            // Box panel
            Box box = Box.createHorizontalBox();
            box.add(txtProgram);
            box.add(btnExePath);
            panelProgram.add(
                box,
                new GridBagConstraints(
                    1,
                    5,
                    2,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.WEST,
                    GridBagConstraints.HORIZONTAL,
                    new Insets(0, 0, 0, 0),
                    0,
                    0));
          }
          // Optional
          {
            lblOption = new JLabel();
            panelProgram.add(
                lblOption,
                new GridBagConstraints(
                    0,
                    6,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.WEST,
                    GridBagConstraints.NONE,
                    new Insets(0, 0, 0, 0),
                    0,
                    0));
            lblOption.setText(
                Message.getString("settingprogramdialog.column_header.argument")); // Start argument
          }
          {
            txtOption = new JTextField();
            panelProgram.add(
                txtOption,
                new GridBagConstraints(
                    1,
                    6,
                    2,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.WEST,
                    GridBagConstraints.HORIZONTAL,
                    new Insets(0, 0, 0, 0),
                    0,
                    0));
          }

          // Start argument description
          {
            // Set startup options when the program starts.
            // The following macros are available. (Source file only)
            // % F = filename
            // % L = line number
            // (EMACS example: +% L)
            JLabel label =
                new JLabel(Message.getString("settingprogramdialog.discription.argument"));
            panelProgram.add(
                label,
                new GridBagConstraints(
                    1,
                    7,
                    2,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.WEST,
                    GridBagConstraints.BOTH,
                    new Insets(0, 0, 0, 0),
                    0,
                    0));
          }

          // External tool add / remove button panel
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
            btnClear.setText(Message.getString("dialog.common.button.clear")); // Clear
            btnClear.setPreferredSize(minSize);
            btnClear.setMargin(minInsets);
            btnClear.addActionListener(this);
          }
        }
      }

      setTitle(
          Message.getString("projectsettingtoolsaction.setup.status")); // External tool settings
      this.setSize(670, 460);

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
   * Set external tool setting properties.
   *
   * @param properities External tool settings properties
   */
  public void setProgramProperties(ProgramProperties properities) {
    this.properities = properities;

    // Create an external tool list
    int count = properities.getProgramCount();
    for (int i = 0; i < count; i++) {
      Program program = properities.getProgram(i);

      // Create row data
      Object[] column = createProgramRowData(program);
      // Add line
      modelProgram.addRow(column);
    }
  }

  /**
   * Get external tool settings.
   *
   * @return External tool settings properties
   */
  public ProgramProperties getProgramProperties() {

    // Get the external tool settings from the external tool settings list
    int count = modelProgram.getRowCount();
    // Clear external tool settings
    this.properities.clearProgram();
    for (int i = 0; i < count; i++) {
      Program prog = new Program();

      Object cell;
      // name
      cell = modelProgram.getValueAt(i, 0);
      String keyname = Message.getKey((String) cell);
      if (StringUtils.isNullOrEmpty(keyname)) {
        // Set the input name by changing the name
        prog.setName((String) cell);
      } else {
        // Set the name key by not changing the name.
        prog.setName(keyname);
      }
      // Pattern
      cell = modelProgram.getValueAt(i, 1);
      prog.setPattern((String) cell);
      // Extension or regular expression
      boolean exts = false;
      cell = modelProgram.getValueAt(i, 2);
      if (Message.getString("settingprogramdialog.label.suffix")
          .equals((String) cell)) { // extension
        exts = true;
      }
      prog.setExts(exts); // extension
      prog.setRegex(!exts); // Regular expressions

      // External program
      boolean relation = false;
      cell = modelProgram.getValueAt(i, 3);
      if (Message.getString("settingprogramdialog.label.association")
          .equals((String) cell)) { // Association
        relation = true;
      }
      prog.setRelation(relation);
      if (!relation) {
        prog.setExename((String) cell);
      }

      // Optional
      cell = modelProgram.getValueAt(i, 4);
      if ((String) cell != null && !((String) cell).isEmpty()) {
        prog.setOption((String) cell);
      }
      // Add external tools
      properities.addProgram(prog);
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
      getProgramProperties();

      // Fire a change event
      this.properities.firePropertyChange();

      // Close the dialog.
      dispose();
      return;
    }
    // Apply
    else if (event.getSource() == this.btnApply) {
      this.result = Constant.OK_DIALOG;

      // Get the changes.
      getProgramProperties();

      // Fire a change event
      this.properities.firePropertyChange();

      return;
    }
    // close
    else if (event.getSource() == this.btnCancel) {
      this.result = Constant.CANCEL_DIALOG;
      // Close the dialog.
      dispose();
      return;
    }
    // Reference
    else if (event.getSource() == this.btnExePath) {
      // Display the file selection dialog.

      // Display the file selection dialog.
      File[] selected =
          SwingUtils.showOpenFileDialog(
              this,
              Message.getString("settingprogramdialog.selectfiledialog.title"),
              null,
              null,
              false); // Program selection
      if (selected == null || selected.length <= 0) return;

      // Set the selection program
      this.txtProgram.setText(selected[0].getAbsolutePath());

      return;
    }
    // update
    else if (event.getSource() == this.btnReg) {
      // Check the input
      if (this.validateProgram(false) == false) {
        // Typing error
        return;
      }
      // Check if the program is set
      if (!chkProgramPath()) {
        JOptionPane.showMessageDialog(
            this,
            Message.getString(
                "settingprogramdialog.errordialog.noprogram.message"), // Enter the program.
            Message.getString("dialog.common.error"), // error
            JOptionPane.ERROR_MESSAGE);
        return;
      }

      // Update
      Program prog = this.getProgram();
      setProgramList(prog);

    }
    // add to
    else if (event.getSource() == this.btnAdd) {
      // Check the input
      if (this.validateProgram(true) == false) {
        // Typing error
        return;
      }
      // Check if the program is set
      if (!chkProgramPath()) {
        JOptionPane.showMessageDialog(
            this,
            Message.getString(
                "settingprogramdialog.errordialog.noprogram.message"), // Enter the program.
            Message.getString("dialog.common.error"), // error
            JOptionPane.ERROR_MESSAGE);
        return;
      }

      // make an addition
      Program prog = this.getProgram();
      addProgramList(prog);
    }
    // Delete
    else if (event.getSource() == this.btnDel) {
      // Selected line
      int selectedrow = this.tblProgram.getSelectedRow();
      if (selectedrow < 0) return;

      int option =
          JOptionPane.showConfirmDialog(
              this,
              Message.getString(
                  "settingprogramdialog.confirmationdialog.delete.message"), // Delete Are you sure
              // you want to?
              Message.getString(
                  "settingprogramdialog.confirmationdialog.delete.title"), // Remove external tools
              JOptionPane.OK_CANCEL_OPTION);
      if (option == JOptionPane.OK_OPTION) {
        // Delete
        Program orog = this.getProgram();
        removeProgramList(orog);
      }
    }
    // new
    else if (event.getSource() == this.btnClear) {
      // Switch the enable of the setting panel
      setSettingPanelEnabled(true);
      // Clear the external tool settings panel.
      clearProgram();
    }
  }

  /**
   * Toggle the enable of the setting panel
   *
   * @param enabled true = enabled
   */
  private void setSettingPanelEnabled(boolean enabled) {
    /** Setting name label */
    this.lblName.setEnabled(enabled);
    /** Setting name text box */
    this.txtName.setEnabled(enabled);
    /** Pattern label */
    this.lblPattern.setEnabled(enabled);
    /** Pattern text box */
    this.txtPattern.setEnabled(enabled);
    /** Extension checkbox */
    this.chkExt.setEnabled(enabled);
    /** Regular expression checkbox */
    this.chkRegex.setEnabled(enabled);
    /** External tool label */
    this.lblProgram.setEnabled(enabled);
    /** Association checkbox */
    this.chkRelation.setEnabled(enabled);
    /** External tool textbox */
    this.txtProgram.setEnabled(enabled);
    /** External program reference button */
    this.btnExePath.setEnabled(enabled);
    /** Options */
    this.txtOption.setEnabled(enabled);
  }

  /**
   * External tool setting list change event. <br>
   * Set the external tool setting information of the selected line in the setting panel.
   *
   * @param event Event information
   */
  @Override
  public void valueChanged(ListSelectionEvent event) {

    if (event.getSource() == this.tblProgram.getSelectionModel()) {
      // Get the selected row.
      int selectedrow = this.tblProgram.getSelectedRow();
      if (selectedrow < 0) return;

      Object cell;
      // name
      cell = modelProgram.getValueAt(selectedrow, 0);
      this.txtName.setText((String) cell);
      // Pattern
      cell = modelProgram.getValueAt(selectedrow, 1);
      this.txtPattern.setText((String) cell);
      // Extension or regular expression
      boolean exts = false;
      cell = modelProgram.getValueAt(selectedrow, 2);
      if (Message.getString("settingprogramdialog.label.suffix")
          .equals((String) cell)) { // extension
        exts = true;
      }
      this.chkExt.setSelected(exts); // extension
      this.chkRegex.setSelected(!exts); // Regular expressions

      // External program
      boolean relation = false;
      cell = modelProgram.getValueAt(selectedrow, 3);
      if (Message.getString("settingprogramdialog.label.association")
          .equals((String) cell)) { // Association
        relation = true;
      }
      this.chkRelation.setSelected(relation);
      if (!relation) {
        this.txtProgram.setText((String) cell);
      } else {
        this.txtProgram.setText(null);
      }

      // Startup options
      cell = modelProgram.getValueAt(selectedrow, 4);
      this.txtOption.setText((String) cell);
    }
    return;
  }

  /**
   * Create row data for external tool setting list model.
   *
   * @param prog External tool data
   * @return line data
   */
  private Object[] createProgramRowData(Program prog) {

    Object[] column = new Object[5];
    // Setting name
    String name = prog.getName();
    if (Message.containsKey(name)) {
      column[0] = Message.getString(name);
    } else {
      column[0] = name;
    }
    // Pattern
    column[1] = prog.getPattern();
    // Extension OR regular expression
    if (prog.isExts()) {
      column[2] = Message.getString("settingprogramdialog.label.suffix"); // extension
    } else if (prog.isRegex()) {
      column[2] = Message.getString("searchfinddialog.checkbox.regex"); // Regular expressions
    }
    // External program
    if (prog.isRelation()) {
      column[3] = Message.getString("settingprogramdialog.label.association"); // Association
    } else {
      column[3] = prog.getExename();
    }
    // Start argument
    column[4] = prog.getOption();

    return column;
  }

  /**
   * Get the external tool object from the external tool settings panel.
   *
   * @return External tool configuration object
   */
  private Program getProgram() {

    Program prog = new Program();

    // name
    prog.setName(this.txtName.getText());
    // Pattern
    prog.setPattern(this.txtPattern.getText());
    // extension
    prog.setExts(this.chkExt.isSelected());
    // Regular expressions
    prog.setRegex(this.chkRegex.isSelected());

    // Association
    prog.setRelation(this.chkRelation.isSelected());
    // External program
    prog.setExename(this.txtProgram.getText());
    // Optional
    prog.setOption(this.txtOption.getText());

    return prog;
  }

  /**
   * Update external tool settings.
   *
   * @param prog External tool settings
   */
  private void setProgramList(Program prog) {

    // Selected line
    int selectedrow = this.tblProgram.getSelectedRow();
    if (selectedrow < 0) return;

    // Create row data
    Object[] column = createProgramRowData(prog);
    // Line update
    modelProgram.removeRow(selectedrow);
    modelProgram.insertRow(selectedrow, column);
    this.tblProgram.setRowSelectionInterval(selectedrow, selectedrow);
  }

  /**
   * Add external tool settings.
   *
   * @param prog External tool settings
   */
  private void addProgramList(Program prog) {
    // Create row data
    Object[] column = createProgramRowData(prog);
    // Add line
    modelProgram.addRow(column);
    int selectedrow = modelProgram.getRowCount() - 1;
    this.tblProgram.setRowSelectionInterval(selectedrow, selectedrow);
  }

  /**
   * Delete external tool settings.
   *
   * @param prog External tool information
   */
  private void removeProgramList(Program prog) {
    // Selected line
    int selectedrow = this.tblProgram.getSelectedRow();
    if (selectedrow < 0) return;

    // Delete line
    modelProgram.removeRow(selectedrow);

    // Clear the external tool settings panel.
    clearProgram();
  }

  /** Clear the external tool settings panel. */
  private void clearProgram() {

    // Clear settings
    /** Setting name text box */
    this.txtName.setText(null);
    /** Pattern text box */
    this.txtPattern.setText(null);
    /** Extension checkbox */
    this.chkExt.setSelected(true);
    /** Regular expression checkbox */
    this.chkRegex.setSelected(false);
    /** Association checkbox */
    this.chkRelation.setSelected(true);
    /** External tool textbox */
    this.txtProgram.setText(null);
    this.txtProgram.setEnabled(false);
    this.btnExePath.setEnabled(false);
    /** Boot options */
    this.txtOption.setText(null);
  }

  /**
   * Check the input.
   *
   * @param addflag Add flag (true = add external tool settings)
   */
  private boolean validateProgram(boolean addflag) {
    // Pattern required
    String pattern = this.txtPattern.getText();

    // Check the input
    if (pattern == null || pattern.isEmpty()) {
      JOptionPane.showMessageDialog(
          this,
          Message.getString(
              "settingprogramdialog.errordialog.nosuffix.message"), // Please enter the extension.
          Message.getString("dialog.common.error"), // error
          JOptionPane.ERROR_MESSAGE);
      return false;
    }

    // Check for duplicate patterns.
    int selectedrow = this.tblProgram.getSelectedRow();
    int count = this.modelProgram.getRowCount();
    boolean exists = false;
    for (int i = 0; i < count; i++) {
      // External tool extension
      String cellKeyword = (String) (this.modelProgram.getValueAt(i, 2));
      if (pattern.equals(cellKeyword)) {
        if (addflag) {
          // In case of addition, the same external tool is prohibited
          exists = true;
          break;
        } else if (i != selectedrow) {
          // In the case of update, NG if the same external tool exists other than the update line
          exists = true;
          break;
        }
      }
    }
    if (exists) {
      JOptionPane.showMessageDialog(
          this,
          Message.getString(
              "settingprogramdialog.errordialog.duplication.message"), // Duplicate extensions
          // cannot be registered.
          Message.getString("dialog.common.error"), // error
          JOptionPane.ERROR_MESSAGE);
      return false;
    }

    return true;
  }

  /**
   * Check box check event
   *
   * @param event Event information
   */
  @Override
  public void itemStateChanged(ItemEvent event) {
    if (event.getSource() == this.chkRelation) {
      // Switching the enable of the external program
      boolean enabled = this.chkRelation.isSelected();
      this.txtProgram.setEnabled(!enabled);
      this.btnExePath.setEnabled(!enabled);
      this.txtOption.setEnabled(!enabled);
    }
  }

  /** Check if the program is specified */
  private boolean chkProgramPath() {
    if (!this.chkRelation.isSelected()) {
      if (this.txtProgram.getText().isEmpty()) return false;
    }
    return true;
  }
}
