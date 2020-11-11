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
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.Arrays;
import java.util.List;
import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;
import javax.swing.text.SimpleAttributeSet;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.data.ProjectPropertyValue;
import jp.riken.kscope.properties.ProjectProperties;
import jp.riken.kscope.properties.RemoteBuildProperties;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Project settings dialog
 *
 * @author RIKEN
 */
public class SettingProjectDialog extends javax.swing.JDialog
    implements ActionListener, ListSelectionListener {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  private static boolean debug = (System.getenv("DEBUG") != null);
  private static boolean debug_l2 = false;

  /** Last access folder */
  private String lastAccessFolder;

  /** Cancel button */
  private JButton btnCancel;
  /** OK button */
  private JButton btnOk;
  /** Apply button */
  private JButton btnApply;
  /** Registration button */
  private JButton btnReg;
  /** Project setting list */
  private JTable ppropertiesTable;
  /** Project setting list data */
  private DefaultTableModel modelProperties;
  /** Project Settings Panel */
  private JPanel panelProperty;

  // private DefaultComboBoxModel<String> list_model;
  private JButton manage_settings_files;
  private JComboBox<String> settings_list;
  private JCheckBox checkUseRemote;
  private String remote_service;

  /** Dialog return value */
  private int result = Constant.CANCEL_DIALOG;

  /** Project Properties */
  ProjectProperties properties;

  /** Column name */
  private final String[] COLUMN_HEADER = {
    "",
    Message.getString("settingprojectdialog.column_header.key"), // Key
    Message.getString("settingprojectdialog.column_header.type"), // type
    Message.getString("settingprogramdialog.label.name"), // name
    Message.getString("settingprojectdialog.column_header.value"), // value
    Message.getString("settingprojectdialog.column_header.message"), // message
    Message.getString("settingprojectdialog.column_header.clo"), // command line option
    Message.getString("settingprojectdialog.column_header.order") // order
  };

  /** Selection button */
  private JButton btnSelect;
  /** Value input text field */
  private JTextField txtValue;

  /** Selected properties */
  private ProjectPropertyValue selectedvalue;

  /**
   * Constructor
   *
   * @param owner parent frame
   * @param modal true = Show modal dialog
   */
  public SettingProjectDialog(Frame owner, boolean modal) {
    super(owner, modal);
    initDebug();
    initGUI();
  }

  /**
   * Constructor
   *
   * @param frame Parent frame
   */
  public SettingProjectDialog(Frame frame) {
    super(frame);
    initDebug();
    initGUI();
  }

  /**
   * Constructor
   *
   * @param frame Parent frame
   * @param modal true = Show modal dialog
   * @param properities Project configuration properties @ wbp.parser.constructor
   */
  public SettingProjectDialog(Frame frame, boolean modal, ProjectProperties properities) {
    super(frame, modal);
    initDebug();
    initGUI();
    setProjectProperties(properities);
  }

  private void initDebug() {
    if (debug) debug_l2 = (System.getenv("DEBUG").equalsIgnoreCase("high"));
  }

  /**
   * Set project settings.
   *
   * @param properties Source configuration properties
   */
  public void setProjectProperties(ProjectProperties properties) {

    ProjectPropertyValue[] values = properties.getPropertyValues();
    // add to table
    for (ProjectPropertyValue value : values) {
      // "PropertyValue", "key", "type", "name", "value", "message"
      Object[] rowData = new Object[8];
      rowData[0] = value;
      rowData[1] = value.getKey();
      rowData[2] = value.getType();
      rowData[3] = Message.getString(value.getName());
      rowData[4] = value.getValue();
      rowData[5] = Message.getString(value.getMessage());
      rowData[6] = value.getCommandlineOption();
      rowData[7] = value.getOrder();
      if (debug) {
        System.out.println(
            "Add row: "
                + rowData[1]
                + " : "
                + rowData[2]
                + " : "
                + rowData[3]
                + " : "
                + rowData[4]
                + " : "
                + rowData[5]
                + " : "
                + rowData[6]
                + " : "
                + rowData[7]);
      }
      if (!rowData[2].toString().equalsIgnoreCase("hidden")) {
        // Project properties with type=="hidden" will not show up and cannot be changed in Project
        // settings dialog
        this.modelProperties.addRow(rowData);
      }
    }

    this.properties = properties;

    return;
  }

  /** Initialize the GUI. */
  private void initGUI() {
    try {
      BorderLayout thisLayout = new BorderLayout();
      thisLayout.setHgap(5);
      thisLayout.setVgap(5);
      getContentPane().setLayout(thisLayout);
      settings_list = new JComboBox<String>();

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

        // Property list
        {
          JPanel panelList = new JPanel();
          panelList.setPreferredSize(new Dimension(300, 250));
          BorderLayout panelListLayout = new BorderLayout();
          panelList.setLayout(panelListLayout);
          panelContent.add(panelList, BorderLayout.WEST);
          {
            JLabel lblList = new JLabel();
            panelList.add(lblList, BorderLayout.NORTH);
            lblList.setText(
                Message.getString(
                    "settingprojectdialog.label.setupprojectlist")); // Project settings / Project
            // settings list
          }
          {
            JScrollPane scrollList = new JScrollPane();
            scrollList.setHorizontalScrollBarPolicy(
                ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            scrollList.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
            panelList.add(scrollList, BorderLayout.CENTER);
            {
              modelProperties = new DefaultTableModel();
              modelProperties.setColumnCount(COLUMN_HEADER.length);
              // Header column name
              String[] columns = COLUMN_HEADER;
              modelProperties.setColumnIdentifiers(columns);
              ppropertiesTable = new JTable();
              scrollList.setViewportView(ppropertiesTable);
              ppropertiesTable.setModel(modelProperties);
              ppropertiesTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
              ppropertiesTable.getSelectionModel().addListSelectionListener(this);
              ppropertiesTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
              ppropertiesTable.setDefaultRenderer(Object.class, new PropertiesTableRenderer());
              ppropertiesTable.setColumnSelectionAllowed(false);
              ppropertiesTable.setDefaultEditor(Object.class, null);

              // Column width setting
              // First column: PropertyValue: Hidden
              {
                TableColumn col = ppropertiesTable.getColumnModel().getColumn(0);
                col.setResizable(false);
                col.setMinWidth(0);
                col.setMaxWidth(0);
              }
              // Second column: Key: Hide
              {
                TableColumn col = ppropertiesTable.getColumnModel().getColumn(1);
                col.setResizable(false);
                col.setMinWidth(0);
                col.setMaxWidth(0);
              }
              // Third column: Type: Hidden
              {
                TableColumn col = ppropertiesTable.getColumnModel().getColumn(2);
                col.setResizable(false);
                col.setMinWidth(0);
                col.setMaxWidth(0);
              }
              // Fourth column: Name
              {
                TableColumn col = ppropertiesTable.getColumnModel().getColumn(3);
                col.setResizable(true);
                col.setMinWidth(130);
              }
              // 5th column: Value
              {
                TableColumn col = ppropertiesTable.getColumnModel().getColumn(4);
                col.setResizable(true);
                col.setMinWidth(165);
              }
              // 6th column: Message: Hidden
              {
                TableColumn col = ppropertiesTable.getColumnModel().getColumn(5);
                col.setResizable(false);
                col.setMinWidth(0);
                col.setMaxWidth(0);
              }
              // 7th column: Message: Hidden
              {
                TableColumn col = ppropertiesTable.getColumnModel().getColumn(6);
                col.setResizable(false);
                col.setMinWidth(0);
                col.setMaxWidth(0);
              }
            }
          }
        }
        // Settings panel
        {
          JPanel panelSettings = new JPanel();
          BorderLayout panelSettingsLayout = new BorderLayout();
          panelContent.add(panelSettings, BorderLayout.CENTER);
          Border borderSettings = new EmptyBorder(0, 7, 0, 0);
          panelSettings.setBorder(borderSettings);
          panelSettings.setLayout(panelSettingsLayout);
          {
            JLabel lblSettings = new JLabel();
            lblSettings.setText(
                Message.getString("mainmenu.project.config")); // Configuration / Configuration
            panelSettings.add(lblSettings, BorderLayout.NORTH);
          }
          this.panelProperty = new JPanel();
          GridBagLayout panelPropertyLayout = new GridBagLayout();
          panelPropertyLayout.columnWidths = new int[] {80, 100, 7, 7};
          panelPropertyLayout.rowHeights = new int[] {7, 30, 30, 7, 7};
          panelPropertyLayout.columnWeights = new double[] {0, 1.0, 0, 0};
          panelPropertyLayout.rowWeights = new double[] {0, 0, 0, 0, 1};
          panelSettings.add(this.panelProperty, BorderLayout.CENTER);
          this.panelProperty.setLayout(panelPropertyLayout);
          this.panelProperty.setPreferredSize(new java.awt.Dimension(320, 234));

          // Setting panel frame
          EtchedBorder titleBorder = (EtchedBorder) BorderFactory.createEtchedBorder();
          Border borderKeyword = new CompoundBorder(titleBorder, new EmptyBorder(17, 7, 0, 7));
          panelProperty.setBorder(borderKeyword);

          // Property name
          {
            JLabel lblName = new JLabel();
            this.panelProperty.add(
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
            JLabel txtName = new JLabel();
            this.panelProperty.add(
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

          // Keyword add / delete button panel
          JPanel panelAddButtons = new JPanel();
          FlowLayout panelAddButtonsLayout = new FlowLayout(FlowLayout.RIGHT);
          Border borderAddButtons = new EmptyBorder(0, 7, 0, 0);
          panelAddButtons.setBorder(borderAddButtons);
          panelAddButtons.setLayout(panelAddButtonsLayout);
          panelSettings.add(panelAddButtons, BorderLayout.SOUTH);

          java.awt.Dimension minSize = new java.awt.Dimension(60, 22);
          Insets minInsets = new Insets(3, 3, 3, 3);
          {
            btnReg = new JButton();
            panelAddButtons.add(btnReg);
            btnReg.setText(Message.getString("dialog.common.button.update")); // update
            btnReg.setPreferredSize(minSize);
            btnReg.setMargin(minInsets);
            btnReg.addActionListener(this);
          }
        }
      }
      setTitle(Message.getString("projectsettingprojectaction.setup.status")); // Project settings
      setSize(640, 300);

    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * Button click event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {
    if (debug_l2) System.out.println("actionPerformed() of FileProjectNewDialog started");
    // Registration
    if (event.getSource() == this.btnOk) {
      this.result = Constant.OK_DIALOG;

      // Update your changes to source properties.
      setProperties();

      // Fire a change event
      this.properties.firePropertyChange();

      // Close the dialog.
      dispose();
      return;
    }
    // Apply
    if (event.getSource() == this.btnApply) {
      this.result = Constant.OK_DIALOG;

      // Update your changes to source properties.
      setProperties();

      // Fire a change event
      this.properties.firePropertyChange();

      return;
    }
    // close
    else if (event.getSource() == this.btnCancel) {
      this.result = Constant.CANCEL_DIALOG;
      // Close the dialog.
      dispose();
      return;
    }
    // Choice
    else if (event.getSource() == this.btnSelect) {
      // current folder
      String currentFolder = this.txtValue.getText();
      if (currentFolder == null || currentFolder.isEmpty()) {
        if (this.lastAccessFolder != null) {
          currentFolder = this.lastAccessFolder;
        } else {
          currentFolder = System.getProperty("user.dir");
        }
      }
      String dlgTitle = "";
      if ("file".equalsIgnoreCase(selectedvalue.getType())) {
        File[] selected = null;
        dlgTitle = Message.getString("informationdialog.selectfiledialog.title"); // Select file
        /* if (selectedvalue! = null
            && ProjectProperties.MAKEFILE_PATH.equalsIgnoreCase (selectedvalue.getKey ())) {
        // Display the Makefile selection dialog.
                selected = SwingUtils.showOpenMakefileDialog (this, dlgTitle, currentFolder, false);
            }
            else */
        {
          // Display the file selection dialog.
          selected = SwingUtils.showOpenFileDialog(this, dlgTitle, currentFolder, null, true);
        }
        if (selected == null || selected.length <= 0) return;
        txtValue.setText(selected[0].toString());
      } else if ("folder".equalsIgnoreCase(selectedvalue.getType())) {
        dlgTitle =
            Message.getString("settingprojectdialog.selectfolderdialog.title"); // Select folder
        // Display the folder selection dialog.
        File[] selected = SwingUtils.showOpenFolderDialog(this, dlgTitle, currentFolder, false);
        if (selected == null || selected.length <= 0) return;
        txtValue.setText(selected[0].toString());
      }

      return;
    }
    // Update / Update
    else if (event.getSource() == this.btnReg) {
      // Set the setting value in the table
      // Get the selected row.
      int selectedrow = this.ppropertiesTable.getSelectedRow();
      if (selectedrow < 0) return;
      if (selectedvalue == null) return;

      int col = 4;

      this.modelProperties.setValueAt(txtValue.getText(), selectedrow, col);

    } else if (event.getSource() == this.manage_settings_files) {
      if (debug) {
        System.out.println("Button manage_settings_files pressed");
      }
      this.setModal(false);
      ManageSettingsFilesDialog manage_files_dialog = new ManageSettingsFilesDialog();
      manage_files_dialog.showDialog();
      refreshSettingsList();
    } else if (event.getSource() == this.settings_list) {
      if (debug_l2) {
        System.out.println("Action on jComboBox settings_list");
      }
      String selection = settings_list.getItemAt(settings_list.getSelectedIndex());
      if (debug_l2) System.out.println("\tSelection=" + selection);

      // Should change ProjectProperties only after "Apply" or "OK" buttons pressed.
      // properties.setSettingsFile(selection);
      // if (debug) System.out.println("\tProject settings file set to "+
      // properties.getSettingsFile());

      // Update GUI table
      int selectedrow = this.ppropertiesTable.getSelectedRow();
      if (selectedrow < 0) return;
      if (selectedvalue == null) return;
      int col = 4;
      this.modelProperties.setValueAt(selection, selectedrow, col);
    } else if (event.getSource() == this.checkUseRemote) {
      if (!this.checkUseRemote.isSelected()) {
        // Set settings_file to null (make project local, i.e. execute make command locally)
        // settings_list.setEnabled(false);
        deactivateSettingsList();
      } else {
        settings_list.setEnabled(false);
        List<String> selections =
            Arrays.asList(ProjectProperties.getRemoteSettings(this.remote_service));
        String[] str_arr = new String[selections.size()];
        DefaultComboBoxModel<String> list_model =
            new DefaultComboBoxModel<String>(selections.toArray(str_arr));
        settings_list.setModel(list_model);
        settings_list.setEnabled(true);
      }
    }
    if (debug_l2) System.out.println("actionPerformed() of SettingsProjectDialog exited");
  }

  /** Make combobox with remote settings file names disabled. */
  private void deactivateSettingsList() {
    String[] str_arr = {""};
    DefaultComboBoxModel<String> empty_list_model = new DefaultComboBoxModel<String>(str_arr);
    settings_list.setModel(empty_list_model);
    settings_list.setEnabled(false);
    this.modelProperties.setValueAt(
        "", this.ppropertiesTable.getSelectedRow(), 4); // Update value in table
  }

  /**
   * Set the last access folder
   *
   * @param folder Last access folder
   */
  public void setLastAccessFolder(String folder) {
    if (folder == null) return;
    this.lastAccessFolder = folder;
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
   * Set the string setting panel
   *
   * @param value String setting property
   */
  private void setTextPanel(ProjectPropertyValue value) {

    if (!("text".equalsIgnoreCase(value.getType()))) return;

    // Set value
    String str = value.getValue();
    this.panelProperty.removeAll();

    // Property name
    {
      JLabel label = new JLabel();
      this.panelProperty.add(
          label,
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
      label.setText(Message.getString("settingprogramdialog.label.name")); // name
    }
    {
      JLabel lblName = new JLabel();
      this.panelProperty.add(
          lblName,
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
      lblName.setText(value.getName());
    }

    // value
    {
      JLabel lblValue = new JLabel();
      this.panelProperty.add(
          lblValue,
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
      lblValue.setText(Message.getString("settingprojectdialog.column_header.value")); // value
    }
    {
      txtValue = new JTextField();
      this.panelProperty.add(
          txtValue,
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
      txtValue.setText(str);
    }
    // Message
    {
      JComponent lblMassage = createMessageLabel(value.getMessage());
      if (lblMassage != null) {
        this.panelProperty.add(
            lblMassage,
            new GridBagConstraints(
                1,
                2,
                2,
                2,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.BOTH,
                new Insets(0, 0, 0, 0),
                0,
                0));
      }
    }
  }

  /**
   * Set the file settings panel
   *
   * @param value File settings properties
   */
  private void setFilePanel(ProjectPropertyValue value) {

    if (!("file".equalsIgnoreCase(value.getType()))
        && !("folder".equalsIgnoreCase(value.getType()))) return;

    String valueLabel = "";
    if ("file".equalsIgnoreCase(value.getType())) {
      valueLabel = Message.getString("settingprojectdialog.label.file-colon"); // File
    } else if ("folder".equalsIgnoreCase(value.getType())) {
      valueLabel = Message.getString("settingprojectdialog.label.folder-colon"); // folder
    }

    // setting file
    String filename = value.getValue();
    this.panelProperty.removeAll();

    // Property name
    {
      JLabel label = new JLabel();
      this.panelProperty.add(
          label,
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
      label.setText(Message.getString("settingprogramdialog.label.name")); // name
    }
    {
      JLabel lblName = new JLabel();
      this.panelProperty.add(
          lblName,
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
      lblName.setText(value.getName());
    }

    // File / Folder
    {
      JLabel lblFile = new JLabel();
      this.panelProperty.add(
          lblFile,
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
      lblFile.setText(valueLabel);
    }
    {
      txtValue = new JTextField();
      this.panelProperty.add(
          txtValue,
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
      txtValue.setText(filename);
    }
    {
      Dimension sizeButton = new Dimension(46, 22);
      btnSelect = new JButton();
      this.panelProperty.add(
          btnSelect,
          new GridBagConstraints(
              2,
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
      btnSelect.setText(Message.getString("dialog.common.button.refer")); // reference
      btnSelect.setMargin(new Insets(5, 0, 5, 0));
      btnSelect.addActionListener(this);
      btnSelect.setMaximumSize(sizeButton);
      btnSelect.setMinimumSize(sizeButton);
      btnSelect.setPreferredSize(sizeButton);
      //            btnFont.setSize(szieButton);
    }

    // Message
    {
      JComponent lblMassage = createMessageLabel(value.getMessage());
      if (lblMassage != null) {
        this.panelProperty.add(
            lblMassage,
            new GridBagConstraints(
                1,
                2,
                2,
                2,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.BOTH,
                new Insets(0, 0, 0, 0),
                0,
                0));
      }
    }
  }

  /**
   * Drop-down list for selecting remote settings files
   *
   * @param ppvalue
   */
  private void setFixedFilePanel(ProjectPropertyValue ppvalue) {
    this.remote_service = ProjectProperties.getRemoteService(ppvalue.getValue());
    if (debug)
      System.out.println(
          "SettingProjectDialog/setFiexdFilePanel: Value="
              + ppvalue.getValue()
              + " Service="
              + remote_service);
    if (!("fixed-file".equalsIgnoreCase(ppvalue.getType()))) return;

    String valueLabel = "";
    valueLabel = Message.getString("settingprojectdialog.label.file-colon"); // File

    // setting file
    String filename = ppvalue.getValue();
    this.panelProperty.removeAll();

    // Property name
    {
      JLabel label = new JLabel();
      this.panelProperty.add(
          label,
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
      label.setText(Message.getString("settingprogramdialog.label.name")); // Name
    }
    {
      JLabel lblName = new JLabel();
      this.panelProperty.add(
          lblName,
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
      lblName.setText(ppvalue.getName());
    }

    // File / Folder
    {
      JLabel lblFile = new JLabel();
      this.panelProperty.add(
          lblFile,
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
      lblFile.setText(valueLabel);
    }
    /*{
        txtValue = new JTextField();
        this.panelProperty.add(txtValue, new GridBagConstraints(1, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));
        txtValue.setText(filename);
    }*/
    {
      String[] selections = ProjectProperties.getRemoteSettings(remote_service);
      if (selections.length < 1) {
        System.out.println(
            "No remote connection settings files found in "
                + RemoteBuildProperties.REMOTE_SETTINGS_DIR);
      } else {
        if (debug) {
          System.out.println(
              "Have remote connection settings in " + RemoteBuildProperties.REMOTE_SETTINGS_DIR);
          System.out.println(selections);
        }
        if (remote_service != null) {
          DefaultComboBoxModel<String> list_model = new DefaultComboBoxModel<String>(selections);
          settings_list.setModel(list_model);
          settings_list.setEnabled(true);
          if (debug) {
            System.out.println("Check objects comparison in JComboBox");
            for (int i = 0; i < settings_list.getItemCount(); i++) {
              String item = settings_list.getItemAt(i);
              if (item == null) continue;
              if (item.equalsIgnoreCase(filename)) {
                System.out.println("Match item " + i + " value: " + filename);
              }
            }
          }
          settings_list.setSelectedItem(ppvalue.getValue());
        } else {
          deactivateSettingsList();
        }
        manage_settings_files =
            new JButton(
                Message.getString("fileprojectnewdialog.kindpanel.button.manage_settings_files"));
        manage_settings_files.setEnabled(true);
        this.panelProperty.add(
            settings_list,
            new GridBagConstraints(
                1,
                1,
                3,
                1,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.HORIZONTAL,
                new Insets(0, 0, 0, 0),
                0,
                0));
        this.panelProperty.add(
            manage_settings_files,
            new GridBagConstraints(
                1,
                2,
                3,
                1,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.NONE,
                new Insets(0, 0, 0, 0),
                0,
                0));
        settings_list.addActionListener(this);
        manage_settings_files.addActionListener(this);
      }
    }

    // Message
    {
      JComponent lblMassage = createMessageLabel(ppvalue.getMessage());
      if (lblMassage != null) {
        this.panelProperty.add(
            lblMassage,
            new GridBagConstraints(
                1,
                3,
                3,
                2,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.BOTH,
                new Insets(0, 0, 0, 0),
                0,
                0));
      }
    }

    // Make project remote / local checkbox
    {
      checkUseRemote =
          new JCheckBox(Message.getString("fileprojectnewdialog.kindpanel.checkbox.useServer"));
      checkUseRemote.addActionListener(this);
      checkUseRemote.setToolTipText(
          Message.getString("fileprojectnewdialog.kindpanel.checkbox.useServer.tooltip"));
      checkUseRemote.setEnabled(true);
      // if (debug) System.out.println(isFullProject() && this.rb_properties.remote_settings_found);
      checkUseRemote.setSelected(filename != null && filename.length() > 0);
      this.panelProperty.add(
          checkUseRemote,
          new GridBagConstraints(
              0,
              5,
              4,
              1,
              0.0,
              0.0,
              GridBagConstraints.WEST,
              GridBagConstraints.NONE,
              new Insets(0, 0, 0, 0),
              0,
              0));
    }
  }

  /**
   * Set the fixed string display panel
   *
   * @param value Fixed string setting property
   */
  private void setFixedTextPanel(ProjectPropertyValue value) {

    if (!"fixed-text".equalsIgnoreCase(value.getType())) return;

    // Set value
    String str = value.getValue();
    this.panelProperty.removeAll();

    // Property name
    {
      JLabel label = new JLabel();
      this.panelProperty.add(
          label,
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
      label.setText(Message.getString("settingprogramdialog.label.name")); // name
    }
    {
      JLabel lblName = new JLabel();
      this.panelProperty.add(
          lblName,
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
      lblName.setText(value.getName());
    }

    // value
    {
      JLabel lblValue = new JLabel();
      this.panelProperty.add(
          lblValue,
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
      lblValue.setText(Message.getString("settingprojectdialog.column_header.value")); // value
    }
    {
      JLabel txtValue = new JLabel();
      this.panelProperty.add(
          txtValue,
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
      txtValue.setText(str);
      txtValue.setBorder(BorderFactory.createEtchedBorder());
      // txtValue.setEditable(false);
    }
    // Message
    {
      JComponent lblMassage = createMessageLabel(value.getMessage());
      if (lblMassage != null) {
        this.panelProperty.add(
            lblMassage,
            new GridBagConstraints(
                1,
                2,
                2,
                2,
                0.0,
                0.0,
                GridBagConstraints.WEST,
                GridBagConstraints.BOTH,
                new Insets(0, 0, 0, 0),
                0,
                0));
      }
    }
  }

  /**
   * Create a message label component
   *
   * @param message Display message
   * @return Message label component
   */
  private JComponent createMessageLabel(String message) {
    if (message == null) return null;

    JTextPane lblMassage = new JTextPane();
    SimpleAttributeSet attr = new SimpleAttributeSet();
    // StyleConstants.setLineSpacing(attr, -0.2f);
    // lblMassage.setParagraphAttributes(attr, true);
    lblMassage.setForeground(UIManager.getColor("Label.foreground"));
    lblMassage.setOpaque(false);
    lblMassage.setEditable(false);
    lblMassage.setFocusable(false);
    lblMassage.setText(message);

    return lblMassage;
  }

  /**
   * Set the property setting panel
   *
   * @param value Setting property
   */
  private void setPropertyPanel(ProjectPropertyValue value) {
    if (value == null) return;

    if ("text".equalsIgnoreCase(value.getType())) {
      // Text settings
      setTextPanel(value);
      btnReg.setEnabled(true);
    } else if ("file".equalsIgnoreCase(value.getType())
        || "folder".equalsIgnoreCase(value.getType())) {
      // File settings / Folder settings
      setFilePanel(value);
      btnReg.setEnabled(true);
    } else if ("fixed-text".equalsIgnoreCase(value.getType())) {
      // Text display only
      setFixedTextPanel(value);
      btnReg.setEnabled(false);
    } else if ("fixed-file".equalsIgnoreCase(value.getType())) {
      // Text display only
      setFixedFilePanel(value);
      btnReg.setEnabled(false);
    }

    this.panelProperty.revalidate();
    this.panelProperty.repaint();
  }

  @Override
  public void valueChanged(ListSelectionEvent event) {
    if (event.getSource() == this.ppropertiesTable.getSelectionModel()) {
      // Get the selected row.
      int selectedrow = this.ppropertiesTable.getSelectedRow();
      if (selectedrow < 0) return;
      if (debug)
        System.out.println(
            "Selected row "
                + selectedrow
                + " key="
                + this.modelProperties.getValueAt(selectedrow, 1)
                + " type="
                + this.modelProperties.getValueAt(selectedrow, 2));

      // "PropertyValue", "key", "type", "name", "value", "message", CLO, order
      ProjectPropertyValue value =
          new ProjectPropertyValue(
              (String) this.modelProperties.getValueAt(selectedrow, 1), // key
              (String) this.modelProperties.getValueAt(selectedrow, 2), // type
              (String) this.modelProperties.getValueAt(selectedrow, 3), // name
              (String) this.modelProperties.getValueAt(selectedrow, 4), // value
              (String) this.modelProperties.getValueAt(selectedrow, 5), // message
              (String) this.modelProperties.getValueAt(selectedrow, 6), // command line option
              (int) this.modelProperties.getValueAt(selectedrow, 7) // order
              );
      // Selected properties
      this.selectedvalue = value;
      if (debug) System.out.println("Selected value: " + value);
      // Display in the settings panel
      this.setPropertyPanel(value);
    }
  }

  /** Set project properties. */
  private void setProperties() {

    int rows = this.modelProperties.getRowCount();

    // "PropertyValue", "key", "type", "name", "value", "message"}
    for (int i = 0; i < rows; i++) {
      ProjectPropertyValue value = (ProjectPropertyValue) this.modelProperties.getValueAt(i, 0);
      value.setValue((String) this.modelProperties.getValueAt(i, 4));
    }
  }

  /***
   * Refresh contents of settings_list JComboBox
   */
  private void refreshSettingsList() {
    if (debug) System.out.println("Refireshing settings_list contents.");
    String[] selections = ProjectProperties.getRemoteSettings();
    // Save selected item
    String selected = (String) this.settings_list.getSelectedItem();
    this.settings_list.removeAllItems();
    for (String s : selections) {
      settings_list.addItem(s);
    }
    // Restore selection in the drop-down list
    this.settings_list.setSelectedItem(selected);
    if (debug_l2) System.out.println("Restore selected item " + selected);
  }

  /**
   * Property table drawing class
   *
   * @author RIKEN
   */
  private class PropertiesTableRenderer extends DefaultTableCellRenderer {
    /** Serial number */
    private static final long serialVersionUID = 1L;

    /**
     * Get the drawing component of the cell
     *
     * @param table Drawing table
     * @param value Cell data
     * @param isSelected Selected state
     * @param hasFocus Focus
     * @param row row index
     * @param column Column index
     * @return drawing component
     */
    @Override
    public Component getTableCellRendererComponent(
        JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {

      Object cellValue = value;
      if (value != null) cellValue = value.toString();
      else cellValue = "";

      super.getTableCellRendererComponent(table, cellValue, isSelected, hasFocus, row, column);

      return this;
    }
  }

  /**
   * Get project settings
   *
   * @return Project settings
   */
  public ProjectProperties getProjectProperties() {
    return properties;
  }
}
