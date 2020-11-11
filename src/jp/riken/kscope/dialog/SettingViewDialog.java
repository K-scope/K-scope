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
import java.awt.Font;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JColorChooser;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
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
import javax.swing.text.StyleConstants;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.component.JColorButton;
import jp.riken.kscope.data.PropertyValue;
import jp.riken.kscope.properties.SourceProperties;

/**
 * Source view settings dialog
 *
 * @author RIKEN
 */
public class SettingViewDialog extends javax.swing.JDialog
    implements ActionListener, ListSelectionListener {
  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Cancel button */
  private JButton btnCancel;
  /** OK button */
  private JButton btnOk;
  /** Apply button */
  private JButton btnApply;
  /** Registration button */
  private JButton btnReg;
  /** Source view settings list */
  private JTable tblProperties;
  /** Source view settings list data */
  private DefaultTableModel modelProperties;
  /** Source View Settings Panel */
  private JPanel panelProperty;

  /** Dialog return value */
  private int result = Constant.CANCEL_DIALOG;

  /** Source view properties */
  SourceProperties properities;

  /** Column name */
  private final String[] COLUMN_HEADER = {
    Message.getString("settingprojectdialog.column_header.propertyvalue"), // PROPERTYVALUE
    Message.getString("settingprojectdialog.column_header.key"), // Key
    Message.getString("settingprojectdialog.column_header.type"), // type
    Message.getString("settingprogramdialog.label.name"), // name
    Message.getString("settingprojectdialog.column_header.value"), // value
    Message.getString("settingprojectdialog.column_header.message")
  }; // message

  /** Font selection button */
  private JButton btnFont;
  /** Color setting button */
  private JColorButton btnColor;
  /** Color valid check box */
  private JCheckBox chkEnabled;
  /** Value setting text box */
  private JTextField txtValue;
  /** Font label */
  private JLabel lblFontName;

  /** Selected properties */
  private PropertyValue selectedvalue;

  /**
   * Constructor
   *
   * @param frame Parent frame
   */
  public SettingViewDialog(Frame frame) {
    super(frame);
    initGUI();
  }

  /**
   * Constructor
   *
   * @param frame Parent frame
   * @param modal true = Show modal dialog
   */
  public SettingViewDialog(Frame frame, boolean modal) {
    super(frame, modal);
    initGUI();
  }

  /**
   * Constructor
   *
   * @param frame Parent frame
   * @param modal true = Show modal dialog
   * @param properities Source configuration properties
   */
  public SettingViewDialog(Frame frame, boolean modal, SourceProperties properities) {
    super(frame, modal);
    initGUI();
    setSourceProperties(properities);
  }

  /**
   * Set source settings.
   *
   * @param properities Source configuration properties
   */
  public void setSourceProperties(SourceProperties properities) {

    PropertyValue[] values = properities.getPropertyValues();

    // add to table
    for (PropertyValue value : values) {
      // "PropertyValue", "key", "type", "name", "value"
      Object[] rowData = new Object[6];
      rowData[0] = value;
      rowData[1] = value.getKey();
      rowData[2] = value.getType();
      rowData[3] = Message.getString(value.getName());
      rowData[4] = value.getValue();
      rowData[5] = Message.getString(value.getMessage());

      this.modelProperties.addRow(rowData);
    }

    this.properities = properities;

    return;
  }

  /** Initialize the GUI. */
  private void initGUI() {
    try {
      BorderLayout thisLayout = new BorderLayout();
      thisLayout.setHgap(5);
      thisLayout.setVgap(5);
      getContentPane().setLayout(thisLayout);

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
          BorderLayout panelListLayout = new BorderLayout();
          panelList.setLayout(panelListLayout);
          panelContent.add(panelList, BorderLayout.CENTER);
          {
            JLabel lblList = new JLabel();
            panelList.add(lblList, BorderLayout.NORTH);
            lblList.setText(
                Message.getString(
                    "settingviewdialog.label.setupsourceviewlist")); // Source view settings list
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
              tblProperties = new JTable();
              scrollList.setViewportView(tblProperties);
              tblProperties.setModel(modelProperties);
              tblProperties.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
              tblProperties.getSelectionModel().addListSelectionListener(this);
              tblProperties.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
              tblProperties.setDefaultRenderer(Object.class, new PropertiesTableRenderer());
              tblProperties.setColumnSelectionAllowed(false);
              tblProperties.setDefaultEditor(Object.class, null);

              // Column width setting
              // First column: PropertyValue: Hidden
              {
                TableColumn col = tblProperties.getColumnModel().getColumn(0);
                col.setResizable(false);
                col.setMinWidth(0);
                col.setMaxWidth(0);
              }
              // Second column: Key: Hide
              {
                TableColumn col = tblProperties.getColumnModel().getColumn(1);
                col.setResizable(false);
                col.setMinWidth(0);
                col.setMaxWidth(0);
              }
              // Third column: Type: Hidden
              {
                TableColumn col = tblProperties.getColumnModel().getColumn(2);
                col.setResizable(false);
                col.setMinWidth(0);
                col.setMaxWidth(0);
              }
              // Fourth column: Name
              {
                TableColumn col = tblProperties.getColumnModel().getColumn(3);
                col.setResizable(true);
                col.setMinWidth(160);
              }
              // 5th column: Value
              {
                TableColumn col = tblProperties.getColumnModel().getColumn(4);
                col.setResizable(true);
                col.setMinWidth(80);
              }
              // 6th column: Message
              {
                TableColumn col = tblProperties.getColumnModel().getColumn(5);
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
          panelContent.add(panelSettings, BorderLayout.EAST);
          Border borderSettings = new EmptyBorder(0, 7, 0, 0);
          panelSettings.setBorder(borderSettings);
          panelSettings.setLayout(panelSettingsLayout);
          {
            JLabel lblSettings = new JLabel();
            lblSettings.setText(Message.getString("mainmenu.project.config")); // Configuration
            panelSettings.add(lblSettings, BorderLayout.NORTH);
          }
          this.panelProperty = new JPanel();
          GridBagLayout panelPropertyLayout = new GridBagLayout();
          panelPropertyLayout.columnWidths = new int[] {80, 100, 80};
          panelPropertyLayout.rowHeights = new int[] {30, 30, 30, 3, 3, 3, 3};
          panelPropertyLayout.columnWeights = new double[] {0.1, 0.1, 0.1};
          panelPropertyLayout.rowWeights = new double[] {0, 0, 0, 0, 0, 0, 1};
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
      setTitle(Message.getString("settingviewdialog.dialog.title")); // Source view settings
      setSize(600, 300);

    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * Set the property setting panel
   *
   * @param value Setting property
   */
  private void setPropertyPanel(PropertyValue value) {
    if (value == null) return;

    if ("font".equalsIgnoreCase(value.getType())) {
      // Font settings
      setFontPanel(value);
    } else if ("color".equalsIgnoreCase(value.getType())) {
      // Color setting
      setColorPanel(value);
    } else if ("integer".equalsIgnoreCase(value.getType())) {
      // Value setting
      setValuePanel(value);
    }
    this.panelProperty.revalidate();
    this.panelProperty.repaint();
  }

  /**
   * Set the font settings panel
   *
   * @param value Font setting property
   */
  private void setFontPanel(PropertyValue value) {

    if (!("font".equalsIgnoreCase(value.getType()))) return;

    // Setting font
    Font font = (Font) value.getValue();
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

    // Font
    {
      JLabel lblFont = new JLabel();
      this.panelProperty.add(
          lblFont,
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
      lblFont.setText(Message.getString("jfontchooserdialog.fontpanel.title")); // font
    }
    {
      lblFontName = new JLabel();
      this.panelProperty.add(
          lblFontName,
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
      setLabelFont(font, lblFontName);
      lblFontName.setVerticalAlignment(JLabel.TOP);
    }
    {
      Dimension sizeButton = new Dimension(46, 22);
      btnFont = new JButton();
      this.panelProperty.add(
          btnFont,
          new GridBagConstraints(
              1,
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
      btnFont.setText(Message.getString("settingviewdialog.label.select")); // Choice
      btnFont.setMargin(new Insets(5, 5, 5, 5));
      btnFont.addActionListener(this);
      btnFont.setMaximumSize(sizeButton);
      btnFont.setMinimumSize(sizeButton);
      btnFont.setPreferredSize(sizeButton);
      //            btnFont.setSize(sizeButton);
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
   * Set the color setting panel
   *
   * @param value Color setting property
   */
  private void setColorPanel(PropertyValue value) {

    if (!("color".equalsIgnoreCase(value.getType()))) return;

    // Set color
    Color color = (Color) value.getValue();

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

    // color
    {
      JLabel lblColor = new JLabel();
      this.panelProperty.add(
          lblColor,
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
      lblColor.setText(Message.getString("settingviewdialog.label.color")); // Color setting
    }
    {
      btnColor = new JColorButton();
      this.panelProperty.add(
          btnColor,
          new GridBagConstraints(
              1,
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
      btnColor.addActionListener(this);
      btnColor.setColor(color);
    }
    {
      chkEnabled = new JCheckBox();
      this.panelProperty.add(
          chkEnabled,
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
      chkEnabled.setText(
          Message.getString("settingkeyworddialog.checkbox.enable")); // Effectiveness
      chkEnabled.addActionListener(this);
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

    // Enable setting
    boolean enabled = (color != null);
    this.btnColor.setEnabled(enabled);
    chkEnabled.setSelected(enabled);
  }

  /**
   * Set the value setting panel
   *
   * @param value Value setting property
   */
  private void setValuePanel(PropertyValue value) {

    if (!("integer".equalsIgnoreCase(value.getType()))) return;

    // Set value
    Integer intValue = (Integer) value.getValue();
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
      JLabel lblColor = new JLabel();
      this.panelProperty.add(
          lblColor,
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
      lblColor.setText(Message.getString("settingprojectdialog.column_header.value")); // value
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
      txtValue.setText(String.valueOf(intValue));
      txtValue.setHorizontalAlignment(JTextField.RIGHT);
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
    StyleConstants.setLineSpacing(attr, -0.2f);
    lblMassage.setParagraphAttributes(attr, true);
    lblMassage.setForeground(UIManager.getColor("Label.foreground"));
    lblMassage.setOpaque(false);
    lblMassage.setEditable(false);
    lblMassage.setFocusable(false);
    lblMassage.setText(message);

    return lblMassage;
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

    // Registration
    if (event.getSource() == this.btnOk) {
      this.result = Constant.OK_DIALOG;

      // Update your changes to source properties.
      setProperties();

      // Fire a change event
      this.properities.firePropertyChange();

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
    // Font, background, active background color
    else if (event.getSource() == this.btnColor) {
      JColorButton button = (JColorButton) event.getSource();
      // Color selection dialog
      Color color =
          JColorChooser.showDialog(
              this,
              Message.getString("settingkeyworddialog.colorchooser.title"), // Color selection
              button.getColor());
      if (color != null) {
        // Set the color for the button
        button.setColor(color);
      }

      return;
    }
    // Source display font
    else if (event.getSource() == this.btnFont) {
      Font deffont = this.lblFontName.getFont();
      JFontChooserDialog dialog = new JFontChooserDialog(this, true, deffont);
      int result = dialog.showDialog();
      if (result == Constant.OK_DIALOG) {
        Font font = dialog.getSelectedFont();
        if (font != null) {
          setLabelFont(font, this.lblFontName);
        }
      }
    }
    // update
    else if (event.getSource() == this.btnReg) {
      // Set the setting value in the table
      // Get the selected row.
      int selectedrow = this.tblProperties.getSelectedRow();
      if (selectedrow < 0) return;
      if (selectedvalue == null) return;

      int col = 4;

      if ("font".equalsIgnoreCase(selectedvalue.getType())) {
        // Font settings
        this.modelProperties.setValueAt(this.lblFontName.getFont(), selectedrow, col);
      } else if ("color".equalsIgnoreCase(selectedvalue.getType())) {
        // Color setting
        if (this.chkEnabled.isSelected()) {
          this.modelProperties.setValueAt(this.btnColor.getColor(), selectedrow, col);
        } else {
          this.modelProperties.setValueAt(null, selectedrow, col);
        }
      } else if ("integer".equalsIgnoreCase(selectedvalue.getType())) {
        // Value setting
        try {
          Integer value = Integer.parseInt(this.txtValue.getText());
          if (value < 0) {
            JOptionPane.showMessageDialog(
                this,
                Message.getString(
                    "settingviewdialog.infodialog.word-erapping.message"), // Set a value of 0 or
                                                                           // more for the number of
                                                                           // wrapping characters.
                Message.getString("settingviewdialog.dialog.title"), // Source view settings
                JOptionPane.INFORMATION_MESSAGE);
            return;
          }
          this.modelProperties.setValueAt(value, selectedrow, col);
        } catch (NumberFormatException e) {
        }
      }
    }
    // Valid check
    else if (event.getSource() == this.chkEnabled) {
      boolean enabled = this.chkEnabled.isSelected();
      this.btnColor.setEnabled(enabled);
    }
  }

  /**
   * Set the font label
   *
   * @param font Setting font
   * @param label Setting label
   */
  private void setLabelFont(Font font, JLabel label) {
    if (font == null) return;
    if (label == null) return;
    label.setFont(font);
    String name = toStringFont(font);
    label.setText(name);
    return;
  }

  /**
   * Get the string representation of the font
   *
   * @param font Setting font
   * @return Font string representation
   */
  private String toStringFont(Font font) {
    if (font == null) return "";
    String name = font.getName();
    name += " " + font.getSize() + " ";
    int style = font.getStyle();
    if ((style & Font.PLAIN) != 0) {
      name += "PLAIN ";
    }
    if ((style & Font.ITALIC) != 0) {
      name += "ITALIC ";
    }
    if ((style & Font.BOLD) != 0) {
      name += "BOLD ";
    }

    return name;
  }

  /** Set source view properties. */
  private void setProperties() {

    int rows = this.modelProperties.getRowCount();

    // "PropertyValue", "key", "type", "name", "value", "message"}
    for (int i = 0; i < rows; i++) {
      PropertyValue value = (PropertyValue) this.modelProperties.getValueAt(i, 0);
      value.setValue(this.modelProperties.getValueAt(i, 4));
    }
  }

  /**
   * Show selection properties
   *
   * @param event Event information
   */
  @Override
  public void valueChanged(ListSelectionEvent event) {

    if (event.getSource() == this.tblProperties.getSelectionModel()) {
      // Get the selected row.
      int selectedrow = this.tblProperties.getSelectedRow();
      if (selectedrow < 0) return;

      // "PropertyValue", "key", "type", "name", "value", "message"
      PropertyValue value =
          new PropertyValue(
              (String) this.modelProperties.getValueAt(selectedrow, 1),
              (String) this.modelProperties.getValueAt(selectedrow, 3),
              (String) this.modelProperties.getValueAt(selectedrow, 2),
              this.modelProperties.getValueAt(selectedrow, 4),
              (String) this.modelProperties.getValueAt(selectedrow, 5));
      // Selected properties
      this.selectedvalue = value;

      // Display in the settings panel
      this.setPropertyPanel(value);
    }
  }

  /**
   * Property table color cell drawing class
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
      Font font = table.getFont();
      java.awt.Color cellColor =
          isSelected ? table.getSelectionBackground() : table.getBackground();
      java.awt.Color foreColor =
          isSelected ? table.getSelectionForeground() : table.getForeground();
      if (value instanceof java.awt.Color) {
        cellValue = null;
        cellColor = (java.awt.Color) value;
      } else if (value instanceof java.awt.Font) {
        // font name
        cellValue = toStringFont((Font) value);
        font = (Font) value;
      }

      super.getTableCellRendererComponent(table, cellValue, isSelected, hasFocus, row, column);
      this.setBackground(cellColor);
      this.setForeground(foreColor);
      this.setFont(font);

      return this;
    }
  }
}
