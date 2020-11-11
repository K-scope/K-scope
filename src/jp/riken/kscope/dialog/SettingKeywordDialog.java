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
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JColorChooser;
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
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.common.KEYWORD_TYPE;
import jp.riken.kscope.component.JColorButton;
import jp.riken.kscope.component.JComponentTitledBorder;
import jp.riken.kscope.data.Keyword;
import jp.riken.kscope.properties.KeywordProperties;
import jp.riken.kscope.utils.StringUtils;

/**
 * Keyword setting dialog class
 *
 * @author RIKEN
 */
public class SettingKeywordDialog extends javax.swing.JDialog
    implements ActionListener, ListSelectionListener, ItemListener {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Cancel button */
  private JButton btnCancel;
  /** OK button */
  private JButton btnOk;
  /** Apply button */
  private JButton btnApply;
  /** Keyword name label */
  private JLabel lblName;
  /** Keyword name text box */
  private JTextField txtName;
  /** Keyword label */
  private JLabel lblKeyword;
  /** Keyword text box */
  private JTextField txtKeyword;
  /** Font color label */
  private JLabel lblColor;
  /** Font color button */
  private JColorButton btnColor;
  /** Style label */
  private JLabel lblStyle;
  /** Italic checkbox */
  private JCheckBox chkItalic;
  /** Void checkbox */
  private JCheckBox chkBold;
  /** Valid checkbox */
  private JCheckBox chkEnabled;
  /** Option label */
  private JLabel lblOption;
  /** Case-sensitive checkbox (true = case-sensitive) */
  private JCheckBox chkSensitivecase;
  /** Regular expression checkbox */
  private JCheckBox chkRegex;
  /** Registration button */
  private JButton btnReg;
  /** Add button */
  private JButton btnAdd;
  /** Delete button */
  private JButton btnDel;
  /** New button */
  private JButton btnNew;

  /** Keyword list */
  private JTable tblKeyword;
  /** Keyword list data */
  private DefaultTableModel modelKeyword;
  /** Border with valid checkbox */
  private JComponentTitledBorder titleBorder;
  /** Keyword setting panel */
  private JPanel panelKeyword;

  /** Dialog return value */
  private int result = Constant.CANCEL_DIALOG;

  /** Keyword Properties */
  KeywordProperties properities;
  /** Settings list header */
  private final String[] COLUMN_HEADER = {
    Message.getString("dialog.common.button.apply"), // Apply
    Message.getString("settingkeyworddialog.columnheader.caption"), // keyword name
    Message.getString("mainmenu.project.config.keyword"), // keyword
    Message.getString("settingkeyworddialog.columnheader.fontcolor"), // Font color
    Message.getString("jfontchooserdialog.fontpanel.label.style"), // style
    Message.getString("settingkeyworddialog.columnheader.upper-lower"), // Case sensitive
    Message.getString("searchfinddialog.checkbox.regex"), // Regular expressions
    Message.getString("settingkeyworddialog.columnheader.forbidden") // Keywords cannot be edited
  };

  private final int[] COLUMN_MINWIDTH = {50, 120, 160, 80, 80, 120, 80, 0, 0};
  private final int[] COLUMN_MAXWIDTH = {50, 0, 0, 80, 80, 120, 80, -1, -1};
  /** COLUMN_HEADER.length + 1: Keyword lock at the end (hidden) */
  private final int COLUMN_COUNT = 8;

  /**
   * Constructor
   *
   * @param frame Parent frame
   */
  public SettingKeywordDialog(JFrame frame) {
    super(frame);
    initGUI();
  }

  /**
   * Constructor
   *
   * @param frame Parent frame
   * @param modal true = Show modal dialog
   */
  public SettingKeywordDialog(Frame frame, boolean modal) {
    super(frame, modal);
    initGUI();
  }

  /**
   * Constructor
   *
   * @param frame Parent frame
   * @param modal true = Show modal dialog
   * @param properities keyword properties
   */
  public SettingKeywordDialog(Frame frame, boolean modal, KeywordProperties properities) {
    super(frame, modal);
    initGUI();
    setKeywordProperties(properities);
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

        // Keyword list
        {
          JPanel panelList = new JPanel();
          BorderLayout panelListLayout = new BorderLayout();
          panelList.setLayout(panelListLayout);
          panelContent.add(panelList, BorderLayout.CENTER);
          {
            JLabel lblList = new JLabel();
            panelList.add(lblList, BorderLayout.NORTH);
            lblList.setText(
                Message.getString("settingkeyworddialog.label.keywordlist")); // Keyword list
          }
          {
            JScrollPane scrollList = new JScrollPane();
            scrollList.setHorizontalScrollBarPolicy(
                ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            scrollList.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
            panelList.add(scrollList, BorderLayout.CENTER);
            {
              modelKeyword = new DefaultTableModel();
              modelKeyword.setColumnCount(COLUMN_COUNT);
              // Header column name
              String[] columns = COLUMN_HEADER;
              modelKeyword.setColumnIdentifiers(columns);
              tblKeyword = new JTable();
              scrollList.setViewportView(tblKeyword);
              tblKeyword.setModel(modelKeyword);
              tblKeyword.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
              tblKeyword.getSelectionModel().addListSelectionListener(this);
              tblKeyword.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
              tblKeyword.setDefaultRenderer(Object.class, new KeywordTableRenderer());
              tblKeyword.setColumnSelectionAllowed(false);
              tblKeyword.setDefaultEditor(Object.class, null);

              // Column width setting
              for (int i = 0; i < tblKeyword.getColumnModel().getColumnCount(); i++) {
                TableColumn col = tblKeyword.getColumnModel().getColumn(i);
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
          panelKeyword = new JPanel();
          GridBagLayout panelKeywordLayout = new GridBagLayout();
          panelKeywordLayout.columnWidths = new int[] {100, 80, 80};
          panelKeywordLayout.rowHeights = new int[] {30, 30, 30, 30, 30, 30, 7};
          panelKeywordLayout.columnWeights = new double[] {0.1, 0.1, 0.1};
          panelKeywordLayout.rowWeights = new double[] {0, 0, 0, 0, 0, 0, 1};
          panelSettings.add(panelKeyword, BorderLayout.CENTER);
          panelKeyword.setLayout(panelKeywordLayout);
          panelKeyword.setPreferredSize(new java.awt.Dimension(320, 234));

          // Valid check button
          chkEnabled =
              new JCheckBox(
                  Message.getString("settingkeyworddialog.checkbox.enable")); // Effectiveness
          chkEnabled.addItemListener(this);
          titleBorder =
              new JComponentTitledBorder(
                  chkEnabled, panelKeyword, BorderFactory.createEtchedBorder());
          Border borderKeyword = new CompoundBorder(titleBorder, new EmptyBorder(7, 7, 0, 7));
          panelKeyword.setBorder(borderKeyword);

          // Keyword name
          {
            lblName = new JLabel();
            panelKeyword.add(
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
            lblName.setText(Message.getString("settingkeyworddialog.label.keywordname")); // name
          }
          {
            txtName = new JTextField();
            panelKeyword.add(
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
          // Keywords
          {
            lblKeyword = new JLabel();
            panelKeyword.add(
                lblKeyword,
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
            lblKeyword.setText(Message.getString("mainmenu.project.config.keyword")); // keyword
          }
          {
            txtKeyword = new JTextField();
            panelKeyword.add(
                txtKeyword,
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
          // Font color
          {
            lblColor = new JLabel();
            panelKeyword.add(
                lblColor,
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
            lblColor.setText(
                Message.getString("settingkeyworddialog.columnheader.fontcolor")); // Font color
          }
          {
            btnColor = new JColorButton();
            panelKeyword.add(
                btnColor,
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
            btnColor.addActionListener(this);
          }
          // Style
          {
            lblStyle = new JLabel();
            panelKeyword.add(
                lblStyle,
                new GridBagConstraints(
                    0,
                    3,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.WEST,
                    GridBagConstraints.NONE,
                    new Insets(0, 0, 0, 0),
                    0,
                    0));
            lblStyle.setText(
                Message.getString("jfontchooserdialog.fontpanel.label.style")); // style
          }
          // Bold
          {
            chkBold = new JCheckBox();
            panelKeyword.add(
                chkBold,
                new GridBagConstraints(
                    1,
                    3,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.WEST,
                    GridBagConstraints.NONE,
                    new Insets(0, 0, 0, 0),
                    0,
                    0));
            chkBold.setText("BOLD");
          }
          // Italic
          {
            chkItalic = new JCheckBox();
            panelKeyword.add(
                chkItalic,
                new GridBagConstraints(
                    2,
                    3,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.WEST,
                    GridBagConstraints.NONE,
                    new Insets(0, 0, 0, 0),
                    0,
                    0));
            chkItalic.setText("ITALIC");
          }
          // Optional
          {
            lblOption = new JLabel();
            panelKeyword.add(
                lblOption,
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
            lblOption.setText(Message.getString("settingkeyworddialog.label.options")); // option
          }
          {
            chkSensitivecase = new JCheckBox();
            panelKeyword.add(
                chkSensitivecase,
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
            chkSensitivecase.setText(
                Message.getString("searchfinddialog.checkbox.upper-lower")); // Case sensitive
          }
          {
            chkRegex = new JCheckBox();
            panelKeyword.add(
                chkRegex,
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
            chkRegex.setText(
                Message.getString("searchfinddialog.checkbox.regex")); // Regular expressions
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
            btnAdd = new JButton();
            panelAddButtons.add(btnAdd);
            btnAdd.setText(Message.getString("settingkeyworddialog.button.add")); // | Add
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
            btnNew = new JButton();
            panelAddButtons.add(btnNew);
            btnNew.setText(Message.getString("informationdialog.button.clear.tooltip")); // clear
            btnNew.setPreferredSize(minSize);
            btnNew.setMargin(minInsets);
            btnNew.addActionListener(this);
          }
        }
      }

      setTitle(Message.getString("settingkeyworddialog.dialog.title")); // Keyword setting
      this.setSize(670, 360);
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
   * Set keyword settings.
   *
   * @param properities keyword setting properties
   */
  public void setKeywordProperties(KeywordProperties properities) {
    this.properities = properities;

    // Create a keyword list
    // "Apply", "Keyword name", "Keyword", "Font color", "Style"
    int count = properities.getKeywordCount();
    for (int i = 0; i < count; i++) {
      Keyword keyword = properities.getKeyword(i);

      // Create row data
      Object[] column = createKeywordRowData(keyword);
      // Add line
      modelKeyword.addRow(column);
    }
  }

  /**
   * Get keyword settings.
   *
   * @return keyword setting property
   */
  public KeywordProperties getKeywordProperties() {

    // Clear keyword properties
    properities.clearKeyword();

    // Get keyword settings from the keyword list
    int count = modelKeyword.getRowCount();
    for (int i = 0; i < count; i++) {
      Keyword keyword = new Keyword(KEYWORD_TYPE.KEYWORD);

      Object cell;
      // Valid / Invalid
      cell = modelKeyword.getValueAt(i, 0);
      boolean enabled =
          Message.getString("settingkeyworddialog.checkbox.enable")
              .equals((String) cell); // Effectiveness
      keyword.setEnabled(enabled);
      // name
      cell = modelKeyword.getValueAt(i, 1);
      String keyname = Message.getKey((String) cell);
      if (StringUtils.isNullOrEmpty(keyname)) {
        // Set the input name by changing the name
        keyword.setName((String) cell);
      } else {
        // Set the name key by not changing the name.
        keyword.setName(keyname);
      }
      // Keywords
      cell = modelKeyword.getValueAt(i, 2);
      keyword.setKeyword((String) cell);
      // Font color
      cell = modelKeyword.getValueAt(i, 3);
      if (cell instanceof java.awt.Color) {
        keyword.setForecolor((java.awt.Color) cell);
      }
      // Style
      // Bold
      cell = modelKeyword.getValueAt(i, 4);
      boolean bold = false;
      if (((String) cell).indexOf("Bold") >= 0) {
        bold = true;
      }
      // Italic
      boolean italic = false;
      if (((String) cell).indexOf("Italic") >= 0) {
        italic = true;
      }
      int style = Font.PLAIN;
      if (bold) style += Font.BOLD;
      if (italic) style += Font.ITALIC;
      keyword.setStyle(style);

      // Case sensitive
      cell = modelKeyword.getValueAt(i, 5);
      boolean sensitivecase = (Boolean) cell;
      keyword.setCaseSensitive(sensitivecase);
      // Regular expressions
      cell = modelKeyword.getValueAt(i, 6);
      boolean regex = (Boolean) cell;
      keyword.setRegex(regex);
      // Keyword lock
      cell = modelKeyword.getValueAt(i, 7);
      boolean keywordlock = (Boolean) cell;
      keyword.setKeywordlock(keywordlock);

      // Add keyword
      properities.addKeyword(keyword);
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
      getKeywordProperties();

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
      getKeywordProperties();

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
      // Color selection dialog
      Color color =
          JColorChooser.showDialog(
              this,
              Message.getString("settingkeyworddialog.colorchooser.title"),
              this.btnColor.getColor()); // Color selection
      if (color != null) {
        // Set a color icon on the button
        this.btnColor.setColor(color);
      }

      return;
    }
    // update
    else if (event.getSource() == this.btnReg) {
      // Check the input
      if (this.validateKeyword(false) == false) {
        // Typing error
        return;
      }

      // Update
      Keyword keyword = this.getKeyword();
      setKeywordList(keyword);

    }
    // add to
    else if (event.getSource() == this.btnAdd) {
      // Check the input
      if (this.validateKeyword(true) == false) {
        // Typing error
        return;
      }

      // make an addition
      Keyword keyword = this.getKeyword();

      // When adding, set keyword editability to false
      keyword.setKeywordlock(false);

      addKeywordList(keyword);
    }
    // Delete
    else if (event.getSource() == this.btnDel) {
      // Delete
      Keyword keyword = this.getKeyword();
      removeKeywordList(keyword);

      // Switch the enable of the setting panel
      setSettingPanelEnabled(true, false);
      // Clear the keyword setting panel
      clearKeyword();
    }
    // clear
    else if (event.getSource() == this.btnNew) {
      // Switch the enable of the setting panel
      setSettingPanelEnabled(true, false);
      // Clear the keyword setting panel.
      clearKeyword();
    }
  }

  /**
   * Enabled / disabled checkbox change event
   *
   * @param event Event information
   */
  @Override
  public void itemStateChanged(ItemEvent event) {

    if (event.getSource() == this.chkEnabled) {
      boolean enabled = this.chkEnabled.isSelected();
      // Switch the enable of the setting panel
      setSettingPanelEnabled(enabled, true);
    }
  }

  /**
   * Toggle the enable of the setting panel
   *
   * @param enabled true = enabled
   * @param keylockable true = Judge keyword lock status
   */
  private void setSettingPanelEnabled(boolean enabled, boolean keylockable) {
    /** Keyword name label */
    this.lblName.setEnabled(enabled);
    /** Keyword name text box */
    this.txtName.setEnabled(enabled);
    /** Keyword label */
    this.lblKeyword.setEnabled(enabled);
    /** Keyword text box */
    this.txtKeyword.setEnabled(enabled);
    /** Font color label */
    this.lblColor.setEnabled(enabled);
    /** Font color button */
    this.btnColor.setEnabled(enabled);
    /** Style label */
    this.lblStyle.setEnabled(enabled);
    /** Italic checkbox */
    this.chkItalic.setEnabled(enabled);
    /** Void checkbox */
    this.chkBold.setEnabled(enabled);
    /** Option label */
    this.lblOption.setEnabled(enabled);
    /** Case-sensitive checkbox */
    this.chkSensitivecase.setEnabled(enabled);
    /** Regular expression checkbox */
    this.chkRegex.setEnabled(enabled);

    if (keylockable) {
      // Do not enable due to keyword lock status
      if (enabled) {
        // Get the selected row.
        int selectedrow = this.tblKeyword.getSelectedRow();

        // Keyword cannot be edited if it is a keyword lock
        if (selectedrow >= 0) {
          // Keyword lock
          Object cell = modelKeyword.getValueAt(selectedrow, 7);
          boolean keywordlock = (Boolean) cell;
          this.txtKeyword.setEnabled(!keywordlock);
          this.chkRegex.setEnabled(!keywordlock);
          this.chkSensitivecase.setEnabled(!keywordlock);
        }
      }
    }
  }

  /**
   * Keyword list change event. <br>
   * Set the keyword information of the selected line in the setting panel.
   *
   * @param event Event information
   */
  @Override
  public void valueChanged(ListSelectionEvent event) {

    if (event.getSource() == this.tblKeyword.getSelectionModel()) {
      // Get the selected row.
      int selectedrow = this.tblKeyword.getSelectedRow();
      if (selectedrow < 0) return;

      Object cell;
      // Valid / Invalid
      cell = modelKeyword.getValueAt(selectedrow, 0);
      boolean enabled =
          Message.getString("settingkeyworddialog.checkbox.enable")
              .equals((String) cell); // Effectiveness
      this.chkEnabled.setSelected(enabled);

      // Switching the setting panel enable by enabling / disabling
      setSettingPanelEnabled(enabled, true);

      // name
      cell = modelKeyword.getValueAt(selectedrow, 1);
      this.txtName.setText((String) cell);
      // Keywords
      cell = modelKeyword.getValueAt(selectedrow, 2);
      this.txtKeyword.setText((String) cell);
      // Font color
      cell = modelKeyword.getValueAt(selectedrow, 3);
      if (cell == null) {
        this.btnColor.setColor(null);
      } else if (cell instanceof java.awt.Color) {
        this.btnColor.setColor((java.awt.Color) cell);
      }
      // Style
      // Bold
      cell = modelKeyword.getValueAt(selectedrow, 4);
      boolean bold = false;
      if (((String) cell).indexOf("Bold") >= 0) {
        bold = true;
      }
      this.chkBold.setSelected(bold);
      // Italic
      boolean italic = false;
      if (((String) cell).indexOf("Italic") >= 0) {
        italic = true;
      }
      this.chkItalic.setSelected(italic);
      // Case sensitive
      cell = modelKeyword.getValueAt(selectedrow, 5);
      boolean sensitivecase = (Boolean) cell;
      this.chkSensitivecase.setSelected(sensitivecase);
      // Regular expressions
      cell = modelKeyword.getValueAt(selectedrow, 6);
      boolean regex = (Boolean) cell;
      this.chkRegex.setSelected(regex);
      // Keyword lock
      cell = modelKeyword.getValueAt(selectedrow, 7);
      boolean keywordlock = (Boolean) cell;
      this.txtKeyword.setEnabled(!keywordlock);
      this.chkRegex.setEnabled(!keywordlock);
      this.chkSensitivecase.setEnabled(!keywordlock);

      // Enabled checkbox Redraw border
      this.chkEnabled.repaint();
      panelKeyword.repaint();
    }
    return;
  }

  /**
   * Create row data for keyword list model.
   *
   * @param keyword keyword data
   * @return line data
   */
  private Object[] createKeywordRowData(Keyword keyword) {
    // COLUMN_COUNT=8
    Object[] column = new Object[COLUMN_COUNT];
    column[0] =
        (keyword.isEnabled())
            ? Message.getString("settingkeyworddialog.checkbox.enable")
            : Message.getString(
                "settingkeyworddialog.data.disable"); // KEY13 = valid / KEY29 = invalid
    if (Message.containsKey(keyword.getName())) {
      column[1] = Message.getString(keyword.getName());
    } else {
      column[1] = keyword.getName();
    }
    if (keyword.getKeyword() != null) {
      column[2] = keyword.getKeyword();
    } else if (keyword.getClassmode() != null) {
      column[2] = keyword.getClassmode();
    }
    column[3] = keyword.getForecolor();
    if (keyword.getStyle() == Font.PLAIN) {
      column[4] = "Plain";
    } else if (keyword.getStyle() == Font.BOLD) {
      column[4] = "Bold";
    } else if (keyword.getStyle() == Font.ITALIC) {
      column[4] = "Italic";
    } else if (keyword.getStyle() == Font.BOLD + Font.ITALIC) {
      column[4] = "Bold Italic";
    }
    column[5] = new Boolean(keyword.isSensitivecase());
    column[6] = new Boolean(keyword.isRegex());
    column[7] = new Boolean(keyword.isKeywordlock());

    return column;
  }

  /**
   * Get the keyword object from the keyword setting panel.
   *
   * @return keyword object
   */
  private Keyword getKeyword() {

    Keyword keyword = new Keyword(KEYWORD_TYPE.KEYWORD);

    // Valid / Invalid
    keyword.setEnabled(this.chkEnabled.isSelected());
    // name
    keyword.setName(this.txtName.getText());
    // Keywords
    keyword.setKeyword(this.txtKeyword.getText());
    // Font color
    keyword.setForecolor(this.btnColor.getColor());
    // Style
    // Bold
    boolean bold = this.chkBold.isSelected();
    // Italic
    boolean italic = this.chkItalic.isSelected();
    int style = Font.PLAIN;
    if (bold) style += Font.BOLD;
    if (italic) style += Font.ITALIC;
    keyword.setStyle(style);
    // Case sensitive
    keyword.setCaseSensitive(this.chkSensitivecase.isSelected());
    // Regular expressions
    keyword.setRegex(this.chkRegex.isSelected());
    // Keyword lock
    keyword.setKeywordlock(!this.txtKeyword.isEnabled());

    return keyword;
  }

  /**
   * Update keywords.
   *
   * @param keyword Keyword information
   */
  private void setKeywordList(Keyword keyword) {

    // Selected line
    int selectedrow = this.tblKeyword.getSelectedRow();
    if (selectedrow < 0) {
      JOptionPane.showMessageDialog(
          this,
          Message.getString(
              "settingkeyworddialog.informationdialog.update.message"), // Select the keyword to be
                                                                        // updated from the list.
          Message.getString(
              "settingkeyworddialog.informationdialog.update.title"), // Update keywords
          JOptionPane.INFORMATION_MESSAGE);
      return;
    }

    // Create row data
    Object[] column = createKeywordRowData(keyword);
    // Line update
    modelKeyword.removeRow(selectedrow);
    modelKeyword.insertRow(selectedrow, column);
    this.tblKeyword.setRowSelectionInterval(selectedrow, selectedrow);
  }

  /**
   * Add keywords.
   *
   * @param keyword Keyword information
   */
  private void addKeywordList(Keyword keyword) {
    // Create row data
    Object[] column = createKeywordRowData(keyword);
    // Add line
    modelKeyword.addRow(column);
    int selectedrow = modelKeyword.getRowCount() - 1;
    this.tblKeyword.setRowSelectionInterval(selectedrow, selectedrow);
  }

  /**
   * Delete the keyword.
   *
   * @param keyword Keyword information
   */
  private void removeKeywordList(Keyword keyword) {
    // Selected line
    int selectedrow = this.tblKeyword.getSelectedRow();
    if (selectedrow < 0) return;
    int option =
        JOptionPane.showConfirmDialog(
            this,
            Message.getString(
                "settingkeyworddialog.confirmdialog.delete.message"), // Delete Are you sure you
                                                                      // want to?
            Message.getString("settingkeyworddialog.confirmdialog.delete.title"), // Delete keywords
            JOptionPane.OK_CANCEL_OPTION);
    if (option == JOptionPane.OK_OPTION) {
      // Delete line
      modelKeyword.removeRow(selectedrow);

      // Clear the keyword setting panel.
      clearKeyword();
    }
  }

  /** Clear the keyword setting panel. */
  private void clearKeyword() {

    // Clear settings
    this.chkEnabled.setSelected(true);
    /** Keyword name text box */
    this.txtName.setText(null);
    /** Keyword text box */
    this.txtKeyword.setText(null);
    /** Font color button */
    this.btnColor.setColor(null);
    /** Italic checkbox */
    this.chkItalic.setSelected(false);
    /** Void checkbox */
    this.chkBold.setSelected(false);
    /** Case-sensitive checkbox */
    this.chkSensitivecase.setSelected(false);
    /** Regular expression checkbox */
    this.chkRegex.setSelected(false);
  }

  /**
   * Check the input.
   *
   * @param addflag Add flag (true = keyword added)
   */
  private boolean validateKeyword(boolean addflag) {
    // Keywords required
    String keyword = this.txtKeyword.getText();

    // Check the input
    if (keyword == null || keyword.isEmpty()) {
      JOptionPane.showMessageDialog(
          this,
          Message.getString(
              "settingkeyworddialog.errordialog.empty.message"), // Please enter a keyword.
          Message.getString("dialog.common.error"), // error
          JOptionPane.ERROR_MESSAGE);
      return false;
    }

    // Check for duplicate keywords.
    int selectedrow = this.tblKeyword.getSelectedRow();
    int count = this.modelKeyword.getRowCount();
    boolean exists = false;
    for (int i = 0; i < count; i++) {
      // Keywords
      String cellKeyword = (String) (this.modelKeyword.getValueAt(i, 2));
      if (keyword.equals(cellKeyword)) {
        if (addflag) {
          // When adding, the same keyword is prohibited
          exists = true;
          break;
        } else if (i != selectedrow) {
          // In the case of update, NG if the same keyword exists other than the update line
          exists = true;
          break;
        }
      }
    }
    if (exists) {
      JOptionPane.showMessageDialog(
          this,
          Message.getString(
              "settingkeyworddialog.errordialog.exist.message"), // Duplicate keywords cannot be
                                                                 // registered.
          Message.getString("dialog.common.error"), // error
          JOptionPane.ERROR_MESSAGE);
      return false;
    }

    return true;
  }

  /**
   * Keyword table color cell drawing class
   *
   * @author RIKEN
   */
  private class KeywordTableRenderer extends DefaultTableCellRenderer {
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

      Object cell = null;
      ;
      Object cellValue = value;
      Font font = table.getFont();
      java.awt.Color cellColor =
          isSelected ? table.getSelectionBackground() : table.getBackground();
      java.awt.Color foreColor =
          isSelected ? table.getSelectionForeground() : table.getForeground();
      if (value instanceof java.awt.Color) {
        cellValue = null;
        cellColor = (java.awt.Color) value;
      }
      if (column == 2) {
        java.awt.Color colorValue = (java.awt.Color) modelKeyword.getValueAt(row, 3);
        if (colorValue != null) foreColor = colorValue;

        // Style

        // Bold
        cell = modelKeyword.getValueAt(row, 4);
        boolean bold = false;
        if (((String) cell).indexOf("Bold") >= 0) {
          bold = true;
        }
        // Italic
        boolean italic = false;
        if (((String) cell).indexOf("Italic") >= 0) {
          italic = true;
        }
        int style = Font.PLAIN;
        if (bold) style += Font.BOLD;
        if (italic) style += Font.ITALIC;
        font = new Font(font.getName(), style, font.getSize());
      }

      super.getTableCellRendererComponent(table, cellValue, isSelected, hasFocus, row, column);
      this.setBackground(cellColor);
      this.setForeground(foreColor);
      this.setFont(font);

      return this;
    }
  }
}
