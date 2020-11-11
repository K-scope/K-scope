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
import java.awt.Font;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JColorChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JSeparator;
import javax.swing.JTextField;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
// import javax.swing.border.LineBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.component.JColorButton;
import jp.riken.kscope.data.RequiredBF;
import jp.riken.kscope.properties.RequiredBFProperties;
import jp.riken.kscope.utils.StringUtils;

/**
 * Request Byte / FLOP Settings Dialog Class
 *
 * @author RIKEN
 */
public class SettingRequiredBFDialog extends javax.swing.JDialog
    implements ActionListener, ChangeListener {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Cancel button */
  private JButton btnCancel;
  /** OK button */
  private JButton btnOk;
  /** Apply button */
  private JButton btnApply;
  /** Button to return to default */
  private JButton btnDefault;
  /** Computational performance */
  private JTextField txtPerformance;
  /** Access name */
  private JLabel[] lblNames;
  /** Throughput calculation mode: with store */
  private JTextField[] txtMem_throughput_calc_mode_stores;
  /** Throughput calculation mode: No store */
  private JTextField[] txtMem_throughput_calc_mode_nostores;
  /** Coefficient */
  private JTextField[] txtCoefs;
  /** Background color button */
  private JColorButton[] btnColors;
  /** Background color enabled / disabled */
  private JCheckBox[] chkColors;
  /** Request Byte calculation */
  private JCheckBox[] chkRequiredbfs;
  /** Dialog return value */
  private int result = Constant.CANCEL_DIALOG;
  /** Request Byte / FLOP configuration property */
  private RequiredBFProperties properities;
  /** Maximum number of access destination items */
  private final int MEMORY_MAXROWS = 8;
  /** Calculation unit: Byte / FLOP */
  private JRadioButton radioBFCalcTypeUnitBF;
  /** Calculation unit: Byte / FLOP */
  private JRadioButton radioBFCalcTypeUnitFB;
  /** Memory throughput calculation mode: Automatic judgment */
  private JRadioButton radioMemThroughputCalcModeAuto;
  /** Memory throughput calculation mode: With store */
  private JRadioButton radioMemThroughputCalcModeStore;
  /** Memory throughput calculation mode: No store */
  private JRadioButton radioMemThroughputCalcModeNostore;
  /** Default size: real */
  private JTextField txtSizeReal;
  /** Default size: integer */
  private JTextField txtSizeInteger;
  /** Memory performance calculation result dialog */
  @SuppressWarnings("unused")
  private RequiredBFDialog ownerDialog;

  /**
   * Constructor
   *
   * @param frame Parent frame
   */
  public SettingRequiredBFDialog(JFrame frame) {
    super(frame);
    ownerDialog = null;
    initGUI();
  }

  /**
   * Constructor
   *
   * @param frame Parent frame
   * @param modal true = Show modal dialog
   */
  public SettingRequiredBFDialog(Frame frame, boolean modal) {
    super(frame, modal);
    ownerDialog = null;
    initGUI();
  }

  /**
   * Constructor
   *
   * @param frame Parent frame
   * @param modal true = Show modal dialog
   * @param properities Request Byte / FLOP property
   */
  public SettingRequiredBFDialog(Frame frame, boolean modal, RequiredBFProperties properities) {
    super(frame, modal);
    this.properities = properities;
    ownerDialog = null;
    initGUI();
    setRequiredBFProperties(this.properities);
  }

  /**
   * Constructor
   *
   * @param owner Memory performance calculation result dialog
   * @param modal true = Show modal dialog
   * @param properities Request Byte / FLOP property
   */
  public SettingRequiredBFDialog(
      RequiredBFDialog owner, boolean modal, RequiredBFProperties properities) {
    super(owner, modal);
    this.properities = properities;
    ownerDialog = owner;
    initGUI();
    setRequiredBFProperties(this.properities);
  }

  /** Initialize the GUI. */
  private void initGUI() {

    try {
      JPanel panelContent = null;
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
          String text = "";
          text = Message.getString("dialog.common.button.ok"); // OK
          btnOk.setText(text);
          btnOk.setPreferredSize(buttonSize);
          btnOk.addActionListener(this);
        }
        {
          btnDefault = new JButton();
          panelButtons.add(btnDefault);
          String text = Message.getString("mainmenu.view.filter.default"); // return to default
          btnDefault.setText(text); // return to default
          btnDefault.setPreferredSize(buttonSize);
          btnDefault.setMargin(new Insets(5, 5, 5, 5));
          btnDefault.addActionListener(this);
          if (text.getBytes().length > 14) {
            Font font = btnDefault.getFont();
            Font newFont = new Font(font.getFontName(), font.getStyle(), font.getSize() - 2);
            btnDefault.setFont(newFont);
            btnDefault.setMargin(new Insets(0, 0, 0, 0));
          }
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
        panelContent = new JPanel();
        panelContent.setLayout(new BoxLayout(panelContent, BoxLayout.Y_AXIS));
        getContentPane().add(panelContent, BorderLayout.CENTER);
        Border border = new EmptyBorder(7, 7, 0, 7);
        panelContent.setBorder(border);

        // Theoretical operation performance
        int TEXT_INPUT = 5;
        {
          JPanel panelPerformance = new JPanel();
          GridBagLayout layoutPerformance = new GridBagLayout();
          layoutPerformance.rowWeights = new double[] {0.0, 0.0};
          layoutPerformance.rowHeights = new int[] {24, 7};
          layoutPerformance.columnWeights = new double[] {0.0, 0.0, 0.0, 1.0};
          layoutPerformance.columnWidths = new int[] {80, 7, 7, 7};
          panelPerformance.setLayout(layoutPerformance);
          panelContent.add(panelPerformance);

          JLabel label =
              new JLabel(
                  Message.getString(
                      "settingrequiredbfdialog.label.performance")); // Theoretical operation
                                                                     // performance
          panelPerformance.add(
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
                  new Insets(7, 4, 7, 4),
                  0,
                  0));
          this.txtPerformance = new JTextField(TEXT_INPUT);
          this.txtPerformance.setHorizontalAlignment(JTextField.RIGHT);
          panelPerformance.add(
              this.txtPerformance,
              new GridBagConstraints(
                  1,
                  0,
                  1,
                  1,
                  0.0,
                  0.0,
                  GridBagConstraints.WEST,
                  GridBagConstraints.NONE,
                  new Insets(0, 4, 0, 4),
                  0,
                  0));
          panelPerformance.add(
              new JLabel("GFLOPS"),
              new GridBagConstraints(
                  2,
                  0,
                  1,
                  1,
                  0.0,
                  0.0,
                  GridBagConstraints.WEST,
                  GridBagConstraints.NONE,
                  new Insets(0, 4, 0, 4),
                  0,
                  0));
        }

        // Request Byte / FLOP setting list
        {
          JPanel panelList = new JPanel();
          BorderLayout panelListLayout = new BorderLayout();
          panelList.setLayout(panelListLayout);
          panelContent.add(panelList, BorderLayout.CENTER);
          String subtitle =
              Message.getString("settingrequiredbfdialog.label.accesslist"); // Access list
          TitledBorder titleBorder = new TitledBorder(BorderFactory.createEtchedBorder(), subtitle);
          panelList.setBorder(titleBorder);
          {
            JPanel panelRequiredBF = new JPanel();
            panelList.add(panelRequiredBF, BorderLayout.CENTER);
            GridBagLayout layoutMemory = new GridBagLayout();
            layoutMemory.rowWeights = new double[] {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0};
            layoutMemory.rowHeights = new int[] {7, 7, 4, 4, 4, 4, 4, 4, 1};
            layoutMemory.columnWeights =
                new double[] {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0};
            layoutMemory.columnWidths = new int[] {50, 4, 64, 64, 4, 64, 4, 50, 50, 4, 50, 7};
            panelRequiredBF.setLayout(layoutMemory);
            // header
            int col = 0;
            // Access destination
            {
              JLabel header =
                  new JLabel(
                      Message.getString(
                          "settingrequiredbfdialog.label.access")); // Access destination
              panelRequiredBF.add(
                  header,
                  new GridBagConstraints(
                      col,
                      0,
                      1,
                      2,
                      0.0,
                      0.0,
                      GridBagConstraints.WEST,
                      GridBagConstraints.BOTH,
                      new Insets(0, 4, 0, 4),
                      0,
                      0));
            }
            // Vertical ruled line
            col++;
            {
              JSeparator spc = new JSeparator(JSeparator.VERTICAL);
              panelRequiredBF.add(
                  spc,
                  new GridBagConstraints(
                      col,
                      0,
                      1,
                      layoutMemory.rowHeights.length,
                      0.0,
                      0.0,
                      GridBagConstraints.CENTER,
                      GridBagConstraints.VERTICAL,
                      new Insets(0, 4, 0, 4),
                      0,
                      0));
            }
            // Memory throughput
            col++;
            {
              JLabel header =
                  new JLabel(
                      Message.getString(
                          "settingrequiredbfdialog.label.throughput")); // Memory throughput (GB /
                                                                        // s)
              header.setHorizontalAlignment(JLabel.CENTER);
              panelRequiredBF.add(
                  header,
                  new GridBagConstraints(
                      col,
                      0,
                      2,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.CENTER,
                      GridBagConstraints.BOTH,
                      new Insets(0, 4, 0, 4),
                      0,
                      0));
            }
            // With store
            {
              JLabel header =
                  new JLabel(
                      Message.getString("settingrequiredbfdialog.label.store")); // with store
              header.setHorizontalAlignment(JLabel.CENTER);
              panelRequiredBF.add(
                  header,
                  new GridBagConstraints(
                      col,
                      1,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.CENTER,
                      GridBagConstraints.BOTH,
                      new Insets(0, 4, 0, 4),
                      0,
                      0));
            }
            // No store
            col++;
            {
              JLabel header =
                  new JLabel(
                      Message.getString("settingrequiredbfdialog.label.nostore")); // no store
              header.setHorizontalAlignment(JLabel.CENTER);
              panelRequiredBF.add(
                  header,
                  new GridBagConstraints(
                      col,
                      1,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.CENTER,
                      GridBagConstraints.BOTH,
                      new Insets(0, 4, 0, 4),
                      0,
                      0));
            }
            // Vertical ruled line
            col++;
            {
              JSeparator spc = new JSeparator(JSeparator.VERTICAL);
              panelRequiredBF.add(
                  spc,
                  new GridBagConstraints(
                      col,
                      0,
                      1,
                      layoutMemory.rowHeights.length,
                      0.0,
                      0.0,
                      GridBagConstraints.CENTER,
                      GridBagConstraints.VERTICAL,
                      new Insets(0, 4, 0, 4),
                      0,
                      0));
            }
            // Coefficient
            col++;
            {
              JLabel header =
                  new JLabel(
                      Message.getString("settingrequiredbfdialog.label.coef")); // coefficient
              header.setHorizontalAlignment(JLabel.CENTER);
              panelRequiredBF.add(
                  header,
                  new GridBagConstraints(
                      col,
                      0,
                      1,
                      2,
                      0.0,
                      0.0,
                      GridBagConstraints.CENTER,
                      GridBagConstraints.BOTH,
                      new Insets(0, 4, 0, 4),
                      0,
                      0));
            }
            // Vertical ruled line
            col++;
            {
              JSeparator spc = new JSeparator(JSeparator.VERTICAL);
              panelRequiredBF.add(
                  spc,
                  new GridBagConstraints(
                      col,
                      0,
                      1,
                      layoutMemory.rowHeights.length,
                      0.0,
                      0.0,
                      GridBagConstraints.CENTER,
                      GridBagConstraints.VERTICAL,
                      new Insets(0, 4, 0, 4),
                      0,
                      0));
            }
            // Background color
            col++;
            {
              JLabel header =
                  new JLabel(
                      Message.getString(
                          "settingrequiredbfdialog.label.bgcolor")); // Background color
              header.setHorizontalAlignment(JLabel.CENTER);
              panelRequiredBF.add(
                  header,
                  new GridBagConstraints(
                      col,
                      0,
                      2,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.CENTER,
                      GridBagConstraints.BOTH,
                      new Insets(0, 4, 0, 4),
                      0,
                      0));
            }
            // Color selection
            {
              JLabel header =
                  new JLabel(
                      Message.getString(
                          "settingrequiredbfdialog.label.bgcolor.select")); // Color selection
              header.setHorizontalAlignment(JLabel.CENTER);
              panelRequiredBF.add(
                  header,
                  new GridBagConstraints(
                      col,
                      1,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.CENTER,
                      GridBagConstraints.BOTH,
                      new Insets(0, 4, 0, 4),
                      0,
                      0));
            }
            // invalid
            col++;
            {
              JLabel header =
                  new JLabel(
                      Message.getString(
                          "settingrequiredbfdialog.label.bgcolor.enabled")); // invalid
              header.setHorizontalAlignment(JLabel.CENTER);
              panelRequiredBF.add(
                  header,
                  new GridBagConstraints(
                      col,
                      1,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.CENTER,
                      GridBagConstraints.BOTH,
                      new Insets(0, 4, 0, 4),
                      0,
                      0));
            }
            // Vertical ruled line
            col++;
            {
              JSeparator spc = new JSeparator(JSeparator.VERTICAL);
              panelRequiredBF.add(
                  spc,
                  new GridBagConstraints(
                      col,
                      0,
                      1,
                      layoutMemory.rowHeights.length,
                      0.0,
                      0.0,
                      GridBagConstraints.CENTER,
                      GridBagConstraints.VERTICAL,
                      new Insets(0, 4, 0, 4),
                      0,
                      0));
            }
            // Request BF calculation
            col++;
            {
              JLabel header =
                  new JLabel(
                      Message.getString(
                          "settingrequiredbfdialog.label.reqbf")); // Request BF calculation target
              header.setHorizontalAlignment(JLabel.CENTER);
              panelRequiredBF.add(
                  header,
                  new GridBagConstraints(
                      col,
                      0,
                      1,
                      2,
                      0.0,
                      0.0,
                      GridBagConstraints.CENTER,
                      GridBagConstraints.BOTH,
                      new Insets(0, 4, 0, 4),
                      0,
                      0));
            }
            // Horizontal ruled line
            {
              JSeparator spc = new JSeparator(JSeparator.HORIZONTAL);
              panelRequiredBF.add(
                  spc,
                  new GridBagConstraints(
                      0,
                      2,
                      layoutMemory.columnWidths.length,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.CENTER,
                      GridBagConstraints.HORIZONTAL,
                      new Insets(0, 4, 0, 4),
                      0,
                      0));
            }

            // table
            // Access name
            this.lblNames = new JLabel[MEMORY_MAXROWS];
            // Memory throughput (with store)
            this.txtMem_throughput_calc_mode_stores = new JTextField[MEMORY_MAXROWS];
            // Memory throughput (no store)
            this.txtMem_throughput_calc_mode_nostores = new JTextField[MEMORY_MAXROWS];
            // Coefficient
            this.txtCoefs = new JTextField[MEMORY_MAXROWS];
            // Background color button
            this.btnColors = new JColorButton[MEMORY_MAXROWS];
            // Enable / disable background color
            this.chkColors = new JCheckBox[MEMORY_MAXROWS];
            // Request BF calculation
            this.chkRequiredbfs = new JCheckBox[MEMORY_MAXROWS];
            int row = 3;
            for (int i = 0; i < MEMORY_MAXROWS; i++) {
              col = 0;
              // Access name
              this.lblNames[i] = new JLabel();
              panelRequiredBF.add(
                  this.lblNames[i],
                  new GridBagConstraints(
                      0,
                      row,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.CENTER,
                      GridBagConstraints.NONE,
                      new Insets(2, 0, 2, 0),
                      0,
                      0));
              // Memory throughput: with store
              col += 2;
              this.txtMem_throughput_calc_mode_stores[i] = new JTextField(TEXT_INPUT);
              this.txtMem_throughput_calc_mode_stores[i].setHorizontalAlignment(JTextField.RIGHT);
              panelRequiredBF.add(
                  this.txtMem_throughput_calc_mode_stores[i],
                  new GridBagConstraints(
                      col,
                      row,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.CENTER,
                      GridBagConstraints.NONE,
                      new Insets(2, 0, 2, 0),
                      0,
                      0));
              // Memory Throughput: No Store
              col++;
              this.txtMem_throughput_calc_mode_nostores[i] = new JTextField(TEXT_INPUT);
              this.txtMem_throughput_calc_mode_nostores[i].setHorizontalAlignment(JTextField.RIGHT);
              panelRequiredBF.add(
                  this.txtMem_throughput_calc_mode_nostores[i],
                  new GridBagConstraints(
                      col,
                      row,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.CENTER,
                      GridBagConstraints.NONE,
                      new Insets(2, 0, 2, 0),
                      0,
                      0));
              // Coefficient
              col += 2;
              this.txtCoefs[i] = new JTextField(TEXT_INPUT);
              this.txtCoefs[i].setHorizontalAlignment(JTextField.RIGHT);
              panelRequiredBF.add(
                  this.txtCoefs[i],
                  new GridBagConstraints(
                      col,
                      row,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.CENTER,
                      GridBagConstraints.NONE,
                      new Insets(2, 0, 2, 0),
                      0,
                      0));
              // Background color button
              col += 2;
              this.btnColors[i] = new JColorButton();
              panelRequiredBF.add(
                  this.btnColors[i],
                  new GridBagConstraints(
                      col,
                      row,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.CENTER,
                      GridBagConstraints.NONE,
                      new Insets(2, 0, 2, 0),
                      0,
                      0));
              // Enable / disable background color
              col++;
              this.chkColors[i] = new JCheckBox();
              panelRequiredBF.add(
                  this.chkColors[i],
                  new GridBagConstraints(
                      col,
                      row,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.CENTER,
                      GridBagConstraints.NONE,
                      new Insets(2, 0, 2, 0),
                      0,
                      0));
              // Event
              this.chkColors[i].addChangeListener(this);
              // Request BF calculation
              col += 2;
              this.chkRequiredbfs[i] = new JCheckBox();
              panelRequiredBF.add(
                  this.chkRequiredbfs[i],
                  new GridBagConstraints(
                      col,
                      row,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.CENTER,
                      GridBagConstraints.NONE,
                      new Insets(2, 0, 2, 0),
                      0,
                      0));

              row++;
            }
          }

          // Optional
          {
            JPanel panelOptions = new JPanel();
            GridBagLayout layoutOption = new GridBagLayout();
            layoutOption.rowWeights = new double[] {0.0, 0.0, 0.0, 1.0};
            layoutOption.rowHeights = new int[] {7, 7, 7, 3};
            layoutOption.columnWeights = new double[] {0.0, 1.0, 0.0};
            layoutOption.columnWidths = new int[] {7, 240, 7};
            panelOptions.setLayout(layoutOption);
            panelContent.add(panelOptions);

            String titleOptions =
                Message.getString("settingrequiredbfdialog.title.option"); // option
            TitledBorder titleOption =
                new TitledBorder(BorderFactory.createEtchedBorder(), titleOptions);
            panelOptions.setBorder(titleOption);

            // Memory throughput setting (store setting)
            JLabel labelMemThroughputCalcMode =
                new JLabel(
                    Message.getString(
                        "settingrequiredbfdialog.label.storemode")); // Throughput store settings
            panelOptions.add(
                labelMemThroughputCalcMode,
                new GridBagConstraints(
                    0,
                    0,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.WEST,
                    GridBagConstraints.NONE,
                    new Insets(0, 4, 0, 4),
                    0,
                    0));
            radioMemThroughputCalcModeAuto =
                new JRadioButton(
                    Message.getString(
                        "settingrequiredbfdialog.option.storemode.auto")); // Automatic judgment
            radioMemThroughputCalcModeStore =
                new JRadioButton(
                    Message.getString("settingrequiredbfdialog.label.store")); // with store
            radioMemThroughputCalcModeNostore =
                new JRadioButton(
                    Message.getString("settingrequiredbfdialog.label.nostore")); // no store
            ButtonGroup groupStore = new ButtonGroup();
            groupStore.add(radioMemThroughputCalcModeAuto);
            groupStore.add(radioMemThroughputCalcModeStore);
            groupStore.add(radioMemThroughputCalcModeNostore);
            JPanel panelStore = new JPanel();
            panelStore.setLayout(new BoxLayout(panelStore, BoxLayout.X_AXIS));
            panelStore.add(radioMemThroughputCalcModeAuto);
            panelStore.add(Box.createRigidArea(new Dimension(7, 1)));
            panelStore.add(radioMemThroughputCalcModeStore);
            panelStore.add(Box.createRigidArea(new Dimension(7, 1)));
            panelStore.add(radioMemThroughputCalcModeNostore);
            panelOptions.add(
                panelStore,
                new GridBagConstraints(
                    1,
                    0,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.WEST,
                    GridBagConstraints.NONE,
                    new Insets(0, 4, 0, 4),
                    0,
                    0));

            // BF calculation unit
            JLabel labelBFCalcType =
                new JLabel(
                    Message.getString("settingrequiredbfdialog.label.unit")); // BF calculation unit
            panelOptions.add(
                labelBFCalcType,
                new GridBagConstraints(
                    0,
                    1,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.WEST,
                    GridBagConstraints.NONE,
                    new Insets(0, 4, 0, 4),
                    0,
                    0));
            this.radioBFCalcTypeUnitBF =
                new JRadioButton(
                    Message.getString("settingrequiredbfdialog.check.unit.bf")); // Byte/FLOP
            this.radioBFCalcTypeUnitFB =
                new JRadioButton(
                    Message.getString("settingrequiredbfdialog.check.unit.fb")); // FLOP/Byte

            ButtonGroup groupUnit = new ButtonGroup();
            groupUnit.add(this.radioBFCalcTypeUnitBF);
            groupUnit.add(this.radioBFCalcTypeUnitFB);
            JPanel panelUnit = new JPanel();
            panelUnit.setLayout(new BoxLayout(panelUnit, BoxLayout.X_AXIS));
            panelUnit.add(this.radioBFCalcTypeUnitBF);
            panelUnit.add(Box.createRigidArea(new Dimension(7, 1)));
            panelUnit.add(this.radioBFCalcTypeUnitFB);
            panelOptions.add(
                panelUnit,
                new GridBagConstraints(
                    1,
                    1,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.WEST,
                    GridBagConstraints.NONE,
                    new Insets(0, 4, 0, 4),
                    0,
                    0));

            // Data type default size
            int TEXT_SIZE = 3;
            JLabel labelSize =
                new JLabel(
                    Message.getString(
                        "settingrequiredbfdialog.option.defaultsize")); // Default size
            JLabel labelReal = new JLabel("real");
            JLabel labelInteger = new JLabel("integer");
            this.txtSizeReal = new JTextField(TEXT_SIZE);
            this.txtSizeReal.setHorizontalAlignment(JTextField.RIGHT);
            this.txtSizeInteger = new JTextField(TEXT_SIZE);
            this.txtSizeInteger.setHorizontalAlignment(JTextField.RIGHT);
            JLabel labelByte = new JLabel("(Byte)");
            panelOptions.add(
                labelSize,
                new GridBagConstraints(
                    0,
                    2,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.WEST,
                    GridBagConstraints.NONE,
                    new Insets(0, 4, 0, 4),
                    0,
                    0));
            JPanel panelSize = new JPanel();
            panelSize.setLayout(new BoxLayout(panelSize, BoxLayout.X_AXIS));
            panelSize.add(labelReal);
            panelSize.add(Box.createRigidArea(new Dimension(7, 1)));
            panelSize.add(this.txtSizeReal);
            panelSize.add(Box.createRigidArea(new Dimension(24, 1)));
            panelSize.add(labelInteger);
            panelSize.add(Box.createRigidArea(new Dimension(7, 1)));
            panelSize.add(this.txtSizeInteger);
            panelSize.add(Box.createRigidArea(new Dimension(14, 1)));
            panelSize.add(labelByte);
            panelOptions.add(
                panelSize,
                new GridBagConstraints(
                    1,
                    2,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.WEST,
                    GridBagConstraints.NONE,
                    new Insets(0, 4, 0, 4),
                    0,
                    0));
          }
        }
      }

      setTitle(Message.getString("settingrequiredbfdialog.title")); // Request Byte / FLOP setting
      this.setResizable(false); // Cannot be resized
      this.pack();

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
   * Set the request Byte / FLOP setting.
   *
   * @param properities Request Byte / FLOP configuration properties
   */
  public void setRequiredBFProperties(RequiredBFProperties properities) {
    if (properities == null) return;

    // Floating point arithmetic performance
    this.txtPerformance.setText(String.valueOf(properities.getFlopPerformance()));

    // Optional: Memory throughput calculation mode (setting with or without store)
    RequiredBFProperties.MEM_THROUGHPUT_CALC_MODE mem_throughput_calc_mode =
        this.properities.getMemThroughputCalcMode();
    if (mem_throughput_calc_mode == RequiredBFProperties.MEM_THROUGHPUT_CALC_MODE.AUTO) {
      this.radioMemThroughputCalcModeAuto.setSelected(true);
    } else if (mem_throughput_calc_mode == RequiredBFProperties.MEM_THROUGHPUT_CALC_MODE.STORE) {
      this.radioMemThroughputCalcModeStore.setSelected(true);
    } else if (mem_throughput_calc_mode == RequiredBFProperties.MEM_THROUGHPUT_CALC_MODE.NOSTORE) {
      this.radioMemThroughputCalcModeNostore.setSelected(true);
    }
    // Option: BF calculation unit
    RequiredBFProperties.BF_CALC_TYPE unit_type = this.properities.getBFCalcType();
    if (unit_type == RequiredBFProperties.BF_CALC_TYPE.BYTE_FLOP) {
      this.radioBFCalcTypeUnitBF.setSelected(true);
    } else if (unit_type == RequiredBFProperties.BF_CALC_TYPE.FLOP_BYTE) {
      this.radioBFCalcTypeUnitFB.setSelected(true);
    }
    // Optional: Default size
    int sizeReal = this.properities.getDefaultSizeReal();
    this.txtSizeReal.setText(String.valueOf(sizeReal));
    int sizeInteger = this.properities.getDefaultSizeInteger();
    this.txtSizeInteger.setText(String.valueOf(sizeInteger));

    // Memory access destination
    int count = properities.getRequiredBFCount();
    for (int i = 0; i < MEMORY_MAXROWS; i++) {
      if (i < count) {
        RequiredBF bf = properities.getRequiredBF(i);
        // Create row data
        // Memory access destination name
        this.lblNames[i].setText(bf.getName());
        // Memory throughput: with store
        this.txtMem_throughput_calc_mode_stores[i].setText(
            String.valueOf(bf.getMemThroughputStore()));
        // Memory Throughput: No Store
        this.txtMem_throughput_calc_mode_nostores[i].setText(
            String.valueOf(bf.getMemThroughputNostore()));
        // Coefficient
        this.txtCoefs[i].setText(String.valueOf(bf.getCoef()));
        // Background color button
        this.btnColors[i].setColor(bf.getBackColor());
        this.btnColors[i].addActionListener(this);
        // Enable / disable background color
        this.chkColors[i].setSelected((bf.getBackColor() == null));
        // Request BF calculation
        this.chkRequiredbfs[i].setSelected((bf.isRequiredBF()));
      } else {
        this.lblNames[i].setVisible(false);
        this.txtMem_throughput_calc_mode_stores[i].setVisible(false);
        this.txtMem_throughput_calc_mode_nostores[i].setVisible(false);
        this.txtCoefs[i].setVisible(false);
        this.btnColors[i].setVisible(false);
        this.chkColors[i].setVisible(false);
        this.chkRequiredbfs[i].setVisible(false);
      }
    }

    // Toggle the enable of the background color selection button
    setEnabledColorButtons();

    this.pack();
  }

  /**
   * Get the request Byte / FLOP setting.
   *
   * @return Request Byte / FLOP property
   */
  public RequiredBFProperties getRequiredBFProperties() {

    // Floating point arithmetic performance
    {
      float value = 0.0f;
      String cell = this.txtPerformance.getText();
      if (cell != null && !(cell.toString()).isEmpty() && StringUtils.isFloat(cell.toString())) {
        value = Float.parseFloat(cell.toString());
      }
      this.properities.setFlopPerformance(value);
    }
    // Optional: Throughput calculation mode
    {
      if (this.radioMemThroughputCalcModeAuto.isSelected()) {
        this.properities.setMemThroughputCalcMode(
            RequiredBFProperties.MEM_THROUGHPUT_CALC_MODE.AUTO);
      } else if (this.radioMemThroughputCalcModeStore.isSelected()) {
        this.properities.setMemThroughputCalcMode(
            RequiredBFProperties.MEM_THROUGHPUT_CALC_MODE.STORE);
      } else if (this.radioMemThroughputCalcModeNostore.isSelected()) {
        this.properities.setMemThroughputCalcMode(
            RequiredBFProperties.MEM_THROUGHPUT_CALC_MODE.NOSTORE);
      }
    }
    // Option: BF calculation unit
    {
      if (this.radioBFCalcTypeUnitBF.isSelected()) {
        this.properities.setCalcType(RequiredBFProperties.BF_CALC_TYPE.BYTE_FLOP);
      } else if (this.radioBFCalcTypeUnitFB.isSelected()) {
        this.properities.setCalcType(RequiredBFProperties.BF_CALC_TYPE.FLOP_BYTE);
      }
    }
    // Optional: Default size
    {
      // real
      int sizeReal = this.properities.DEFUALT_DATASIZE;
      String cellReal = this.txtSizeReal.getText();
      if (cellReal != null
          && !(cellReal.toString()).isEmpty()
          && StringUtils.isNumeric(cellReal.toString())) {
        sizeReal = Integer.parseInt(cellReal);
      }
      this.properities.setDefaultSizeReal(sizeReal);
      // Integer
      int sizeInteger = this.properities.DEFUALT_DATASIZE;
      String cellInteger = this.txtSizeInteger.getText();
      if (cellInteger != null
          && !(cellInteger).isEmpty()
          && StringUtils.isNumeric(cellInteger.toString())) {
        sizeInteger = Integer.parseInt(cellInteger);
      }
      this.properities.setDefaultSizeInteger(sizeInteger);
    }

    // Get the request Byte / FLOP setting from the request Byte / FLOP setting list
    int count = this.properities.getRequiredBFCount();
    for (int i = 0; i < count; i++) {
      // Memory throughput: with store
      {
        float value = 0.0f;
        String cell = this.txtMem_throughput_calc_mode_stores[i].getText();
        if (cell != null && !(cell.toString()).isEmpty() && StringUtils.isFloat(cell.toString())) {
          value = Float.parseFloat(cell.toString());
        }
        this.properities.getRequiredBF(i).setMemThroughputStore(value);
      }
      // Memory Throughput: No Store
      {
        float value = 0.0f;
        String cell = this.txtMem_throughput_calc_mode_nostores[i].getText();
        if (cell != null && !(cell.toString()).isEmpty() && StringUtils.isFloat(cell.toString())) {
          value = Float.parseFloat(cell.toString());
        }
        this.properities.getRequiredBF(i).setMemThroughputNostore(value);
      }
      // Coefficient
      {
        float value = 0.0f;
        String cell = this.txtCoefs[i].getText();
        if (cell != null && !(cell.toString()).isEmpty() && StringUtils.isFloat(cell.toString())) {
          value = Float.parseFloat(cell.toString());
        }
        this.properities.getRequiredBF(i).setCoef(value);
      }
      // Background color
      {
        if (!this.chkColors[i].isSelected()) {
          Color back = this.btnColors[i].getColor();
          this.properities.getRequiredBF(i).setBackColor(back);
        } else {
          this.properities.getRequiredBF(i).setBackColor(null);
        }
      }
      // Request BF calculation
      {
        boolean value = this.chkRequiredbfs[i].isSelected();
        this.properities.getRequiredBF(i).setRequiredBF(value);
      }
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
      getRequiredBFProperties();

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
      getRequiredBFProperties();

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
    // return to default
    else if (event.getSource() == this.btnDefault) {
      setRequiredBFProperties(this.properities.getDefaultProperties());
      return;
    }
    // Background color
    else if (event.getSource() instanceof JColorButton) {
      // Color selection dialog
      JColorButton button = (JColorButton) event.getSource();
      Color color =
          JColorChooser.showDialog(
              this,
              Message.getString("settingrequiredbfdialog.colorchooser.title"),
              button.getColor()); // Color selection
      if (color != null) {
        // Set a color icon on the button
        button.setColor(color);
      }
      return;
    }
  }

  /** Toggle the enable of the background color selection button */
  private void setEnabledColorButtons() {
    for (int i = 0; i < this.chkColors.length; i++) {
      // Toggle the enable of the background color selection button
      this.btnColors[i].setEnabled(!this.chkColors[i].isSelected());
    }
  }

  /**
   * Background color enable / disable checkbox change event. Toggle the enable of the background
   * color selection button
   *
   * @param event event
   */
  @Override
  public void stateChanged(ChangeEvent event) {
    for (int i = 0; i < this.chkColors.length; i++) {
      if (event.getSource() == this.chkColors[i]) {
        // Toggle the enable of the background color selection button
        setEnabledColorButtons();
        return;
      }
    }
  }
}
