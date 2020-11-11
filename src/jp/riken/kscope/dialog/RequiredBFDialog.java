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
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.data.RequiredBFResult;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.properties.RequiredBFProperties;
import jp.riken.kscope.properties.RequiredBFProperties.BF_CALC_TYPE;
import jp.riken.kscope.properties.VariableMemoryProperties;
import jp.riken.kscope.service.AnalysisMemoryService;

/**
 * Memory performance calculation result dialog class
 *
 * @author RIKEN
 */
public class RequiredBFDialog extends javax.swing.JDialog implements ActionListener {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Cancel button */
  private JButton btnCancel;
  /** OK button */
  private JButton btnOk;
  /** Variable access destination setting button */
  private JButton btnVariable;
  /** Request Byte / FLOP configuration property */
  private RequiredBFProperties propertiesMemoryband;
  /** Variable access destination memory setting */
  private VariableMemoryProperties propertiesVariable;
  /** Dialog return value */
  private int result = Constant.CANCEL_DIALOG;
  /** Return button to previous selection block */
  private JButton btnPrev;
  /** Button to go to next selection block */
  private JButton btnNext;
  /** Load calculation result label */
  private LabeledTextFeild txtLoad;
  /** Store calculation result label */
  private LabeledTextFeild txtStore;
  /** Calculation result label */
  private LabeledTextFeild txtFlop;
  /** Request B / F calculation result label */
  private LabeledTextFeild txtRequired;
  /** Throughput calculation result label */
  private LabeledTextFeild txtThroughput;
  /** Effective B / F calculation result label */
  private LabeledTextFeild txtEffective;
  /** Peak performance ratio calculation result label */
  private LabeledTextFeild txtPeak;
  /** Request B / F setting button */
  private JButton btnSetting;
  /** Calculation range label */
  private JLabel lblBlock;
  /** Analysis view addition panel */
  private JCheckBox chkAddList;
  /** Dialog width setting */
  private final int DEFAULT_WIDTH = 480;
  /** Variable access destination setting dialog */
  private VariableAccessDialog nextDialog;
  /** Parent dialog flag true = first called dialog, false = called from another dialog */
  private boolean ownerDialog;
  /** Selection block */
  private IBlock[] selectedblocks;
  /** Variable access settings */
  private JPanel panelVariable;
  /** Request Byte / FLOP calculation service */
  private AnalysisMemoryService serviceMemory;
  /** Currently displayed block */
  private int blockindex;
  /** Request B / F unit */
  private JLabel lblUnitRequired;
  /** Effective B / F unit */
  private JLabel lblUnitEffective;
  /** Byte / FLOP unit string */
  private final String UNIT_BITE_FLOP = "(Byte/FLOP)";
  /** FLOP / Byte unit character string */
  private final String UNIT_FLOP_BITE = "(FLOP/Byte)";

  /**
   * Constructor
   *
   * @param frame Parent frame
   */
  public RequiredBFDialog(JFrame frame) {
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
  public RequiredBFDialog(Frame frame, boolean modal) {
    super(frame, modal);
    nextDialog = null;
    this.ownerDialog = true;
    initGUI();
  }

  /** Initialize the GUI. */
  private void initGUI() {
    try {
      // Button panel
      {
        JPanel panelButtons = new JPanel();
        getContentPane().add(panelButtons, BorderLayout.SOUTH);
        GridBagLayout layout = new GridBagLayout();
        layout.rowWeights = new double[] {0.0};
        layout.rowHeights = new int[] {24};
        layout.columnWeights = new double[] {1.0, 0.0, 1.0};
        layout.columnWidths = new int[] {120, 50, 120};
        panelButtons.setLayout(layout);
        panelButtons.setPreferredSize(new java.awt.Dimension(390, 45));

        // Main button size
        java.awt.Dimension buttonSize = new java.awt.Dimension(96, 22);
        {
          btnOk = new JButton();
          panelButtons.add(
              this.btnOk,
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
          String text = Message.getString("dialog.common.button.close"); // close
          btnOk.setText(text);
          btnOk.setPreferredSize(buttonSize);
          btnOk.addActionListener(this);
        }
        {
          chkAddList = new JCheckBox();
          panelButtons.add(
              this.chkAddList,
              new GridBagConstraints(
                  2,
                  0,
                  1,
                  1,
                  0.0,
                  0.0,
                  GridBagConstraints.WEST,
                  GridBagConstraints.NONE,
                  new Insets(0, 14, 0, 0),
                  0,
                  0));
          String text =
              Message.getString("requiredbfdialog.checkbox.addlist"); // Add to list and close
          chkAddList.setText(text);
          chkAddList.setSelected(true);
        }
      }
      // Content panel
      {
        JPanel panelContent = new JPanel(new BorderLayout());
        getContentPane().add(panelContent, BorderLayout.CENTER);
        Border border = new EmptyBorder(7, 7, 0, 7);
        panelContent.setBorder(border);

        // Select block
        {
          JPanel panelHeader = new JPanel(new BorderLayout());
          panelContent.add(panelHeader, BorderLayout.NORTH);
          GridBagLayout layout = new GridBagLayout();
          layout.rowWeights = new double[] {0.0, 1.0};
          layout.rowHeights = new int[] {24, 4};
          layout.columnWeights = new double[] {0.0, 1.0, 0.0};
          layout.columnWidths = new int[] {50, 50, 50};
          panelHeader.setLayout(layout);
          JPanel panel = new JPanel();
          panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
          String text =
              Message.getString("requiredbfdialog.label.calculateatea"); // Calculation range
          JLabel label = new JLabel(text);
          this.lblBlock = new JLabel("jacobi[962] do loop=1, nn");
          panel.add(Box.createGlue());
          panel.add(label);
          panel.add(Box.createRigidArea(new Dimension(16, 1)));
          panel.add(this.lblBlock);
          panel.add(Box.createGlue());
          panelHeader.add(
              panel,
              new GridBagConstraints(
                  1,
                  0,
                  1,
                  1,
                  0.0,
                  0.0,
                  GridBagConstraints.CENTER,
                  GridBagConstraints.HORIZONTAL,
                  new Insets(0, 0, 0, 0),
                  0,
                  0));
          this.btnPrev = new JButton("<<");
          this.btnNext = new JButton(">>");
          this.btnPrev.setMargin(new Insets(0, 3, 0, 3));
          this.btnNext.setMargin(new Insets(0, 3, 0, 3));
          this.btnPrev.addActionListener(this);
          this.btnNext.addActionListener(this);
          panelHeader.add(
              this.btnPrev,
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
          panelHeader.add(
              this.btnNext,
              new GridBagConstraints(
                  2,
                  0,
                  1,
                  1,
                  0.0,
                  0.0,
                  GridBagConstraints.EAST,
                  GridBagConstraints.NONE,
                  new Insets(0, 0, 0, 0),
                  0,
                  0));
        }
        // Memory performance calculation result
        {
          JPanel panelList = new JPanel(new BorderLayout());
          panelContent.add(panelList, BorderLayout.CENTER);
          String text =
              Message.getString(
                  "requiredbfdialog.frame.performance"); // Memory performance calculation result
          TitledBorder titleBorder = new TitledBorder(BorderFactory.createEtchedBorder(), text);
          panelList.setBorder(titleBorder);
          {
            JPanel panelMemory = new JPanel();
            panelList.add(panelMemory, BorderLayout.CENTER);
            GridBagLayout layoutMemory = new GridBagLayout();
            layoutMemory.rowWeights = new double[] {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0};
            layoutMemory.rowHeights = new int[] {7, 24, 24, 24, 24, 24, 24, 2};
            layoutMemory.columnWeights = new double[] {0.0, 0.0, 0.0, 1.0};
            layoutMemory.columnWidths = new int[] {50, 64, 64, 7};
            panelMemory.setLayout(layoutMemory);

            int columns = 10;
            // Load
            {
              JLabel header = new JLabel("Load");
              panelMemory.add(
                  header,
                  new GridBagConstraints(
                      0,
                      0,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.EAST,
                      GridBagConstraints.NONE,
                      new Insets(0, 8, 0, 8),
                      0,
                      0));
              this.txtLoad = new LabeledTextFeild(columns);
              panelMemory.add(
                  this.txtLoad,
                  new GridBagConstraints(
                      1,
                      0,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.WEST,
                      GridBagConstraints.NONE,
                      new Insets(0, 8, 0, 8),
                      0,
                      0));
              JLabel unit = new JLabel("(Byte)");
              panelMemory.add(
                  unit,
                  new GridBagConstraints(
                      2,
                      0,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.WEST,
                      GridBagConstraints.NONE,
                      new Insets(0, 8, 0, 8),
                      0,
                      0));
            }
            // Store
            {
              JLabel header = new JLabel("Store");
              panelMemory.add(
                  header,
                  new GridBagConstraints(
                      0,
                      1,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.EAST,
                      GridBagConstraints.NONE,
                      new Insets(0, 8, 0, 8),
                      0,
                      0));
              this.txtStore = new LabeledTextFeild(columns);
              panelMemory.add(
                  this.txtStore,
                  new GridBagConstraints(
                      1,
                      1,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.WEST,
                      GridBagConstraints.NONE,
                      new Insets(0, 8, 0, 8),
                      0,
                      0));
              JLabel unit = new JLabel("(Byte)");
              panelMemory.add(
                  unit,
                  new GridBagConstraints(
                      2,
                      1,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.WEST,
                      GridBagConstraints.NONE,
                      new Insets(0, 8, 0, 8),
                      0,
                      0));
            }
            // FLOP
            {
              String textlabel =
                  Message.getString("requiredbfdialog.label.flop"); // Number of operations
              JLabel header = new JLabel(textlabel);
              panelMemory.add(
                  header,
                  new GridBagConstraints(
                      0,
                      2,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.EAST,
                      GridBagConstraints.NONE,
                      new Insets(0, 8, 0, 8),
                      0,
                      0));
              this.txtFlop = new LabeledTextFeild(columns);
              panelMemory.add(
                  this.txtFlop,
                  new GridBagConstraints(
                      1,
                      2,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.WEST,
                      GridBagConstraints.NONE,
                      new Insets(0, 8, 0, 8),
                      0,
                      0));
              JLabel unit = new JLabel("(FLOP)");
              panelMemory.add(
                  unit,
                  new GridBagConstraints(
                      2,
                      2,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.WEST,
                      GridBagConstraints.NONE,
                      new Insets(0, 8, 0, 8),
                      0,
                      0));
            }
            // Request B / F
            {
              String textlabel =
                  Message.getString("requiredbfdialog.label.required"); // Request B / F
              JLabel header = new JLabel(textlabel);
              panelMemory.add(
                  header,
                  new GridBagConstraints(
                      0,
                      3,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.EAST,
                      GridBagConstraints.NONE,
                      new Insets(0, 8, 0, 8),
                      0,
                      0));
              this.txtRequired = new LabeledTextFeild(columns);
              panelMemory.add(
                  this.txtRequired,
                  new GridBagConstraints(
                      1,
                      3,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.WEST,
                      GridBagConstraints.NONE,
                      new Insets(0, 8, 0, 8),
                      0,
                      0));
              lblUnitRequired = new JLabel(UNIT_BITE_FLOP);
              panelMemory.add(
                  lblUnitRequired,
                  new GridBagConstraints(
                      2,
                      3,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.WEST,
                      GridBagConstraints.NONE,
                      new Insets(0, 8, 0, 8),
                      0,
                      0));
            }
            // Throughput
            {
              String textlabel =
                  Message.getString("requiredbfdialog.label.throughput"); // throughput
              JLabel header = new JLabel(textlabel);
              panelMemory.add(
                  header,
                  new GridBagConstraints(
                      0,
                      4,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.EAST,
                      GridBagConstraints.NONE,
                      new Insets(0, 8, 0, 8),
                      0,
                      0));
              this.txtThroughput = new LabeledTextFeild(columns);
              panelMemory.add(
                  this.txtThroughput,
                  new GridBagConstraints(
                      1,
                      4,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.WEST,
                      GridBagConstraints.NONE,
                      new Insets(0, 8, 0, 8),
                      0,
                      0));
              JLabel unit = new JLabel("(GB/s)");
              panelMemory.add(
                  unit,
                  new GridBagConstraints(
                      2,
                      4,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.WEST,
                      GridBagConstraints.NONE,
                      new Insets(0, 8, 0, 8),
                      0,
                      0));
            }
            // Effective B / F
            {
              String textlabel =
                  Message.getString("requiredbfdialog.label.effective"); // Effective B / F
              JLabel header = new JLabel(textlabel);
              panelMemory.add(
                  header,
                  new GridBagConstraints(
                      0,
                      5,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.EAST,
                      GridBagConstraints.NONE,
                      new Insets(0, 8, 0, 8),
                      0,
                      0));
              this.txtEffective = new LabeledTextFeild(columns);
              panelMemory.add(
                  this.txtEffective,
                  new GridBagConstraints(
                      1,
                      5,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.WEST,
                      GridBagConstraints.NONE,
                      new Insets(0, 8, 0, 8),
                      0,
                      0));
              lblUnitEffective = new JLabel(UNIT_BITE_FLOP);
              panelMemory.add(
                  lblUnitEffective,
                  new GridBagConstraints(
                      2,
                      5,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.WEST,
                      GridBagConstraints.NONE,
                      new Insets(0, 8, 0, 8),
                      0,
                      0));
            }
            // Peak performance ratio
            {
              String textlabel =
                  Message.getString("requiredbfdialog.label.peak"); // Peak performance ratio
              JLabel header = new JLabel(textlabel);
              panelMemory.add(
                  header,
                  new GridBagConstraints(
                      0,
                      6,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.EAST,
                      GridBagConstraints.NONE,
                      new Insets(0, 8, 0, 8),
                      0,
                      0));
              this.txtPeak = new LabeledTextFeild(columns);
              panelMemory.add(
                  this.txtPeak,
                  new GridBagConstraints(
                      1,
                      6,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.WEST,
                      GridBagConstraints.NONE,
                      new Insets(0, 8, 0, 8),
                      0,
                      0));
              JLabel unit = new JLabel("(%)");
              panelMemory.add(
                  unit,
                  new GridBagConstraints(
                      2,
                      6,
                      1,
                      1,
                      0.0,
                      0.0,
                      GridBagConstraints.WEST,
                      GridBagConstraints.NONE,
                      new Insets(0, 8, 0, 8),
                      0,
                      0));
            }
          }
        }
        // Calculation settings
        {
          JPanel panelSettings = new JPanel();
          panelSettings.setLayout(new BoxLayout(panelSettings, BoxLayout.Y_AXIS));
          panelContent.add(panelSettings, BorderLayout.SOUTH);
          panelSettings.add(Box.createRigidArea(new Dimension(1, 7)));
          // Throughput settings
          {
            JPanel panel = new JPanel();
            FlowLayout layout = new FlowLayout(FlowLayout.RIGHT, 0, 0);
            layout.setHgap(24);
            panel.setLayout(layout);
            panelSettings.add(panel);
            String text =
                Message.getString(
                    "requiredbfdialog.setting.label.throughput"); // Throughput settings
            JLabel header = new JLabel(text);
            panel.add(header);
            this.btnSetting =
                new JButton(Message.getString("dialog.common.button.setting")); // Configuration);
            panel.add(this.btnSetting);
            this.btnSetting.setMargin(new Insets(0, 3, 0, 3));
            this.btnSetting.addActionListener(this);
          }
          panelSettings.add(Box.createRigidArea(new Dimension(1, 4)));
          // Variable access settings: No calculation when displaying from the variable access
          // settings dialog.
          if (this.ownerDialog) {
            panelVariable = new JPanel();
            FlowLayout layout = new FlowLayout(FlowLayout.RIGHT, 0, 0);
            layout.setHgap(24);
            panelVariable.setLayout(layout);
            panelSettings.add(panelVariable);
            String text =
                Message.getString(
                    "requiredbfdialog.setting.label.variable"); // Variable access settings
            JLabel header = new JLabel(text);
            panelVariable.add(header);
            // Variable access destination setting button
            btnVariable =
                new JButton(Message.getString("dialog.common.button.setting")); // Configuration);
            panelVariable.add(btnVariable);
            this.btnVariable.setMargin(new Insets(0, 3, 0, 3));
            this.btnVariable.addActionListener(this);
          }
        }
      }
      String text = Message.getString("requiredbfdialog.title"); // Request Byte / FLOP calculation
      setTitle(text);
      this.pack();

      Dimension size = new Dimension(DEFAULT_WIDTH, this.getSize().height);
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
    // Calculate the request Byte / FLOP.
    this.blockindex = 0;
    setArrowButton();
    if (this.selectedblocks != null && this.selectedblocks.length > 0) {
      calculateRequiredByteFlop(this.selectedblocks[this.blockindex]);
    }

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
      if (this.chkAddList.isSelected()) {
        // Add to list.
        setAnalysisPanel();
      }
      // Close the dialog.
      dispose();
      return;
    }
    if (event.getSource() == this.btnVariable) {
      // Display the variable access destination setting dialog.
      if (this.nextDialog != null) {
        this.setVisible(false);
        this.nextDialog.setOwnerDialog(false);
        this.nextDialog.showDialog();

        // Calculate the request Byte / FLOP.
        if (this.selectedblocks != null && this.selectedblocks.length > 0) {
          calculateRequiredByteFlop(this.selectedblocks[this.blockindex]);
        }
        this.setVisible(true);
      }
      return;
    }
    // close
    else if (event.getSource() == this.btnCancel) {
      this.result = Constant.CANCEL_DIALOG;
      // Close the dialog.
      dispose();
      return;
    }
    // Throughput settings
    else if (event.getSource() == this.btnSetting) {
      // Display the request Byte / FLOP setting dialog.
      SettingRequiredBFDialog dialog =
          new SettingRequiredBFDialog(this, true, this.propertiesMemoryband);
      dialog.showDialog();
      // Calculate the request Byte / FLOP.
      if (this.selectedblocks != null && this.selectedblocks.length > 0) {
        calculateRequiredByteFlop(this.selectedblocks[this.blockindex]);
      }
      return;
    }
    // Previous block display
    else if (event.getSource() == this.btnPrev) {
      if (this.blockindex > 0) this.blockindex--;
      setArrowButton();
      // Calculate the request Byte / FLOP.
      if (this.selectedblocks != null && this.selectedblocks.length > 0) {
        calculateRequiredByteFlop(this.selectedblocks[this.blockindex]);
      }
    }
    // Next block display
    else if (event.getSource() == this.btnNext) {
      if (this.selectedblocks != null && this.selectedblocks.length > 0) {
        if (this.blockindex < this.selectedblocks.length - 1) this.blockindex++;
      }
      setArrowButton();
      // Calculate the request Byte / FLOP.
      if (this.selectedblocks != null && this.selectedblocks.length > 0) {
        calculateRequiredByteFlop(this.selectedblocks[this.blockindex]);
      }
    }
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
    this.blockindex = 0;
    setArrowButton();
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
   * Label alternative text field class. Non-editable transparent text box
   *
   * @author RIKEN
   */
  class LabeledTextFeild extends JTextField {
    /** Serial number */
    private static final long serialVersionUID = 1L;

    /** Constructor */
    public LabeledTextFeild() {
      super();
      setLabeled();
    }

    /**
     * Constructor
     *
     * @param text Display string
     */
    public LabeledTextFeild(String text) {
      super(text);
      setLabeled();
    }

    /**
     * Constructor
     *
     * @param text Display string
     * @param columns Display width
     */
    public LabeledTextFeild(String text, int columns) {
      super(text, columns);
      setLabeled();
    }

    /**
     * Constructor
     *
     * @param columns Display width
     */
    public LabeledTextFeild(int columns) {
      super(columns);
      setLabeled();
    }

    /** Set the display. Cannot be edited like a label. */
    private void setLabeled() {
      this.setOpaque(false);
      this.setEditable(false);
      this.setBorder(null);
      this.setHorizontalAlignment(JTextField.RIGHT);
    }
  }

  /**
   * Set the variable access destination setting dialog.
   *
   * @param dialog Variable access destination setting dialog
   */
  public void setVariableAccessDialog(VariableAccessDialog dialog) {
    this.nextDialog = dialog;
  }

  /**
   * Set the parent dialog flag.
   *
   * @param owner Parent dialog flag
   */
  public void setOwnerDialog(boolean owner) {
    this.ownerDialog = owner;
    // Hide the variable access settings panel
    this.panelVariable.setVisible(this.ownerDialog);
  }

  /**
   * Set the request Byte / FLOP calculation service.
   *
   * @param service Request Byte / FLOP calculation service
   */
  public void setServiceMemory(AnalysisMemoryService service) {
    this.serviceMemory = service;
  }

  /**
   * Set the request Byte / FLOP setting property.
   *
   * @param properities Request Byte / FLOP configuration properties
   */
  public void setPropertiesMemoryband(RequiredBFProperties properities) {
    this.propertiesMemoryband = properities;
  }

  /** Set the display of the Next button before the display block. */
  private void setArrowButton() {
    if (this.selectedblocks == null || this.selectedblocks.length <= 0) {
      this.btnPrev.setVisible(false);
      this.btnNext.setVisible(false);
      return;
    } else if (this.selectedblocks.length == 1) {
      this.btnPrev.setVisible(false);
      this.btnNext.setVisible(false);
      return;
    }

    this.btnPrev.setVisible(true);
    this.btnNext.setVisible(true);
    this.btnPrev.setEnabled(true);
    this.btnNext.setEnabled(true);
    if (this.blockindex == 0) {
      this.btnPrev.setEnabled(false);
    } else if (this.blockindex == this.selectedblocks.length - 1) {
      this.btnNext.setEnabled(false);
    }
  }

  /**
   * Calculate the request Byte / FLOP.
   *
   * @param block Calculation block
   */
  private void calculateRequiredByteFlop(IBlock block) {
    if (block == null) return;
    // Calculate the request Byte / FLOP
    RequiredBFResult result = this.serviceMemory.calcRequiredBF(block);
    // Display the request Byte / FLOP calculation result
    setRequiredByteFlopResult(result);
  }

  /**
   * Display the request Byte / FLOP calculation result.
   *
   * @param result Request Byte / FLOP calculation result
   */
  private void setRequiredByteFlopResult(RequiredBFResult result) {
    clearRequiredByteFlopResult();
    if (result == null) return;

    // block
    this.lblBlock.setText(result.getBlock().toString());
    // Load calculation result
    this.txtLoad.setText(String.format("%d", result.getLoad()));
    // Store calculation result
    this.txtStore.setText(String.format("%d", result.getStore()));
    // Calculation result of number of operations
    this.txtFlop.setText(String.format("%d", result.getOperand()));
    // Request B / F calculation result
    float required = result.getRequiredBF();
    if (this.propertiesMemoryband.getBFCalcType() == BF_CALC_TYPE.FLOP_BYTE) {
      required = result.getRequiredFB();
    }
    this.txtRequired.setText(String.format("%.2f", required));
    // Throughput calculation result label
    this.txtThroughput.setText(String.format("%.2f", result.getThroughput()));
    // Effective B / F calculation result label
    float effective = result.getEffectiveBF();
    if (this.propertiesMemoryband.getBFCalcType() == BF_CALC_TYPE.FLOP_BYTE) {
      effective = result.getEffectiveFB();
    }
    this.txtEffective.setText(String.format("%.2f", effective));
    // Peak performance ratio calculation result label
    float peak = result.getPeakBF();
    if (this.propertiesMemoryband.getBFCalcType() == BF_CALC_TYPE.FLOP_BYTE) {
      peak = result.getPeakFB();
    }
    peak *= 100.0F; // % display
    this.txtPeak.setText(String.format("%.2f", peak));
    // Unit display
    this.lblUnitRequired.setText(UNIT_BITE_FLOP);
    this.lblUnitEffective.setText(UNIT_BITE_FLOP);
    if (this.propertiesMemoryband.getBFCalcType() == BF_CALC_TYPE.FLOP_BYTE) {
      this.lblUnitRequired.setText(UNIT_FLOP_BITE);
      this.lblUnitEffective.setText(UNIT_FLOP_BITE);
    }
  }

  /** Clear the request Byte / FLOP calculation result. */
  private void clearRequiredByteFlopResult() {
    // block
    this.lblBlock.setText("");
    // Load calculation result
    this.txtLoad.setText("");
    // Store calculation result
    this.txtStore.setText("");
    /** Calculation result label */
    this.txtFlop.setText("");
    /** Request B / F calculation result label */
    this.txtRequired.setText("");
    /** Throughput calculation result label */
    this.txtThroughput.setText("");
    /** Effective B / F calculation result label */
    this.txtEffective.setText("");
    /** Peak performance ratio calculation result label */
    this.txtPeak.setText("");
  }

  /** Add the calculation result to the analysis view. */
  private void setAnalysisPanel() {
    if (this.selectedblocks == null) return;

    List<RequiredBFResult> list = new ArrayList<RequiredBFResult>();
    for (IBlock block : this.selectedblocks) {
      // Calculate the request Byte / FLOP
      RequiredBFResult result = this.serviceMemory.calcRequiredBF(block);
      if (result != null) {
        list.add(result);
      }
    }

    if (list.size() <= 0) return;
    this.serviceMemory.setAnalysisPanel(list.toArray(new RequiredBFResult[0]));
  }
}
