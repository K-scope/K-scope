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
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.properties.ProfilerProperties;
// import jp.riken.kscope.utils.StringUtils;

/**
 * Detailed profiler measurement interval setting dialog class
 *
 * @author RIKEN
 */
public class EprofStatementDialog extends javax.swing.JDialog implements ActionListener {

  /** Default serial number */
  private static final long serialVersionUID = 1L;
  /** OK button */
  private JButton btnOk;
  /** CANCEL button */
  private JButton btnCancel;
  /** Group name text box */
  private JTextField txtGroupname;
  /** Detail number text box */
  private JTextField txtNumber;
  /** Priority level */
  private JTextField txtLevel;
  /** Profiler Properties */
  private ProfilerProperties properties;
  /** Dialog return value */
  private int result = Constant.CANCEL_DIALOG;
  /** Group name label */
  private JLabel labelName;
  /** Detail number label */
  private JLabel labelNumber;
  /** Priority label */
  private JLabel labelLevel;

  /**
   * Constructor
   *
   * @param owner parent frame
   * @param modal true = Show modal dialog
   */
  public EprofStatementDialog(Frame owner, boolean modal) {
    super(owner, modal);
    initGUI();
  }

  /** Initialize the screen */
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
        // Profiler
        {
          JPanel panelContent = new JPanel();
          GridBagLayout panelContentLayout = new GridBagLayout();
          panelContentLayout.columnWidths = new int[] {120, 160};
          panelContentLayout.rowHeights = new int[] {25, 25, 25};
          panelContentLayout.columnWeights = new double[] {1.0, 1.0};
          panelContentLayout.rowWeights = new double[] {1.0, 1.0, 1.0};
          getContentPane().add(panelContent, BorderLayout.CENTER);
          panelContent.setLayout(panelContentLayout);
          panelContent.setPreferredSize(new java.awt.Dimension(390, 230));
          // group name
          {
            labelName = new JLabel();
            panelContent.add(
                labelName,
                new GridBagConstraints(
                    0,
                    0,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.EAST,
                    GridBagConstraints.NONE,
                    new Insets(0, 0, 0, 5),
                    0,
                    0));
            labelName.setText(
                Message.getString("eprofstatementdialog.groupname.title")); // group name
          }
          {
            txtGroupname = new JTextField();
            panelContent.add(
                txtGroupname,
                new GridBagConstraints(
                    1,
                    0,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.WEST,
                    GridBagConstraints.HORIZONTAL,
                    new Insets(0, 5, 0, 10),
                    0,
                    0));
          }
          // Detail number
          {
            labelNumber = new JLabel();
            panelContent.add(
                labelNumber,
                new GridBagConstraints(
                    0,
                    1,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.EAST,
                    GridBagConstraints.NONE,
                    new Insets(0, 0, 0, 5),
                    0,
                    0));
            labelNumber.setText(
                Message.getString("eprofstatementdialog.detailnum.title")); // Detail number
          }
          {
            txtNumber = new JTextField();
            panelContent.add(
                txtNumber,
                new GridBagConstraints(
                    1,
                    1,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.WEST,
                    GridBagConstraints.HORIZONTAL,
                    new Insets(0, 5, 0, 10),
                    0,
                    0));
          }
          // Priority level
          {
            labelLevel = new JLabel();
            panelContent.add(
                labelLevel,
                new GridBagConstraints(
                    0,
                    2,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.EAST,
                    GridBagConstraints.NONE,
                    new Insets(0, 0, 0, 5),
                    0,
                    0));
            labelLevel.setText(
                Message.getString("eprofstatementdialog.priority.title")); // Priority level
          }
          {
            txtLevel = new JTextField();
            panelContent.add(
                txtLevel,
                new GridBagConstraints(
                    1,
                    2,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.WEST,
                    GridBagConstraints.HORIZONTAL,
                    new Insets(0, 5, 0, 10),
                    0,
                    0));
          }
        }
      }
      // Set the text box enable.
      setEnabledText();

      this.setTitle(
          Message.getString("eprofstatementdialog.dialog.desc")); // Measurement interval setting
      this.setSize(320, 160);

    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /** Set the text box enable. */
  private void setEnabledText() {
    if (this.properties == null) return;

    // group name
    boolean enabledName = this.properties.existsMacroErofName();
    labelName.setEnabled(enabledName);
    txtGroupname.setEnabled(enabledName);

    // Detail number
    boolean enabledNumber = this.properties.existsMacroErofNumber();
    labelNumber.setEnabled(enabledNumber);
    txtNumber.setEnabled(enabledNumber);

    // Priority level
    boolean enabledLevel = this.properties.existsMacroErofNumber();
    labelLevel.setEnabled(enabledLevel);
    txtLevel.setEnabled(enabledLevel);
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
      // Measurement interval setting check
      if (validateEprof()) {
        this.result = Constant.OK_DIALOG;
        // Close the dialog.
        dispose();
        return;
      }
    }
    // Cancel
    else if (event.getSource() == this.btnCancel) {
      this.result = Constant.CANCEL_DIALOG;
      // Close the dialog.
      dispose();
      return;
    }
  }

  /**
   * Input check of measurement interval setting
   *
   * @return true = normal
   */
  private boolean validateEprof() {
    String error = "";
    if (this.txtGroupname.isEnabled()) {
      if (this.txtGroupname.getText() == null || this.txtGroupname.getText().isEmpty()) {
        error +=
            Message.getString(
                "eprofstatementdialog.groupname.error.message"); // Please enter the group name. \ n
      }
    }
    if (this.txtNumber.isEnabled()) {
      if (this.txtNumber.getText() == null || this.txtNumber.getText().isEmpty()) {
        error +=
            Message.getString(
                "eprofstatementdialog.detailnum.error.message"); // Please enter the detailed
        // number. \ n
      }
      //            else if (!StringUtils.isNumeric(this.txtNumber.getText())){
      // error + = "Enter the detail number as a number. \ N";
      //            }
    }
    if (this.txtLevel.isEnabled()) {
      if (this.txtLevel.getText() == null || this.txtLevel.getText().isEmpty()) {
        error +=
            Message.getString(
                "eprofstatementdialog.priority.error.message"); // Enter the priority level. \ n
      }
      //            else if (!StringUtils.isNumeric(this.txtLevel.getText())){
      // error + = "Enter the priority level as a number. \ N";
      //            }
    }
    if (error != null && !error.isEmpty()) {
      JOptionPane.showMessageDialog(
          this,
          error.trim(),
          Message.getString("dialog.common.error"), // error
          JOptionPane.ERROR_MESSAGE);
      return false;
    }

    return true;
  }

  /**
   * Get the group name
   *
   * @return group name
   */
  public String getGroupname() {
    if (!this.txtGroupname.isEditable()) {
      return null;
    }
    return this.txtGroupname.getText();
  }

  /**
   * Get the detail number
   *
   * @return Detail number
   */
  public String getNumber() {
    if (!this.txtNumber.isEnabled()) {
      return null;
    }
    return this.txtNumber.getText();
  }

  /**
   * Get priority level
   *
   * @return Priority level
   */
  public String getLevel() {
    if (!this.txtLevel.isEnabled()) {
      return null;
    }
    return this.txtLevel.getText();
  }

  /**
   * Set profiler properties
   *
   * @param properties Profiler properties
   */
  public void setProperties(ProfilerProperties properties) {
    this.properties = properties;
    // Set the text box enable.
    setEnabledText();
  }
}
