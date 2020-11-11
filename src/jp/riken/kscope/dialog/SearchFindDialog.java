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
// import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;

/**
 * Search dialog class
 *
 * @author RIKEN
 */
public class SearchFindDialog extends javax.swing.JDialog implements ActionListener {
  /** Serial number */
  private static final long serialVersionUID = 1L;
  /** Search button */
  private JButton btnOk;
  /** Close button */
  private JButton btnClose;
  /** Word search button */
  private JCheckBox chkWord;
  /** Search other files */
  private JCheckBox chkOpenFiles;
  /** Regular expression search */
  private JCheckBox chkRegex;
  /** Case-sensitive search (true = case-sensitive) */
  private JCheckBox chkSensitivecase;
  /** Search string */
  private JTextField txtSearch;

  /** Dialog return value */
  private int result = Constant.CANCEL_DIALOG;

  /**
   * Constructor
   *
   * @param owner parent frame
   * @param modal true = Show modal dialog
   */
  public SearchFindDialog(Frame owner, boolean modal) {
    super(owner, modal);
    initGUI();
  }

  /**
   * Constructor
   *
   * @param frame Parent frame
   */
  public SearchFindDialog(JFrame frame) {
    super(frame);
    initGUI();
  }

  /** Initialize the GUI. */
  private void initGUI() {
    try {
      // Button panel
      {
        JPanel panelButtons = new JPanel();
        FlowLayout panelButtonsLayout = new FlowLayout();
        panelButtonsLayout.setHgap(10);
        panelButtonsLayout.setVgap(10);
        panelButtons.setLayout(panelButtonsLayout);
        getContentPane().add(panelButtons, BorderLayout.SOUTH);
        panelButtons.setPreferredSize(new java.awt.Dimension(390, 41));

        // search button
        java.awt.Dimension buttonSize = new java.awt.Dimension(96, 22);
        {
          btnOk = new JButton();
          btnOk.setText(Message.getString("mainmenu.search")); // search
          btnOk.setPreferredSize(buttonSize);
          btnOk.addActionListener(this);
          panelButtons.add(btnOk);
        }
        // Close button
        {
          btnClose = new JButton();
          btnClose.setText(Message.getString("dialog.common.button.cancel")); // Cancel
          btnClose.setMargin(new Insets(0, 5, 0, 5));
          btnClose.setPreferredSize(buttonSize);
          btnClose.addActionListener(this);
          panelButtons.add(btnClose);
        }
      }

      // Search content
      {
        JPanel panelContent = new JPanel();
        GridBagLayout panelContentLayout = new GridBagLayout();
        panelContentLayout.columnWidths = new int[] {10, 100, 10, 10};
        panelContentLayout.rowHeights = new int[] {7, 7, 7, 7, 7, 7, 7};
        panelContentLayout.columnWeights = new double[] {0.0, 0.0, 1.0, 0.0};
        panelContentLayout.rowWeights = new double[] {0.0, 0.1, 0.1, 0.1, 0.1, 0.1, 0};
        getContentPane().add(panelContent, BorderLayout.CENTER);
        panelContent.setLayout(panelContentLayout);

        // descriptive label
        {
          JLabel label = new JLabel();
          panelContent.add(
              label,
              new GridBagConstraints(
                  1,
                  0,
                  2,
                  1,
                  0.0,
                  0.0,
                  GridBagConstraints.NORTHWEST,
                  GridBagConstraints.NONE,
                  new Insets(0, 0, 7, 0),
                  0,
                  0));
          label.setText(
              Message.getString(
                  "searchfinddialog.label.desc")); // Perform a text search from the displayed
                                                   // source code.
          // label.setForeground(Color.BLUE);
        }
        // Search label
        {
          JLabel lblSearch = new JLabel();
          panelContent.add(
              lblSearch,
              new GridBagConstraints(
                  1,
                  1,
                  1,
                  1,
                  0.0,
                  0.0,
                  GridBagConstraints.EAST,
                  GridBagConstraints.NONE,
                  new Insets(0, 0, 0, 0),
                  0,
                  0));
          lblSearch.setText(
              Message.getString("searchfinddialog.label.searchword")); // Search character:
        }
        // Search text box
        {
          txtSearch = new JTextField();
          panelContent.add(
              txtSearch,
              new GridBagConstraints(
                  2,
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
        // Option label
        {
          JLabel lblOption = new JLabel();
          panelContent.add(
              lblOption,
              new GridBagConstraints(
                  1,
                  2,
                  1,
                  1,
                  0.0,
                  0.0,
                  GridBagConstraints.EAST,
                  GridBagConstraints.NONE,
                  new Insets(0, 0, 0, 0),
                  0,
                  0));
          lblOption.setText(Message.getString("searchfinddialog.label.options")); // option:
        }
        // Optional: Case sensitive
        {
          chkSensitivecase = new JCheckBox();
          panelContent.add(
              chkSensitivecase,
              new GridBagConstraints(
                  2,
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
          chkSensitivecase.setText(
              Message.getString("searchfinddialog.checkbox.upper-lower")); // Case sensitive
          chkSensitivecase.setMargin(new java.awt.Insets(2, 10, 2, 1));
        }
        // Optional: Word search
        {
          chkWord = new JCheckBox();
          panelContent.add(
              chkWord,
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
          chkWord.setText(Message.getString("searchfinddialog.checkbox.word")); // word search
          chkWord.setMargin(new java.awt.Insets(2, 10, 2, 1));
        }
        // Optional: Regular expression
        {
          chkRegex = new JCheckBox();
          panelContent.add(
              chkRegex,
              new GridBagConstraints(
                  2,
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
          chkRegex.setText(
              Message.getString("searchfinddialog.checkbox.regex")); // Regular expressions
          chkRegex.setMargin(new java.awt.Insets(2, 10, 2, 1));
        }
        // Optional: Search for other open sources
        {
          chkOpenFiles = new JCheckBox();
          panelContent.add(
              chkOpenFiles,
              new GridBagConstraints(
                  2,
                  5,
                  1,
                  1,
                  0.0,
                  0.0,
                  GridBagConstraints.WEST,
                  GridBagConstraints.NONE,
                  new Insets(0, 0, 0, 0),
                  0,
                  0));
          chkOpenFiles.setText(
              Message.getString(
                  "searchfinddialog.checkbox.othersource")); // Search other open sources
          chkOpenFiles.setMargin(new java.awt.Insets(2, 10, 2, 1));
        }
      }
      this.setTitle(Message.getString("mainmenu.search")); // search
      this.setSize(435, 240);
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

    // Registration
    if (event.getSource() == this.btnOk) {
      // Check the input
      if (!validateSearch()) {
        // error
        return;
      }
      this.result = Constant.OK_DIALOG;
      // Close the dialog.
      dispose();
      return;
    }
    // close
    else if (event.getSource() == this.btnClose) {
      this.result = Constant.CANCEL_DIALOG;
      // Close the dialog.
      dispose();
      return;
    }
  }

  /**
   * Get case sensitive
   *
   * @return Case sensitive
   */
  public boolean isSensitivecase() {
    return this.chkSensitivecase.isSelected();
  }

  /**
   * Get a regular expression
   *
   * @return regular expression
   */
  public boolean isRegex() {
    return this.chkRegex.isSelected();
  }

  /**
   * Get word search
   *
   * @return word search
   */
  public boolean isWord() {
    return this.chkWord.isSelected();
  }

  /**
   * Get other open file searches
   *
   * @return Search for other open files
   */
  public boolean isOpenFiles() {
    return this.chkOpenFiles.isSelected();
  }

  /**
   * Get the search string
   *
   * @return Search string
   */
  public String getSearchText() {
    return this.txtSearch.getText();
  }

  /**
   * Set the search string
   *
   * @param text Search string
   */
  public void setSearchText(String text) {
    this.txtSearch.setText(text);
  }

  /**
   * Check input
   *
   * @return true = Input check OK
   */
  private boolean validateSearch() {
    String text = this.txtSearch.getText();
    if (text != null) text = text.trim();
    if (text == null || text.isEmpty()) {
      JOptionPane.showMessageDialog(
          this,
          Message.getString(
              "searchfinddialog.errordialog.empty.message"), // Please enter the search string.
          Message.getString("dialog.common.error"), // error
          JOptionPane.ERROR_MESSAGE);
      return false;
    }

    return true;
  }
}
