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
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.common.EXPLORE_PANEL;

/**
 * Tree search dialog class
 *
 * @author RIKEN
 */
public class SearchTreeDialog extends javax.swing.JDialog implements ActionListener {
  /** Serial number */
  private static final long serialVersionUID = 1L;
  /** Search button */
  private JButton btnOk;
  /** Close button */
  private JButton btnClose;
  /** Word search button */
  private JCheckBox chkWord;
  /** Regular expression search */
  private JCheckBox chkRegex;
  /** Case-insensitive search */
  private JCheckBox chkSensitivecase;
  /** Search string */
  private JTextField txtSearch;
  /** Search folder */
  private JTextField txtFolder;
  /** Folder reference button */
  private JButton btnFolder;
  /** Reference tree model */
  private TreeModel modelTree;
  /** Reference tree type */
  EXPLORE_PANEL panelType;
  /** Selected tree path */
  private TreeNode[] selectedNodes;

  /** Dialog return value */
  private int result = Constant.CANCEL_DIALOG;

  /**
   * Constructor
   *
   * @param owner parent frame
   * @param modal true = Show modal dialog
   */
  public SearchTreeDialog(Frame owner, boolean modal) {
    super(owner, modal);
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
        panelContentLayout.rowWeights = new double[] {0.0, 0.1, 0.1, 0.1, 0.1, 0.1, 0.0};
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
                  "searchtreedialog.label.desc")); // Perform a text search on the tree node.
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
                  3,
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
          chkRegex.setText(
              Message.getString("searchfinddialog.checkbox.regex")); // Regular expressions
          chkRegex.setMargin(new java.awt.Insets(2, 10, 2, 1));
        }
        {
          JLabel lblFolder = new JLabel();
          panelContent.add(
              lblFolder,
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
          lblFolder.setText(Message.getString("searchtreedialog.label.searchnode")); // Search node:
        }
        {
          JPanel panelFolder = new JPanel();
          GridBagLayout layoutFolder = new GridBagLayout();
          panelContent.add(
              panelFolder,
              new GridBagConstraints(
                  2,
                  2,
                  1,
                  1,
                  0.0,
                  0.0,
                  GridBagConstraints.CENTER,
                  GridBagConstraints.BOTH,
                  new Insets(0, 0, 0, 0),
                  0,
                  0));
          // panelFolder.setBorder(new LineBorder(new java.awt.Color(0,0,0), 1, false));
          layoutFolder.rowWeights = new double[] {0.1};
          layoutFolder.rowHeights = new int[] {7};
          layoutFolder.columnWeights = new double[] {1.0, 0.0};
          layoutFolder.columnWidths = new int[] {7, 7};
          panelFolder.setLayout(layoutFolder);
          {
            txtFolder =
                new JTextField(Message.getString("searchtreedialog.text.allnode")); // All nodes
            txtFolder.setEditable(false);
            panelFolder.add(
                txtFolder,
                new GridBagConstraints(
                    0,
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
          {
            btnFolder = new JButton();
            panelFolder.add(
                btnFolder,
                new GridBagConstraints(
                    1,
                    0,
                    1,
                    1,
                    0.0,
                    0.0,
                    GridBagConstraints.CENTER,
                    GridBagConstraints.NONE,
                    new Insets(0, 0, 0, 0),
                    0,
                    0));
            btnFolder.setText(Message.getString("dialog.common.button.refer")); // reference
            btnFolder.setPreferredSize(new java.awt.Dimension(82, 22));
            btnFolder.addActionListener(this);
          }
        }
      }
      this.setTitle(Message.getString("mainmenu.search.tree")); // Tree search
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
    // Reference
    else if (event.getSource() == this.btnFolder) {
      TreeChooserDialog dialog = new TreeChooserDialog(this, true, this.panelType, this.modelTree);
      dialog.setSelectedTreeNodes(this.selectedNodes);
      int result = dialog.showDialog();
      if (result == Constant.CANCEL_DIALOG) return;

      // Selected tree path
      setSelectedTreeNodes(dialog.getSelectedTreeNodes());
      return;
    }
  }

  /**
   * Get the selected node
   *
   * @return Selected node
   */
  public TreeNode[] getSelectedTreeNodes() {
    return this.selectedNodes;
  }

  /**
   * Set the selection node.
   *
   * @param nodes Selected nodes
   */
  public void setSelectedTreeNodes(TreeNode[] nodes) {
    this.selectedNodes = nodes;
    String selected = "";
    if (nodes != null) {
      for (int i = 0; i < nodes.length; i++) {
        selected += nodes[i].toString();
        selected += ", ";
      }
      selected = selected.trim();
      selected = selected.substring(0, selected.length() - 1);
    } else {
      selected = Message.getString("searchtreedialog.text.allnode"); // All nodes
    }
    this.txtFolder.setText(selected);
  }

  /**
   * Set up a reference tree model
   *
   * @param type Tree type
   * @param model Tree model
   */
  public void setReferenceTreeModel(EXPLORE_PANEL type, TreeModel model) {
    this.panelType = type;
    this.modelTree = model;
  }

  /**
   * Get word search
   *
   * @return true = word search
   */
  public boolean isSearchWord() {
    return this.chkWord.isSelected();
  }

  /**
   * Get a regular expression search
   *
   * @return true = regular expression search
   */
  public boolean isSearchRegex() {
    return this.chkRegex.isSelected();
  }

  /**
   * Get case-sensitive search
   *
   * @return true = Case sensitive search
   */
  public boolean isSearchSensitivecase() {
    return this.chkSensitivecase.isSelected();
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
