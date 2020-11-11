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
import java.util.ArrayList;
import java.util.List;
import javax.swing.Box;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.component.ObjectTree;
import jp.riken.kscope.utils.ResourceUtils;

/**
 * Tree selection dialog
 *
 * @author RIKEN
 */
public class TreeChooserDialog extends javax.swing.JDialog implements ActionListener {

  /** Serial number */
  private static final long serialVersionUID = 1L;
  /** Selection tree */
  private ObjectTree treeSelect;
  /** OK button */
  private JButton btnOk;
  /** Cancel button */
  private JButton btnCancel;
  /** Expand all button */
  private JButton btnExpand;
  /** All storage buttons */
  private JButton btnCollapse;

  /** Dialog return value */
  private int result = Constant.CANCEL_DIALOG;

  /**
   * Constructor
   *
   * @param owner parent frame
   * @param modal true = Show modal dialog
   * @param type Tree type
   * @param model Tree model
   */
  public TreeChooserDialog(Frame owner, boolean modal, EXPLORE_PANEL type, TreeModel model) {
    super(owner, modal);
    initGUI(type, model);
  }

  /**
   * Constructor
   *
   * @param owner Parent dialog
   * @param modal true = Show modal dialog
   * @param type Tree type
   * @param model Tree model
   */
  public TreeChooserDialog(JDialog owner, boolean modal, EXPLORE_PANEL type, TreeModel model) {
    super(owner, modal);
    initGUI(type, model);
  }

  /**
   * Initialize the GUI.
   *
   * @param type Tree type
   * @param model Tree model
   */
  private void initGUI(EXPLORE_PANEL type, TreeModel model) {
    try {
      BorderLayout thisLayout = new BorderLayout();
      getContentPane().setLayout(thisLayout);

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
      // Selection tree
      {
        JPanel panelContent = new JPanel();
        GridBagLayout panelContentLayout = new GridBagLayout();
        panelContentLayout.columnWidths = new int[] {10, 64, 10, 10};
        panelContentLayout.rowHeights = new int[] {32, 32, 10, 10};
        panelContentLayout.columnWeights = new double[] {0.0, 0.0, 1.0, 0.0};
        panelContentLayout.rowWeights = new double[] {0.0, 0.0, 1.0, 0.0};
        getContentPane().add(panelContent, BorderLayout.CENTER);
        panelContent.setLayout(panelContentLayout);
        panelContent.setPreferredSize(new java.awt.Dimension(390, 230));
        // Label
        {
          JLabel label = new JLabel();
          panelContent.add(
              label,
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
          label.setText(
              Message.getString("treechooserdialog.label.selecttreenode")); // Select tree node
        }

        // Trace destination table
        {
          treeSelect = new ObjectTree();
          JScrollPane scrollTrace = new JScrollPane(treeSelect);
          treeSelect
              .getSelectionModel()
              .setSelectionMode(TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION);
          treeSelect.setModel(model);
          treeSelect.setRootVisible(true);
          treeSelect.setShowsRootHandles(true);

          scrollTrace.setHorizontalScrollBarPolicy(
              ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
          scrollTrace.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
          panelContent.add(
              scrollTrace,
              new GridBagConstraints(
                  1,
                  1,
                  2,
                  2,
                  0.0,
                  0.0,
                  GridBagConstraints.CENTER,
                  GridBagConstraints.BOTH,
                  new Insets(0, 0, 0, 0),
                  0,
                  0));
        }
        // Button panel
        {
          JPanel panelSubButtons = new JPanel();
          FlowLayout panelSubButtonsLayout = new FlowLayout();
          panelSubButtonsLayout.setAlignment(FlowLayout.RIGHT);
          panelSubButtonsLayout.setHgap(0);
          panelSubButtonsLayout.setVgap(0);
          panelSubButtons.setLayout(panelSubButtonsLayout);
          panelContent.add(
              panelSubButtons,
              new GridBagConstraints(
                  2,
                  0,
                  1,
                  1,
                  0.0,
                  0.0,
                  GridBagConstraints.SOUTHEAST,
                  GridBagConstraints.HORIZONTAL,
                  new Insets(0, 0, 0, 0),
                  0,
                  0));

          java.awt.Dimension buttonSize = new java.awt.Dimension(18, 18);
          {
            Icon icon = ResourceUtils.getIcon("expandall.gif");
            btnExpand = new JButton(icon);
            btnExpand.setContentAreaFilled(false);
            btnExpand.setBorderPainted(false);
            btnExpand.setPreferredSize(buttonSize);
            btnExpand.setMinimumSize(buttonSize);
            btnExpand.setMaximumSize(buttonSize);
            panelSubButtons.add(btnExpand);
            btnExpand.addActionListener(this);
          }
          // Margin setting
          panelSubButtons.add(Box.createHorizontalStrut(5));
          {
            Icon icon = ResourceUtils.getIcon("collapseall.gif");
            btnCollapse = new JButton(icon);
            btnCollapse.setContentAreaFilled(false);
            btnCollapse.setBorderPainted(false);
            btnCollapse.setPreferredSize(buttonSize);
            btnCollapse.setMinimumSize(buttonSize);
            btnCollapse.setMaximumSize(buttonSize);
            panelSubButtons.add(btnCollapse);
            btnCollapse.addActionListener(this);
          }
          // Margin setting
          panelSubButtons.add(Box.createHorizontalStrut(5));
        }

        // Tooltip settings
        btnExpand.setToolTipText(
            Message.getString("treechooserdialog.tooltip.expandall")); // Expand all
        btnCollapse.setToolTipText(
            Message.getString("treechooserdialog.tooltip.collapseall")); // All stored
      }

      this.setTitle(Message.getString("treechooserdialog.dialog.title")); // Node selection
      this.setSize(480, 280);

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

    // OK
    if (event.getSource() == this.btnOk) {
      this.result = Constant.OK_DIALOG;
      // Close the dialog.
      dispose();
      return;
    }
    // Cancel
    else if (event.getSource() == this.btnCancel) {
      this.result = Constant.CANCEL_DIALOG;
      // Close the dialog.
      dispose();
      return;
    }
    // Expand all
    else if (event.getSource() == this.btnExpand) {
      expandTreeAll();
    }
    // store everything
    else if (event.getSource() == this.btnCollapse) {
      collapseTreeAll();
    }
    return;
  }

  /** Stores the entire tree. */
  public void collapseTreeAll() {
    int row = this.treeSelect.getRowCount() - 1;
    while (row >= 0) {
      this.treeSelect.collapseRow(row);
      row--;
    }
    // Expand only root node
    this.treeSelect.expandRow(0);
  }

  /** Expand the entire tree. */
  public void expandTreeAll() {
    int row = 0;
    while (row < this.treeSelect.getRowCount()) {
      this.treeSelect.expandRow(row);
      row++;
    }
  }
  /**
   * Get the selected node
   *
   * @return Selected node
   */
  public TreeNode[] getSelectedTreeNodes() {
    TreePath[] paths = this.treeSelect.getSelectionPaths();
    if (paths == null) return null;

    TreeNode[] nodes = new TreeNode[paths.length];
    for (int i = 0; i < paths.length; i++) {
      nodes[i] = (TreeNode) paths[i].getLastPathComponent();
    }
    return nodes;
  }

  /**
   * Set the selection node.
   *
   * @param nodes Selected nodes
   */
  public void setSelectedTreeNodes(TreeNode[] nodes) {
    if (nodes == null) return;

    TreePath[] paths = new TreePath[nodes.length];
    for (int i = 0; i < nodes.length; i++) {
      TreeNode treeNode = nodes[i];
      List<Object> pathNodes = new ArrayList<Object>();
      if (nodes[i] != null) {
        pathNodes.add(treeNode);
        treeNode = treeNode.getParent();
        while (treeNode != null) {
          pathNodes.add(0, treeNode);
          treeNode = treeNode.getParent();
        }
        paths[i] = new TreePath(pathNodes.toArray());
      }
    }
    this.treeSelect.setSelectionPaths(paths);
  }
}
