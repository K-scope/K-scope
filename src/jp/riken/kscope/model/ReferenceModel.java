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

package jp.riken.kscope.model;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Observable;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import jp.riken.kscope.Message;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Reference list model
 *
 * @author RIKEN
 */
public class ReferenceModel extends Observable {

  /** Reference list tree model */
  private DefaultTreeModel treeModel;

  /** Title */
  private String title;

  /** Constructor */
  public ReferenceModel() {
    clearTreeModel();
  }

  /** Notify model changes */
  public void notifyModel() {
    this.setChanged();
    this.notifyObservers();
    this.clearChanged();
  }

  /** Clear the reference list tree. */
  public void clearTreeModel() {
    DefaultMutableTreeNode rootNode =
        new DefaultMutableTreeNode(
            Message.getString("mainmenu.analysis.dec-def-ref")); // Declaration / reference list
    treeModel = new DefaultTreeModel(rootNode);
    this.title = null;

    notifyModel();
  }

  /**
   * Get the root node
   *
   * @return root node
   */
  public DefaultMutableTreeNode getRootNode() {
    return (DefaultMutableTreeNode) treeModel.getRoot();
  }

  /**
   * Get a tree model
   *
   * @return Tree model
   */
  public DefaultTreeModel getTreeModel() {
    return treeModel;
  }

  /**
   * Set up a tree model
   *
   * @param tree Tree model
   */
  public void setTreeModel(DefaultTreeModel tree) {
    this.treeModel = tree;

    // Update tree
    notifyModel();
  }

  /**
   * Get the title
   *
   * @return title
   */
  public String getTitle() {
    return title;
  }

  /**
   * Set the title
   *
   * @param title Title
   */
  public void setTitle(String title) {
    this.title = title;
  }

  /**
   * Output table information to a file.
   *
   * @param file Output file
   */
  public void writeFile(File file) {

    // Root node
    DefaultMutableTreeNode root = getRootNode();
    if (root == null) return;
    if (root.getChildCount() <= 0) return;

    try {
      // File output
      PrintWriter pw = new PrintWriter(new BufferedWriter(new FileWriter(file)));

      // Make the tree a CSV string.
      String buf = SwingUtils.toCsv(root);
      // File output
      pw.print(buf);

      pw.close();
    } catch (IOException ex) {
      ex.printStackTrace();
    }
  }

  /**
   * Whether the model is empty
   *
   * @return Whether it is empty (true: empty, false: with data)
   */
  public boolean isEmpty() {
    DefaultMutableTreeNode root = getRootNode();
    if (root == null) return true;
    return (root.getChildCount() < 1);
  }
}
