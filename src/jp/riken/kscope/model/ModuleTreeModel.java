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
import javax.swing.SwingUtilities;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import jp.riken.kscope.language.Program;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Module tree model
 *
 * @author RIKEN
 */
public class ModuleTreeModel extends Observable {

  /** Tree root node */
  private DefaultMutableTreeNode rootNode;
  /** Tree model */
  private DefaultTreeModel treeModel;
  /** Database */
  private Program languageDb;

  /** Constructor */
  public ModuleTreeModel() {
    clearTreeModel();
  }

  /** Notify model changes */
  public void notifyModel() {

    SwingUtilities.invokeLater(
        new Runnable() {
          @Override
          public void run() {
            setChanged();
            notifyObservers();
            clearChanged();
          }
        });
  }

  /** Clear the module tree. */
  public void clearTreeModel() {
    rootNode = new DefaultMutableTreeNode("Module tree");
    treeModel = new DefaultTreeModel(rootNode);
    notifyModel();
  }

  /**
   * Get the tree root node
   *
   * @return Tree root node
   */
  public DefaultMutableTreeNode getRootNode() {
    return rootNode;
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
   * Output tree information to a file.
   *
   * @param file Output file
   */
  public void writeFile(File file) {
    // Root node
    if (this.treeModel == null) return;
    DefaultMutableTreeNode root = (DefaultMutableTreeNode) this.treeModel.getRoot();
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
   * Get the database
   *
   * @return database
   */
  public Program getLanguageDb() {
    return this.languageDb;
  }

  /**
   * Set up the database.
   *
   * @param languageDb database
   */
  public void setLanguageDb(Program language) {
    this.languageDb = language;
  }
}
