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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Observable;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Trace result model
 *
 * @author RIKEN
 */
public class TraceResultModel extends Observable {

  /** Trace tree model */
  private DefaultTreeModel treeModel;
  /** Title */
  private String title;
  /** Trace target variable name */
  private String traceWord;
  /** Block selection label */
  private String blocklabel;
  /** Trace selection block */
  private IBlock selectedBlock;
  /** Trace path */
  private IBlock[] tracePath;

  /** Constructor */
  public TraceResultModel() {
    clearTreeModel();
  }

  /** Notify model changes */
  public void notifyModel() {
    this.setChanged();
    this.notifyObservers();
    this.clearChanged();
  }

  /** Clear the trace tree. */
  public void clearTreeModel() {
    this.treeModel = null;
    this.title = null;
    this.traceWord = null;
    this.blocklabel = null;
    this.selectedBlock = null;
    this.tracePath = null;

    notifyModel();
  }

  /**
   * Get the traceroute block.
   *
   * @return Traceroute block
   */
  public IBlock getRootBlock() {
    DefaultMutableTreeNode node = getRootNode();
    if (node == null) return null;
    if (node.getUserObject() instanceof IBlock) {
      return (IBlock) node.getUserObject();
    }
    return null;
  }

  /**
   * Get the root node
   *
   * @return root node
   */
  public DefaultMutableTreeNode getRootNode() {
    if (treeModel == null) return null;
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
   * Get the title
   *
   * @return title
   */
  public String getTitle() {
    // return title;
    if (this.traceWord == null || this.traceWord.length() <= 0) {
      return title;
    }
    if (this.tracePath == null || this.tracePath.length <= 0) {
      return title;
    }
    StringBuffer buf = new StringBuffer();
    for (IBlock block : this.tracePath) {
      if (block == null) continue;
      if (block instanceof Procedure) {
        buf.append(((Procedure) block).get_name());
        buf.append("->");
      }
    }
    buf.append(this.traceWord);

    return buf.toString();
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
   * Get the name of the variable to be traced.
   *
   * @return Traced variable name
   */
  public String getTraceWord() {
    return traceWord;
  }

  /**
   * Set the variable name to be traced.
   *
   * @param word Trace target variable name
   */
  public void setTraceWord(String word) {
    this.traceWord = word;
  }

  /**
   * Get the block selection label.
   *
   * @return Block selection label
   */
  public String getBlocklabel() {
    return blocklabel;
  }

  /**
   * Set the block selection label.
   *
   * @param blocklabel Block selection label
   */
  public void setBlocklabel(String blocklabel) {
    this.blocklabel = blocklabel;
  }

  /**
   * Get the trace selection block
   *
   * @return Trace selection block
   */
  public IBlock getSelectedBlock() {
    return selectedBlock;
  }

  /**
   * Set the trace selection block
   *
   * @param selectedBlock Trace selected block
   */
  public void setSelectedBlock(IBlock selectedBlock) {
    this.selectedBlock = selectedBlock;
  }

  /**
   * Check if they are the same trace. <br>
   *
   * @param model Compared trace model
   * @return true = Same trace result
   */
  public boolean equalsTrace(TraceResultModel model) {
    // Mismatch if trace result does not exist
    if (this.traceWord == null) return false;
    if (this.treeModel == null) return false;
    if (this.getRootBlock() == null) return false;

    // Check trace variables
    if (!this.traceWord.equals(model.getTraceWord())) {
      return false;
    }

    // Trace result root node matches
    if (this.getRootBlock() != model.getRootBlock()) {
      return false;
    }

    return true;
  }

  /**
   * Get the trace path
   *
   * @return Trace path
   */
  public IBlock[] getTracePath() {
    return tracePath;
  }

  /**
   * Set the trace path
   *
   * @param tracePath Tracepath
   */
  public void setTracePath(IBlock[] tracePath) {
    this.tracePath = tracePath;
  }

  /**
   * Add a trace path
   *
   * @param block Additional block
   */
  public void addTracePath(IBlock block) {
    if (block == null) return;
    List<IBlock> list = new ArrayList<IBlock>();
    if (this.tracePath != null && this.tracePath.length > 0) {
      list.addAll(Arrays.asList(this.tracePath));
    }
    list.add(block);

    this.tracePath = list.toArray(new IBlock[0]);
  }

  /**
   * Add a trace path
   *
   * @param blocks Additional block list
   */
  public void addTracePaths(IBlock[] blocks) {
    if (blocks == null || blocks.length <= 0) return;
    List<IBlock> list = new ArrayList<IBlock>();
    if (this.tracePath != null && this.tracePath.length > 0) {
      list.addAll(Arrays.asList(this.tracePath));
    }
    list.addAll(Arrays.asList(blocks));

    this.tracePath = list.toArray(new IBlock[0]);
  }

  /**
   * Whether the model is empty
   *
   * @return Whether it is empty (ture: empty, false: with data)
   */
  public boolean isEmpty() {
    DefaultMutableTreeNode root = getRootNode();
    if (root == null) return true;
    return (root.getChildCount() < 1);
  }
}
