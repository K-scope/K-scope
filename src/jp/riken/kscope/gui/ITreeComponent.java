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
package jp.riken.kscope.gui;

import java.io.File;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import jp.riken.kscope.action.ExploreTreeChangeAction;
import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.IBlock;

/**
 * Tree panel component interface. <br>
 * Interface of panel components with tree structure
 *
 * @author RIKEN
 */
public interface ITreeComponent {

  /** Stores the entire tree of selection tabs. */
  public void collapseTreeAll();

  /** Expand the entire tree on the Selection tab. */
  public void expandTreeAll();

  /** Expand the nodes under the selection tree on the selection tab. */
  public void expandTreeSelect();

  /**
   * Get the selected file
   *
   * @return selection file
   */
  public SourceFile[] getSelectedSourceFiles();

  /**
   * Get the folder / file of the selected node
   *
   * @return Selected folder / file
   */
  public File[] getSelectedNodeFiles();

  /**
   * Get selected source code line information
   *
   * @return Selected source code line information
   */
  public CodeLine[] getSelectedCodeLines();

  /**
   * Get the selected block
   *
   * @return selection block
   */
  public IBlock[] getSelectedBlocks();

  /**
   * Export explorer tree
   *
   * @param file Output file
   */
  public void export(File file);

  /**
   * Set the explorer panel identifier
   *
   * @return enumPanel Explorer panel identifier
   */
  public EXPLORE_PANEL getEnumPanel();

  /**
   * Get the currently selected node.
   *
   * @return Selected node
   */
  public DefaultMutableTreeNode getSelectedNode();

  /**
   * Get the currently selected node list.
   *
   * @return Selected node list
   */
  public DefaultMutableTreeNode[] getSelectedNodes();

  /**
   * Change tree Register the listener.
   *
   * @param action Tree change listener
   */
  public void addTreeSelectionListener(ExploreTreeChangeAction action);

  /**
   * Set the selected node
   *
   * @param node Selected node
   */
  public void setSelectedNode(Object node);

  /**
   * Set the selected node
   *
   * @param nodes Selected nodes
   */
  public void setSelectedNodes(Object[] nodes);

  /**
   * Get a tree model
   *
   * @return Tree model
   */
  public TreeModel getTreeModel();

  /**
   * Select a node from the tree path
   *
   * @param path Tree path
   */
  public void setSelectionPath(TreePath path);

  /**
   * Select a node range
   *
   * @param startnode Selection start node
   * @param endnode Selection end node
   */
  public void setSelectedNodeArea(Object startnode, Object endnode);

  /**
   * Add node selection
   *
   * @param startnode Selection start node
   * @param endnode Selection end node
   */
  public void addSelectedNodeArea(Object startnode, Object endnode);

  /**
   * Add a selection node
   *
   * @param nodes Selected nodes
   */
  public void addSelectedNodes(Object[] nodes);

  /** Raise a change event for the selected node */
  public void fireSelectNodeChanged();
}
