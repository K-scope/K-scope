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
package jp.riken.kscope.component;

import javax.swing.tree.TreeNode;
import jp.riken.kscope.data.SearchOption;

/**
 * Search filter tree model class
 *
 * @author RIKEN
 */
public class SearchTreeModel extends FilterTreeModel {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Search criteria */
  private SearchOption searchOption;
  /** Search node */
  private TreeNode[] searchNodes;
  /** Filter application flag */
  private boolean applyFilter;

  /**
   * Constructor
   *
   * @param node Root node
   */
  public SearchTreeModel(SearchTreeNode node) {
    super((SearchTreeNode) node);
    this.applyFilter = false;
  }

  /** Perform a node search */
  @Override
  public void find() {
    if (this.root != null) {
      SearchTreeNode node = (SearchTreeNode) root;
      // Set search conditions
      node.setSearchOption(this);
      node.setApplyFilter(this.applyFilter);
      if (this.applyFilter) {
        // Set filter
        node.setListFilter(this.getListFilter());
      }

      // Node search
      node.find();

      // Tree change event
      Object[] path = {root};
      fireTreeStructureChanged(this, path, null, null);
    }
  }

  /**
   * Get the number of child nodes of the parent node
   *
   * @param parent parent node
   * @return Number of child nodes
   */
  @Override
  public int getChildCount(Object parent) {
    if (parent instanceof SearchTreeNode) {
      return (((SearchTreeNode) parent).getChildCount());
    }
    return 0;
  }

  /**
   * Get the child node of the parent node
   *
   * @param parent parent node
   * @param index Child node index
   * @return child node
   */
  @Override
  public Object getChild(Object parent, int index) {
    if (parent instanceof SearchTreeNode) {
      return (((SearchTreeNode) parent).getChildAt(index));
    }
    return null;
  }

  /**
   * Get search node
   *
   * @return searchNodes Search nodes
   */
  public TreeNode[] getSearchNodes() {
    return searchNodes;
  }

  /**
   * Set search node
   *
   * @param searchNodes Search nodes
   */
  public void setSearchNodes(TreeNode[] searchNodes) {
    this.searchNodes = searchNodes;
  }

  /**
   * Get search criteria.
   *
   * @return Search criteria
   */
  public SearchOption getSearchOption() {
    return this.searchOption;
  }

  /**
   * Set search conditions.
   *
   * @param searchOption Search criteria
   */
  public void setSearchOption(SearchOption searchOption) {
    this.searchOption = searchOption;
  }

  /**
   * Apply node filter
   *
   * @param filter true = Apply node filter
   */
  public void setApplyFilter(boolean filter) {
    applyFilter = filter;
  }
}
