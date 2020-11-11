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

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeNode;
import jp.riken.kscope.data.SearchOption;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Search filter node class
 *
 * @author RIKEN
 */
public class SearchTreeNode extends FilterTreeNode {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Search criteria */
  private SearchOption searchOption;
  /** Search node */
  private TreeNode[] searchNodes;
  /** Filter application flag */
  private boolean applyFilter;

  /**
   * Add node flag true = Add node to parent node. If the node is a search match or if there are
   * child nodes
   */
  private boolean passed = true;

  /** Search matcher node list */
  private List<SearchTreeNode> filteredChildren = new ArrayList<SearchTreeNode>();

  /** Node search result flag true = node matches search results */
  private boolean match = false;

  /**
   * Constructor
   *
   * @param userObject Node user object
   */
  public SearchTreeNode(Object userObject) {
    super(userObject);
  }

  /**
   * Constructor
   *
   * @param node Tree node
   */
  public SearchTreeNode(DefaultMutableTreeNode node) {
    this(node.getUserObject());
    for (int i = 0; i < node.getChildCount(); i++) {
      DefaultMutableTreeNode child = (DefaultMutableTreeNode) node.getChildAt(i);
      SearchTreeNode searchChild = new SearchTreeNode(child);
      // Set search conditions.
      searchChild.setSearchOption(this);
      // Add child node
      this.add(searchChild);
    }
  }

  /** Constructor */
  public SearchTreeNode() {
    super();
  }

  /** Perform node search */
  @Override
  public void find() {
    passed = false;
    filteredChildren.clear();
    if (!validateSearch()) {
      // Since there are no search conditions, add unconditionally
      passed = true;
      // Child node search
      passFilterDown();
    } else if (pass(this)) {
      // Add node by search match
      passed = true;
      // Child node search
      passFilterDown();
    } else {
      // Child node search
      passFilterDown();
      passed = filteredChildren.size() != 0;
    }
  }

  /**
   * Check if it is an additional node. <br>
   * true = Search match node. <br>
   * true = No search criteria. <br>
   *
   * @param node node
   * @return true = additional node
   */
  protected boolean pass(SearchTreeNode node) {

    if (this.applyFilter) {
      // Node filter check
      if (!this.pass((FilterTreeNode) node)) {
        return false;
      }
    }

    // Search condition check
    if (!validateSearch()) {
      return true;
    }

    // Check if it is a search target node
    if (!isSearchNode(node)) {
      return false;
    }

    // Check if it is the node class to be searched
    Object obj = node.getUserObject();
    if (!isSearchClass(obj)) {
      return false;
    }
    // node string
    String nodeText = obj.toString();
    if (obj instanceof File) {
      File file = (File) obj;
      if (node.getParent() == null) {
        nodeText = file.getAbsolutePath();
      } else {
        nodeText = file.getName();
      }
    }
    if (nodeText == null) return false;

    boolean result = false;
    if (this.searchOption.isVariable()) {
      // Variable (= trace) search
      result = StringUtils.existsSearchWord(nodeText, this.searchOption.getSearchText());
    } else {
      // Text search
      result =
          StringUtils.existsSearchText(
              nodeText,
              this.searchOption.getSearchText(),
              this.searchOption.isSensitivecase(),
              this.searchOption.isRegex(),
              this.searchOption.isWord());
    }
    // Node search results
    this.match = result;

    return result;
  }

  /**
   * Check if it is a search target node. <br>
   * If no search node is set, all are true. <br>
   * Set the same node as the search node or a child node as the search target node
   *
   * @param node Search target node
   * @return true = Search target node
   */
  private boolean isSearchNode(SearchTreeNode node) {
    if (searchNodes == null) return true;

    // Is it a search node?
    for (TreeNode searchNode : this.searchNodes) {
      if (!(searchNode instanceof DefaultMutableTreeNode)) {
        continue;
      }
      if (((DefaultMutableTreeNode) searchNode).getUserObject() == node.getUserObject()) {
        return true;
      }
      if (SwingUtils.isChildNode((DefaultMutableTreeNode) searchNode, node)) {
        return true;
      }
    }

    return false;
  }

  /** Search for child nodes. */
  private void passFilterDown() {
    int realChildCount = super.getChildCount();
    for (int i = 0; i < realChildCount; i++) {
      SearchTreeNode realChild = (SearchTreeNode) super.getChildAt(i);
      // Set search conditions.
      realChild.setSearchOption(this);
      realChild.setApplyFilter(this.applyFilter);
      if (this.applyFilter) {
        // Set filter
        realChild.setListFilter(this.getListFilter());
      }

      realChild.find();
      if (realChild.isPassed()) {
        filteredChildren.add(realChild);
      }
    }
  }

  /**
   * Add a child node
   *
   * @param node Add-on node
   */
  public void add(SearchTreeNode node) {
    super.add(node);
    node.find();
    if (node.isPassed()) {
      filteredChildren.add(node);
    }
  }

  /**
   * Delete child node
   *
   * @param childIndex Child node index
   */
  @Override
  public void remove(int childIndex) {
    // Search condition check
    if (!validateSearch()) {
      // as child indexes might be inconsistent..
      throw new IllegalStateException("Can't remove while the filter is active");
    }
    super.remove(childIndex);
  }

  /**
   * Get the number of child nodes
   *
   * @return Number of child nodes
   */
  @Override
  public int getChildCount() {
    // Search condition check
    if (!validateSearch()) {
      return super.getChildCount();
    }
    return (filteredChildren.size());
  }

  /**
   * Get a child node
   *
   * @return Child node index
   */
  @Override
  public SearchTreeNode getChildAt(int index) {
    // Search condition check
    if (!validateSearch()) {
      return (SearchTreeNode) super.getChildAt(index);
    }
    return filteredChildren.get(index);
  }

  /**
   * Get whether it is an additional node to the parent node.
   *
   * @return true = additional node
   */
  public boolean isPassed() {
    return passed;
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
   * Set search conditions
   *
   * @param node Search node
   */
  public void setSearchOption(SearchTreeNode node) {

    // Set search conditions
    this.searchOption = node.searchOption;
    // Set the search node
    this.setSearchNodes(node.searchNodes);
  }

  /**
   * Set search conditions
   *
   * @param model Search model
   */
  public void setSearchOption(SearchTreeModel model) {
    // Set search conditions
    this.searchOption = model.getSearchOption();
    // Set the search node
    this.setSearchNodes(model.getSearchNodes());
  }

  /**
   * Get node search results
   *
   * @return node search results
   */
  public boolean isMatch() {
    return match;
  }

  /**
   * Check if search conditions are set.
   *
   * @return true = Search conditions have been set
   */
  private boolean validateSearch() {
    if (this.searchOption == null) return false;
    if (this.searchOption.getSearchText() == null) return false;
    if (this.searchOption.getSearchText().isEmpty()) return false;

    if (this.applyFilter) {
      // Filter application check
      return this.validateFilter();
    } else {
      // Filter not set
      return true;
    }
  }

  /**
   * Check if it is the node object to be searched.
   *
   * @param node node user object
   * @return true = Search target
   */
  private boolean isSearchClass(Object node) {
    if (this.searchOption == null) return false;
    if (this.searchOption.getSearchClass() != null) {
      // Check if it is the node object to be searched.
      return (this.searchOption.getSearchClass().isInstance(node));
    }
    return true;
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
