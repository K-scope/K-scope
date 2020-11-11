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

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import javax.swing.tree.DefaultMutableTreeNode;
import jp.riken.kscope.common.FILTER_TYPE;

/**
 * Filter node class
 *
 * @author RIKEN
 */
public class FilterTreeNode extends DefaultMutableTreeNode {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /**
   * Add node flag true = Add node to parent node. If the node has a filter match or if there are
   * child nodes
   */
  private boolean passed = true;

  /** Filter matcher node list */
  private List<FilterTreeNode> filteredChildren = new ArrayList<FilterTreeNode>();

  /** Node filter class */
  private List<FILTER_TYPE> listFilter;

  /** Unexpanded depth of offspring nodes */
  private int depth;

  /**
   * Constructor
   *
   * @param userObject Node user object
   */
  public FilterTreeNode(Object userObject) {
    super(userObject);
    depth = 0;
  }

  /**
   * Constructor
   *
   * @param userObject Node user object
   * @param depth node
   */
  public FilterTreeNode(Object userObject, int depth) {
    super(userObject);
    this.depth = depth;
  }

  /** Constructor */
  public FilterTreeNode() {
    super();
  }

  /** Perform node filtering */
  public void find() {
    passed = false;
    filteredChildren.clear();
    // Check for filter
    if (!validateFilter()) {
      // Since there is no filter, add unconditionally
      passed = true;
      // Child node search
      passFilterDown();
    } else if (pass(this)) {
      // Add node by matching filter class
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
   * true = Filter match node. <br>
   * true = No filter condition. <br>
   *
   * @param node node
   * @return true = additional node
   */
  protected boolean pass(FilterTreeNode node) {

    // Check for filter
    if (!validateFilter()) {
      return true;
    }

    // Check if it is a filtered node class
    Object obj = node.getUserObject();
    if (!isFilter(obj)) {
      return false;
    }
    return true;
  }

  /** Filter child nodes. */
  private void passFilterDown() {
    int childCount = super.getChildCount();
    for (int i = 0; i < childCount; i++) {
      FilterTreeNode child = (FilterTreeNode) super.getChildAt(i);
      // Set the filter.
      child.setListFilter(this.listFilter);

      child.find();
      if (child.isPassed()) {
        filteredChildren.add(child);
      }
    }
  }

  /**
   * Add a child node
   *
   * @param node Add-on node
   * @return Add-on node
   */
  public FilterTreeNode add(FilterTreeNode node) {
    FilterTreeNode result = node;
    if (!containsChild(node)) {
      int index = super.getChildCount();
      super.insert(node, index);
    } else {
      result = (FilterTreeNode) equalsChild(node);
    }

    // Set the filter.
    node.setListFilter(this.listFilter);

    node.find();
    if (node.isPassed()) {
      if (!containsList(node, filteredChildren)) {
        filteredChildren.add(node);
      }
    }
    return result;
  }

  /**
   * Delete child node
   *
   * @param childIndex Child node index
   */
  @Override
  public void remove(int childIndex) {
    // Check for filter
    if (!validateFilter()) {
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
    // Check for filter
    if (!validateFilter()) {
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
  public FilterTreeNode getChildAt(int index) {
    // Check for filter
    if (!validateFilter()) {
      return (FilterTreeNode) super.getChildAt(index);
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
   * Check if the filter is set.
   *
   * @return true = Filtered
   */
  protected boolean validateFilter() {
    if (this.listFilter == null) return false;
    if (this.listFilter.contains(FILTER_TYPE.ALL)) {
      // All are displayed, so no filter is applied
      return false;
    }

    return true;
  }

  /**
   * Check if it is a node object to be filtered.
   *
   * @param node node user object
   * @return true = Filtered node (display node)
   */
  private boolean isFilter(Object node) {
    if (this.listFilter == null) return false;

    // Check if it is a node object to be filtered.
    for (FILTER_TYPE filter : this.listFilter) {
      if (filter.isFilter(node)) {
        return true;
      }
    }

    return false;
  }

  /**
   * Get node filter
   *
   * @return node filter
   */
  public List<FILTER_TYPE> getListFilter() {
    return listFilter;
  }

  /**
   * Set node filter
   *
   * @param list node filter
   */
  public void setListFilter(List<FILTER_TYPE> list) {
    this.listFilter = list;
  }

  /**
   * Get the unexpanded depth of the offspring node
   *
   * @return Unexpanded depth of offspring nodes
   */
  public int getDepth() {
    return this.depth;
  }

  /**
   * Set the unexpanded depth of offspring nodes
   *
   * @param depth Unexpanded depth of offspring nodes
   */
  public void setDepth(int depth) {
    this.depth = depth;
  }

  /** Delete all child elements */
  @Override
  public void removeAllChildren() {
    // Temporarily remove the filter.
    List<FILTER_TYPE> filters = this.listFilter;
    this.listFilter = null;
    for (int i = super.getChildCount() - 1; i >= 0; i--) {
      super.remove(i);
    }
    filteredChildren.clear();
    this.listFilter = filters;
  }

  /**
   * Check if it exists in the child node.
   *
   * @param child Target node
   * @return true = child node
   */
  private boolean containsChild(FilterTreeNode child) {
    return (equalsChild(child) != null);
  }

  /**
   * Get the same node from a child node.
   *
   * @param child Target node
   * @return Same node
   */
  private DefaultMutableTreeNode equalsChild(FilterTreeNode child) {
    if (child == null) {
      return null;
    }
    try {
      if (super.getChildCount() == 0) {
        return null;
      }
      for (int i = 0; i < super.getChildCount(); i++) {
        if (super.getChildAt(i) == null) continue;
        if (super.getChildAt(i) == child) {
          return (DefaultMutableTreeNode) super.getChildAt(i);
        }
        if (((DefaultMutableTreeNode) super.getChildAt(i)).getUserObject()
            == child.getUserObject()) {
          return (DefaultMutableTreeNode) super.getChildAt(i);
        }
      }
    } catch (Exception e) {
      e.printStackTrace();
    }
    return null;
  }

  /**
   * Check if a check node exists in the node list
   *
   * @param child Check node
   * @param list Node list
   * @return true = exists
   */
  private boolean containsList(FilterTreeNode child, List<FilterTreeNode> list) {
    if (child == null) {
      return false;
    }
    if (list == null || list.size() <= 0) {
      return false;
    }
    try {
      CopyOnWriteArrayList<FilterTreeNode> copyList =
          new CopyOnWriteArrayList<FilterTreeNode>(list);
      for (FilterTreeNode node : copyList) {
        if (child == null) return false;
        if (node == null) continue;
        if (node == child) {
          return true;
        }
        if (node.getUserObject() == child.getUserObject()) {
          return true;
        }
      }
    } catch (Exception e) {
      e.printStackTrace();
    }
    return false;
  }
}
