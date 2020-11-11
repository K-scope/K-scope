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

import java.util.List;
import javax.swing.tree.DefaultTreeModel;
import jp.riken.kscope.common.FILTER_TYPE;

/**
 * Filter tree model class
 *
 * @author RIKEN
 */
public class FilterTreeModel extends DefaultTreeModel {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Node filter class */
  private List<FILTER_TYPE> listFilter;

  /**
   * Constructor
   *
   * @param node Root node
   */
  public FilterTreeModel(FilterTreeNode node) {
    super(node);
  }

  /** Run node filter */
  public void find() {
    if (this.root != null) {
      FilterTreeNode node = (FilterTreeNode) root;
      // Set filter
      node.setListFilter(this.listFilter);

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
    if (parent instanceof FilterTreeNode) {
      return (((FilterTreeNode) parent).getChildCount());
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
    if (parent instanceof FilterTreeNode) {
      return (((FilterTreeNode) parent).getChildAt(index));
    }
    return null;
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
}
