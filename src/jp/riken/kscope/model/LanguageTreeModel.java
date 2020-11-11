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

import java.util.ArrayList;
import java.util.List;
import java.util.Observable;
import javax.swing.SwingUtilities;
import jp.riken.kscope.common.FILTER_TYPE;
import jp.riken.kscope.component.FilterTreeModel;
import jp.riken.kscope.component.FilterTreeNode;
import jp.riken.kscope.language.Program;

/**
 * Structural tree model
 *
 * @author RIKEN
 */
public class LanguageTreeModel extends Observable {

  /** Tree model */
  private FilterTreeModel treeModel;
  /** Structure tree filter */
  private List<FILTER_TYPE> listFilter;
  /** Database */
  private Program languageDb;

  /** Constructor */
  public LanguageTreeModel() {
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

  /** Clear the structure tree. */
  public void clearTreeModel() {
    FilterTreeNode rootNode = new FilterTreeNode("Structure tree");
    treeModel = new FilterTreeModel(rootNode);

    notifyModel();
  }

  /**
   * Get the root node
   *
   * @return root node
   */
  public FilterTreeNode getRootNode() {
    return (FilterTreeNode) treeModel.getRoot();
  }

  /**
   * Get a tree model
   *
   * @return Tree model
   */
  public FilterTreeModel getTreeModel() {
    return treeModel;
  }

  /**
   * Get the structure tree filter
   *
   * @return Structure tree filter
   */
  public List<FILTER_TYPE> getListFilter() {
    return listFilter;
  }

  /**
   * Set the structure tree filter
   *
   * @param list Structure tree filter
   */
  public void setListFilter(FILTER_TYPE[] list) {
    this.listFilter = new ArrayList<FILTER_TYPE>();
    if (list != null) {
      this.listFilter.addAll(java.util.Arrays.asList(list));
    }

    notifyModel();
  }

  /** Perform tree model filtering. */
  public void filter() {
    if (treeModel != null) {
      // Set the filter
      treeModel.setListFilter(this.listFilter);

      // Filter execution
      treeModel.find();
    }
  }

  /**
   * Check if the structure information tree is already set.
   *
   * @return true = configured
   */
  public boolean isSetLanguageTree() {
    FilterTreeNode root = getRootNode();
    if (root == null) return false;
    if (root.getChildCount() <= 0) return false;

    return true;
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
