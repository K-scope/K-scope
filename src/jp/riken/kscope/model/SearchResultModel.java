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
import java.util.List;
import java.util.Observable;
import javax.swing.tree.DefaultMutableTreeNode;
import jp.riken.kscope.Message;
import jp.riken.kscope.component.SearchTreeModel;
import jp.riken.kscope.component.SearchTreeNode;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Search result information model
 *
 * @author RIKEN
 */
public class SearchResultModel extends Observable {

  /** Trace tree model */
  private SearchTreeModel treeModel;
  /** Title */
  private String title;
  /** Search result information list */
  private List<CodeLine> listResult = null;

  /** Search string */
  private String searchText;
  /** Case sensitive (true = case sensitive) */
  private boolean sensitivecase;
  /** Regular expressions */
  private boolean regex;
  /** Word search */
  private boolean word;

  /** Constructor */
  public SearchResultModel() {
    super();

    // Clear search results
    clearSearchResult();
  }

  /** Notify model changes */
  public void notifyModel() {
    this.setChanged();
    this.notifyObservers();
    this.clearChanged();
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
  public SearchTreeModel getTreeModel() {
    return treeModel;
  }

  /**
   * Add search results
   *
   * @param result Search results
   */
  public void addSearchResult(CodeLine result) {

    // Add search results.
    if (this.listResult == null) {
      this.listResult = new ArrayList<CodeLine>();
    }
    this.listResult.add(result);
    // Notify model changes
    notifyModel();
  }

  /** Clear the search results. */
  public void clearSearchResult() {
    if (this.listResult == null) {
      this.listResult = new ArrayList<CodeLine>();
    }
    this.listResult.clear();

    SearchTreeNode rootNode =
        new SearchTreeNode(Message.getString("mainmenu.window.analysis.search")); // search results
    this.treeModel = new SearchTreeModel(rootNode);

    this.title = null;
    this.searchText = null;

    // Notify model changes
    notifyModel();
  }

  /**
   * Get the search result list
   *
   * @return Search result list
   */
  public List<CodeLine> getSearchResultList() {
    return this.listResult;
  }

  /**
   * Get the number of search result lists
   *
   * @return Number of search result lists
   */
  public int getSearchResultListCount() {
    if (this.listResult == null) return 0;
    return this.listResult.size();
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
   * Get case sensitive
   *
   * @return Case sensitive
   */
  public boolean isSensitivecase() {
    return sensitivecase;
  }

  /**
   * Set case sensitivity
   *
   * @param sensitivecase Case sensitive
   */
  public void setSensitivecase(boolean sensitivecase) {
    this.sensitivecase = sensitivecase;
  }

  /**
   * Get a regular expression
   *
   * @return regular expression
   */
  public boolean isRegex() {
    return regex;
  }

  /**
   * Set regular expression
   *
   * @param regex regular expression
   */
  public void setRegex(boolean regex) {
    this.regex = regex;
  }

  /**
   * Get word search
   *
   * @return word search
   */
  public boolean isWord() {
    return word;
  }

  /**
   * Set up word search
   *
   * @param word word search
   */
  public void setWord(boolean word) {
    this.word = word;
  }

  /**
   * Get the search string
   *
   * @return Search string
   */
  public String getSearchText() {
    return searchText;
  }

  /**
   * Set the search string
   *
   * @param searchText Search string
   */
  public void setSearchText(String searchText) {
    this.searchText = searchText;
  }

  /**
   * Whether the model is empty
   *
   * @return Whether it is empty (true: empty, false: with data)
   */
  public boolean isEmpty() {
    DefaultMutableTreeNode root = getRootNode();
    if (root == null) return true;
    if (root.getChildCount() < 1) return true;
    return false;
  }
}
