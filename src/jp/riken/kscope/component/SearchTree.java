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

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import javax.swing.JComponent;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SearchOption;
import jp.riken.kscope.utils.StringUtils;

/**
 * Search tree class
 *
 * @author RIKEN
 */
public class SearchTree extends ObjectTree {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Search criteria */
  private SearchOption searchOption;
  /** Foreground view */
  private Color forecolor;
  /** Display background color */
  private Color backcolor;
  /** Style */
  private int fontstyle = Font.PLAIN;

  /** Constructor */
  public SearchTree() {
    super();
    // Setting the node drawing class of the tree
    SearchTreeCellRenderer renderer = new SearchTreeCellRenderer();
    this.setCellRenderer(renderer);
  }

  /**
   * Search tree node rendering class
   *
   * @author RIKEN
   */
  public class SearchTreeCellRenderer extends ObjectTreeCellRenderer {

    /** Serial number */
    private static final long serialVersionUID = 1L;

    /** Constructor */
    public SearchTreeCellRenderer() {}

    /**
     * Methods that determine how to draw the nodes in the tree
     *
     * @param t The tree you are painting
     * @param value Displayed value
     * @param selected True if a node is selected
     * @param expanded True if expanded
     * @param leaf True if the element is a leaf
     * @param row Node index
     * @param hasFocus True if the specified node has focus
     * @return A component with a paint () method that draws the specified value
     * @see javax.swing.tree.TreeCellRenderer @ see javax.swing.tree.DefaultTreeCellRenderer
     */
    @Override
    public Component getTreeCellRendererComponent(
        JTree t,
        Object value,
        boolean selected,
        boolean expanded,
        boolean leaf,
        int row,
        boolean hasFocus) {

      JComponent c =
          (JComponent)
              super.getTreeCellRendererComponent(t, value, selected, expanded, leaf, row, hasFocus);

      Object obj = ((DefaultMutableTreeNode) value).getUserObject();
      // Are search conditions set or are the search target nodes?
      if (validateSearch(obj)) {
        String text = getText();
        String prefix = null;
        if (obj instanceof CodeLine) {
          text = ((CodeLine) obj).getStatement();
          prefix = String.valueOf(((CodeLine) obj).getStartLine());
          prefix = prefix + " : ";
        }
        if (SearchTree.this.searchOption.isVariable()) {
          // Variable (= trace) search
          text =
              StringUtils.searchWordToHtml(
                  text,
                  SearchTree.this.searchOption.getSearchText(),
                  SearchTree.this.forecolor,
                  SearchTree.this.backcolor,
                  SearchTree.this.fontstyle);
        } else {
          // Text search
          text =
              StringUtils.searchTextToHtml(
                  text,
                  SearchTree.this.searchOption.getSearchText(),
                  SearchTree.this.forecolor,
                  SearchTree.this.backcolor,
                  SearchTree.this.fontstyle,
                  SearchTree.this.searchOption.isSensitivecase(),
                  SearchTree.this.searchOption.isRegex(),
                  SearchTree.this.searchOption.isWord());
        }
        if (prefix != null) {
          if (text.indexOf("<html>") == 0) {
            text = "<html>" + prefix + text.substring(6);
          } else {
            text = prefix + text;
          }
        }
        this.setText(text);
      }

      return c;
    }
  }

  /**
   * Get the foreground view
   *
   * @return Foreground view
   */
  public Color getForecolor() {
    return this.forecolor;
  }

  /**
   * Set the foreground view
   *
   * @param color Foreground view
   */
  public void setForecolor(Color color) {
    this.forecolor = color;
  }

  /**
   * Get the display background color
   *
   * @return Display background color
   */
  public Color getBackcolor() {
    return this.backcolor;
  }

  /**
   * Set the display background color
   *
   * @param color Display background color
   */
  public void setBackcolor(Color color) {
    this.backcolor = color;
  }

  /**
   * Get font style
   *
   * @return font style
   */
  public int getFontstyle() {
    return fontstyle;
  }

  /**
   * Set font style
   *
   * @param fontstyle Font style
   */
  public void setFontstyle(int fontstyle) {
    this.fontstyle = fontstyle;
  }

  /**
   * Set up a tree model
   *
   * @param model Tree model
   */
  public void setModel(SearchTreeModel model) {
    super.setModel(model);

    // Set search conditions
    this.searchOption = model.getSearchOption();
  }

  /**
   * Check if search conditions are set.
   *
   * @param node node user object
   * @return true = Search conditions have been set
   */
  private boolean validateSearch(Object node) {
    if (this.searchOption == null) return false;
    if (this.searchOption.getSearchText() == null) return false;
    if (this.searchOption.getSearchText().isEmpty()) return false;
    if (this.searchOption.getSearchClass() != null) {
      // Check if it is the node object to be searched.
      return (this.searchOption.getSearchClass().isInstance(node));
    }
    return true;
  }

  /**
   * Get search criteria
   *
   * @return Search criteria
   */
  public SearchOption getSearchOption() {
    return searchOption;
  }

  /**
   * Set search conditions
   *
   * @param searchOption Search criteria
   */
  public void setSearchOption(SearchOption searchOption) {
    this.searchOption = searchOption;
  }
}
