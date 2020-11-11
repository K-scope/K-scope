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
package jp.riken.kscope.service;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeNode;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.FILTER_TYPE;
import jp.riken.kscope.component.FilterTreeNode;
import jp.riken.kscope.component.SearchTreeModel;
import jp.riken.kscope.component.SearchTreeNode;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SearchOption;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.Block;
import jp.riken.kscope.language.Condition;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProcedureUsage;
import jp.riken.kscope.language.Selection;
import jp.riken.kscope.language.Substitution;
import jp.riken.kscope.model.SearchResultModel;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.utils.SwingUtils;
import jp.riken.kscope.utils.TextFileReader;

/**
 * Analysis: Search service class
 *
 * @author RIKEN
 */
public class AnalysisSearchService extends AnalysisBaseService {

  /** Search string */
  private String searchText;
  /** Case sensitive (true = case sensitive) */
  private boolean sensitivecase;
  /** Regular expressions */
  private boolean regex;
  /** Word search */
  private boolean word;
  /** Variable search (= trace) */
  private boolean variable;
  /** Search tree node */
  private TreeNode[] searchNodes;
  /** Search result model */
  private SearchResultModel searchModel;
  /** Search source explorer tree node */
  DefaultMutableTreeNode exploreTreeNode;
  /** Working set for determining circulation in tree generation */
  private HashSet<String> recursiveSub = new HashSet<String>();
  /** Thread execution flag true: Continue execution / false: Cancel. */
  private boolean m_running = true;
  /** Search file list */
  private SourceFile[] searchFiles;
  /** Searched procedure list */
  private Map<Procedure, DefaultMutableTreeNode> searchedProcedures;
  /** Number of search result nodes */
  private int searchedNodeCount;
  /** Maximum number of search results */
  private final int MAX_SEARCHED_NODECOUNT = 1024;
  /** Error message */
  private String errorMessage;

  /** Constructor */
  public AnalysisSearchService() {
    this.setErrorMessage(null);
  }

  /** Perform a tree search */
  public void searchTree() {
    if (this.exploreTreeNode == null) return;

    // Generate a search tree node
    SearchTreeNode searchRoot = new SearchTreeNode(this.exploreTreeNode);

    // Search tree model
    SearchTreeModel treeModel = this.searchModel.getTreeModel();
    treeModel.setRoot(searchRoot);

    // Create search criteria
    SearchOption search = getSearchOption();

    // Set search conditions
    treeModel.setSearchOption(search);
    treeModel.setSearchNodes(this.searchNodes);

    // Perform tree search
    treeModel.find();

    // Search title
    String title =
        Message.getString("analysissearchservice.searchword", this.searchText); // Search word
    searchModel.setTitle(title);
    // Set search conditions
    searchModel.setSearchText(this.searchText);

    searchModel.notifyModel();
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
   * @param text Search string
   */
  public void setSearchText(String text) {
    this.searchText = text;
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
   * Get variable search (= trace)
   *
   * @return Variable search (= trace)
   */
  public boolean isVariable() {
    return variable;
  }

  /**
   * Set variable search (= trace)
   *
   * @param variable Variable search (= trace)
   */
  public void setVariable(boolean variable) {
    this.variable = variable;
  }

  /**
   * Get the search tree node
   *
   * @return searchNodes Search tree nodes
   */
  public TreeNode[] getSearchNodes() {
    return searchNodes;
  }

  /**
   * Set up a search tree node
   *
   * @param searchNodes Search tree nodes
   */
  public void setSearchNodes(TreeNode[] searchNodes) {
    this.searchNodes = searchNodes;
  }

  /**
   * Get the search result model
   *
   * @return searchModel Search result model
   */
  public SearchResultModel getSearchModel() {
    return searchModel;
  }

  /**
   * Set the search result model
   *
   * @param searchModel Search result model
   */
  public void setSearchModel(SearchResultModel searchModel) {
    this.searchModel = searchModel;
  }

  /**
   * Set the search source explorer tree node
   *
   * @param exploreTreeNode Search source explorer tree node
   */
  public void setExploreTreeNode(DefaultMutableTreeNode exploreTreeNode) {
    this.exploreTreeNode = exploreTreeNode;
  }

  /** Perform a source search */
  public void searchFile() {
    searchFile(this.searchFiles);
  }

  /**
   * Perform a source search
   *
   * @param files Search target files
   */
  public void searchFile(SourceFile[] files) {
    if (this.exploreTreeNode == null) return;
    if (files == null) return;
    Application.status.setProgressStart(true);

    // Get the line of code that matches the search results
    ArrayList<CodeLine> list = new ArrayList<CodeLine>();
    for (SourceFile file : files) {
      // Cancel check
      if (this.isCancel()) {
        break;
      }
      Application.status.setMessageStatus("searching... : " + file.toString());

      CodeLine[] codes = readSourceFile(file);
      if (codes == null || codes.length <= 0) continue;
      for (CodeLine code : codes) {
        // Check if the maximum number of searches has been reached.
        if (this.searchedNodeCount > this.MAX_SEARCHED_NODECOUNT) {
          String msg = Message.getString("analysissearchservice.error.maxsearchedcount");
          this.addErrorInfo(msg);
          this.setErrorMessage(msg);
          cancelRunning(); // Stop the search.
          break;
        }

        String line = code.getStatement();
        // Does it match the search results?
        boolean result = false;
        if (this.variable) {
          // Variable (= trace) search
          result = StringUtils.existsSearchWord(line, this.searchText);
        } else {
          // Text search
          result =
              StringUtils.existsSearchText(
                  line, this.searchText, this.sensitivecase, this.regex, this.word);
        }
        if (result) {
          list.add(code);
          this.searchedNodeCount++;
        }
      }
    }

    // Generate a search tree node
    SearchTreeNode searchRoot = new SearchTreeNode(this.exploreTreeNode);

    // List tree nodes in the forward direction
    Enumeration<?> depth = searchRoot.preorderEnumeration();
    while (depth.hasMoreElements()) {
      SearchTreeNode treeNode = (SearchTreeNode) depth.nextElement();
      if (treeNode == null || treeNode.getUserObject() == null) continue;
      if (!(treeNode.getUserObject() instanceof SourceFile)) continue;
      // Is it the source file to be searched?
      addNodeSearchCodeLine(treeNode, list);
    }

    // Search tree model
    SearchTreeModel treeModel = this.searchModel.getTreeModel();
    treeModel.setRoot(searchRoot);

    // Create search criteria
    SearchOption search = getSearchOption();
    // For source search, set the search class in the source code line
    search.setSearchClass(CodeLine.class);

    // Set search conditions
    treeModel.setSearchOption(search);
    treeModel.setSearchNodes(this.searchNodes);
    // Do not apply filter
    treeModel.setApplyFilter(false);

    // Perform tree search
    treeModel.find();

    // Search title
    String title =
        Message.getString("analysissearchservice.searchword", this.searchText); // Search word:
    searchModel.setTitle(title);
    // Set search conditions
    searchModel.setSearchText(this.searchText);

    Application.status.setProgressStart(false);
    Application.status.setMessageStatus(null);

    searchModel.notifyModel();
  }

  /**
   * Add a line of search result code to the source file node
   *
   * @param node Source file node
   * @param list Search result code line list
   */
  private void addNodeSearchCodeLine(SearchTreeNode node, ArrayList<CodeLine> list) {

    if (node == null || node.getUserObject() == null) return;
    if (!(node.getUserObject() instanceof SourceFile)) return;
    if (list == null || list.size() <= 0) return;

    // Tree node source file
    SourceFile file = (SourceFile) node.getUserObject();
    if (file == null) return;
    for (CodeLine line : list) {
      // Add a line of code to the matched source file node
      if (file.equals(line.getSourceFile())) {
        // Add a line of code to the child node
        node.add(new SearchTreeNode(line));
      }
    }

    return;
  }

  /**
   * Get a line of code from a file
   *
   * @param file Source file
   * @return Code line list
   */
  private CodeLine[] readSourceFile(SourceFile file) {

    ArrayList<CodeLine> list = new ArrayList<CodeLine>();
    try {
      TextFileReader reader = new TextFileReader(file.getFile());
      String line;
      int line_no = 0;
      while ((line = reader.readLine()) != null) {
        line_no++;
        // Generate and add code lines
        String fn = null;
        if (file != null) {
          fn = file.getPath();
        }
        list.add(new CodeLine(file, line.trim(), line_no, fn));
      }

    } catch (Exception e) {
      return null;
    }

    if (list.size() <= 0) return null;

    // Read code line list
    return list.toArray(new CodeLine[0]);
  }

  /**
   * Search the search string from the database and reflect it in the tree. Not all structure trees
   * have been expanded, so search from the database.
   */
  public void searchLanguage() {

    if (this.exploreTreeNode == null) return;

    // Get the filter list.
    List<FILTER_TYPE> listFilter = null;
    if (exploreTreeNode instanceof FilterTreeNode) {
      listFilter = ((FilterTreeNode) exploreTreeNode).getListFilter();
    }
    // Generate a search tree root node
    SearchTreeNode searchRoot = new SearchTreeNode(this.exploreTreeNode.getUserObject());

    // Search tree model
    SearchTreeModel treeModel = this.searchModel.getTreeModel();
    treeModel.setRoot(searchRoot);

    // Create search criteria
    SearchOption search = getSearchOption();

    // Set search conditions
    treeModel.setSearchOption(search);
    treeModel.setSearchNodes(this.searchNodes);

    Application.status.setProgressStart(true);
    this.searchedNodeCount = 0;
    // Perform tree search
    if (this.exploreTreeNode.getChildCount() > 0) {
      DefaultMutableTreeNode child = (DefaultMutableTreeNode) this.exploreTreeNode.getChildAt(0);
      Block searchblock = null;
      SearchTreeNode node = null;
      if (child.getUserObject() instanceof Procedure) {
        searchblock = ((Procedure) child.getUserObject()).getBody();
        SearchTreeNode mainNode = new SearchTreeNode(child.getUserObject());
        searchRoot.add(mainNode);
        node = mainNode;
      } else if (child.getUserObject() instanceof Block) {
        searchblock = (Block) child.getUserObject();
        node = searchRoot;
      }
      if (searchblock != null) {
        // Search execution
        DefaultMutableTreeNode findnode =
            searchBlocks(searchblock, (DefaultMutableTreeNode) node, this.searchNodes);
        if (findnode != null) {
          for (int i = 0; i < findnode.getChildCount(); i++) {
            node.add(new SearchTreeNode((DefaultMutableTreeNode) findnode.getChildAt(i)));
          }
          // Apply a filter and search again
          treeModel.setApplyFilter(true);
          treeModel.setListFilter(listFilter);
          treeModel.find();
        }
      }
    }
    Application.status.setProgressStart(false);
    Application.status.setMessageStatus(null);

    // Search title
    String title =
        Message.getString("analysissearchservice.searchword", this.searchText); // Search word
    searchModel.setTitle(title);
    // Set search conditions
    searchModel.setSearchText(this.searchText);

    searchModel.notifyModel();
  }

  /**
   * Create a search condition class.
   *
   * @return Search condition class
   */
  private SearchOption getSearchOption() {
    // Create search criteria
    SearchOption search = new SearchOption();
    search.setSearchText(this.searchText);
    search.setRegex(this.regex);
    search.setWord(this.word);
    search.setSensitivecase(this.sensitivecase);
    search.setVariable(this.variable);

    return search;
  }

  /**
   * Recursively search for processing blocks and generate child nodes.
   *
   * @param block Processing block
   * @param parent Additional node
   * @param searchNodeList Search target node list
   * @return Search result node
   */
  private DefaultMutableTreeNode searchBlocks(
      Block block, DefaultMutableTreeNode parent, TreeNode[] searchNodeList) {
    if (block == null) return null;
    // Cancel check
    if (this.isCancel()) {
      return null;
    }
    // Check if it is a search target block.
    if (!isSearchBlock(parent, searchNodeList)) {
      return null;
    }
    // Check if the maximum number of searches has been reached.
    if (this.searchedNodeCount > this.MAX_SEARCHED_NODECOUNT) {
      String msg = Message.getString("analysissearchservice.error.maxsearchedcount");
      this.addErrorInfo(msg);
      this.setErrorMessage(msg);
      cancelRunning(); // Stop the search.
      return null;
    }

    DefaultMutableTreeNode current = new DefaultMutableTreeNode(parent);
    current.setParent((DefaultMutableTreeNode) parent.getParent());
    List<Block> blocks = block.getBlocks();
    for (Block blk : blocks) {
      // When the target is a procedure call
      if (blk instanceof ProcedureUsage) {
        ProcedureUsage call = (ProcedureUsage) blk;
        String callName = call.getCallName();
        if (callName.equalsIgnoreCase("")) {
          continue;
        }

        DefaultMutableTreeNode child = new DefaultMutableTreeNode(call);
        child.setParent(parent);
        // Check if it is a search target block.
        if (!isSearchBlock(child, searchNodeList)) continue;

        if (call.getCallDefinition() != null) {
          Procedure proc = call.getCallDefinition();
          DefaultMutableTreeNode procNode = new DefaultMutableTreeNode(proc);
          procNode.setParent(child);

          // Check if it is a search target block.
          if (!isSearchBlock(procNode, searchNodeList)) continue;
          Application.status.setMessageStatus("searching... : " + proc.toString());

          // Inspected check
          DefaultMutableTreeNode procClone = null;
          if (containsSearchedProcedures(proc)) {
            procClone = SwingUtils.cloneTreeNode(getSearchedProcedureNode(proc));
            if (procClone == null) {
              continue;
            }
          }

          // Check circulation
          if (!SwingUtils.recursiveTreeNode(procNode) && !this.recursiveSub.contains(callName)) {
            if (procClone != null) {
              child.add(procClone);
              child.setParent(null);
              current.add(child);
              int nodecount = SwingUtils.getAllChildCount(current.getRoot());
              this.searchedNodeCount += nodecount;
              continue;
            }

            recursiveSub.add(callName);
            DefaultMutableTreeNode find = searchBlocks(proc.getBody(), procNode, searchNodeList);
            if (find != null && find.getChildCount() > 0) {
              while (find.getChildCount() > 0) {
                procNode.add((DefaultMutableTreeNode) find.getChildAt(0));
                this.searchedNodeCount++;
              }
            }
            recursiveSub.remove(callName);
          }

          if (isMatchText(procNode.toString()) || procNode.getChildCount() > 0) {
            procNode.setParent(null);
            child.add(procNode);
            this.searchedNodeCount++;
          } else {
            procNode = null;
          }
          // Add checked procedure
          addSearchedProcedures(proc, procNode);
        }
        if (isMatchText(child.toString()) || child.getChildCount() > 0) {
          child.setParent(null);
          current.add(child);
          this.searchedNodeCount++;
        }
      } else if (blk instanceof Selection) {
        Selection select = (Selection) blk;
        DefaultMutableTreeNode child = new DefaultMutableTreeNode(select);
        child.setParent(parent);
        // Check if it is a search target block.
        if (!isSearchBlock(child, searchNodeList)) continue;
        // For SELECT statement
        if (select.isSelect()) {
          for (Condition cond : select.getConditions()) {
            DefaultMutableTreeNode child2 = new DefaultMutableTreeNode(cond);
            child2.setParent(child);
            // Check if it is a search target block.
            if (!isSearchBlock(child2, searchNodeList)) continue;
            DefaultMutableTreeNode find = searchBlocks(cond, child2, searchNodeList);
            if (find != null && find.getChildCount() > 0) {
              while (find.getChildCount() > 0) {
                child2.add((DefaultMutableTreeNode) find.getChildAt(0));
                this.searchedNodeCount++;
              }
            }
            if (isMatchText(cond.toString()) || child2.getChildCount() > 0) {
              child2.setParent(null);
              child.add(child2);
              this.searchedNodeCount++;
            }
          }
          if (isMatchText(child.toString()) || child.getChildCount() > 0) {
            child.setParent(null);
            current.add(child);
            this.searchedNodeCount++;
          }
          // For IF, WHERE statements
        } else {
          Block cond0 = select.getConditions().get(0);
          DefaultMutableTreeNode find = searchBlocks(cond0, child, searchNodeList);
          if (find != null && find.getChildCount() > 0) {
            while (find.getChildCount() > 0) {
              child.add((DefaultMutableTreeNode) find.getChildAt(0));
              this.searchedNodeCount++;
            }
          }
          if (isMatchText(child.toString()) || child.getChildCount() > 0) {
            child.setParent(null);
            current.add(child);
            this.searchedNodeCount++;
          }
          for (int j = 1; j < select.getConditions().size(); j++) {
            Condition cond = select.getConditions().get(j);
            DefaultMutableTreeNode condnode = new DefaultMutableTreeNode(cond);
            condnode.setParent(child);
            // Check if it is a search target block.
            if (!isSearchBlock(condnode, searchNodeList)) continue;
            find = searchBlocks(cond, condnode, searchNodeList);
            if (find != null && find.getChildCount() > 0) {
              while (find.getChildCount() > 0) {
                condnode.add((DefaultMutableTreeNode) find.getChildAt(0));
                this.searchedNodeCount++;
              }
            }
            if (isMatchText(condnode.toString()) || condnode.getChildCount() > 0) {
              condnode.setParent(null);
              current.add(condnode);
              this.searchedNodeCount++;
            }
          }
        }
      } else if (blk instanceof Substitution) {
        DefaultMutableTreeNode child = new DefaultMutableTreeNode(blk);
        child.setParent(parent);
        // Check if it is a search target block.
        if (!isSearchBlock(child, searchNodeList)) continue;
        DefaultMutableTreeNode find = searchBlocks(blk, current, searchNodeList);
        if (isMatchText(child.toString()) || child.getChildCount() > 0) {
          child.setParent(null);
          current.add(child);
          this.searchedNodeCount++;
        }
        if (find != null && find.getChildCount() > 0) {
          while (find.getChildCount() > 0) {
            current.add((DefaultMutableTreeNode) find.getChildAt(0));
            this.searchedNodeCount++;
          }
        }
      } else {
        DefaultMutableTreeNode child = new DefaultMutableTreeNode(blk);
        child.setParent(parent);
        // Check if it is a search target block.
        if (!isSearchBlock(child, searchNodeList)) continue;
        DefaultMutableTreeNode find = searchBlocks(blk, child, searchNodeList);
        if (find != null && find.getChildCount() > 0) {
          while (find.getChildCount() > 0) {
            child.add((DefaultMutableTreeNode) find.getChildAt(0));
            this.searchedNodeCount++;
          }
        }
        if (isMatchText(child.toString()) || child.getChildCount() > 0) {
          child.setParent(null);
          current.add(child);
          this.searchedNodeCount++;
        }
      }
    }
    if (current.getChildCount() <= 0) {
      return null;
    }
    current.setParent(null);

    return current;
  }

  /**
   * Perform a character string search according to the search conditions.
   *
   * @param nodeText Search target string
   * @return true = match
   */
  private boolean isMatchText(String nodeText) {
    boolean result = false;
    SearchOption searchOption = getSearchOption();
    if (searchOption.isVariable()) {
      // Variable (= trace) search
      result = StringUtils.existsSearchWord(nodeText, searchOption.getSearchText());
    } else {
      // Text search
      result =
          StringUtils.existsSearchText(
              nodeText,
              searchOption.getSearchText(),
              searchOption.isSensitivecase(),
              searchOption.isRegex(),
              searchOption.isWord());
    }
    return result;
  }

  /**
   * Check if thread execution is cancelled
   *
   * @return true = Cancel
   */
  public boolean isCancel() {
    return !this.m_running;
  }

  /** Cancel the thread execution. */
  public void cancelRunning() {
    m_running = false;
  }

  /**
   * Check if it is a search target block. If the search node list is null, all search targets
   *
   * @param parent Search block node
   * @param searchNodeList Search node list
   * @return true = Search target block
   */
  private boolean isSearchBlock(DefaultMutableTreeNode parent, TreeNode[] searchNodeList) {
    if (parent == null) return false;
    if (searchNodeList == null || searchNodeList.length <= 0) return true;
    Object[] parentBlocks = parent.getUserObjectPath();

    SEARCH_LOOP:
    for (TreeNode searchnode : searchNodeList) {
      TreeNode node = searchnode;
      if (!(node instanceof DefaultMutableTreeNode)) continue;
      Object[] searchBlocks = ((DefaultMutableTreeNode) node).getUserObjectPath();

      // Check if it is the parent block of the search node list
      if (parentBlocks.length <= searchBlocks.length) {
        for (int i = 0; i < parentBlocks.length; i++) {
          Object parentObj = parentBlocks[i];
          if (parentObj instanceof DefaultMutableTreeNode) {
            parentObj = ((DefaultMutableTreeNode) parentObj).getUserObject();
          }
          Object searchObj = searchBlocks[i];
          if (searchObj instanceof DefaultMutableTreeNode) {
            searchObj = ((DefaultMutableTreeNode) searchObj).getUserObject();
          }
          if (parentObj != searchObj) {
            continue SEARCH_LOOP;
          }
        }
        return true;
      } else {
        for (int i = 0; i < searchBlocks.length; i++) {
          Object parentObj = parentBlocks[i];
          if (parentObj instanceof DefaultMutableTreeNode) {
            parentObj = ((DefaultMutableTreeNode) parentObj).getUserObject();
          }
          Object searchObj = searchBlocks[i];
          if (searchObj instanceof DefaultMutableTreeNode) {
            searchObj = ((DefaultMutableTreeNode) searchObj).getUserObject();
          }
          if (parentObj != searchObj) {
            continue SEARCH_LOOP;
          }
        }
        return true;
      }
    }

    return false;
  }

  /**
   * Get the search file list.
   *
   * @return Search file list
   */
  public SourceFile[] getSearchFiles() {
    return searchFiles;
  }

  /**
   * Set the search file list.
   *
   * @param files Search file list
   */
  public void setSearchFiles(SourceFile[] files) {
    this.searchFiles = files;
  }

  /**
   * Check if it has been added to the searched procedure list.
   *
   * @param proc procedure
   * @return true = added
   */
  private boolean containsSearchedProcedures(Procedure proc) {
    if (this.searchedProcedures == null) return false;
    return this.searchedProcedures.containsKey(proc);
  }

  /**
   * Get the tree node from the searched procedure list.
   *
   * @param proc Searched procedure
   * @return tree node
   */
  private DefaultMutableTreeNode getSearchedProcedureNode(Procedure proc) {
    if (this.searchedProcedures == null) return null;
    return this.searchedProcedures.get(proc);
  }

  /**
   * Add to the searched procedure list.
   *
   * @param proc procedure
   * @param node Tree node
   */
  private void addSearchedProcedures(Procedure proc, DefaultMutableTreeNode node) {
    if (this.searchedProcedures == null) {
      this.searchedProcedures = new HashMap<Procedure, DefaultMutableTreeNode>();
    }
    this.searchedProcedures.put(proc, node);
  }

  /**
   * Get the error message.
   *
   * @return error message
   */
  public String getErrorMessage() {
    return errorMessage;
  }

  /**
   * Set error message
   *
   * @param message Error message
   */
  public void setErrorMessage(String message) {
    this.errorMessage = message;
  }
}
