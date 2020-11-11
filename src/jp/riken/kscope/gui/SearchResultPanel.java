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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Observable;
import java.util.Observer;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import javax.swing.border.CompoundBorder;
import javax.swing.border.LineBorder;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;
import jp.riken.kscope.Message;
import jp.riken.kscope.action.ViewOpenAnalysisLineAction;
import jp.riken.kscope.action.ViewOpenLanguageTreeAction;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.KEYWORD_TYPE;
import jp.riken.kscope.common.TRACE_DIR;
import jp.riken.kscope.component.SearchTree;
import jp.riken.kscope.component.SearchTreeModel;
import jp.riken.kscope.component.SearchTreeNode;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.Keyword;
import jp.riken.kscope.data.SearchOption;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.menu.MainMenu;
import jp.riken.kscope.model.SearchResultModel;
import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Search result panel class
 *
 * @author RIKEN
 */
public class SearchResultPanel extends AnalisysPanelBase
    implements Observer, IAnalisysComponent, ActionListener, MouseListener {

  /** Serial number */
  private static final long serialVersionUID = 1L;
  /** Reference list tree */
  private SearchTree treeSearch;
  /** Clear button */
  private JButton btnClear;
  /** Export button */
  private JButton btnExport;
  /** Open file */
  private JButton btnOpenFile;
  /** Move: Up button */
  private JButton btnMoveUp;
  /** Move: Down button */
  private JButton btnMoveDown;
  /** Search result label */
  private JLabel label;
  /** Expand all button */
  private JButton btnExpand;
  /** All storage buttons */
  private JButton btnCollapse;
  /** New structure tree */
  private JButton btnNewTree;
  /** Refresh button */
  private JButton btnRefresh;

  /** Search result table model */
  private SearchResultModel model;
  /** Action to open the relevant part */
  private ViewOpenAnalysisLineAction actionOpen;
  /** Action to open new structure tree */
  private ViewOpenLanguageTreeAction actionOpenLanguageTree;

  /** Constructor */
  public SearchResultPanel() {
    super();

    // Initialize.
    initialize();
  }

  /**
   * Constructor
   *
   * @param panel Analysis information panel identifier
   */
  public SearchResultPanel(ANALYSIS_PANEL panel) {
    super(panel);

    // Initialize.
    initialize();
  }

  /** Initialize. */
  private void initialize() {

    // Generate a model
    model = new SearchResultModel();
    // Set the observer.
    model.addObserver(this);

    // Initialize the GUI.
    initGUI();
  }

  /** Initialize the GUI. */
  private void initGUI() {
    try {
      BorderLayout thisLayout = new BorderLayout();
      this.setLayout(thisLayout);
      //            setPreferredSize(new Dimension(400, 64));

      // Information label at the top, button placement panel
      {
        JPanel panelTop = new JPanel();
        panelTop.setLayout(new BorderLayout());
        this.add(panelTop, BorderLayout.NORTH);
        panelTop.setBorder(
            new CompoundBorder(
                new LineBorder(Color.BLACK, 1), BorderFactory.createEmptyBorder(0, 5, 0, 20)));
        // Button layout panel
        {
          JPanel panelButtons = new JPanel();
          panelButtons.setLayout(new BoxLayout(panelButtons, BoxLayout.LINE_AXIS));
          panelTop.add(panelButtons, BorderLayout.EAST);
          java.awt.Dimension buttonSize = new java.awt.Dimension(24, 24);
          // Clear button
          {
            Icon icon = ResourceUtils.getIcon("removeall.gif");
            btnClear = new JButton(icon);
            panelButtons.add(btnClear);
            btnClear.setContentAreaFilled(false);
            btnClear.setBorderPainted(false);
            btnClear.setPreferredSize(buttonSize);
            btnClear.setMinimumSize(buttonSize);
            btnClear.setMaximumSize(buttonSize);
          }
          // Refresh button
          {
            Icon icon = ResourceUtils.getIcon("refresh.gif");
            btnRefresh = new JButton(icon);
            panelButtons.add(btnRefresh);
            btnRefresh.setContentAreaFilled(false);
            btnRefresh.setBorderPainted(false);
            btnRefresh.setPreferredSize(buttonSize);
            btnRefresh.setMinimumSize(buttonSize);
            btnRefresh.setMaximumSize(buttonSize);
          }
          {
            Icon icon = ResourceUtils.getIcon("expandall.gif");
            btnExpand = new JButton(icon);
            btnExpand.setContentAreaFilled(false);
            btnExpand.setBorderPainted(false);
            btnExpand.setPreferredSize(buttonSize);
            btnExpand.setMinimumSize(buttonSize);
            btnExpand.setMaximumSize(buttonSize);
            panelButtons.add(btnExpand);
          }
          {
            Icon icon = ResourceUtils.getIcon("collapseall.gif");
            btnCollapse = new JButton(icon);
            btnCollapse.setContentAreaFilled(false);
            btnCollapse.setBorderPainted(false);
            btnCollapse.setPreferredSize(buttonSize);
            btnCollapse.setMinimumSize(buttonSize);
            btnCollapse.setMaximumSize(buttonSize);
            panelButtons.add(btnCollapse);
          }
          {
            Icon icon = ResourceUtils.getIcon("arrow_up.gif");
            btnMoveUp = new JButton(icon);
            btnMoveUp.setContentAreaFilled(false);
            btnMoveUp.setBorderPainted(false);
            btnMoveUp.setPreferredSize(buttonSize);
            btnMoveUp.setMinimumSize(buttonSize);
            btnMoveUp.setMaximumSize(buttonSize);
            panelButtons.add(btnMoveUp);
          }
          {
            Icon icon = ResourceUtils.getIcon("arrow_down.gif");
            btnMoveDown = new JButton(icon);
            btnMoveDown.setContentAreaFilled(false);
            btnMoveDown.setBorderPainted(false);
            btnMoveDown.setPreferredSize(buttonSize);
            btnMoveDown.setMinimumSize(buttonSize);
            btnMoveDown.setMaximumSize(buttonSize);
            panelButtons.add(btnMoveDown);
          }
          {
            Icon icon = ResourceUtils.getIcon("new_tree.gif");
            btnNewTree = new JButton(icon);
            btnNewTree.setContentAreaFilled(false);
            btnNewTree.setBorderPainted(false);
            btnNewTree.setPreferredSize(buttonSize);
            btnNewTree.setMinimumSize(buttonSize);
            btnNewTree.setMaximumSize(buttonSize);
            panelButtons.add(btnNewTree);
          }
          {
            Icon icon = ResourceUtils.getIcon("openfile.gif");
            btnOpenFile = new JButton(icon);
            btnOpenFile.setContentAreaFilled(false);
            btnOpenFile.setBorderPainted(false);
            btnOpenFile.setPreferredSize(buttonSize);
            btnOpenFile.setMinimumSize(buttonSize);
            btnOpenFile.setMaximumSize(buttonSize);
            panelButtons.add(btnOpenFile);
          }
          {
            Icon icon = ResourceUtils.getIcon("save.gif");
            btnExport = new JButton(icon);
            btnExport.setContentAreaFilled(false);
            btnExport.setBorderPainted(false);
            btnExport.setPreferredSize(buttonSize);
            btnExport.setMinimumSize(buttonSize);
            btnExport.setMaximumSize(buttonSize);
            panelButtons.add(btnExport);
          }
        }
        // Label placement
        {
          label = new JLabel();
          panelTop.add(label, BorderLayout.CENTER);
          // label.setText();
        }
      }
      {
        {
          // Search result tree
          treeSearch = new SearchTree();

          SearchTreeNode rootNode =
              new SearchTreeNode(
                  Message.getString("mainmenu.window.analysis.search")); // search results
          SearchTreeModel treeModel = new SearchTreeModel(rootNode);
          treeSearch.setModel(treeModel);
          treeSearch.setRootVisible(true);
          treeSearch.setShowsRootHandles(true);

          // Do not expand nodes by double-clicking.
          treeSearch.setToggleClickCount(0);
          // Only one line can be selected
          treeSearch.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);

          // Scroll pine
          JScrollPane scrollTable = new JScrollPane(treeSearch);
          scrollTable.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
          scrollTable.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
          scrollTable.getViewport().setBackground(Color.WHITE);

          add(scrollTable);
        }
      }

      // Tooltip settings
      btnClear.setToolTipText(Message.getString("informationdialog.button.clear.tooltip")); // clear
      btnOpenFile.setToolTipText(
          Message.getString("searchresultpanel.tooltip.open")); // Open the search result part
      btnExport.setToolTipText(Message.getString("mainmenu.file.export")); // export
      btnMoveUp.setToolTipText(Message.getString("searchresultpanel.tooltip.preview")); // Forward
      btnMoveDown.setToolTipText(Message.getString("searchresultpanel.tooltip.next")); // next
      btnExpand.setToolTipText(
          Message.getString("treechooserdialog.tooltip.expandall")); // Expand all
      btnCollapse.setToolTipText(
          Message.getString("treechooserdialog.tooltip.collapseall")); // All stored
      btnNewTree.setToolTipText(
          Message.getString("searchresultpanel.tooltip.new")); // New structure tree
      btnRefresh.setToolTipText(
          Message.getString("searchresultpanel.tooltip.update")); // Update search results

      // Add event
      btnMoveUp.addActionListener(this);
      btnMoveDown.addActionListener(this);
      btnExpand.addActionListener(this);
      btnCollapse.addActionListener(this);
      btnOpenFile.addActionListener(this);
      treeSearch.addMouseListener(this);
      btnNewTree.addActionListener(this);

    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * Search result model change notification event
   *
   * @param o Notification source
   * @param arg Notification item
   */
  @Override
  public void update(Observable o, Object arg) {
    // Update the search result model.
    updateModel();
  }

  /** Update the search result model. */
  private void updateModel() {
    // Search result model settings
    this.treeSearch.setModel(this.model.getTreeModel());

    // Panel title
    this.label.setText(this.model.getTitle());

    // Enable switching of new structure tree button
    boolean enabled = false;
    DefaultMutableTreeNode root = this.model.getRootNode();
    if (root != null && root.getChildCount() > 0) {
      // The first child node must be Procedure.
      DefaultMutableTreeNode child = (DefaultMutableTreeNode) root.getChildAt(0);
      if (child != null
          && child.getUserObject() != null
          && child.getUserObject() instanceof Procedure) {
        enabled = true;
      }
    }
    Icon icon = null;
    if (enabled) {
      // Enable button icon
      icon = ResourceUtils.getIcon("new_tree.gif");
    } else {
      // Button icon for disabling
      icon = ResourceUtils.getIcon("new_tree_gray.gif");
    }
    this.btnNewTree.setIcon(icon);
    this.btnNewTree.setEnabled(enabled);
  }

  /**
   * Get the search result model
   *
   * @return Search result model
   */
  public SearchResultModel getModel() {
    return model;
  }

  /**
   * Set focus listener
   *
   * @param listener Focus listener
   */
  @Override
  public void addTabFocusListener(TabFocusListener listener) {
    this.addFocusListener(listener);
    // Set focus listener for child components as well
    SwingUtils.addChildFocusListener(this, listener);
  }

  /** Export */
  @Override
  public void export(File file) {
    if (this.model == null) return;

    model.writeFile(file);
  }

  /**
   * Set an action listener on the panel. <br>
   * Assign the created action listener to the menu bar to the panel button.
   *
   * @param menu Menu bar
   */
  @Override
  public void setActionListener(MainMenu menu) {
    // Analysis information export action
    this.btnExport.addActionListener(menu.getActionExportAnalysis());
    // Open the search result
    actionOpen = (ViewOpenAnalysisLineAction) menu.getActionOpenAnalysisLine();
    // New structure tree action
    this.actionOpenLanguageTree = menu.getActionOpenLanguageTree();
    // clear
    this.btnClear.addActionListener(menu.getActionSearchResult(TRACE_DIR.END));
    // Update search results
    this.btnRefresh.addActionListener(menu.getActionSearchResult(TRACE_DIR.REFRESH));
  }

  /**
   * Get the code information of the selected line. <br>
   * A code information object is set in the first column of the table model.
   *
   * @return code information
   */
  @Override
  public CodeLine getSelectedCodeLine() {

    // Get the source file object for the selected file.
    DefaultMutableTreeNode node =
        (DefaultMutableTreeNode) this.treeSearch.getLastSelectedPathComponent();
    if (node == null) return null;
    if (node.getUserObject() == null) return null;

    // Is it a block object?
    if (node.getUserObject() instanceof IBlock) {
      IBlock block = (IBlock) node.getUserObject();
      return block.getStartCodeLine();
    } else if (node.getUserObject() instanceof CodeLine) {
      return (CodeLine) node.getUserObject();
    }

    return null;
  }

  /** Clear the model. */
  @Override
  public void clearModel() {
    // Model clear
    model.clearSearchResult();
  }

  /** Close the tab */
  @Override
  public void closeTab() {}

  /**
   * Button click event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {

    if (event.getSource() == this.btnMoveUp) {
      moveUp();
    } else if (event.getSource() == this.btnMoveDown) {
      moveDown();
    } else if (event.getSource() == this.btnExpand) {
      // Expand all
      expandTreeAll();
    } else if (event.getSource() == this.btnCollapse) {
      // store everything
      collapseTreeAll();
    } else if (event.getSource() == this.btnOpenFile) {
      // Open the relevant part
      openAnalysisLine();
    } else if (event.getSource() == this.btnNewTree) {
      // New structure tree
      DefaultMutableTreeNode node =
          (DefaultMutableTreeNode) this.treeSearch.getLastSelectedPathComponent();
      if (node == null) return;
      if (node.getUserObject() == null) return;
      if (!(node.getUserObject() instanceof IBlock)) return;
      this.actionOpenLanguageTree.openLanguageTree((IBlock) node.getUserObject());
    }
  }

  /**
   * Move the selected line up. <br>
   * If not selected, select the first line. <br>
   * For the first line, select the last line.
   */
  public void moveUp() {
    // Selected node
    SearchTreeNode selectnode = (SearchTreeNode) this.treeSearch.getLastSelectedPathComponent();
    SearchTreeNode root = (SearchTreeNode) this.treeSearch.getModel().getRoot();

    // List tree nodes in the forward direction
    SearchTreeNode movenode = null;
    SearchTreeNode prevnode = null;

    // List tree nodes in the forward direction
    Enumeration<?> depth = root.preorderEnumeration();
    while (depth.hasMoreElements()) {
      SearchTreeNode treeNode = (SearchTreeNode) depth.nextElement();
      boolean match = treeNode.isMatch();
      // Do a node search
      if (treeNode == selectnode) {
        if (prevnode != null) {
          movenode = prevnode;
          break;
        }
      }
      // Last search node
      if (match) {
        prevnode = treeNode;
      }
    }
    if (movenode == null) {
      movenode = prevnode;
    }
    if (movenode == null) return;

    // Get the path of the selected node
    TreePath path = SwingUtils.getTreePath(movenode);

    // Select the selected path
    this.treeSearch.setSelectionPath(path);
    this.treeSearch.scrollPathToVisible(path);

    // Open the relevant part
    this.btnOpenFile.doClick();
  }

  /**
   * Move the selected line down. <br>
   * If not selected, select the first line. <br>
   * For the last line, select the first line.
   */
  public void moveDown() {

    // Selected node
    SearchTreeNode selectnode = (SearchTreeNode) this.treeSearch.getLastSelectedPathComponent();
    SearchTreeNode root = (SearchTreeNode) this.treeSearch.getModel().getRoot();

    // List tree nodes in the forward direction
    SearchTreeNode movenode = null;
    SearchTreeNode firstnode = null;
    SearchTreeNode currentnode = null;

    // List tree nodes in the forward direction
    Enumeration<?> depth = root.preorderEnumeration();
    while (depth.hasMoreElements()) {
      SearchTreeNode treeNode = (SearchTreeNode) depth.nextElement();
      boolean match = treeNode.isMatch();
      // First search node
      if (firstnode == null && match) {
        firstnode = treeNode;
        // Since there is no selected node at present, the first search node is selected, so there
        // is no need to search after that.
        if (selectnode == null) {
          break;
        }
      }
      // Do a node search
      if (treeNode == selectnode) {
        currentnode = treeNode;
        continue;
      }
      if (currentnode != null && match) {
        movenode = treeNode;
        break;
      }
    }
    if (movenode == null) {
      movenode = firstnode;
    }
    if (movenode == null) return;

    // Get the path of the selected node
    TreePath path = SwingUtils.getTreePath(movenode);

    // Select the selected path
    this.treeSearch.setSelectionPath(path);
    this.treeSearch.scrollPathToVisible(path);

    // Open the relevant part
    this.btnOpenFile.doClick();
  }

  /** Open the relevant part */
  public void openAnalysisLine() {
    // Selected tree path
    TreePath path = this.treeSearch.getSelectionPath();
    // Select the selection tree path
    this.actionOpen.openSearchLine(path);
  }

  /**
   * Get the selected block
   *
   * @return selection block
   */
  @Override
  public IBlock getSelectedBlock() {

    // Get the source file object for the selected file.
    DefaultMutableTreeNode node =
        (DefaultMutableTreeNode) this.treeSearch.getLastSelectedPathComponent();
    if (node == null) return null;
    if (node.getUserObject() == null) return null;

    // Is it a block object?
    if (node.getUserObject() instanceof IBlock) {
      return (IBlock) node.getUserObject();
    }

    return null;
  }

  /**
   * Get additional selection information
   *
   * @return Selectable additional information
   */
  @Override
  public IInformation getSelectedInformation() {

    // Get the source file object for the selected file.
    DefaultMutableTreeNode node =
        (DefaultMutableTreeNode) this.treeSearch.getLastSelectedPathComponent();
    if (node == null) return null;
    if (node.getUserObject() == null) return null;

    // Is it an additional information object?
    if (node.getUserObject() instanceof IInformation) {
      return (IInformation) node.getUserObject();
    }

    return null;
  }

  /**
   * Set source view properties
   *
   * @param properties Source view properties
   */
  @Override
  public void setSourceProperties(SourceProperties properties) {
    // Set the marking color of the trace tree.
    // Letter color
    this.treeSearch.setForecolor(properties.getSearchFontColor());
    // Background color
    this.treeSearch.setBackcolor(properties.getSearchBackgroundColor());
    // Style
    this.treeSearch.setFontstyle(KscopeProperties.SEARCHTREE_FONTSTYLE);

    // Redraw tree
    this.treeSearch.invalidate();
    this.treeSearch.validate();
    this.treeSearch.repaint();
  }

  /** Stores the entire tree on the selection tab. */
  public void collapseTreeAll() {
    int row = this.treeSearch.getRowCount() - 1;
    while (row >= 0) {
      this.treeSearch.collapseRow(row);
      row--;
    }
    // Expand only root node
    this.treeSearch.expandRow(0);
  }

  /** Expand the entire tree on the Selection tab. */
  public void expandTreeAll() {
    int row = 0;
    while (row < this.treeSearch.getRowCount()) {
      this.treeSearch.expandRow(row);
      row++;
    }
  }

  /**
   * Mouse click event
   *
   * @param event Mouse event information
   */
  @Override
  public void mouseClicked(MouseEvent event) {
    // Double click check
    if (SwingUtilities.isLeftMouseButton(event) && event.getClickCount() == 2) {
      // Open the relevant part
      this.btnOpenFile.doClick();
    }
  }

  /**
   * Mouse button down event
   *
   * @param e Mouse event information
   */
  @Override
  public void mousePressed(MouseEvent e) {}

  /**
   * Mouse button up event
   *
   * @param e Mouse event information
   */
  @Override
  public void mouseReleased(MouseEvent e) {}

  /**
   * Mouseover event
   *
   * @param e Mouse event information
   */
  @Override
  public void mouseEntered(MouseEvent e) {}

  /**
   * Mouse out event
   *
   * @param e Mouse event information
   */
  @Override
  public void mouseExited(MouseEvent e) {}

  /**
   * Get the keyword list of search results
   *
   * @return Search keyword list
   */
  public Keyword[] getSearchKeywords() {

    SearchTreeModel model = (SearchTreeModel) this.treeSearch.getModel();
    if (model == null) return null;
    SearchTreeNode root = (SearchTreeNode) model.getRoot();

    // Keyword list
    List<Keyword> list = new ArrayList<Keyword>();

    // Search criteria
    SearchOption option = model.getSearchOption();

    // List tree nodes in the forward direction
    Enumeration<?> depth = root.preorderEnumeration();
    while (depth.hasMoreElements()) {
      DefaultMutableTreeNode treeNode = (DefaultMutableTreeNode) depth.nextElement();

      // Do a node search
      if (treeNode == null || treeNode.getUserObject() == null) {
        continue;
      }
      if (treeNode.getUserObject() instanceof CodeLine) {
        CodeLine line = (CodeLine) treeNode.getUserObject();
        Keyword word = new Keyword(KEYWORD_TYPE.SEARCH);
        word.setKeyword(option.getSearchText());
        word.setCaseSensitive(option.isSensitivecase());
        word.setRegex(option.isRegex());
        word.setSearchWord(option.isWord());
        word.setSearchVariable(false);
        word.setSearchLine(line);

        list.add(word);
      }
    }

    if (list.size() <= 0) return null;

    return list.toArray(new Keyword[0]);
  }

  /** Copy the selection to the clipboard. */
  @Override
  public void copyClipboard() {

    // Get the source file object for the selected file.
    DefaultMutableTreeNode node =
        (DefaultMutableTreeNode) this.treeSearch.getLastSelectedPathComponent();
    if (node == null) return;
    if (node.getUserObject() == null) return;
    String text = node.getUserObject().toString();

    // copy to clipboard
    SwingUtils.copyClipboard(text);
  }

  /** Whether there is information to export */
  @Override
  public boolean isExportable() {
    if (this.model == null) return false;
    return (!this.model.isEmpty());
  }
}
