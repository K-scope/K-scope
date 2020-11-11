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
import java.awt.Dimension;
import java.awt.event.ActionListener;
import java.awt.event.MouseListener;
import java.io.File;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Observable;
import java.util.Observer;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.border.LineBorder;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import jp.riken.kscope.Message;
import jp.riken.kscope.action.ExploreTreeChangeAction;
import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.component.ObjectTree;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.menu.ModuleTreePopupMenu;
import jp.riken.kscope.model.ModuleTreeModel;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.utils.ResourceUtils;

/**
 * Module tree tab panel class
 *
 * @author RIKEN
 */
public class ModuleTreePanel extends javax.swing.JPanel
    implements ITabComponent, ITreeComponent, Observer {
  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Expand all button */
  private JButton btnExpand;
  /** All storage buttons */
  private JButton btnCollapse;
  /** Module tree */
  private ObjectTree treeExplore;

  /** Module tree model */
  private ModuleTreeModel model;

  /** Explorer panel identifier */
  private EXPLORE_PANEL enumPanel;
  /** Parent component */
  private ITabComponent parentCompornent;
  /** Open file button */
  private JButton btnOpenFile;
  /** Export button */
  private JButton btnExport;

  /** Constructor */
  public ModuleTreePanel() {
    super();

    // Initialize
    initialize();
  }

  /**
   * Constructor
   *
   * @param panel Explorer panel identifier
   */
  public ModuleTreePanel(EXPLORE_PANEL panel) {
    this.enumPanel = panel;

    // Initialize
    initialize();
  }

  /** Initialize */
  private void initialize() {

    // Generate a model
    model = new ModuleTreeModel();
    // Set the observer.
    model.addObserver(this);

    initGUI();
  }

  /** Perform GUI initialization */
  private void initGUI() {
    try {
      BorderLayout thisLayout = new BorderLayout();
      this.setLayout(thisLayout);
      setPreferredSize(new Dimension(200, 300));
      {
        JPanel panelContent = new JPanel();
        BorderLayout panelContentLayout = new BorderLayout();
        panelContent.setLayout(panelContentLayout);
        this.add(panelContent, BorderLayout.NORTH);
        panelContent.setPreferredSize(new java.awt.Dimension(200, 24));
        panelContent.setBorder(new LineBorder(new java.awt.Color(0, 0, 0), 1, false));
        {
          JPanel panelButtons = new JPanel();
          panelButtons.setLayout(new BoxLayout(panelButtons, BoxLayout.LINE_AXIS));
          panelButtons.setPreferredSize(new java.awt.Dimension(120, 24));
          panelContent.add(panelButtons, BorderLayout.EAST);
          java.awt.Dimension buttonSize = new java.awt.Dimension(18, 18);
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
          // Margin setting
          panelButtons.add(Box.createHorizontalStrut(5));
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
          // Margin setting
          panelButtons.add(Box.createHorizontalStrut(5));
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
          // Margin setting
          panelButtons.add(Box.createHorizontalStrut(5));
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
      }
      {
        JScrollPane scrollPane = new JScrollPane();
        this.add(scrollPane, BorderLayout.CENTER);
        {
          treeExplore = new ObjectTree();
          scrollPane.setViewportView(treeExplore);
          treeExplore.setModel(model.getTreeModel());
          treeExplore.setRootVisible(true);
          treeExplore.setShowsRootHandles(true);
        }
      }

      // Do not expand nodes by double-clicking.
      treeExplore.setToggleClickCount(0);

      // Tooltip settings
      btnExpand.setToolTipText(
          Message.getString("treechooserdialog.tooltip.expandall")); // Expand all
      btnCollapse.setToolTipText(
          Message.getString("informationpanel.tooltip.openblock")); // All stored
      btnOpenFile.setToolTipText(
          Message.getString("treechooserdialog.tooltip.collapseall")); // open the selection
      btnExport.setToolTipText(Message.getString("mainmenu.file.export")); // export

    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * Tree model change notification event
   *
   * @param o Notification source
   * @param arg Notification item
   */
  @Override
  public void update(Observable o, Object arg) {

    ModuleTreeModel observer = (ModuleTreeModel) o;

    // Set the database.
    this.treeExplore.setLanguageDb(this.model.getLanguageDb());
    // Tree redraw
    this.treeExplore.setModel(observer.getTreeModel());
    this.treeExplore.updateUI();
  }

  /**
   * Get a tree model
   *
   * @return Tree model
   */
  public ModuleTreeModel getModel() {
    return this.model;
  }

  /** Stores the entire tree on the selection tab. */
  @Override
  public void collapseTreeAll() {
    int row = this.treeExplore.getRowCount() - 1;
    while (row >= 0) {
      this.treeExplore.collapseRow(row);
      row--;
    }
    // Expand only root node
    this.treeExplore.expandRow(0);
  }

  /** Expand the entire tree on the Selection tab. */
  @Override
  public void expandTreeAll() {
    int row = 0;
    while (row < this.treeExplore.getRowCount()) {
      this.treeExplore.expandRow(row);
      row++;
    }
  }

  /** Expand the nodes under the selection tree on the selection tab. */
  @Override
  public void expandTreeSelect() {
    TreePath[] paths = this.treeExplore.getSelectionPaths();
    if (paths == null) return;

    for (int i = 0; i < paths.length; i++) {
      // Expand under the tree path.
      visitAll(paths[i], true);
    }
  }

  /**
   * Expand under the tree path and display it in a collapsed manner.
   *
   * @param parent Tree path
   * @param expand Expand (true) / Collapse (false)
   */
  public void visitAll(TreePath parent, boolean expand) {
    TreeNode node = (TreeNode) parent.getLastPathComponent();
    if (!node.isLeaf() && node.getChildCount() >= 0) {
      Enumeration<?> e = node.children();
      while (e.hasMoreElements()) {
        TreeNode n = (TreeNode) e.nextElement();
        TreePath path = parent.pathByAddingChild(n);
        visitAll(path, expand);
      }
    }
    if (expand) this.treeExplore.expandPath(parent);
    else this.treeExplore.collapsePath(parent);

    return;
  }

  /**
   * Get the source file under the selected node.
   *
   * @return Source file list
   */
  @Override
  public SourceFile[] getSelectedSourceFiles() {
    CodeLine[] lines = getSelectedCodeLines();
    if (lines == null || lines.length <= 0) return null;

    List<SourceFile> list = new ArrayList<SourceFile>();
    for (int i = 0; i < lines.length; i++) {
      if (lines[i].getSourceFile() == null) continue;
      if (lines[i].getSourceFile().getFile() == null) continue;
      list.add(lines[i].getSourceFile());
    }
    if (list.size() <= 0) return null;
    return list.toArray(new SourceFile[0]);
  }

  /**
   * Get the selected file
   *
   * @return selection file
   */
  @Override
  public File[] getSelectedNodeFiles() {
    SourceFile[] files = getSelectedSourceFiles();
    if (files == null || files.length <= 0) return null;
    File[] list = new File[files.length];
    for (int i = 0; i < files.length; i++) {
      list[i] = files[i].getFile();
    }

    return list;
  }

  /**
   * Get the parent component.
   *
   * @return Parent component
   */
  @Override
  public ITabComponent getParentComponent() {
    return this.parentCompornent;
  }

  /**
   * Set the parent component.
   *
   * @param component Parent component
   */
  @Override
  public void setParentComponent(ITabComponent component) {
    this.parentCompornent = component;
  }

  /**
   * Set tab focus listener
   *
   * @param listener Focus listener
   */
  @Override
  public void addTabFocusListener(TabFocusListener listener) {
    this.addFocusListener(listener);
    if (this.treeExplore != null) {
      this.treeExplore.addFocusListener(listener);
    }
  }

  /** Close the active tab. */
  @Override
  public void closeTabComponent() {
    // Close with parent component
    this.parentCompornent.closeTabComponent();
  }

  /**
   * Get selected source code line information
   *
   * @return Selected source code line information
   */
  @Override
  public CodeLine[] getSelectedCodeLines() {

    ArrayList<CodeLine> list = new ArrayList<CodeLine>();

    IBlock[] blocks = getSelectedBlocks();
    if (blocks == null) return null;

    for (int i = 0; i < blocks.length; i++) {
      CodeLine start = blocks[i].getStartCodeLine();
      CodeLine end = blocks[i].getEndCodeLine();
      if (start == null) continue;

      // Create one CodeLine with start line number + end line number
      if (end != null && start.getEndLine() < end.getEndLine()) {
        start.setEndLine(end.getEndLine());
      }
      list.add(start);
    }
    if (list.size() <= 0) return null;

    return list.toArray(new CodeLine[0]);
  }

  /**
   * Get the selected block
   *
   * @return selection block
   */
  @Override
  public IBlock[] getSelectedBlocks() {

    ArrayList<IBlock> list = new ArrayList<IBlock>();

    // Get the source file object for the selected file.
    TreePath[] paths = this.treeExplore.getSelectionPaths();
    if (paths == null) return null;

    for (int i = 0; i < paths.length; i++) {
      int count = paths[i].getPath().length;
      // Other than root
      if (count >= 2) {
        DefaultMutableTreeNode node = (DefaultMutableTreeNode) paths[i].getPath()[count - 1];
        Object obj = node.getUserObject();
        // Is it a block object?
        if (obj instanceof IBlock) {
          list.add((IBlock) obj);
        }
      }
    }
    if (list.size() <= 0) return null;

    return list.toArray(new IBlock[0]);
  }

  /**
   * Export explorer tree
   *
   * @param file Output file
   */
  @Override
  public void export(File file) {
    if (this.model == null) return;

    model.writeFile(file);
  }

  /**
   * Get the explorer panel identifier
   *
   * @return Explorer panel identifier
   */
  @Override
  public EXPLORE_PANEL getEnumPanel() {
    return this.enumPanel;
  }

  /**
   * Set the module tree pop-up menu
   *
   * @param menuPopup Module tree pop-up menu
   */
  public void setPopupMenu(ModuleTreePopupMenu menuPopup) {

    // Pop-up menu settings
    this.treeExplore.setComponentPopupMenu((JPopupMenu) menuPopup);

    // Set the action listener on the expand button
    this.btnExpand.addActionListener(menuPopup.getActionTreeExpandAll());
    // Set the action listener for the storage button
    this.btnCollapse.addActionListener(menuPopup.getActionTreeCollapseAll());
    // Set an action listener for the file open button
    this.btnOpenFile.addActionListener((ActionListener) menuPopup.getActionOpenFile());
    // Register for double-click event (open file)
    this.treeExplore.addMouseListener((MouseListener) menuPopup.getActionOpenFile());
    // Register for export
    this.btnExport.addActionListener(menuPopup.getActionExportExplore());
  }

  /**
   * Get the currently selected node.
   *
   * @return Selected node
   */
  @Override
  public DefaultMutableTreeNode getSelectedNode() {
    Object obj = treeExplore.getLastSelectedPathComponent();
    if (obj == null) return null;

    if (obj instanceof DefaultMutableTreeNode) {
      return (DefaultMutableTreeNode) obj;
    }

    return null;
  }

  /**
   * Get the currently selected node list.
   *
   * @return Selected node list
   */
  @Override
  public DefaultMutableTreeNode[] getSelectedNodes() {
    TreePath[] paths = treeExplore.getSelectionPaths();
    if (paths == null) return null;

    List<DefaultMutableTreeNode> list = new ArrayList<DefaultMutableTreeNode>();
    for (int i = 0; i < paths.length; i++) {
      Object obj = paths[i].getLastPathComponent();
      if (obj == null) continue;
      if (obj instanceof DefaultMutableTreeNode) {
        list.add((DefaultMutableTreeNode) obj);
      }
    }
    if (list.size() <= 0) return null;

    return list.toArray(new DefaultMutableTreeNode[0]);
  }

  /**
   * Change tree Register the listener.
   *
   * @param action Tree change listener
   */
  @Override
  public void addTreeSelectionListener(ExploreTreeChangeAction action) {
    this.treeExplore.addTreeSelectionListener(action);
  }

  /**
   * Set the selected node
   *
   * @param selectnode Select node
   */
  @Override
  public void setSelectedNode(Object selectnode) {
    if (selectnode == null) return;
    // Changed to set the node path as it is when the argument is a node (2014/4/8 ohichi)
    if (selectnode instanceof DefaultMutableTreeNode) {
      TreePath path = new TreePath(((DefaultMutableTreeNode) selectnode).getPath());
      this.treeExplore.setSelectionPath(path);
      this.treeExplore.scrollPathToVisibleForVertical(path);
    } else {
      // Set the selected node
      this.treeExplore.setSelectedNode(selectnode);
    }
    return;
  }

  /**
   * Set multiple selection node
   *
   * @param selectnodes Select node list
   */
  @Override
  public void setSelectedNodes(Object[] selectnodes) {
    if (selectnodes == null) return;

    // Set the selected node
    this.treeExplore.setSelectedNodes(selectnodes);

    return;
  }

  /**
   * Select a node range
   *
   * @param startnode Selection start node
   * @param endnode Selection end node
   */
  @Override
  public void setSelectedNodeArea(Object startnode, Object endnode) {
    if (startnode == null && endnode == null) return;

    // Set the selected node
    this.treeExplore.setSelectedNodeArea(startnode, endnode);
  }

  /**
   * Add node selection
   *
   * @param startnode Selection start node
   * @param endnode Selection end node
   */
  @Override
  public void addSelectedNodeArea(Object startnode, Object endnode) {
    if (startnode == null && endnode == null) return;

    // Set the selected node
    this.treeExplore.addSelectedNodeArea(startnode, endnode);
  }

  /**
   * Get a tree model
   *
   * @return Tree model
   */
  @Override
  public TreeModel getTreeModel() {
    return this.treeExplore.getModel();
  }

  /**
   * Select a tree node.
   *
   * @param path Selected tree path
   */
  @Override
  public void setSelectionPath(TreePath path) {
    // Get the actual tree path
    TreePath real = this.treeExplore.getRealTreePath(path);
    if (real != null) {
      // Select a tree path
      this.treeExplore.setSelectionPath(real);

      // Sucrose to show the path node.
      this.treeExplore.scrollPathToVisibleForVertical(real);

      // Activate the tab
      ((JTabbedPane) this.parentCompornent).setSelectedComponent(this);
    }
  }

  /** Update the drawing of the panel. */
  @Override
  public void updateUI() {
    if (treeExplore != null) {
      // redraw
      treeExplore.updateUI();
    }
    super.updateUI();
  }

  /**
   * Add multiple selection node
   *
   * @param selectnodes Select node list
   */
  @Override
  public void addSelectedNodes(Object[] selectnodes) {
    if (selectnodes == null) return;

    // Set the selected node
    this.treeExplore.addSelectedNodes(selectnodes);

    return;
  }

  /**
   * Set source view properties
   *
   * @param properties Source view properties
   */
  public void setSourceProperties(SourceProperties properties) {
    this.treeExplore.setSourceProperties(properties);
  }

  /** Raise a change event for the selected node */
  @Override
  public void fireSelectNodeChanged() {
    this.treeExplore.fireSelectNodeChanged();
  }
}
