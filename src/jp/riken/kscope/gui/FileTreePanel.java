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
import java.util.Arrays;
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
import javax.swing.tree.MutableTreeNode;
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
import jp.riken.kscope.menu.ITreePopupMenu;
import jp.riken.kscope.model.FileTreeModel;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.utils.ResourceUtils;

/**
 * File tree tab panel class.
 *
 * @author RIKEN
 */
public class FileTreePanel extends javax.swing.JPanel
    implements ITabComponent, ITreeComponent, Observer {

  /** Serial number */
  private static final long serialVersionUID = 1L;
  /** Tree expansion button */
  private JButton btnExpand;
  /** Tree storage button */
  private JButton btnCollapse;
  /** Tree Pine */
  private ObjectTree treeExplore;
  /** Button panel */
  private JPanel panelButtons;
  /** Open file button */
  private JButton btnOpenFile;
  /** Export button */
  private JButton btnExport;

  /** File tree model */
  private FileTreeModel model;

  /** Explorer panel identifier */
  private EXPLORE_PANEL enumPanel;
  /** Parent component */
  private ITabComponent parentCompornent = null;

  /** XML File Tree Pop-up Menu */
  @SuppressWarnings("unused")
  private ITreePopupMenu menuPopup;

  /** Constructor */
  public FileTreePanel() {
    super();

    // Initialize
    initialize();
  }

  /**
   * Constructor
   *
   * @param panel Explorer panel identifier
   */
  public FileTreePanel(EXPLORE_PANEL panel) {
    super();

    // Panel identifier
    this.enumPanel = panel;

    // Initialize
    initialize();
  }

  /** Initialize */
  private void initialize() {
    // Initialize the GUI
    initGUI();

    // Generate file tree model
    this.model = new FileTreeModel();
    this.model.addObserver(this);
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
          panelButtons = new JPanel();
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
          treeExplore.setRootVisible(true);
          treeExplore.setShowsRootHandles(true);
          scrollPane.setViewportView(treeExplore);
        }
      }

      // Tooltip settings
      btnExpand.setToolTipText(
          Message.getString("treechooserdialog.tooltip.expandall")); // Expand all
      btnCollapse.setToolTipText(
          Message.getString("treechooserdialog.tooltip.collapseall")); // All stored
      btnOpenFile.setToolTipText(
          Message.getString("filetreepanel.tooltip.selective")); // open the selection
      btnExport.setToolTipText(Message.getString("mainmenu.file.export")); // export

    } catch (Exception e) {
      e.printStackTrace();
    }
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
   * Set focus listener
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
   * Get the file tree model
   *
   * @return model File tree model
   */
  public FileTreeModel getModel() {
    return model;
  }

  /**
   * Set the initial tree display title
   *
   * @param title Title
   */
  public void initTreeTitle(String title) {
    this.model.setInitTreeTitle(title);
    this.treeExplore.setModel(this.model.getTreeModel());
  }

  /**
   * Get the source file under the selected node.
   *
   * @return Source file list
   */
  @Override
  public SourceFile[] getSelectedSourceFiles() {

    ArrayList<SourceFile> list = new ArrayList<SourceFile>();

    // Get the source file object for the selected file.
    TreePath[] paths = this.treeExplore.getSelectionPaths();
    if (paths == null) return null;

    for (int i = 0; i < paths.length; i++) {
      int count = paths[i].getPath().length;
      // Other than root
      if (count >= 2) {
        DefaultMutableTreeNode node = (DefaultMutableTreeNode) paths[i].getPath()[count - 1];
        Object obj = node.getUserObject();
        // Is it a source file object?
        if (obj instanceof SourceFile) {
          // Add source file
          list.add((SourceFile) obj);
        }
      }
    }

    return list.toArray(new SourceFile[0]);
  }

  /**
   * Get the source file under the selected node.
   *
   * @return Source file list
   */
  public SourceFile[] getSelectChildSourceFiles() {

    ArrayList<SourceFile> list = new ArrayList<SourceFile>();

    // Get the source file object for the selected file.
    TreePath[] paths = this.treeExplore.getSelectionPaths();
    if (paths == null) return null;

    for (int i = 0; i < paths.length; i++) {
      int count = paths[i].getPath().length;
      // Other than root
      if (count >= 2) {
        MutableTreeNode node = (MutableTreeNode) paths[i].getPath()[count - 1];
        SourceFile[] child_list = getSourceFiles((DefaultMutableTreeNode) node);
        if (child_list != null) {
          // Add source file
          list.addAll(Arrays.asList(child_list));
        }
      } else if (count == 1) {
        // Add all elements for root
        return getAllSourceFiles();
      }
    }

    return list.toArray(new SourceFile[0]);
  }

  /**
   * Get the source file of the child element of the specified node.
   *
   * @param node Child element search node
   * @return Source file list
   */
  private SourceFile[] getSourceFiles(DefaultMutableTreeNode node) {

    if (node == null) return null;

    ArrayList<SourceFile> list = new ArrayList<SourceFile>();

    // Add yourself
    Object obj = node.getUserObject();
    // Is it a source file object?
    if (obj instanceof SourceFile) {
      // Add source file
      list.add((SourceFile) obj);
    }

    // Search for child elements
    int count = node.getChildCount();
    for (int i = 0; i < count; i++) {
      DefaultMutableTreeNode child = (DefaultMutableTreeNode) node.getChildAt(i);
      Object child_obj = child.getUserObject();
      if (child_obj == null) continue;

      // Get the source file from the child element
      SourceFile[] child_list = getSourceFiles(child);
      if (child_list != null) {
        // Add source file
        list.addAll(Arrays.asList(child_list));
      }
    }

    if (list.size() <= 0) return null;

    return list.toArray(new SourceFile[0]);
  }

  /**
   * Get all source files on the tree.
   *
   * @return Source file list
   */
  public SourceFile[] getAllSourceFiles() {

    // Root node
    DefaultMutableTreeNode node = (DefaultMutableTreeNode) this.treeExplore.getModel().getRoot();
    if (node == null) return null;

    ArrayList<SourceFile> list = new ArrayList<SourceFile>();
    SourceFile[] child_list = getSourceFiles(node);
    if (child_list != null) {
      // Add source file
      list.addAll(Arrays.asList(child_list));
    }
    if (list.size() <= 0) return null;

    return list.toArray(new SourceFile[0]);
  }

  /** Stores the entire tree of selection tabs. */
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
   * Set the file tree pop-up menu
   *
   * @param menuPopup File tree pop-up menu
   */
  public void setPopupMenu(ITreePopupMenu menuPopup) {
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

    this.menuPopup = menuPopup;
  }

  /**
   * Get the selected file
   *
   * @return selection file
   */
  @Override
  public File[] getSelectedNodeFiles() {

    ArrayList<File> list = new ArrayList<File>();

    // Get the source file object for the selected file.
    TreePath[] paths = this.treeExplore.getSelectionPaths();
    if (paths == null) return null;

    for (int i = 0; i < paths.length; i++) {
      int count = paths[i].getPath().length;
      DefaultMutableTreeNode node = (DefaultMutableTreeNode) paths[i].getPath()[count - 1];

      Object obj = node.getUserObject();
      // Is it a source file object?
      if (obj instanceof SourceFile) {
        // Add source file
        list.add(((SourceFile) obj).getFile());
      } else if (obj instanceof File) {
        // Add source file
        list.add((File) obj);
      } else if (obj instanceof String) {
        node = (DefaultMutableTreeNode) paths[i].getPath()[0];
        if (node.getUserObject() instanceof File) {
          StringBuffer buf = new StringBuffer();
          obj = node.getUserObject();
          buf.append(((File) obj).getAbsoluteFile());
          buf.append(File.separator);
          for (int j = 1; j < count; j++) {
            node = (DefaultMutableTreeNode) paths[i].getPath()[j];
            obj = node.getUserObject();
            buf.append((String) obj);
            buf.append(File.separator);
          }
          buf.deleteCharAt(buf.length() - 1);
          list.add(new File(buf.toString()));
        }
      }
    }

    return list.toArray(new File[0]);
  }

  /**
   * Property model change notification event
   *
   * @param o Notification source
   * @param arg Notification item
   */
  @Override
  public void update(Observable o, Object arg) {

    // Current file list
    SourceFile[] oldlist = getAllSourceFiles();
    int oldcount = oldlist != null ? oldlist.length : 0;

    FileTreeModel observer = (FileTreeModel) o;
    // Save the current node state
    this.treeExplore.storeTreeNode();

    // Tree redraw
    this.treeExplore.setModel(observer.getTreeModel());
    this.treeExplore.updateUI();

    // Restore node state
    this.treeExplore.restoreTreeNode();

    // Since it is the first file setting, expand and display all
    if (oldcount == 0) {
      // Expand all
      this.expandTreeAll();
    }
  }

  /**
   * Get selected source code line information
   *
   * @return Selected source code line information
   */
  @Override
  public CodeLine[] getSelectedCodeLines() {
    // Get the source file under the selected node.
    SourceFile[] files = getSelectedSourceFiles();
    if (files == null || files.length <= 0) return null;

    CodeLine[] lines = new CodeLine[files.length];
    for (int i = 0; i < files.length; i++) {

      lines[i] = new CodeLine(files[i], files[i].getPath());
    }

    return lines;
  }

  /**
   * Get the selected block
   *
   * @return selection block
   */
  @Override
  public IBlock[] getSelectedBlocks() {
    // No blocks in the source tree
    return null;
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

    // Set the selected node
    this.treeExplore.setSelectedNode(selectnode);

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
