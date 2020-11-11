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
import java.awt.Container;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.List;
import javax.swing.JComponent;
import javax.swing.JTree;
import javax.swing.JViewport;
import javax.swing.event.TreeExpansionEvent;
import javax.swing.event.TreeWillExpandListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.ExpandVetoException;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import jp.riken.kscope.language.Block;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProcedureUsage;
import jp.riken.kscope.language.Selection;
import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.service.LanguageService;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Row highlight tree component
 *
 * @author RIKEN
 */
public class JEntireRowTree extends javax.swing.JTree implements TreeWillExpandListener {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Tree selection background color (= cornflowerblue) */
  private static Color SELECTION_BACKGROUND = new Color(0x66, 0x99, 0xFF);
  /** Tree selection text color */
  private static Color SELECTION_FORECOLOR = new Color(255, 255, 255);

  /** Currently selected node */
  private List<TreePath> currentSelected;
  /** Currently deployed node */
  private List<TreePath> currentExpanded;

  /**
   * Tree Look & Feel class. <br>
   * Select a node by selecting a row in the tree.
   */
  private EntireRowTreeUI treeUI;

  /** Constructor */
  public JEntireRowTree() {

    // Tree Look & Feel generation
    treeUI = new EntireRowTreeUI();
    this.setUI(treeUI);

    // Transparency setting
    this.setOpaque(false);

    // Setting the node drawing class of the tree
    EntireRowCellRenderer renderer = new EntireRowCellRenderer();
    this.setCellRenderer(renderer);

    this.addTreeWillExpandListener(this);
  }

  /**
   * Set the look and feel of the tree when updating the document. <br>
   * The TreeUI set in the constructor is canceled, so set it every time.
   */
  @Override
  public void updateUI() {
    super.updateUI();
    this.setUI(treeUI);
  }

  /**
   * Draw the component
   *
   * @param g graphics
   */
  @Override
  protected void paintComponent(Graphics g) {

    try {
      // Draw the background color
      g.setColor(getBackground());
      g.fillRect(0, 0, getWidth(), getHeight());

      if (getSelectionCount() > 0) {
        int count = getSelectionCount();
        int[] list = getSelectionRows();
        if (count > 0 && list != null) {
          // Fill the entire selected line.
          for (int i : list) {
            Rectangle r = getRowBounds(i);
            g.setColor(SELECTION_BACKGROUND);
            g.fillRect(0, r.y, getWidth(), r.height);
          }
        }
      }
      super.paintComponent(g);

      // Draw a border
      if (getLeadSelectionPath() != null) {
        Rectangle r = getRowBounds(getRowForPath(getLeadSelectionPath()));
        if (r != null) {
          g.setColor(SELECTION_BACKGROUND.darker());
          g.drawRect(0, r.y, getWidth() - 1, r.height - 1);
        }
      }
    } catch (Exception ex) {
      //            ex.printStackTrace();
    }
  }

  /**
   * Set the background color of the selected line. <br>
   * Default = cornflowerblue
   *
   * @param color Background color
   */
  public void setSelectionBackground(Color color) {
    JEntireRowTree.SELECTION_BACKGROUND = color;
  }

  /**
   * Set the text color of the selected line. <br>
   * Default = white
   *
   * @param color Text color
   */
  public void setSelectionForeColor(Color color) {
    JEntireRowTree.SELECTION_FORECOLOR = color;
  }

  /**
   * Tree Look & Feel class. <br>
   * Select a node by selecting a row in the tree.
   *
   * @author RIKEN
   */
  private class EntireRowTreeUI extends javax.swing.plaf.basic.BasicTreeUI {

    /**
     * Get the border of the node. <br>
     * Change the node border from node width to tree width.
     *
     * @param tree tree
     * @param path Node path
     * @return node border
     */
    @Override
    public Rectangle getPathBounds(JTree tree, TreePath path) {
      if (tree != null && treeState != null) {
        return getPathBounds(path, tree.getInsets(), new Rectangle());
      }
      return null;
    }

    /**
     * Get the border of the node. <br>
     * Change the node border from node width to tree width.
     *
     * @param path Node path
     * @param insets Margins
     * @param bounds Border
     * @return node border
     */
    private Rectangle getPathBounds(TreePath path, Insets insets, Rectangle bounds) {
      bounds = treeState.getBounds(path, bounds);
      if (bounds != null) {
        bounds.x = insets.left;
        bounds.width = tree.getWidth();
        bounds.y += insets.top;
      }
      return bounds;
    }

    /**
     * Paint vertical lines. <br>
     * For Mac, do not paint vertical lines.
     *
     * @param g Graphics
     * @param c Tree component
     * @param x X position
     * @param top TOP position
     * @param bottom BOTTOM position
     */
    @Override
    protected void paintVerticalLine(Graphics g, JComponent c, int x, int top, int bottom) {
      if (!KscopeProperties.isMac()) {
        super.paintVerticalLine(g, c, x, top, bottom);
      }
    }

    /**
     * Paint the horizon. <br>
     * For Mac, do not paint the horizon.
     *
     * @param g Graphics
     * @param c Tree component
     * @param y Y position
     * @param left LEFT position
     * @param right RIGHT position
     */
    @Override
    protected void paintHorizontalLine(Graphics g, JComponent c, int y, int left, int right) {
      if (!KscopeProperties.isMac()) {
        super.paintHorizontalLine(g, c, y, left, right);
      }
    }
  }

  /**
   * Tree node drawing class
   *
   * @author RIKEN
   */
  public class EntireRowCellRenderer extends DefaultTreeCellRenderer {
    /** Serial number */
    private static final long serialVersionUID = 1L;

    /**
     * Get the node component. <br>
     * Draw the background color and text color of the node component
     *
     * @param tree tree
     * @param value Node value
     * @param selected Selected state
     * @param expanded Expanded state
     * @param leaf Leaf state
     * @param row row number
     * @param hasFocus Focus
     * @return node component
     */
    @Override
    public Component getTreeCellRendererComponent(
        JTree tree,
        Object value,
        boolean selected,
        boolean expanded,
        boolean leaf,
        int row,
        boolean hasFocus) {
      Component comp =
          super.getTreeCellRendererComponent(tree, value, selected, expanded, leaf, row, hasFocus);
      if (comp instanceof JComponent) {
        JComponent label = (JComponent) comp;
        // modify setBackground for mac osx by @hira at 2013/05/30
        if (selected) {
          label.setBackground(SELECTION_BACKGROUND);
          label.setForeground(SELECTION_FORECOLOR);
          label.setOpaque(true);
        } else {
          label.setBackground(tree.getBackground());
          label.setForeground(tree.getForeground());
          label.setOpaque(false);
        }
        // label.setBackground(selected?SELECTION_BACKGROUND:tree.getBackground());
        // label.setForeground(selected?SELECTION_FORECOLOR:tree.getForeground());
        // label.setOpaque(true);
      }

      return comp;
    }
  }

  /**
   * Set the selected node
   *
   * @param selectnodes Select node user object
   */
  public void addSelectedNodes(Object[] selectnodes) {
    setSelectedNodes(selectnodes, true);
    return;
  }

  /**
   * Set the selected node
   *
   * @param selectnodes Select node user object
   * @param addflag Add selection flag (true = add selection)
   */
  protected void setSelectedNodes(Object[] selectnodes, boolean addflag) {
    if (selectnodes == null) return;

    TreeModel model = this.getModel();
    DefaultMutableTreeNode root = (DefaultMutableTreeNode) model.getRoot();

    List<TreePath> selectpaths = new ArrayList<TreePath>();
    // List tree nodes in the forward direction
    Enumeration<?> depth = root.preorderEnumeration();
    while (depth.hasMoreElements()) {
      DefaultMutableTreeNode treeNode = (DefaultMutableTreeNode) depth.nextElement();

      // Do a node search
      if (treeNode == null || treeNode.getUserObject() == null) {
        continue;
      }
      for (int i = 0; i < selectnodes.length; i++) {
        if (selectnodes[i] == null) continue;
        if (treeNode.getUserObject().equals(selectnodes[i])) {
          TreePath path = new TreePath(treeNode.getPath());
          // If the node is hidden, add the parent node
          if (treeNode instanceof FilterTreeNode) {
            if (!((FilterTreeNode) treeNode).isPassed()) {
              path = path.getParentPath();
            }
          }
          selectpaths.add(path);
          break;
        }
      }
      if (selectpaths.size() >= selectnodes.length) {
        break;
      }
    }
    if (selectpaths.size() > 0) {
      if (addflag) {
        this.addSelectionPaths(selectpaths.toArray(new TreePath[0]));
      } else {
        this.setSelectionPath(selectpaths.get(0));
        this.scrollPathToVisibleForVertical(selectpaths.get(0));
        this.setSelectionPaths(selectpaths.toArray(new TreePath[0]));
      }
    }

    return;
  }

  /**
   * Select a node range
   *
   * @param startnode Selection start node
   * @param endnode Selection end node
   */
  public void setSelectedNodeArea(Object startnode, Object endnode) {
    setSelectedNodeArea(startnode, endnode, false);
    return;
  }

  /**
   * Select a node range
   *
   * @param startnode Selection start node
   * @param endnode Selection end node
   */
  public void addSelectedNodeArea(Object startnode, Object endnode) {
    setSelectedNodeArea(startnode, endnode, true);
    return;
  }

  /**
   * Select a node range
   *
   * @param startnode Selection start node
   * @param endnode Selection end node
   * @param addflag Add selection flag (true = add selection)
   */
  private void setSelectedNodeArea(Object startnode, Object endnode, boolean addflag) {
    if (startnode == null && endnode == null) return;

    // First, select the selection start node and the selection end node separately.
    if (addflag) {
      addSelectedNodes(new Object[] {startnode, endnode});
    } else {
      setSelectedNodes(new Object[] {startnode, endnode}, false);
    }
    if (startnode == null || endnode == null) return;

    TreeModel model = this.getModel();
    DefaultMutableTreeNode root = (DefaultMutableTreeNode) model.getRoot();

    List<TreePath> selectpaths = null;
    // List tree nodes in the forward direction
    Enumeration<?> depth = root.preorderEnumeration();
    while (depth.hasMoreElements()) {
      DefaultMutableTreeNode treeNode = (DefaultMutableTreeNode) depth.nextElement();

      // Do a node search
      if (treeNode == null || treeNode.getUserObject() == null) {
        continue;
      }
      if (selectpaths == null) {
        if (treeNode.getUserObject().equals(startnode)) {
          TreePath path = new TreePath(treeNode.getPath());
          selectpaths = new ArrayList<TreePath>();
          selectpaths.add(path);
          if (startnode == endnode) {
            break;
          }
        }
      } else {
        TreePath path = new TreePath(treeNode.getPath());
        if (this.isVisible(path)) {
          selectpaths.add(path);
        }
        if (treeNode.getUserObject().equals(endnode)) {
          break;
        }
      }
    }
    if (selectpaths != null && selectpaths.size() > 0) {
      if (addflag) {
        this.addSelectionPaths(selectpaths.toArray(new TreePath[0]));
      } else {
        this.setSelectionPath(selectpaths.get(0));
        this.scrollPathToVisibleForVertical(selectpaths.get(0));
        this.setSelectionPaths(selectpaths.toArray(new TreePath[0]));
      }
    }

    return;
  }

  /**
   * Scrolls to see the nodes specified by the path. <br>
   * Do not scroll horizontally, only scroll vertically.
   *
   * @param path Display path
   */
  public void scrollPathToVisibleForVertical(TreePath path) {

    Container cont = this.getParent();
    JViewport view = null;
    Point orgin = null;
    if (cont instanceof JViewport) {
      view = (JViewport) cont;
      // View position before scrolling
      orgin = view.getViewPosition();
    }

    // Scroll the selected path to the display position
    this.scrollPathToVisible(path);

    // Do not scroll horizontally
    if (view != null && orgin != null) {
      Point dest = view.getViewPosition();
      // Restore the horizontal scroll position.
      dest.x = orgin.x;
      view.setViewPosition(dest);
    }
  }

  /**
   * Get the tree path. <br>
   * Search as the same path if UserObject is the same even if the node is another object node
   *
   * @param path Tree path
   * @return Real tree path
   */
  public TreePath getRealTreePath(TreePath path) {
    if (path == null) return null;
    DefaultMutableTreeNode root = (DefaultMutableTreeNode) this.getModel().getRoot();
    Object[] objs = path.getPath();
    if (objs == null || objs.length <= 0) return null;
    DefaultMutableTreeNode[] pathNodes = new DefaultMutableTreeNode[objs.length];
    for (int i = 0; i < objs.length; i++) {
      pathNodes[i] = (DefaultMutableTreeNode) objs[i];
    }

    TreePath searchPath = searchTreePath(root, pathNodes);

    return searchPath;
  }

  /**
   * Search the tree path. <br>
   * Search as the same path if UserObject is the same even if the node is another object node
   *
   * @param parent Current node
   * @param pathNodes Search tree node list
   * @return Tree path
   */
  private TreePath searchTreePath(
      DefaultMutableTreeNode parent, DefaultMutableTreeNode[] pathNodes) {

    TreePath parentPath = getNodePath(parent, pathNodes);
    if (parentPath == null) return null;
    if (parentPath.getPathCount() == pathNodes.length) return parentPath;

    for (int i = 0; i < parent.getChildCount(); i++) {
      DefaultMutableTreeNode child = (DefaultMutableTreeNode) parent.getChildAt(i);
      Object[] objs = child.getPath();
      if (objs == null || objs.length <= 0) return null;
      DefaultMutableTreeNode[] nodes = new DefaultMutableTreeNode[objs.length];
      for (int j = 0; j < objs.length; j++) {
        nodes[j] = (DefaultMutableTreeNode) objs[j];
      }

      TreePath childPath = searchTreePath(child, pathNodes);
      if (childPath != null) {
        if (childPath.getPathCount() == pathNodes.length) return childPath;
      }
    }

    return null;
  }

  /**
   * Search the tree path. <br>
   * Search as the same path if UserObject is the same even if the node is another object node
   *
   * @param node Current node
   * @param pathNodes Search tree node list
   * @return Tree path
   */
  private TreePath getNodePath(DefaultMutableTreeNode node, DefaultMutableTreeNode[] pathNodes) {

    Object[] objs = node.getPath();
    if (objs == null || objs.length <= 0) return null;
    DefaultMutableTreeNode[] nodes = new DefaultMutableTreeNode[objs.length];
    for (int i = 0; i < objs.length; i++) {
      nodes[i] = (DefaultMutableTreeNode) objs[i];
    }
    if (nodes.length > pathNodes.length) return null;

    for (int i = 0; i < nodes.length; i++) {
      if (nodes[i].getUserObject() != pathNodes[i].getUserObject()) {
        return null;
      }
    }

    return SwingUtils.getTreePath(node);
  }

  /** Save the current node state */
  public void storeTreeNode() {

    // Get the currently selected node
    TreePath[] selectedPath = this.getSelectionPaths();
    this.currentSelected = new ArrayList<TreePath>();
    if (selectedPath != null) {
      this.currentSelected.addAll(Arrays.asList(selectedPath));
    }
    // Get the current deployment node
    this.currentExpanded = new ArrayList<TreePath>();
    TreeModel model = this.getModel();
    if (model == null) return;

    DefaultMutableTreeNode root = (DefaultMutableTreeNode) model.getRoot();

    // List tree nodes in the forward direction
    Enumeration<?> depth = root.preorderEnumeration();
    while (depth.hasMoreElements()) {
      DefaultMutableTreeNode next = (DefaultMutableTreeNode) depth.nextElement();
      TreePath path = new TreePath(next.getPath());
      if (this.isExpanded(path)) {
        Object obj = next.getUserObject();
        this.currentExpanded.add(path);
      }
    }
    return;
  }

  /** Restore node state */
  public void restoreTreeNode() {

    TreeModel model = this.getModel();
    if (model == null) return;

    DefaultMutableTreeNode root = (DefaultMutableTreeNode) model.getRoot();

    // List tree nodes in the forward direction
    Enumeration<?> depth = root.preorderEnumeration();
    while (depth.hasMoreElements()) {
      DefaultMutableTreeNode next = (DefaultMutableTreeNode) depth.nextElement();
      TreePath path = new TreePath(next.getPath());
      Object obj = next.getUserObject();

      if (this.currentSelected != null) {
        for (TreePath selected : currentSelected) {
          if (path.equals(selected)) {
            this.setSelectionPath(path);
            this.scrollPathToVisible(path);
          }
        }
      }

      if (this.currentExpanded != null) {
        for (TreePath expanded : currentExpanded) {
          if (path.equals(expanded)) {
            this.expandPath(path);
          }
        }
      }
    }
    return;
  }

  /**
   * Get the last tree node.
   *
   * @return Last tree node
   */
  public DefaultMutableTreeNode getLastTreeNode() {

    TreeModel model = this.getModel();
    DefaultMutableTreeNode root = (DefaultMutableTreeNode) model.getRoot();
    DefaultMutableTreeNode lastnode = null;

    // List tree nodes in the forward direction
    Enumeration<?> depth = root.preorderEnumeration();
    while (depth.hasMoreElements()) {
      lastnode = (DefaultMutableTreeNode) depth.nextElement();
    }
    return lastnode;
  }

  /**
   * Check if the tree path is displayed. <br>
   * Hide if not displayed in the filter node
   *
   * @param path Tree path
   * @return true = display
   */
  @Override
  public boolean isVisible(TreePath path) {
    if (path == null) return false;
    Object obj = path.getLastPathComponent();
    if (obj == null) return false;
    if (obj instanceof FilterTreeNode) {
      FilterTreeNode node = (FilterTreeNode) obj;
      if (!node.isPassed()) {
        return false;
      }
    }
    return super.isVisible(path);
  }

  /** Event called when opening the tree */
  @Override
  public void treeWillExpand(TreeExpansionEvent event) throws ExpandVetoException {
    TreePath path = event.getPath();
    Object obj =
        path.getLastPathComponent(); // The end node (which we are trying to open this time)
    if (obj instanceof FilterTreeNode) {
      int depth = ((FilterTreeNode) obj).getDepth();
      if (depth > 0) {
        // ((FilterTreeNode)obj).removeAllChildren();
        // System.out.println("expand=" + obj + ", depth=" + depth);
        LanguageService service = new LanguageService();
        int writedepth = 0;
        if (((FilterTreeNode) obj).getUserObject() instanceof ProcedureUsage) {
          ProcedureUsage call = (ProcedureUsage) ((FilterTreeNode) obj).getUserObject();
          service.writeProcedureUsage(call, ((FilterTreeNode) obj), true, writedepth);
        } else if (((FilterTreeNode) obj).getUserObject() instanceof Selection) {
          Selection select = (Selection) ((FilterTreeNode) obj).getUserObject();
          service.writeSelection(select, ((FilterTreeNode) obj), true, writedepth);
        } else if (((FilterTreeNode) obj).getUserObject() instanceof Block) {
          Block block = (Block) ((FilterTreeNode) obj).getUserObject();
          service.writeBlocks(block, ((FilterTreeNode) obj), true, writedepth);
        } else if (((FilterTreeNode) obj).getUserObject() instanceof Procedure) {
          Procedure proc = (Procedure) ((FilterTreeNode) obj).getUserObject();
          service.writeProcedure(proc, ((FilterTreeNode) obj), true, writedepth);
        }
        ((FilterTreeNode) obj).setDepth(writedepth);
      }
    }
  }

  /** Event called when closing the tree */
  @Override
  public void treeWillCollapse(TreeExpansionEvent event) throws ExpandVetoException {}

  /**
   * Expand the selection node. The structure tree has unexpanded nodes, so expand it in advance.
   *
   * @param selectnode Select node user object
   * @param parent Expand parent node
   * @return Deployment node
   */
  public DefaultMutableTreeNode expandObjectPath(Object selectnode, DefaultMutableTreeNode parent) {
    if (selectnode == null) return null;
    if (parent == null) return null;

    if (parent.getUserObject().equals(selectnode)) {
      return parent;
    }
    DefaultMutableTreeNode parentnode = parent;
    if (parent.getUserObject() instanceof Selection) {
      parentnode = (DefaultMutableTreeNode) parent.getParent();
    }
    TreePath parentpath = new TreePath(parentnode.getPath());
    // this.expandPath(parentpath);

    // List tree nodes in the forward direction
    Enumeration<?> depth = parentnode.preorderEnumeration();
    while (depth.hasMoreElements()) {
      DefaultMutableTreeNode treeNode = (DefaultMutableTreeNode) depth.nextElement();

      // Do a node search
      if (treeNode == null || treeNode.getUserObject() == null) {
        continue;
      }
      if (treeNode.getUserObject().equals(selectnode)) {
        this.expandPath(parentpath);
        TreePath path = new TreePath(treeNode.getPath());
        this.expandPath(path);
        return treeNode;
      }
    }

    // Close because the node did not exist
    // this.collapsePath(parentpath);

    return null;
  }
}
