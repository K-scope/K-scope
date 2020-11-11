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
import java.awt.Dimension;
import java.io.File;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.FILE_TYPE;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.information.InformationBlock;
import jp.riken.kscope.information.InformationBlocks;
import jp.riken.kscope.language.ArrayExpression;
import jp.riken.kscope.language.Condition;
import jp.riken.kscope.language.ExecutableBody;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProcedureUsage;
import jp.riken.kscope.language.Program;
import jp.riken.kscope.language.Repetition;
import jp.riken.kscope.language.Return;
import jp.riken.kscope.language.Selection;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.utils.ResourceUtils;

/**
 * Object tree class. <br>
 * Display the icon according to the node object.
 *
 * @author RIKEN
 */
public class ObjectTree extends JEntireRowTree {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Additional information node text color */
  private Color fontColorInformation;
  /** Database */
  private Program languageDb;
  /** Broken link text color */
  private Color fontColorBrokenLink;
  /** Event block flag: true = prohibited */
  private boolean eventBlocked;
  /** Node Searched Procedure List */
  private Map<Procedure, List<Object>> parentLists;

  /** Constructor */
  public ObjectTree() {
    super();
    this.eventBlocked = false;
    // Setting the node drawing class of the tree
    ObjectTreeCellRenderer renderer = new ObjectTreeCellRenderer();
    this.setCellRenderer(renderer);
  }

  /**
   * Object tree node rendering class
   *
   * @author RIKEN
   */
  public class ObjectTreeCellRenderer extends EntireRowCellRenderer {

    /** Serial number */
    private static final long serialVersionUID = 1L;

    /** Folder icon: Close */
    private Icon iconFolder;
    /** Folder icon: Expand */
    private Icon iconExpanded;
    /** XML file icon */
    private Icon iconXml;
    /** Fortran file icon */
    private Icon iconFortran;
    /** call block icon */
    private Icon callIcon;
    /** call null block icon */
    private Icon callnullIcon;
    /** mpi block icon */
    private Icon mpiIcon;
    /** if block icon */
    private Icon ifIcon;
    /** if block icon */
    private Icon doIcon;
    /** do block icon */
    private Icon returnIcon;
    /** Other block icons */
    @SuppressWarnings("unused")
    private Icon otherIcon;

    /** Constructor */
    public ObjectTreeCellRenderer() {

      /** Folder icon: Close */
      iconFolder = ResourceUtils.getIcon("folder.gif");
      /** Folder icon: Expand */
      iconExpanded = ResourceUtils.getIcon("folderexpand.gif");
      /** XML file icon */
      iconXml = ResourceUtils.getIcon("xmldoc.gif");
      /** Fortran file icon */
      iconFortran = ResourceUtils.getIcon("fortran.gif");

      callIcon = ResourceUtils.getIcon("call.gif");
      callnullIcon = ResourceUtils.getIcon("call_null.gif");
      mpiIcon = ResourceUtils.getIcon("mpi.gif");
      ifIcon = ResourceUtils.getIcon("if.gif");
      doIcon = ResourceUtils.getIcon("do.gif");
      returnIcon = ResourceUtils.getIcon("return.gif");
    }

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

      // Local variables
      String str;
      // Cast to display string
      str = "" + value;
      // get the nodes in the tree
      JComponent c =
          (JComponent)
              super.getTreeCellRendererComponent(t, str, selected, expanded, leaf, row, hasFocus);

      // Display icon settings
      Object obj = ((DefaultMutableTreeNode) value).getUserObject();
      if (obj instanceof ProcedureUsage) {
        if (obj.toString().toLowerCase().startsWith("call mpi_")) {
          setIcon(mpiIcon);
        } else if (((ProcedureUsage) obj).getCallDefinition() != null) {
          setIcon(callIcon);
        } else {
          setIcon(callnullIcon);
        }
      } else if (obj instanceof Selection) {
        setIcon(ifIcon);
      } else if (obj instanceof Condition) {
        setIcon(ifIcon);
      } else if (obj instanceof Repetition) {
        setIcon(doIcon);
      } else if (obj instanceof ArrayExpression) {
        setIcon(doIcon);
      } else if (obj instanceof Return) {
        setIcon(returnIcon);
      } else if (obj instanceof SourceFile) {
        SourceFile file = (SourceFile) obj;
        if (FILE_TYPE.isFortranFile(file.getFile())) {
          this.setIcon(iconFortran);
        } else if (FILE_TYPE.isXcodemlFile(file.getFile())) {
          this.setIcon(iconXml);
        }
      } else if (obj instanceof File) {
        File file = (File) ((DefaultMutableTreeNode) value).getUserObject();
        // Folder
        if (expanded) {
          this.setIcon(iconExpanded);
        } else {
          this.setIcon(iconFolder);
        }
        if (row > 0) {
          // Display only the folder name except the root
          setText(file.getName());
        }
      } else if (obj instanceof IBlock) {
        setIcon(null);
        // Process to make the leaf text color gray in the case of header definition that does not
        // have a file locally
        IBlock val = (IBlock) obj;
        CodeLine code = val.getStartCodeLine();
        if (code == null) {
          code = val.getEndCodeLine();
        }
        if (code != null
            && (code.getSourceFile() == null
                || (code.getSourceFile() != null && code.getSourceFile().getFile() == null))) {
          this.setForeground(fontColorBrokenLink);
        }
      } else {
        setIcon(null);
      }

      // Change the font color of the node where the additional information exists
      if (fontColorInformation != null && obj instanceof IInformation) {
        IInformation info = (IInformation) obj;
        if (info.getInformation() != null && !(info.getInformation().getContent().equals(""))) {
          // Change node font color
          drawInformationColor(c);
        }
        // Check if it is included in the additional information of multiple range specification
        if (containsInformationBlocks(info)) {
          // Change node font color
          drawInformationColor(c);
        }
      }
      if (c instanceof JLabel) {
        JLabel label = (JLabel) c;
        int width = 60;
        if (label.getIcon() != null) {
          width += label.getIcon().getIconWidth();
        }

        if (label.getText() != null) {
          width +=
              SwingUtilities.computeStringWidth(
                  label.getFontMetrics(label.getFont()), label.getText());
        }

        Dimension size = new Dimension(width, (int) label.getPreferredSize().getHeight());
        label.setPreferredSize(size);
        label.revalidate();
        label.repaint();
        label.updateUI();
      }
      return c;
    }

    /**
     * Change the text color of the node for which additional information is set
     *
     * @param label Node component
     */
    private void drawInformationColor(JComponent label) {
      if (fontColorInformation != null) {
        label.setForeground(fontColorInformation);
        label.setOpaque(true);
      }
    }

    /**
     * Check if it is included in the additional information of multiple range specification.
     *
     * @param info Check node
     * @return true = Included in additional information with multiple ranges
     */
    private boolean containsInformationBlocks(IInformation info) {
      if (languageDb == null) return false;
      if (info == null) return false;
      InformationBlocks infos = languageDb.getInformationBlocks();
      if (infos == null || infos.size() <= 0) return false;
      for (InformationBlock block : infos) {
        if (block.getInformation() == null
            || block.getInformation().getContent() == null
            || block.getInformation().getContent().isEmpty()) continue;
        if (block.getStartBlock() == info) {
          return true;
        }
      }
      return false;
    }
  }

  /**
   * Set the selected node
   *
   * @param blocks Selected node user object
   */
  public void setSelectedChainNode(Object[] blocks) {
    if (blocks == null || blocks.length <= 0) return;

    TreeModel model = this.getModel();
    DefaultMutableTreeNode root = (DefaultMutableTreeNode) model.getRoot();

    // List tree nodes in the forward direction
    int count = 0;
    int maxMatch = 0;
    DefaultMutableTreeNode matchNode = null;
    Enumeration<?> depth = root.preorderEnumeration();
    while (depth.hasMoreElements()) {
      DefaultMutableTreeNode treeNode = (DefaultMutableTreeNode) depth.nextElement();

      // Do a node search
      if (treeNode == null || treeNode.getUserObject() == null) {
        continue;
      }

      TreeNode[] objs = treeNode.getPath();
      if (objs == null || objs.length <= 0) continue;
      DefaultMutableTreeNode[] paths = new DefaultMutableTreeNode[objs.length];
      for (int i = 0; i < objs.length; i++) {
        paths[i] = (DefaultMutableTreeNode) objs[i];
      }

      int match = matchChainNode(blocks, paths);
      if (maxMatch < match) {
        matchNode = treeNode;
        maxMatch = match;
      }
      count++;
    }

    if (matchNode != null) {
      TreePath path = new TreePath(matchNode.getPath());
      this.setSelectionPath(path);
      this.scrollPathToVisibleForVertical(path);
    }
    return;
  }

  /**
   * Get the number of matching nodes in the block list and path node list
   *
   * @param blocks block list
   * @param paths Path node list
   * @return Number of matches
   */
  private int matchChainNode(Object[] blocks, DefaultMutableTreeNode[] paths) {
    if (blocks == null || blocks.length <= 0) return -1;
    if (paths == null || paths.length <= 0) return -1;
    // if (blocks.length > paths.length) return -1;

    int blockidx = 0;
    int pathidx = 0;
    while (blockidx < blocks.length && pathidx < paths.length) {
      if (blocks[blockidx] == paths[pathidx].getUserObject()) {
        blockidx++;
        pathidx++;
      } else {
        pathidx++;
      }
    }

    return blockidx + 1;
  }

  /**
   * Select the path node
   *
   * @param path path
   */
  @Override
  public void setSelectionPath(TreePath path) {
    if (!(path.getLastPathComponent() instanceof FilterTreeNode)) {
      super.setSelectionPath(path);
      return;
    }
    if (this.isVisible(path)) {
      super.setSelectionPath(path);
      return;
    }

    Object[] objs = path.getPath();
    List<Object> list = new ArrayList<Object>();
    // for (int i=objs.length-1; i>=0; i--) {
    for (int i = 0; i < objs.length; i++) {
      if (!(objs[i] instanceof FilterTreeNode)) continue;
      FilterTreeNode node = (FilterTreeNode) objs[i];
      list.add(node);
      if (node.isPassed()) {
        // TreePath select = SwingUtils.getTreePath(node);
        TreePath select = new TreePath(list.toArray());
        super.setSelectionPath(select);
        // return;
      }
    }
  }

  /**
   * Expand the selection node. The structure tree has unexpanded nodes, so expand it in advance.
   *
   * @param path Selective expansion path
   */
  public void expandSelectionPath(TreePath path) {
    Object[] objs = path.getPath();
    List<FilterTreeNode> list = new ArrayList<FilterTreeNode>();
    for (int i = 0; i < objs.length; i++) {
      if (!(objs[i] instanceof DefaultMutableTreeNode)) continue;
      FilterTreeNode node = new FilterTreeNode(((DefaultMutableTreeNode) objs[i]).getUserObject());
      list.add(node);
      // TreePath select = SwingUtils.getTreePath(node);
      TreePath select = new TreePath(list.toArray());
      super.expandPath(select);
    }
  }

  /**
   * Set source view properties
   *
   * @param properties Source view properties
   */
  public void setSourceProperties(SourceProperties properties) {
    // Tree selection node background color
    if (properties.getBackgoundSelectNodeColor() != null) {
      this.setSelectionBackground(properties.getBackgoundSelectNodeColor());
    }
    // Additional information node font color
    if (properties.getInformationNodeFontColor() != null) {
      this.setInformationNodeFontColor(properties.getInformationNodeFontColor());
    }

    // Broken link text color
    if (properties.getBrokenLinkNodeFontColor() != null) {
      this.setBrokenLinkNodeFontColor(properties.getBrokenLinkNodeFontColor());
    }

    this.repaint();
  }

  /**
   * Additional information node Set font color. <br>
   *
   * @param color Additional information node font color
   */
  public void setInformationNodeFontColor(Color color) {
    fontColorInformation = color;
  }

  /**
   * Expand the selection node. The structure tree has unexpanded nodes, so expand it in advance.
   *
   * @param selectnode Select node user object
   */
  public void expandObjectPath(Object selectnode) {
    if (selectnode == null) return;

    // Get the hierarchy of selected nodes
    clearParentLists();
    List<Object> parents = getLanguagePath(selectnode);
    if (parents == null || parents.size() <= 0) return;

    // Deploy from root
    TreeModel model = this.getModel();
    DefaultMutableTreeNode node = (DefaultMutableTreeNode) model.getRoot();
    for (int i = parents.size() - 1; i >= 0; i--) {
      DefaultMutableTreeNode expandnode = expandObjectPath(parents.get(i), node);
      if (expandnode != null) {
        node = expandnode;
      }
    }
  }

  /**
   * Set the selected node
   *
   * @param selectnode Select node user object
   */
  public void setSelectedNode(Object selectnode) {
    if (selectnode == null) return;

    // Get the hierarchy of selected nodes
    clearParentLists();
    List<Object> parents = getLanguagePath(selectnode);

    TreeModel model = this.getModel();
    DefaultMutableTreeNode root = (DefaultMutableTreeNode) model.getRoot();

    // List tree nodes in the forward direction
    TreePath lastpath = null;
    this.eventBlocked = true;
    Enumeration<?> depth = root.preorderEnumeration();
    NODE_LOOP:
    while (depth.hasMoreElements()) {
      DefaultMutableTreeNode treeNode = (DefaultMutableTreeNode) depth.nextElement();
      // Do a node search
      if (treeNode == null || treeNode.getUserObject() == null) {
        continue;
      }
      if (treeNode.getUserObject().equals(selectnode)) {
        if (treeNode instanceof FilterTreeNode) {
          if (!((FilterTreeNode) treeNode).isPassed()) {
            break;
          }
        }
        TreePath path = new TreePath(treeNode.getPath());
        this.setSelectionPath(path);
        this.scrollPathToVisibleForVertical(path);
        lastpath = path;
        break;
      }
      if (parents != null) {
        for (Object obj : parents) {
          if (treeNode.getUserObject().equals(obj)) {
            if (lastpath != null) {
              Object lastObj =
                  ((DefaultMutableTreeNode) lastpath.getLastPathComponent()).getUserObject();
              if (lastObj == obj) {
                break NODE_LOOP;
              }
            }
            TreePath path = new TreePath(treeNode.getPath());
            this.setSelectionPath(path);
            this.scrollPathToVisibleForVertical(path);
            lastpath = path;
            break;
          }
        }
      }
    }
    this.eventBlocked = false;
    if (lastpath != null) {
      // Raise a tree node change event.
      super.setSelectionPath(null);
      super.setSelectionPath(lastpath);
      super.scrollPathToVisibleForVertical(lastpath);
    }
    return;
  }

  /**
   * Get the database structure hierarchy. A hierarchical list is a list of children to parents.
   *
   * @param block Database configuration block
   * @return Database structure hierarchical list
   */
  private List<Object> getLanguagePath(Object block) {
    if (block == null) return null;

    // Get the hierarchy of selected nodes
    List<Object> parents = new ArrayList<Object>();
    Object child = block;
    while (child != null) {
      if (child instanceof ExecutableBody) {
        parents.add(child);
        child = ((ExecutableBody) child).getParent();
        if (child instanceof Procedure) {
          if (((Procedure) child).isProgram()) {
            parents.add(child);
            break;
          }
        }
      } else if (child instanceof Procedure) {
        if (((Procedure) child).isProgram()) {
          break;
        }
        // Searched Procedure
        List<Object> searched = getParentLists((Procedure) child);
        if (searched != null) {
          parents.add(child);
          parents.addAll(searched);
          child = searched.get(searched.size() - 1);
          continue;
        }
        if (parents.size() > 0) {
          if (parents.get(parents.size() - 1) != child) {
            parents.add(child);
          } else {
            break;
          }
        }
        Set<ProcedureUsage> calls = ((Procedure) child).getCallMember();
        if (calls != null && calls.size() > 0) {
          ProcedureUsage[] array = calls.toArray(new ProcedureUsage[0]);
          List<Object> listMax = null;
          List<Object> listProg = null;
          for (ProcedureUsage useage : array) {
            // Check if it is a circular reference.
            if (isRecursiveBlock(parents, useage)) {
              return null;
            }
            List<Object> listPath = getLanguagePath(useage);
            if (listPath == null) continue;
            if (listMax == null) listMax = listPath;
            else if (listMax.size() < listPath.size()) listMax = listPath;
            if (listPath.get(listPath.size() - 1) instanceof Procedure) {
              Object last = listPath.get(listPath.size() - 1);
              if (((Procedure) last).isProgram()) {
                if (listProg == null) listProg = listPath;
                else if (listProg.size() > listPath.size()) listProg = listPath;
              }
            }
          }
          if (listProg != null) {
            Object last = listProg.get(listProg.size() - 1);
            if (last instanceof Procedure) {
              if (((Procedure) last).isProgram()) {
                addParentLists((Procedure) child, listMax);
                child = listProg.get(listProg.size() - 1);
                parents.addAll(listProg);
                return parents;
              }
            }
          }
          if (listMax != null) {
            addParentLists((Procedure) child, listMax);
            child = listMax.get(listMax.size() - 1);
            parents.addAll(listMax);
          } else {
            child = null;
          }
        } else {
          child = null;
        }
      } else if (child instanceof IBlock) {
        parents.add(child);
        child = ((IBlock) child).getMotherBlock();
      } else {
        child = null;
      }
    }
    if (parents.size() <= 0) return null;

    return parents;
  }

  /**
   * Select a node range
   *
   * @param startnode Selection start node
   * @param endnode Selection end node
   */
  @Override
  public void setSelectedNodeArea(Object startnode, Object endnode) {
    expandObjectPath(startnode);
    setSelectedNode(startnode);
    expandObjectPath(endnode);
    setSelectedNode(endnode);

    super.setSelectedNodeArea(startnode, endnode);

    return;
  }

  /**
   * Set up the database.
   *
   * @param language database
   */
  public void setLanguageDb(Program language) {
    this.languageDb = language;
  }

  /**
   * Set the selected node
   *
   * @param selectnodes Select node user object
   */
  public void setSelectedNodes(Object[] selectnodes) {
    if (selectnodes == null) return;
    for (Object obj : selectnodes) {
      expandObjectPath(obj);
      setSelectedNode(obj);
    }
    super.setSelectedNodes(selectnodes, false);
    return;
  }

  /**
   * Set the broken link text color. <br>
   *
   * @param color Broken link text color
   */
  public void setBrokenLinkNodeFontColor(Color color) {
    fontColorBrokenLink = color;
  }

  @Override
  protected void fireValueChanged(TreeSelectionEvent e) {
    if (!this.eventBlocked) {
      super.fireValueChanged(e);
    }
  }

  /**
   * Check if it is a circular reference.
   *
   * @param list Block list
   * @param block Call block
   * @return true = circular reference
   */
  private boolean isRecursiveBlock(List<Object> list, Object block) {
    if (list == null) return false;
    if (block == null) return false;
    for (Object obj : list) {
      if (obj == block) {
        return true;
      }
    }
    return false;
  }

  /**
   * Add to node-searched Procedure list.
   *
   * @param proc Node Search Procedure
   * @param objs Search result parent list
   */
  private void addParentLists(Procedure proc, List<Object> objs) {
    if (this.parentLists == null) {
      this.parentLists = new java.util.HashMap<Procedure, List<Object>>();
    }
    this.parentLists.put(proc, objs);
  }

  /** Clear the node-searched Procedure list */
  private void clearParentLists() {
    this.parentLists = null;
    this.parentLists = new java.util.HashMap<Procedure, List<Object>>();
  }

  /**
   * Get the search result parent list from the node-searched Procedure list.
   *
   * @param proc Node Search Procedure
   * @return Search result parent list
   */
  private List<Object> getParentLists(Procedure proc) {
    if (this.parentLists == null) return null;
    return this.parentLists.get(proc);
  }

  /** Raise a change event for the selected node */
  public void fireSelectNodeChanged() {
    TreePath[] paths = this.getSelectionPaths();
    super.setSelectionPaths(null);
    super.setSelectionPaths(paths);
  }
}
