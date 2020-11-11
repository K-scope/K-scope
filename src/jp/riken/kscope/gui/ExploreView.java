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

import java.awt.Component;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import javax.swing.JPanel;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import jp.riken.kscope.Message;
import jp.riken.kscope.action.ExploreTreeChangeAction;
import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.common.FILTER_TYPE;
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.menu.ITreePopupMenu;
import jp.riken.kscope.menu.LanguageTreePopupMenu;
import jp.riken.kscope.menu.ModuleTreePopupMenu;
import jp.riken.kscope.model.LanguageTreeModel;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Explorer view class. <br>
 * Place structures, modules, sources and XML tabs.
 *
 * @author RIKEN
 */
public class ExploreView extends ClosableTabbedPane
    implements ITabComponent, PropertyChangeListener {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Module Explorer Panel */
  private ModuleTreePanel moduleTreePanel;
  /** Source Explorer Panel */
  private FileTreePanel sourceTreePanel;
  /** XML Explorer Panel */
  private FileTreePanel xmlTreePanel;
  /** Structure tree pop-up menu */
  private LanguageTreePopupMenu menuLanguagePopup;
  /** Tree change listener */
  private ExploreTreeChangeAction actionTreeChange;

  /** Constructor */
  public ExploreView() {
    super(FRAME_VIEW.EXPLORE_VIEW);
    initGUI();
  }

  /**
   * Initialize. <br>
   * Place tabs for structures, modules, sources and XML.
   */
  private void initGUI() {
    try {
      // Structure Tree Panel
      LanguageTreePanel languageTreePanel = new LanguageTreePanel(EXPLORE_PANEL.LANGUAGE);
      this.addTab(EXPLORE_PANEL.LANGUAGE.getTabName(), languageTreePanel);

      // Module Tree Panel
      moduleTreePanel = new ModuleTreePanel(EXPLORE_PANEL.MODULE);
      this.addTab(
          Message.getString("mainmenu.window.explore.module"), // module
          moduleTreePanel);

      // Source File Panel
      sourceTreePanel = new FileTreePanel(EXPLORE_PANEL.SOURCE);
      sourceTreePanel.initTreeTitle(
          Message.getString("exploreview.treename.source")); // source file
      this.addTab(
          Message.getString("mainmenu.window.explore.source"), // Source
          sourceTreePanel);

      // XML File Panel
      xmlTreePanel = new FileTreePanel(EXPLORE_PANEL.XML);
      xmlTreePanel.initTreeTitle(Message.getString("exploreview.treename.xml")); // XML file
      this.addTab(
          Message.getString("mainmenu.window.explore.xml"), // XML
          xmlTreePanel);

      // Set the initial display to the structure tree tab.
      this.setSelectedIndex(0);

    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /** Close tab */
  @Override
  public void closeTabComponent() {
    int index = this.getSelectedIndex();
    this.remove(index);
  }

  /**
   * Check if the XML Explorer panel is active
   *
   * @return true = XML Explorer panel active
   */
  public boolean isActiveXmlTreePanel() {
    if (this.getSelectedComponent() == this.xmlTreePanel) {
      return true;
    }
    return false;
  }

  /** Stores the entire tree on the selection tab. */
  public void collapseTreeAll() {
    ITreeComponent tree = (ITreeComponent) this.getSelectedComponent();
    if (tree == null) return;
    tree.collapseTreeAll();
  }

  /** Expand the entire tree on the Selection tab. */
  public void expandTreeAll() {
    ITreeComponent tree = (ITreeComponent) this.getSelectedComponent();
    if (tree == null) return;
    tree.expandTreeAll();
  }

  /** Expand the nodes under the selection tree on the selection tab. */
  public void expandTreeSelect() {
    ITreeComponent tree = (ITreeComponent) this.getSelectedComponent();
    if (tree == null) return;
    tree.expandTreeSelect();
  }

  /**
   * Get the selected file
   *
   * @return selection file
   */
  public SourceFile[] getSelectedSourceFiles() {
    ITreeComponent tree = (ITreeComponent) this.getSelectedComponent();
    if (tree == null) return null;
    return tree.getSelectedSourceFiles();
  }

  /**
   * Get the folder / file of the selected node
   *
   * @return Selected folder / file
   */
  public File[] getSelectedNodeFiles() {
    ITreeComponent tree = (ITreeComponent) this.getSelectedComponent();
    if (tree == null) return null;
    return tree.getSelectedNodeFiles();
  }

  /**
   * Get selected source code line information
   *
   * @return Selected source code line information
   */
  public CodeLine[] getSelectedCodeLines() {
    ITreeComponent tree = (ITreeComponent) this.getSelectedComponent();
    if (tree == null) return null;
    return tree.getSelectedCodeLines();
  }

  /**
   * Get the selected block
   *
   * @return selection block
   */
  public IBlock[] getSelectedBlocks() {
    ITreeComponent tree = (ITreeComponent) this.getSelectedComponent();
    if (tree == null) return null;
    return tree.getSelectedBlocks();
  }

  /**
   * Get the currently selected node.
   *
   * @return Selected node
   */
  public DefaultMutableTreeNode getSelectedNode() {
    ITreeComponent tree = (ITreeComponent) this.getSelectedComponent();
    if (tree == null) return null;
    return tree.getSelectedNode();
  }

  /**
   * Get the currently selected node list.
   *
   * @return Selected node list
   */
  public DefaultMutableTreeNode[] getSelectedNodes() {
    ITreeComponent tree = (ITreeComponent) this.getSelectedComponent();
    if (tree == null) return null;
    return tree.getSelectedNodes();
  }

  /**
   * Close tab
   *
   * @param index Close tab index
   */
  @Override
  protected void closeTab(int index) {

    if (index < 0) return;

    // Close tab
    Component tab = this.getComponentAt(index);
    tab.setVisible(false);
    if (tab instanceof ITabComponent) {
      ((ITabComponent) tab).closeTabComponent();
    } else {
      this.remove(index);
    }
  }

  /**
   * Get the Structure Explorer panel
   *
   * @return Structure Explorer Panel
   */
  public LanguageTreePanel getPanelLanguageTree() {

    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      Component comp = this.getComponentAt(i);
      if (comp instanceof ITreeComponent) {
        EXPLORE_PANEL type = ((ITreeComponent) comp).getEnumPanel();
        if (type == EXPLORE_PANEL.LANGUAGE) {
          LanguageTreePanel panel = (LanguageTreePanel) comp;
          return panel;
        }
      }
    }
    return null;
  }

  /**
   * Get the Module Explorer panel
   *
   * @return Module Explorer Panel
   */
  public ModuleTreePanel getPanelModuleTree() {
    return this.moduleTreePanel;
  }

  /**
   * Get the Source Explorer panel
   *
   * @return Source Explorer Panel
   */
  public FileTreePanel getPanelSourceTree() {
    return this.sourceTreePanel;
  }

  /**
   * Get the XML Explorer panel
   *
   * @return XML Explorer Panel
   */
  public FileTreePanel getPanelXmlTree() {
    return this.xmlTreePanel;
  }

  /**
   * Set the source tree pop-up menu
   *
   * @param menuPopup Sourcetree pop-up menu
   */
  public void setSourcePopupMenu(ITreePopupMenu menuPopup) {
    sourceTreePanel.setPopupMenu(menuPopup);
  }

  /**
   * Set the pop-up menu of the XML file tree
   *
   * @param menuPopup XML file tree pop-up menu
   */
  public void setXmlPopupMenu(ITreePopupMenu menuPopup) {
    xmlTreePanel.setPopupMenu(menuPopup);
  }

  /**
   * Set the structure tree pop-up menu
   *
   * @param menuPopup Structure tree pop-up menu
   */
  public void setLanguagePopupMenu(LanguageTreePopupMenu menuPopup) {

    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      Component comp = this.getComponentAt(i);
      if (comp instanceof ITreeComponent) {
        EXPLORE_PANEL type = ((ITreeComponent) comp).getEnumPanel();
        if (type == EXPLORE_PANEL.LANGUAGE) {
          LanguageTreePanel panel = (LanguageTreePanel) comp;
          panel.setPopupMenu(menuPopup);
        }
      }
    }
    this.menuLanguagePopup = menuPopup;
  }

  /**
   * Get the currently selected tree panel.
   *
   * @return selection tree panel
   */
  public ITreeComponent getSelectedPanel() {
    int index = this.getSelectedIndex();
    if (index < 0) return null;
    Component comp = this.getComponentAt(index);
    if (comp instanceof ITreeComponent) {
      return (ITreeComponent) comp;
    }
    return null;
  }

  /**
   * Set the module tree pop-up menu
   *
   * @param menuPopup Module tree pop-up menu
   */
  public void setModulePopupMenu(ModuleTreePopupMenu menuPopup) {
    this.moduleTreePanel.setPopupMenu(menuPopup);
  }

  /**
   * Change tree Register the listener.
   *
   * @param action Tree change listener
   */
  public void addTreeSelectionListener(ExploreTreeChangeAction action) {

    // Change tree Listener registration
    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      Component comp = this.getComponentAt(i);
      if (comp instanceof ITreeComponent) {
        ((ITreeComponent) comp).addTreeSelectionListener(action);
      }
    }

    // Change tab Listener registration
    this.addChangeListener(action);

    this.actionTreeChange = action;
  }

  /**
   * Get the selection tree panel identifier
   *
   * @return Selection tree panel identifier
   */
  public EXPLORE_PANEL getSelectedEnumPanel() {
    ITreeComponent tree = getSelectedPanel();
    if (tree == null) return EXPLORE_PANEL.UNKNOWN;

    return tree.getEnumPanel();
  }

  /**
   * Set the selected node
   *
   * @param node Selected node
   */
  public void setSelectedNode(Object node) {

    // Setting the selected node
    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      Component comp = this.getComponentAt(i);
      if (comp instanceof ITreeComponent) {
        ((ITreeComponent) comp).setSelectedNode(node);
      }
    }
  }

  /**
   * Set the selected node
   *
   * @param nodes Selected nodes
   */
  public void setSelectedNodes(Object[] nodes) {

    // Setting the selected node
    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      Component comp = this.getComponentAt(i);
      if (comp instanceof ITreeComponent) {
        ((ITreeComponent) comp).setSelectedNodes(nodes);
      }
    }
  }

  /**
   * Select a node range
   *
   * @param startnode Selection start node
   * @param endnode Selection end node
   */
  public void setSelectedNodeArea(Object startnode, Object endnode) {

    // Setting the selected node
    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      Component comp = this.getComponentAt(i);
      if (comp instanceof ITreeComponent) {
        ((ITreeComponent) comp).setSelectedNodeArea(startnode, endnode);
      }
    }
  }

  /**
   * Add node selection
   *
   * @param startnode Selection start node
   * @param endnode Selection end node
   */
  public void addSelectedNodeArea(Object startnode, Object endnode) {

    // Setting the selected node
    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      Component comp = this.getComponentAt(i);
      if (comp instanceof ITreeComponent) {
        ((ITreeComponent) comp).addSelectedNodeArea(startnode, endnode);
      }
    }
  }

  /**
   * Activate the specified explorer information panel. <br>
   * Open if closed
   *
   * @param panel Selective analysis information panel identifier
   */
  public void setSelectedPanel(EXPLORE_PANEL panel) {

    Component viewpanel = null;
    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      Component comp = this.getComponentAt(i);
      if (comp instanceof ITreeComponent) {
        if (((ITreeComponent) comp).getEnumPanel() == panel) {
          viewpanel = comp;
          this.setSelectedIndex(i);
          return;
        }
      }
    }

    // The tab is not displayed.
    if (viewpanel == null) {

      /** Structure Explorer Panel */
      if (panel == EXPLORE_PANEL.LANGUAGE) {
        viewpanel = createLanguageTreePanel();
      }
      /** Module Explorer Panel */
      if (moduleTreePanel.getEnumPanel() == panel) {
        viewpanel = moduleTreePanel;
      }
      /** Source Explorer Panel */
      else if (sourceTreePanel.getEnumPanel() == panel) {
        viewpanel = sourceTreePanel;
      }
      /** XML Explorer Panel */
      else if (xmlTreePanel.getEnumPanel() == panel) {
        viewpanel = xmlTreePanel;
      }
    }
    if (viewpanel == null) return;

    // The tab is not displayed, so add it
    this.addTab(panel.getTabName(), viewpanel);
    this.setSelectedIndex(this.getTabCount() - 1);

    return;
  }

  /**
   * Close the specified explorer information panel. <br>
   *
   * @param panel Selective analysis information panel identifier
   */
  public void closePanel(EXPLORE_PANEL panel) {
    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      Component comp = this.getComponentAt(i);
      if (comp instanceof ITreeComponent) {
        if (((ITreeComponent) comp).getEnumPanel() == panel) {
          this.closeTab(i);
          return;
        }
      }
    }
    return;
  }

  /**
   * Get a tree model
   *
   * @return Tree model
   */
  public TreeModel getTreeModel() {
    ITreeComponent tree = getSelectedPanel();
    if (tree == null) return null;

    return tree.getTreeModel();
  }

  /**
   * Select a tree node.
   *
   * @param path Selected tree path
   */
  public void setSelectionPath(TreePath path) {

    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      Component comp = this.getComponentAt(i);
      if (comp instanceof ITreeComponent) {
        ((ITreeComponent) comp).setSelectionPath(path);
      }
    }
  }

  /**
   * Open a new structure tree tab. <br>
   * If it is already displayed, only activate it.
   *
   * @param model Structural model
   */
  public void viewLanguageTree(LanguageTreeModel model) {
    if (model == null) return;

    // Check if it has been displayed on the second node from the root
    DefaultMutableTreeNode root = model.getRootNode();
    if (root == null || root.getChildCount() <= 0) return;
    DefaultMutableTreeNode child = (DefaultMutableTreeNode) root.getChildAt(0);
    TreePath path = SwingUtils.getTreePath(child);

    LanguageTreePanel languageTreePanel = null;
    boolean isblank = false;
    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      Component comp = this.getComponentAt(i);
      if (comp instanceof ITreeComponent) {
        EXPLORE_PANEL type = ((ITreeComponent) comp).getEnumPanel();
        if (type == EXPLORE_PANEL.LANGUAGE) {
          LanguageTreePanel panel = (LanguageTreePanel) comp;
          if (panel.existsTreePath(path)) {
            languageTreePanel = panel;
            break;
          } else if (panel.isBlankTreeModel()) {
            languageTreePanel = panel;
            isblank = true;
            break;
          }
        }
      }
    }

    if (languageTreePanel == null) {
      // Create a new tree because the same structure tree does not exist
      languageTreePanel = createLanguageTreePanel();
      // set the model
      languageTreePanel.setModel(model);
      // change the tab name
      setLanguageTabname(languageTreePanel, model);
    } else if (isblank) {
      // set the model
      languageTreePanel.setModel(model);
    }

    // Activate
    this.setSelectedComponent(languageTreePanel);
  }

  /**
   * Set the structure tab name
   *
   * @param panel Structure tab
   * @param model Structural tree model
   */
  private void setLanguageTabname(LanguageTreePanel panel, LanguageTreeModel model) {
    if (model == null) return;

    // Generate a tab name from the second node from the root
    DefaultMutableTreeNode root = model.getRootNode();
    if (root == null || root.getChildCount() <= 0) return;
    DefaultMutableTreeNode child = (DefaultMutableTreeNode) root.getChildAt(0);
    Object obj = child.getUserObject();
    if (obj == null) return;
    String tabname = null;
    if (obj instanceof Procedure) {
      tabname = ((Procedure) obj).get_name();
    }
    if (tabname == null) return;

    tabname = EXPLORE_PANEL.LANGUAGE.getTabName() + " (" + tabname + ")";
    int count = this.getTabCount();
    int index = 0;
    for (int i = 0; i < count; i++) {
      Component comp = this.getComponentAt(i);
      if (comp == panel) {
        index = i;
        break;
      }
    }

    // Set the tab name
    setTabTitle(index, tabname);
  }

  /**
   * Create a structure tree panel
   *
   * @return Structure Tree Panel
   */
  public LanguageTreePanel createLanguageTreePanel() {
    LanguageTreePanel languageTreePanel = new LanguageTreePanel(EXPLORE_PANEL.LANGUAGE);
    this.addTab(EXPLORE_PANEL.LANGUAGE.getTabName(), languageTreePanel);

    // Structure tree pop-up menu
    if (this.menuLanguagePopup != null) {
      languageTreePanel.setPopupMenu(this.menuLanguagePopup);
    }
    // Tree change listener
    if (this.actionTreeChange != null) {
      languageTreePanel.addTreeSelectionListener(this.actionTreeChange);
    }

    return languageTreePanel;
  }

  /**
   * Clear tree models other than XML tree. <br>
   * Clear the related information that accompanies the clearing of the Fortran database.
   */
  public void clearTreeModel() {

    int count = this.getTabCount();
    // Get the first structure tab index
    int languageIndex = -1;
    for (int i = 0; i < count; i++) {
      Component comp = this.getComponentAt(i);
      if (comp instanceof ITreeComponent) {
        EXPLORE_PANEL type = ((ITreeComponent) comp).getEnumPanel();
        if (type == EXPLORE_PANEL.LANGUAGE) {
          languageIndex = i;
          break;
        }
      }
    }

    for (int i = count - 1; i >= 0; i--) {
      Component comp = this.getComponentAt(i);
      if (comp instanceof ITreeComponent) {
        EXPLORE_PANEL type = ((ITreeComponent) comp).getEnumPanel();
        if (type == EXPLORE_PANEL.LANGUAGE) {
          LanguageTreePanel panel = (LanguageTreePanel) comp;
          panel.getModel().clearTreeModel();
          if (languageIndex != i) {
            // Delete all but the first structure tab
            this.remove(i);
          }
        } else if (type == EXPLORE_PANEL.MODULE) {
          ModuleTreePanel panel = (ModuleTreePanel) comp;
          panel.getModel().clearTreeModel();
        } else if (type == EXPLORE_PANEL.SOURCE) {
          FileTreePanel panel = (FileTreePanel) comp;
          panel.getModel().clearTreeModel();
        }
      }
    }
  }

  /**
   * Get the model on the Structure tab
   *
   * @return Structural model list
   */
  public LanguageTreeModel[] getLanguageModels() {

    List<LanguageTreeModel> list = new ArrayList<LanguageTreeModel>();
    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      Component comp = this.getComponentAt(i);
      if (comp instanceof ITreeComponent) {
        EXPLORE_PANEL type = ((ITreeComponent) comp).getEnumPanel();
        if (type == EXPLORE_PANEL.LANGUAGE) {
          LanguageTreePanel panel = (LanguageTreePanel) comp;
          // Get only the panel displaying the tree
          if (!panel.isBlankTreeModel()) {
            list.add(panel.getModel());
          }
        }
      }
    }
    if (list.size() <= 0) return null;

    return list.toArray(new LanguageTreeModel[0]);
  }

  /**
   * Set a filter in the structure tree
   *
   * @param filters Structural tree filters
   */
  public void setLanguageTreeFilter(FILTER_TYPE[] filters) {

    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      Component comp = this.getComponentAt(i);
      if (comp instanceof ITreeComponent) {
        EXPLORE_PANEL type = ((ITreeComponent) comp).getEnumPanel();
        if (type == EXPLORE_PANEL.LANGUAGE) {
          LanguageTreePanel panel = (LanguageTreePanel) comp;
          panel.setLanguageTreeFilter(filters);
        }
      }
    }
  }

  /**
   * Select a tree node.
   *
   * @param blocks Selected block list
   */
  public void setSelectedBlocks(IBlock[] blocks) {

    // Setting the selected node
    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      Component comp = this.getComponentAt(i);
      if (comp instanceof LanguageTreePanel) {
        ((LanguageTreePanel) comp).setSelectedBlocks(blocks);
      }
    }
  }

  /** Update the drawing of the panel. */
  @Override
  public void updateUI() {
    super.updateUI();

    // Setting the selected node
    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      Component comp = this.getComponentAt(i);
      if (comp instanceof JPanel) {
        ((JPanel) comp).updateUI();
      }
    }
  }

  /**
   * Add a selection node
   *
   * @param nodes Selected nodes
   */
  public void addSelectedNodes(Object[] nodes) {

    // Setting the selected node
    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      Component comp = this.getComponentAt(i);
      if (comp instanceof ITreeComponent) {
        ((ITreeComponent) comp).addSelectedNodes(nodes);
      }
    }
  }

  /**
   * Property change event
   *
   * @param event Event information
   */
  @Override
  public void propertyChange(PropertyChangeEvent event) {

    // Change source view properties such as source display font, font color, etc.
    if (event.getNewValue() instanceof SourceProperties) {
      SourceProperties properties = (SourceProperties) event.getNewValue();
      int count = this.getTabCount();
      for (int i = 0; i < count; i++) {
        Component comp = this.getComponentAt(i);
        if (comp instanceof ITreeComponent) {
          EXPLORE_PANEL type = ((ITreeComponent) comp).getEnumPanel();
          if (type == EXPLORE_PANEL.LANGUAGE) {
            LanguageTreePanel panel = (LanguageTreePanel) comp;
            panel.setSourceProperties(properties);
          } else if (type == EXPLORE_PANEL.MODULE) {
            ModuleTreePanel panel = (ModuleTreePanel) comp;
            panel.setSourceProperties(properties);
          } else if (type == EXPLORE_PANEL.SOURCE || type == EXPLORE_PANEL.XML) {
            FileTreePanel panel = (FileTreePanel) comp;
            panel.setSourceProperties(properties);
          }
        }
      }
    }
  }

  /** Raise a change event for the selected node */
  public void fireSelectNodeChanged() {

    // Setting the selected node
    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      Component comp = this.getComponentAt(i);
      if (comp instanceof ITreeComponent) {
        ((ITreeComponent) comp).fireSelectNodeChanged();
      }
    }
    // redraw
    updateUI();
  }
}
