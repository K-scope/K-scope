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
import java.util.Arrays;
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
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;
import jp.riken.kscope.Message;
import jp.riken.kscope.action.ViewOpenAnalysisLineAction;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.KEYWORD_TYPE;
import jp.riken.kscope.common.TRACE_DIR;
import jp.riken.kscope.component.SearchTree;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.Keyword;
import jp.riken.kscope.data.SearchOption;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.menu.MainMenu;
import jp.riken.kscope.model.TraceResultModel;
import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Trace result panel class
 *
 * @author RIKEN
 */
public class TraceResultPanel extends AnalisysPanelBase
    implements Observer, IAnalisysComponent, ActionListener, MouseListener, TreeSelectionListener {

  /** Serial number */
  private static final long serialVersionUID = 1L;
  /** Trace tree */
  private SearchTree treeTrace;
  /** Export button */
  private JButton btnExport;
  /** Open file */
  private JButton btnOpenFile;
  /** Move: Up button */
  private JButton btnMoveUp;
  /** Move: Down button */
  private JButton btnMoveDown;
  /** Move: IN button */
  private JButton btnMoveIn;
  /** Move: Out button */
  private JButton btnMoveOut;
  /** Move: Forward button */
  private JButton btnMoveForward;
  /** Refresh button */
  private JButton btnRefresh;

  /** Trace result label */
  private JLabel label;

  /** Trace result table model */
  private TraceResultModel model;
  /** Action to open the relevant part */
  private ViewOpenAnalysisLineAction actionOpen;

  /** Constructor */
  public TraceResultPanel() {
    super();

    // Initialize.
    initialize();
  }

  /**
   * Constructor
   *
   * @param panel Analysis information panel identifier
   */
  public TraceResultPanel(ANALYSIS_PANEL panel) {
    super(panel);

    // Initialize.
    initialize();
  }

  /** Initialize. */
  private void initialize() {

    // Generate a model
    model = new TraceResultModel();
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
            Icon icon = ResourceUtils.getIcon("arrow_right.gif");
            btnMoveIn = new JButton(icon);
            btnMoveIn.setContentAreaFilled(false);
            btnMoveIn.setBorderPainted(false);
            btnMoveIn.setPreferredSize(buttonSize);
            btnMoveIn.setMinimumSize(buttonSize);
            btnMoveIn.setMaximumSize(buttonSize);
            panelButtons.add(btnMoveIn);
          }
          {
            Icon icon = ResourceUtils.getIcon("arrow_left.gif");
            btnMoveOut = new JButton(icon);
            btnMoveOut.setContentAreaFilled(false);
            btnMoveOut.setBorderPainted(false);
            btnMoveOut.setPreferredSize(buttonSize);
            btnMoveOut.setMinimumSize(buttonSize);
            btnMoveOut.setMaximumSize(buttonSize);
            panelButtons.add(btnMoveOut);
          }
          {
            Icon icon = ResourceUtils.getIcon("forward.gif");
            btnMoveForward = new JButton(icon);
            btnMoveForward.setContentAreaFilled(false);
            btnMoveForward.setBorderPainted(false);
            btnMoveForward.setPreferredSize(buttonSize);
            btnMoveForward.setMinimumSize(buttonSize);
            btnMoveForward.setMaximumSize(buttonSize);
            panelButtons.add(btnMoveForward);
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
          // Trace tree
          treeTrace = new SearchTree();

          DefaultMutableTreeNode rootNode =
              new DefaultMutableTreeNode(
                  Message.getString("mainmenu.window.analysis.trace")); // trace
          DefaultTreeModel treeModel = new DefaultTreeModel(rootNode);
          treeTrace.setModel(treeModel);
          treeTrace.setRootVisible(true);
          treeTrace.setShowsRootHandles(true);

          // Do not expand nodes by double-clicking.
          treeTrace.setToggleClickCount(0);
          // Only one line can be selected
          treeTrace.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);

          // Scroll pine
          JScrollPane scrollTable = new JScrollPane(treeTrace);
          scrollTable.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
          scrollTable.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
          scrollTable.getViewport().setBackground(Color.WHITE);

          add(scrollTable);
        }
      }

      // Tooltip settings
      btnOpenFile.setToolTipText(
          Message.getString("traceresultpanel.tooltip.open")); // Open the trace location
      btnExport.setToolTipText(Message.getString("mainmenu.file.export")); // export
      btnMoveUp.setToolTipText(Message.getString("traceresultpanel.tooltip.up")); // Trace: Up
      btnMoveDown.setToolTipText(Message.getString("traceresultpanel.tooltip.down")); // Trace: Down
      btnMoveIn.setToolTipText(Message.getString("traceresultpanel.tooltip.in")); // Trace: In
      btnMoveOut.setToolTipText(Message.getString("traceresultpanel.tooltip.out")); // Trace: Out
      btnMoveForward.setToolTipText(
          Message.getString("traceresultpanel.tooltip.foward")); // Trace: Forward
      btnRefresh.setToolTipText(
          Message.getString("traceresultpanel.tooltip.update")); // Trace: Update

      // Button event
      btnMoveUp.addActionListener(this);
      btnMoveDown.addActionListener(this);
      btnOpenFile.addActionListener(this);
      treeTrace.addMouseListener(this);
      treeTrace.addTreeSelectionListener(this);

    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * Trace result model change notification event
   *
   * @param o Notification source
   * @param arg Notification item
   */
  @Override
  public void update(Observable o, Object arg) {
    // Update the trace result model.
    updateModel();
  }

  /** Update the trace result model. */
  private void updateModel() {
    // Tree model settings
    this.treeTrace.setModel(this.model.getTreeModel());
    // Set the selected node
    if (this.model.getSelectedBlock() != null) {
      this.treeTrace.setSelectedNode(this.model.getSelectedBlock());
      // Open the relevant part
      openTraceBlock();
    }

    // Set search conditions
    SearchOption searchOption = new SearchOption();
    searchOption.setSearchText(this.model.getTraceWord()); // Trace variable
    searchOption.setVariable(true); // Variable search
    this.treeTrace.setSearchOption(searchOption);

    // Panel title
    this.label.setText(this.model.getTitle());

    // Toggle button enable by selected node
    setEnableButtons();
  }

  /**
   * Get the trace result model
   *
   * @return Trace result model
   */
  public TraceResultModel getModel() {
    return model;
  }

  /**
   * Set the trace result model
   *
   * @param model Trace result model
   */
  public void setModel(TraceResultModel model) {
    if (model == null) return;
    this.model = model;

    // Set the observer.
    this.model.deleteObservers();
    this.model.addObserver(this);

    // Update the trace result model.
    updateModel();
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
    // Trace
    this.btnMoveIn.addActionListener(menu.getActionAnalysisTrace(TRACE_DIR.IN));
    this.btnMoveOut.addActionListener(menu.getActionAnalysisTrace(TRACE_DIR.OUT));
    this.btnMoveForward.addActionListener(menu.getActionAnalysisTrace(TRACE_DIR.FORWARD));
    // Trace: Update
    this.btnRefresh.addActionListener(menu.getActionAnalysisTrace(TRACE_DIR.REFRESH));

    // Open the search result
    actionOpen = (ViewOpenAnalysisLineAction) menu.getActionOpenAnalysisLine();
  }

  /**
   * Get the code information of the selected line. <br>
   * A code information object is set in the first column of the table model.
   *
   * @return code information
   */
  @Override
  public CodeLine getSelectedCodeLine() {

    IBlock block = getSelectedBlock();
    if (block == null) return null;

    return block.getStartCodeLine();
  }

  /** Clear the model. */
  @Override
  public void clearModel() {
    // Model clear
    model.clearTreeModel();
  }

  /** Close the tab */
  @Override
  public void closeTab() {}

  /**
   * Get the selected block
   *
   * @return selection block
   */
  @Override
  public IBlock getSelectedBlock() {

    // Get the source file object for the selected file.
    DefaultMutableTreeNode node =
        (DefaultMutableTreeNode) this.treeTrace.getLastSelectedPathComponent();
    if (node == null) return null;
    if (node.getUserObject() == null) return null;

    // Is it a block object?
    if (node.getUserObject() instanceof IBlock) {
      // Does the start CodeLine exist?
      IBlock block = (IBlock) node.getUserObject();
      CodeLine line = block.getStartCodeLine();
      if (line == null) {
        IBlock parent = block.getMotherBlock();
        if (parent != null) {
          return parent;
        }
      }
      return block;
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
        (DefaultMutableTreeNode) this.treeTrace.getLastSelectedPathComponent();
    if (node == null) return null;
    if (node.getUserObject() == null) return null;

    // Is it an additional information object?
    if (node.getUserObject() instanceof IInformation) {
      return (IInformation) node.getUserObject();
    }

    return null;
  }

  /**
   * Button click event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {

    if (event.getSource() == this.btnMoveUp) {
      // Trace: Up
      traceUp();
    } else if (event.getSource() == this.btnMoveDown) {
      // Trace: Up
      traceDown();
    } else if (event.getSource() == this.btnOpenFile) {
      // Open the relevant part
      openTraceBlock();
    }
  }

  /** Open the relevant part */
  private void openTraceBlock() {
    List<IBlock> list = new ArrayList<IBlock>();
    // Trace path
    IBlock[] paths = this.model.getTracePath();
    if (paths != null && paths.length > 0) {
      list.addAll(Arrays.asList(paths));
    }
    // Add a route selector node
    if (this.treeTrace.getLastSelectedPathComponent() != this.treeTrace.getModel().getRoot()) {
      // Select block
      IBlock block = getSelectedBlock();
      if (block != null) {
        list.add(block);
      }
    }
    // open the selection block
    this.actionOpen.openTraceBlocks(list.toArray(new IBlock[0]));
  }

  /** Trace: Up */
  public void traceUp() {

    DefaultMutableTreeNode currentnode =
        (DefaultMutableTreeNode) this.treeTrace.getLastSelectedPathComponent();

    TreeModel model = this.treeTrace.getModel();
    if (model == null) return;
    DefaultMutableTreeNode root = (DefaultMutableTreeNode) model.getRoot();
    if (root == null) return;
    TreePath selectPath = null;
    TreePath lastPath = null;
    if (currentnode == null) {
      selectPath = new TreePath(root);
    } else {
      // List tree nodes in the forward direction
      Enumeration<?> depth = root.preorderEnumeration();
      while (depth.hasMoreElements()) {
        DefaultMutableTreeNode node = (DefaultMutableTreeNode) depth.nextElement();
        if (currentnode == node) {
          // If the selected node is the root path, selectPath = null.
          selectPath = lastPath;
          break;
        }
        // One node path before
        lastPath = new TreePath(node.getPath());
      }
    }

    // If the selection path does not exist, select the root path
    // If the selected path does not exist, if the selected node is the root path
    if (selectPath == null) {
      selectPath = new TreePath(root);
    }

    // Select the selected path
    this.treeTrace.setSelectionPath(selectPath);
    this.treeTrace.scrollPathToVisible(selectPath);

    // Open the relevant part
    this.btnOpenFile.doClick();
  }

  /** Trace: Go down */
  public void traceDown() {

    DefaultMutableTreeNode currentnode =
        (DefaultMutableTreeNode) this.treeTrace.getLastSelectedPathComponent();

    TreeModel model = this.treeTrace.getModel();
    if (model == null) return;
    DefaultMutableTreeNode root = (DefaultMutableTreeNode) model.getRoot();
    if (root == null) return;
    TreePath currentPath = null;
    TreePath selectPath = null;
    if (currentnode == null) {
      selectPath = new TreePath(root);
    } else {
      // List tree nodes in the forward direction
      Enumeration<?> depth = root.preorderEnumeration();
      while (depth.hasMoreElements()) {
        DefaultMutableTreeNode node = (DefaultMutableTreeNode) depth.nextElement();
        if (currentPath != null) {
          selectPath = new TreePath(node.getPath());
          break;
        }
        if (currentnode == node) {
          // Current node path
          currentPath = new TreePath(node.getPath());
        }
      }
    }

    // If the selected path does not exist, if the selected node is the last path
    if (selectPath != null) {
      this.treeTrace.setSelectionPath(selectPath);
      this.treeTrace.scrollPathToVisible(selectPath);
    }

    // Open the relevant part
    this.btnOpenFile.doClick();
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
    this.treeTrace.setForecolor(properties.getSearchFontColor());
    // Background color
    this.treeTrace.setBackcolor(properties.getSearchBackgroundColor());
    // Style
    this.treeTrace.setFontstyle(KscopeProperties.SEARCHTREE_FONTSTYLE);

    // Redraw tree
    this.treeTrace.invalidate();
    this.treeTrace.validate();
    this.treeTrace.repaint();
  }

  /**
   * Get the keyword list of trace results
   *
   * @return Trace keyword list
   */
  public Keyword[] getTraceKeywords() {

    TreeModel model = this.treeTrace.getModel();
    if (model == null) return null;
    DefaultMutableTreeNode root = (DefaultMutableTreeNode) model.getRoot();

    // Keyword list
    List<Keyword> list = new ArrayList<Keyword>();

    // Trace variable
    String variable = this.model.getTraceWord();

    // List tree nodes in the forward direction
    Enumeration<?> depth = root.preorderEnumeration();
    while (depth.hasMoreElements()) {
      DefaultMutableTreeNode treeNode = (DefaultMutableTreeNode) depth.nextElement();

      // Do a node search
      if (treeNode == null || treeNode.getUserObject() == null) {
        continue;
      }
      if (treeNode.getUserObject() instanceof IBlock) {
        IBlock block = (IBlock) treeNode.getUserObject();
        CodeLine line = block.getStartCodeLine();
        if (line == null) {
          IBlock parent = block.getMotherBlock();
          if (parent != null) {
            line = parent.getStartCodeLine();
          }
        }
        Keyword word = new Keyword(KEYWORD_TYPE.TRACE);
        word.setKeyword(variable);
        word.setSearchWord(true);
        word.setCaseSensitive(false);
        word.setSearchVariable(true);
        word.setSearchLine(line);

        list.add(word);
      }
    }

    if (list.size() <= 0) return null;

    return list.toArray(new Keyword[0]);
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
   * Trace tree selection change event
   *
   * @param event Event information
   */
  @Override
  public void valueChanged(TreeSelectionEvent event) {
    if (event.getSource() == this.treeTrace) {
      DefaultMutableTreeNode currentnode =
          (DefaultMutableTreeNode) this.treeTrace.getLastSelectedPathComponent();
      if (currentnode == null) return;
      if (currentnode.getUserObject() instanceof IBlock) {
        // Set the selection block
        this.model.setSelectedBlock((IBlock) currentnode.getUserObject());

        // Toggle button enable by selected node
        setEnableButtons();
      }
    }
  }

  /** Toggle button enable by selected node. */
  private void setEnableButtons() {
    DefaultMutableTreeNode currentnode =
        (DefaultMutableTreeNode) this.treeTrace.getLastSelectedPathComponent();
    if (currentnode == null) return;
    if (currentnode.getUserObject() instanceof IBlock) {
      // Selected node
      // Route selection
      boolean enabledMoveUp = true;
      boolean enabledMoveDown = true;
      if (currentnode == this.treeTrace.getModel().getRoot()) {
        enabledMoveUp = false;
      }
      if (currentnode == this.treeTrace.getLastTreeNode()) {
        enabledMoveDown = false;
      }

      Icon iconMoveUp = null;
      if (enabledMoveUp) {
        iconMoveUp = ResourceUtils.getIcon("arrow_up.gif");
      } else {
        iconMoveUp = ResourceUtils.getIcon("arrow_up_gray.gif");
      }
      Icon iconMoveDown = null;
      if (enabledMoveDown) {
        iconMoveDown = ResourceUtils.getIcon("arrow_down.gif");
      } else {
        iconMoveDown = ResourceUtils.getIcon("arrow_down_gray.gif");
      }
      btnMoveUp.setIcon(iconMoveUp);
      btnMoveUp.setEnabled(enabledMoveUp);
      btnMoveDown.setIcon(iconMoveDown);
      btnMoveDown.setEnabled(enabledMoveDown);
    }

    // Enable switching of forward button
    boolean enabled = (this.model.getTracePath() != null && this.model.getTracePath().length > 2);
    Icon icon = null;
    if (enabled) {
      icon = ResourceUtils.getIcon("forward.gif");
    } else {
      icon = ResourceUtils.getIcon("forward_gray.gif");
    }
    this.btnMoveForward.setEnabled(enabled);
    this.btnMoveForward.setIcon(icon);
  }

  /** Copy the selection to the clipboard. */
  @Override
  public void copyClipboard() {

    // Get the source file object for the selected file.
    DefaultMutableTreeNode node =
        (DefaultMutableTreeNode) this.treeTrace.getLastSelectedPathComponent();
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
