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
import java.awt.event.MouseListener;
import java.io.File;
import java.util.Observable;
import java.util.Observer;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.border.CompoundBorder;
import javax.swing.border.LineBorder;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeSelectionModel;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.component.ObjectTree;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.menu.MainMenu;
import jp.riken.kscope.model.ReferenceModel;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Reference list panel class
 *
 * @author RIKEN
 */
public class ReferencePanel extends AnalisysPanelBase
    implements Observer, IAnalisysComponent, ActionListener {

  /** Serial number */
  private static final long serialVersionUID = 1L;
  /** Reference list tree */
  private ObjectTree treeReference;
  /** Clear button */
  private JButton btnClear;
  /** Export button */
  private JButton btnExport;
  /** Open file */
  private JButton btnOpenFile;
  /** Reference list label */
  private JLabel label;
  /** Expand all button */
  private JButton btnExpand;
  /** All storage buttons */
  private JButton btnCollapse;

  /** Reference list panel model */
  private ReferenceModel model;

  /** Constructor */
  public ReferencePanel() {
    super();

    // Initialize.
    initialize();
  }

  /**
   * Constructor
   *
   * @param panel Analysis information panel identifier
   */
  public ReferencePanel(ANALYSIS_PANEL panel) {
    super(panel);

    // Initialize.
    initialize();
  }

  /** Initialize. */
  private void initialize() {

    // Generate a model
    model = new ReferenceModel();
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
            btnClear.addActionListener(
                new ActionListener() {
                  @Override
                  public void actionPerformed(ActionEvent e) {
                    // Model clear
                    clearModel();
                  }
                });
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
          // Reference list tree
          treeReference = new ObjectTree();
          treeReference.setModel(model.getTreeModel());
          treeReference.setRootVisible(true);
          treeReference.setShowsRootHandles(true);

          // Do not expand nodes by double-clicking.
          treeReference.setToggleClickCount(0);
          // Only one line can be selected
          treeReference
              .getSelectionModel()
              .setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);

          // Scroll pine
          JScrollPane scrollTable = new JScrollPane(treeReference);
          scrollTable.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
          scrollTable.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
          scrollTable.getViewport().setBackground(Color.WHITE);

          add(scrollTable);
        }
      }

      // Tooltip settings
      btnClear.setToolTipText(Message.getString("informationdialog.button.clear.tooltip")); // clear
      btnOpenFile.setToolTipText(
          Message.getString("referencepanel.tooltip.result")); // Open the search result part
      btnExport.setToolTipText(Message.getString("mainmenu.file.export")); // export
      btnExpand.setToolTipText(
          Message.getString("treechooserdialog.tooltip.expandall")); // Expand all
      btnCollapse.setToolTipText(
          Message.getString("treechooserdialog.tooltip.collapseall")); // All stored

      // Add event
      btnExpand.addActionListener(this);
      btnCollapse.addActionListener(this);

    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * Reference list model change notification event
   *
   * @param o Notification source
   * @param arg Notification item
   */
  @Override
  public void update(Observable o, Object arg) {
    // Reference list model
    ReferenceModel observer = (ReferenceModel) o;
    treeReference.setModel(model.getTreeModel());

    // Panel title
    this.label.setText(observer.getTitle());
  }

  /**
   * Get reference list model
   *
   * @return Search result model
   */
  public ReferenceModel getModel() {
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
    if (this.model.isEmpty()) return;

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
    // Open the reference list
    this.btnOpenFile.addActionListener((ActionListener) menu.getActionOpenAnalysisLine());
    this.treeReference.addMouseListener((MouseListener) menu.getActionOpenAnalysisLine());
  }

  /**
   * Get the code information of the selected node. <br>
   *
   * @return code information
   */
  @Override
  public CodeLine getSelectedCodeLine() {
    IBlock block = getSelectedBlock();
    if (block == null) return null;

    return block.getStartCodeLine();
  }

  /**
   * Get a block of selected nodes
   *
   * @return selection block
   */
  @Override
  public IBlock getSelectedBlock() {

    // Get the source file object for the selected file.
    DefaultMutableTreeNode node =
        (DefaultMutableTreeNode) this.treeReference.getLastSelectedPathComponent();
    if (node == null) return null;
    if (node.getUserObject() == null) return null;

    // Is it a block object?
    if (node.getUserObject() instanceof IBlock) {
      return (IBlock) node.getUserObject();
    }

    return null;
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
   * Get additional selection information
   *
   * @return Selectable additional information
   */
  @Override
  public IInformation getSelectedInformation() {

    // Get the source file object for the selected file.
    DefaultMutableTreeNode node =
        (DefaultMutableTreeNode) this.treeReference.getLastSelectedPathComponent();
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
  public void setSourceProperties(SourceProperties properties) {}

  /**
   * Button click event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {
    if (event.getSource() == this.btnExpand) {
      // Expand all
      expandTreeAll();
    } else if (event.getSource() == this.btnCollapse) {
      // store everything
      collapseTreeAll();
    }
  }

  /** Stores the entire tree of selection tabs. */
  public void collapseTreeAll() {
    int row = this.treeReference.getRowCount() - 1;
    while (row >= 0) {
      this.treeReference.collapseRow(row);
      row--;
    }
    // Expand only root node
    this.treeReference.expandRow(0);
  }

  /** Expand the entire tree on the Selection tab. */
  public void expandTreeAll() {
    int row = 0;
    while (row < this.treeReference.getRowCount()) {
      this.treeReference.expandRow(row);
      row++;
    }
  }

  /** Copy the selection to the clipboard. */
  @Override
  public void copyClipboard() {

    // Get the source file object for the selected file.
    DefaultMutableTreeNode node =
        (DefaultMutableTreeNode) this.treeReference.getLastSelectedPathComponent();
    if (node == null) return;
    if (node.getUserObject() == null) return;
    String text = node.getUserObject().toString();

    // copy to clipboard
    SwingUtils.copyClipboard(text);
  }

  /** Whether or not there is export information */
  @Override
  public boolean isExportable() {
    if (this.model == null) return false;
    return (!this.model.isEmpty());
  }
}
