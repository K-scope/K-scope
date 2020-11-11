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
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.FlowLayout;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.util.Arrays;
import java.util.Observable;
import java.util.Observer;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.RowSorter;
import javax.swing.SortOrder;
import javax.swing.SwingUtilities;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.JTableHeader;
import jp.riken.kscope.Message;
import jp.riken.kscope.action.AnalysisOperandAction;
import jp.riken.kscope.action.AnalysisReferenceAction;
import jp.riken.kscope.action.AnalysisScopeAction;
import jp.riken.kscope.action.EditInformationEditAction;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.component.JStripeTable;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.menu.MainMenu;
import jp.riken.kscope.menu.VariablePopupMenu;
import jp.riken.kscope.model.VariableTableModel;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Variable characteristic list panel class
 *
 * @author RIKEN
 */
public class VariableTablePanel extends AnalisysPanelBase
    implements Observer, IAnalisysComponent, MouseListener, ActionListener {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Clear button */
  private JButton btnClear;
  /** Additional information button */
  private JButton btnEdit;
  /** Calculation count button */
  private JButton btnOperand;
  /** Declaration / definition / reference button */
  private JButton btnReference;
  /** Variable scope button */
  private JButton btnScope;
  /** Open file button */
  private JButton btnOpenFile;
  /** Export button */
  private JButton btnExport;
  /** Variable characteristic list label */
  private JLabel label;
  /** Content Box */
  private Box contentInfo;
  /** Margin box */
  private final Component glue = Box.createVerticalGlue();
  /** Scroll pine */
  private JScrollPane scrollPane;

  /** Variable characteristic list table model */
  private VariableTableModel model;

  /** Expand button icon */
  private Icon expand_icon = ResourceUtils.getIcon("expand_arrow.gif");
  /** Storage button icon */
  private Icon collapse_icon = ResourceUtils.getIcon("collapse_arrow.gif");

  /** Variable characteristics list Context menu */
  private VariablePopupMenu variablePopupMenu;

  /** Select additional information node panel */
  private NodePanel selectedInfo;

  /** Additional information editing action */
  private EditInformationEditAction actionEdit;
  /** Arithmetic count action */
  private AnalysisOperandAction actionOperand;
  /** Declaration / Definition / Reference Action */
  private AnalysisReferenceAction actionReference;
  /** Variable scope action */
  private AnalysisScopeAction actionScope;
  /** Selection panel background color */
  private Color colorSelectedPanel;

  /** Constructor */
  public VariableTablePanel() {
    super();

    // Generate a model
    model = new VariableTableModel();
    // Set the observer.
    model.addObserver(this);

    // Initialize the GUI.
    initGUI();
  }

  /**
   * Constructor
   *
   * @param proparties Analysis Information Panel Identifier
   */
  public VariableTablePanel(ANALYSIS_PANEL proparties) {
    super(proparties);

    // Generate a model
    model = new VariableTableModel();
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
      //            setPreferredSize(new Dimension(400, 24));

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
            btnClear.setPreferredSize(buttonSize);
            btnClear.setMinimumSize(buttonSize);
            btnClear.setMaximumSize(buttonSize);
            btnClear.setContentAreaFilled(false);
            btnClear.setBorderPainted(false);
          }
          // Margin setting
          // panelButtons.add(Box.createHorizontalStrut(5));
          // Additional information edit button
          {
            Icon icon = ResourceUtils.getIcon("edit_info.gif");
            btnEdit = new JButton(icon);
            panelButtons.add(btnEdit);
            btnEdit.setContentAreaFilled(false);
            btnEdit.setBorderPainted(false);
            btnEdit.setPreferredSize(buttonSize);
            btnEdit.setMinimumSize(buttonSize);
            btnEdit.setMaximumSize(buttonSize);
          }
          // Calculation count button
          {
            Icon icon = ResourceUtils.getIcon("count.gif");
            btnOperand = new JButton(icon);
            panelButtons.add(btnOperand);
            btnOperand.setContentAreaFilled(false);
            btnOperand.setBorderPainted(false);
            btnOperand.setPreferredSize(buttonSize);
            btnOperand.setMinimumSize(buttonSize);
            btnOperand.setMaximumSize(buttonSize);
          }
          // Declaration / definition / reference button
          {
            Icon icon = ResourceUtils.getIcon("reference.gif");
            btnReference = new JButton(icon);
            panelButtons.add(btnReference);
            btnReference.setContentAreaFilled(false);
            btnReference.setBorderPainted(false);
            btnReference.setPreferredSize(buttonSize);
            btnReference.setMinimumSize(buttonSize);
            btnReference.setMaximumSize(buttonSize);
          }
          // Variable scope button
          {
            Icon icon = ResourceUtils.getIcon("area.gif");
            btnScope = new JButton(icon);
            panelButtons.add(btnScope);
            btnScope.setContentAreaFilled(false);
            btnScope.setBorderPainted(false);
            btnScope.setPreferredSize(buttonSize);
            btnScope.setMinimumSize(buttonSize);
            btnScope.setMaximumSize(buttonSize);
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
          // label.setText("");
        }
      }
      {
        {
          // Variable characteristic list panel
          contentInfo = Box.createVerticalBox();
          // contentInfo.setBorder(BorderFactory.createLineBorder(Color.RED, 1));
          contentInfo.setOpaque(false);

          // Scroll pine
          scrollPane = new JScrollPane();
          scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
          scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
          scrollPane.setViewportView(contentInfo);
          scrollPane.getViewport().setBackground(Color.WHITE);
          // Adjust scroll amount
          scrollPane.getVerticalScrollBar().setUnitIncrement(Constant.VERTICALSCROLL_INCREMENT);
          add(scrollPane);
        }
      }
      // Add event
      btnClear.addActionListener(this);
      btnEdit.addActionListener(this);
      btnOperand.addActionListener(this);
      btnReference.addActionListener(this);
      btnScope.addActionListener(this);

      // Tooltip settings
      btnClear.setToolTipText(Message.getString("informationdialog.button.clear.tooltip")); // clear
      btnEdit.setToolTipText(
          Message.getString("replacementresulttablepanel.tooltip.info")); // Additional information
      btnOperand.setToolTipText(
          Message.getString("mainmenu.project.config.operation")); // Count the number of operations
      btnReference.setToolTipText(
          Message.getString(
              "mainmenu.analysis.dec-def-ref")); // Declaration / Definition / Reference
      btnScope.setToolTipText(
          Message.getString("mainmenu.analysis.valiablescope")); // Variable valid area
      btnOpenFile.setToolTipText(
          Message.getString("informationpanel.tooltip.openblock")); // open the selection
      btnExport.setToolTipText(Message.getString("mainmenu.file.export")); // export

    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * Variable characteristic list Model change notification event
   *
   * @param o Notification source
   * @param arg Notification item
   */
  @Override
  public void update(Observable o, Object arg) {
    // Clear variable characteristic list
    clearComponent();

    // Table model
    VariableTableModel observer = (VariableTableModel) o;

    // Panel title
    this.label.setText(observer.getTitle());

    int count = observer.getListProcedureInfoCount();
    for (int i = 0; i < count; i++) {
      VariableTableModel.ProcedureInfo info = observer.getProcedureInfo(i);

      // Add variable characteristic list
      addVariableInfo(info);
    }
  }

  /**
   * Add variable characteristic list
   *
   * @param info Variable characteristic list information
   */
  public void addVariableInfo(VariableTableModel.ProcedureInfo info) {

    // Create additional components
    JComponent component = makeRowsPanel(info);

    // Add a component to the content panel
    addComponent(component);
  }

  /** Clear the content panel */
  public void clearComponent() {
    if (this.contentInfo.getComponentCount() > 0) {
      this.contentInfo.removeAll();
    }

    // redraw
    refreshPanel();

    // Clear the selective additional information block
    this.selectedInfo = null;
  }

  /**
   * Add components to the content panel
   *
   * @param component Additional component
   */
  private void addComponent(final JComponent component) {
    // Change the size of additional components
    component.setMaximumSize(new Dimension(Short.MAX_VALUE, component.getPreferredSize().height));

    // Place additional components left-justified
    component.setAlignmentX(Component.LEFT_ALIGNMENT);

    // Add component
    this.contentInfo.remove(glue);
    //        this.contentInfo.add(Box.createVerticalStrut(5));
    this.contentInfo.add(component);
    this.contentInfo.add(glue);

    EventQueue.invokeLater(
        new Runnable() {
          @Override
          public void run() {
            component.scrollRectToVisible(component.getBounds());
            scrollPane.getViewport().setViewPosition(new Point(0, 0));
          }
        });

    // redraw
    refreshPanel();

    return;
  }

  /** Redraw the panel */
  private void refreshPanel() {

    // redraw
    this.contentInfo.revalidate();
    this.validate();
    this.repaint();
  }

  /**
   * Addition of variable characteristic list panel
   *
   * @param info Variable characteristic list information
   * @return Variable characteristics list panel
   */
  private JComponent makeRowsPanel(VariableTableModel.ProcedureInfo info) {
    // Variable characteristic list panel
    NodePanel rows = new NodePanel(info);
    rows.setLayout(new BoxLayout(rows, BoxLayout.Y_AXIS));
    rows.setOpaque(false);
    rows.setBorder(BorderFactory.createLineBorder(Color.GRAY, 1));
    rows.addMouseListener(this);

    // Name panel
    JPanel panelName = new JPanel();
    panelName.setLayout(new FlowLayout(FlowLayout.LEFT, 5, 0));
    panelName.setOpaque(false);

    // Panel expansion button: The initial display is the expansion button
    java.awt.Dimension buttonSize = new java.awt.Dimension(18, 18);
    JButton button = new JButton(expand_icon);
    button.setContentAreaFilled(false);
    button.setBorderPainted(false);
    button.setPreferredSize(buttonSize);
    button.setMinimumSize(buttonSize);
    button.setMaximumSize(buttonSize);

    // Name label
    panelName.add(button);
    String name = "BLOCK";
    if (info.getBlock() != null) {
      name = info.getBlock().toString();
    }
    JLabel label = new JLabel(name);
    label.setOpaque(false);
    panelName.add(label);

    // Variable characteristic list panel
    JPanel panelTable = new JPanel();
    panelTable.setLayout(new BoxLayout(panelTable, BoxLayout.Y_AXIS));
    panelTable.setOpaque(false);
    panelTable.setBorder(new EmptyBorder(5, 40, 5, 40));

    // Variable characteristic list table
    JTable tableVariable = new JStripeTable();
    tableVariable.setModel(info.getTableModel());

    tableVariable.setAutoCreateColumnsFromModel(false);
    tableVariable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    tableVariable.setColumnSelectionAllowed(false);
    tableVariable.addMouseListener(this);
    // Table change event
    VariableListSelectionListener listener = new VariableListSelectionListener(tableVariable);
    tableVariable.getSelectionModel().addListSelectionListener(listener);

    // Table column width setting
    DefaultTableColumnModel columnModel = (DefaultTableColumnModel) tableVariable.getColumnModel();
    model.setTableColumnWidth(columnModel);

    // Draw table headers and rows separately
    JTableHeader tableHeader = tableVariable.getTableHeader();

    tableVariable.setShowGrid(false);
    tableVariable.setIntercellSpacing(new Dimension(0, 0));

    // sort
    tableVariable.setAutoCreateRowSorter(true);
    int index = 1;
    tableVariable
        .getRowSorter()
        .setSortKeys(Arrays.asList(new RowSorter.SortKey(index, SortOrder.DESCENDING)));

    // Border setting of variable characteristic list table
    LineBorder border = new LineBorder(Color.GRAY, 1, false);
    tableVariable.setBorder(border);
    tableHeader.setBorder(border);

    // Add to variable characteristic list panel
    panelTable.add(tableHeader);
    panelTable.add(tableVariable);

    tableVariable.validate();
    tableVariable.updateUI();

    // Setting the action listener of the variable characteristic list expansion button
    button.addActionListener(new VariableExpandAction(panelTable));

    // Add name panel and body panel to variable characteristic list panel
    rows.add(panelName);
    rows.add(panelTable);

    // set the table
    rows.setTableVariable(tableVariable);

    // Set the context menu
    tableVariable.setComponentPopupMenu(this.variablePopupMenu);

    return rows;
  }

  /**
   * Variable Characteristics Node Panel
   *
   * @author RIKEN
   */
  private class NodePanel extends JPanel {
    /** Serial number */
    private static final long serialVersionUID = 1L;

    /** Variable characteristic block */
    private VariableTableModel.ProcedureInfo block;
    /** Variable characteristic table */
    private JTable tableVariable;

    /**
     * Constructor
     *
     * @param block Variable characteristic block
     */
    public NodePanel(VariableTableModel.ProcedureInfo block) {
      this.block = block;
    }

    /**
     * Get the variable characteristic block
     *
     * @return Variable characteristic block
     */
    @SuppressWarnings("unused")
    public VariableTableModel.ProcedureInfo getBlock() {
      return this.block;
    }

    /**
     * Set variable characteristic table
     *
     * @param tableVariable variable characteristic table
     */
    public void setTableVariable(JTable tableVariable) {
      this.tableVariable = tableVariable;
    }

    /**
     * Get the variable characteristic table
     *
     * @return Variable characteristic table
     */
    public JTable getTableVariable() {
      return this.tableVariable;
    }
  }

  /**
   * Get variable characteristic list table model
   *
   * @return Variable characteristics list Table model
   */
  public VariableTableModel getModel() {
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

    // Open the relevant part
    this.btnOpenFile.addActionListener((ActionListener) menu.getActionOpenAnalysisLine());
  }

  /** Clear the model. */
  @Override
  public void clearModel() {
    // Model clear
    model.clearVariable();
  }

  /**
   * Variable Characteristics Panel Expand Button Action Listener
   *
   * @author RIKEN
   */
  private class VariableExpandAction implements ActionListener {
    /** Variable characteristics list table panel */
    private JPanel panelRow;

    /**
     * Constructor
     *
     * @param panel Variable characteristic list panel for switching display
     */
    public VariableExpandAction(JPanel panel) {
      this.panelRow = panel;
    }

    /**
     * Button click event
     *
     * @param event Event information
     */
    @Override
    public void actionPerformed(ActionEvent event) {
      JButton btn = (JButton) event.getSource();
      // Toggle the display of the variable characteristic list panel
      if (panelRow.isVisible()) {
        // Hide the variable characteristic list panel
        btn.setIcon(collapse_icon);
        panelRow.setVisible(false);
      } else {
        // Display the variable characteristic list panel
        btn.setIcon(expand_icon);
        panelRow.setVisible(true);
      }

      return;
    }
  }

  /**
   * Mouse click event
   *
   * @param event Mouse event information
   */
  @Override
  public void mouseClicked(MouseEvent event) {
    // Click check
    if (SwingUtilities.isLeftMouseButton(event)) {

      NodePanel panel = null;
      if (event.getSource() instanceof NodePanel) {
        panel = (NodePanel) event.getSource();
      } else if (event.getSource() instanceof JTable) {
        // Parent parent panel
        Container cont = ((JTable) event.getSource()).getParent().getParent();
        if (cont instanceof NodePanel) {
          panel = (NodePanel) cont;
        }
      }

      // Set the selective additional information block
      this.selectedInfo = panel;

      // Set the background color of the selection panel.
      setSelectedBackgroud(this.selectedInfo);

      // Double click
      if (event.getClickCount() == 2) {
        // Open the relevant part
        this.btnOpenFile.doClick();
      }
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

  /** Close the tab */
  @Override
  public void closeTab() {}

  private class VariableListSelectionListener implements ListSelectionListener {
    /** Listener target table */
    private JTable table;

    /**
     * Constructor
     *
     * @param table Listener target table
     */
    public VariableListSelectionListener(JTable table) {
      this.table = table;
    }

    /**
     * Variable characteristic list table selection change event.
     *
     * @param event Event information
     */
    @Override
    public void valueChanged(ListSelectionEvent event) {
      if (event.getValueIsAdjusting()) return;

      int selection = table.getSelectedRow();
      // Convert to the number of rows in the table model
      int modelRow = table.convertRowIndexToModel(selection);
      if (modelRow < 0) return;
      Object cell = this.table.getModel().getValueAt(modelRow, 0);
      if (cell == null) return;
      if (cell instanceof VariableDefinition) {
        // Set the selection variable declaration statement
        VariableTablePanel.this.model.setSelectedVariable((VariableDefinition) cell);
      }
    }
  }

  /**
   * Get selected source code line information (unused)
   *
   * @return Selected source code line information
   */
  @Override
  public CodeLine getSelectedCodeLine() {
    return null;
  }

  /**
   * Set the context menu
   *
   * @param variablePopupMenu Context menu
   */
  public void setPopupMenu(VariablePopupMenu variablePopupMenu) {
    this.variablePopupMenu = variablePopupMenu;

    // Additional information
    this.actionEdit = variablePopupMenu.getActionAnalysisInformation();
    // Calculation count
    this.actionOperand = variablePopupMenu.getActionAnalysisOperand();
    // Declaration / Definition / Reference
    this.actionReference = variablePopupMenu.getActionAnalysisReference();
    // Variable valid area
    this.actionScope = variablePopupMenu.getActionAnalysisScope();
  }

  /**
   * Get the selected block
   *
   * @return selection block
   */
  @Override
  public IBlock getSelectedBlock() {
    VariableDefinition value = this.model.getSelectedVariable();
    if (value instanceof IBlock) {
      return (IBlock) value;
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
    VariableDefinition value = this.model.getSelectedVariable();
    if (value instanceof IInformation) {
      return (IInformation) value;
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
    // Background color of selection panel
    this.colorSelectedPanel = properties.getBackgoundView2Color();
    // Set the background color of the selection panel.
    setSelectedBackgroud(this.selectedInfo);
  }

  /**
   * Button click event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {
    // clear
    if (event.getSource() == this.btnClear) {
      // Model clear
      clearModel();
    }
    // Edit additional information
    else if (event.getSource() == this.btnEdit) {
      VariableDefinition value = this.model.getSelectedVariable();
      if (value instanceof IInformation) {
        this.actionEdit.editInformation((IInformation) value);
      }
    }
    // Count the number of operations
    else if (event.getSource() == this.btnOperand) {
      IBlock block = this.getSelectedBlock();
      if (block != null) {
        IBlock[] blocks = {block};
        actionOperand.analysisOperand(blocks);
      }
    }
    // Declaration / Definition / Reference
    else if (event.getSource() == this.btnReference) {
      VariableDefinition variable = this.model.getSelectedVariable();
      actionReference.analysisReference(variable);
    }
    // Variable valid area
    else if (event.getSource() == this.btnScope) {
      VariableDefinition variable = this.model.getSelectedVariable();
      actionScope.analysisScope(variable);
    }
  }

  /** Copy the selection to the clipboard. */
  @Override
  public void copyClipboard() {
    if (this.selectedInfo == null) return;
    String text = SwingUtils.toCsvOfSeletedRows(this.selectedInfo.getTableVariable());
    if (text == null) return;

    // copy to clipboard
    SwingUtils.copyClipboard(text);
  }

  /** Whether there is information to export */
  @Override
  public boolean isExportable() {
    if (this.model == null) return false;
    return (!this.model.isEmpty());
  }

  /** Set the background color of the selection panel. */
  private void setSelectedBackgroud(JPanel panel) {
    // Clear all
    if (this.contentInfo != null) {
      SwingUtils.setBackgroundChildPanel(this.contentInfo, null);
    }
    // Change the background color of the selection panel to the selection color
    if (panel != null) {
      SwingUtils.setBackgroundChildPanel(panel, this.colorSelectedPanel);
    }
  }
}
