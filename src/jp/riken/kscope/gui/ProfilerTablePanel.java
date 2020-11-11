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
import java.util.ArrayList;
import java.util.List;
import java.util.Observable;
import java.util.Observer;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableModel;
import jp.riken.kscope.Message;
import jp.riken.kscope.action.ProfilerInformationEditAction;
import jp.riken.kscope.action.ViewOpenAnalysisLineAction;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.common.PROFILERINFO_TYPE;
import jp.riken.kscope.component.JStripeTable;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.menu.MainMenu;
import jp.riken.kscope.menu.ProfilerPopupMenu;
import jp.riken.kscope.model.ProfilerCallGraphModel;
import jp.riken.kscope.model.ProfilerCostTableModel;
import jp.riken.kscope.model.ProfilerEventCounterModel;
import jp.riken.kscope.model.ProfilerTableBaseModel;
import jp.riken.kscope.profiler.ProfilerBaseData;
import jp.riken.kscope.properties.ProfilerProperties;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Profiler: Cost Information Panel Class
 *
 * @author RIKEN
 */
public class ProfilerTablePanel extends AnalisysPanelBase
    implements Observer, IAnalisysComponent, MouseListener, ActionListener {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Clear button */
  private JButton btnClear;
  /** Additional information button */
  private JButton btnEdit;
  /** Open file button */
  private JButton btnOpenFile;
  /** Export button */
  private JButton btnExport;
  /** Table column display switching button */
  private JButton btnPulldown;
  /** Sort button */
  // private JButton btnSort;
  /** Profiler: Cost Information Label */
  private JLabel label;
  /** Content Box */
  private Box contentInfo;
  /** Margin box */
  private final Component glue = Box.createVerticalGlue();
  /** Scroll pine */
  private JScrollPane scrollPane;

  /** Profiler: Cost Information Table Model */
  private ProfilerTableBaseModel model;

  /** Expand button icon */
  private Icon expand_icon = ResourceUtils.getIcon("expand_arrow.gif");
  /** Storage button icon */
  private Icon collapse_icon = ResourceUtils.getIcon("collapse_arrow.gif");

  /** Profiler: Cost Information Context Menu */
  private ProfilerPopupMenu costinfoPopupMenu;

  /** Selection table */
  private JTable selectedTable;
  /** Select Profiler Panel */
  private NodePanel selectedPanel;
  /** Additional information editing action */
  private ProfilerInformationEditAction actionEdit;
  /** Action to open the relevant part */
  private ViewOpenAnalysisLineAction actionOpenAnalysis;

  private JPopupMenu menuVisibledColumns;

  /** Sort status */
  @SuppressWarnings("unused")
  private boolean viewSort = false;
  /** Selection panel background color */
  private Color colorSelectedPanel;

  /**
   * Constructor
   *
   * @param panel Analysis information panel identifier
   * @throws Exception Analysis information panel identifier invalid
   */
  public ProfilerTablePanel(ANALYSIS_PANEL panel) throws Exception {
    super(panel);

    // Generate a model
    model = factoryProfilerTableModel(panel);

    // Set the observer.
    model.addObserver(this);

    // Initialize the GUI.
    initGUI();
  }

  /**
   * Generate profiler model
   *
   * @param panel Panel identifier
   * @return Profiler model
   * @throws Exception Panel identifier exception
   */
  private ProfilerTableBaseModel factoryProfilerTableModel(ANALYSIS_PANEL panel) throws Exception {

    ProfilerTableBaseModel profilerModel = null;
    // Generate a model
    switch (panel) {
      case COST_LINE:
        profilerModel = new ProfilerCostTableModel(PROFILERINFO_TYPE.COST_LINE);
        break;
      case COST_LOOP:
        profilerModel = new ProfilerCostTableModel(PROFILERINFO_TYPE.COST_LOOP);
        break;
      case COST_PROCEDURE:
        profilerModel = new ProfilerCostTableModel(PROFILERINFO_TYPE.COST_PROCEDURE);
        break;
      case CALLGRAPH:
        profilerModel = new ProfilerCallGraphModel(PROFILERINFO_TYPE.CALLGRAPH);
        break;
      case EVENTCOUNTER_CACHE:
        profilerModel = new ProfilerEventCounterModel(PROFILERINFO_TYPE.EVENTCOUNTER_CACHE);
        break;
      case EVENTCOUNTER_INSTRUCTIONS:
        profilerModel = new ProfilerEventCounterModel(PROFILERINFO_TYPE.EVENTCOUNTER_INSTRUCTIONS);
        break;
      case EVENTCOUNTER_MEM_ACCESS:
        profilerModel = new ProfilerEventCounterModel(PROFILERINFO_TYPE.EVENTCOUNTER_MEM_ACCESS);
        break;
      case EVENTCOUNTER_PERFORMANCE:
        profilerModel = new ProfilerEventCounterModel(PROFILERINFO_TYPE.EVENTCOUNTER_PERFORMANCE);
        break;
      case EVENTCOUNTER_STATISTICS:
        profilerModel = new ProfilerEventCounterModel(PROFILERINFO_TYPE.EVENTCOUNTER_STATISTICS);
        break;
      default:
        throw new Exception(
            Message.getString(
                "profilertablepanel.exception.panelidentiferinvalid")); // The analysis information
                                                                        // panel identifier is
                                                                        // invalid.
    }

    return profilerModel;
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
          {
            //    Icon icon = ResourceUtils.getIcon("sort_gray.gif");
            //    btnSort = new JButton(icon);
            //    btnSort.setContentAreaFilled(false);
            //    btnSort.setBorderPainted(false);
            //    btnSort.setPreferredSize(buttonSize);
            //    btnSort.setMinimumSize(buttonSize);
            //    btnSort.setMaximumSize(buttonSize);
            //    panelButtons.add(btnSort);
          }
          {
            Icon icon = ResourceUtils.getIcon("popupmenu.gif");
            btnPulldown = new JButton(icon);
            btnPulldown.setContentAreaFilled(false);
            btnPulldown.setBorderPainted(false);
            btnPulldown.setPreferredSize(buttonSize);
            btnPulldown.setMinimumSize(buttonSize);
            btnPulldown.setMaximumSize(buttonSize);
            panelButtons.add(btnPulldown);
          }
        }

        // Label placement
        {
          label = new JLabel();
          panelTop.add(label, BorderLayout.CENTER);
          label.setText("Profiler Infomation");
        }
      }
      {
        {
          // Profiler: Cost Information Panel
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
      btnOpenFile.addActionListener(this);
      btnPulldown.addActionListener(this);
      // btnSort.addActionListener(this);

      // Tooltip settings
      btnClear.setToolTipText(Message.getString("informationdialog.button.clear.tooltip")); // clear
      btnEdit.setToolTipText(
          Message.getString("profilertablepanel.tooltip.info")); // Additional information
      btnOpenFile.setToolTipText(
          Message.getString("profilertablepanel.tooltip.open")); // open the selection
      btnExport.setToolTipText(Message.getString("mainmenu.file.export")); // export
      btnPulldown.setToolTipText(
          Message.getString("profilertablepanel.tooltip.column")); // Select display column
      // btnSort.setToolTipText (Message.getString ("profilertablepanel.tooltip.sort")); // Sort by
      // line number

    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * Profiler: Cost information model change notification event
   *
   * @param o Notification source
   * @param arg Notification item
   */
  @Override
  public void update(Observable o, Object arg) {
    // Profiler: Clear cost information
    clearComponent();

    // Table model
    ProfilerTableBaseModel observer = (ProfilerTableBaseModel) o;

    // Panel title
    this.label.setText(observer.getTitle());

    int count = observer.getInfoMapCount();
    for (int i = 0; i < count; i++) {
      String key = observer.getSubTitle(i);
      if (key == null) continue;
      TableModel table = observer.getInfoTableModel(i);

      // Profiler: Add cost information
      addInfoTable(key, table);
    }
  }

  /**
   * Add profiler information
   *
   * @param key Profiler information identification string
   * @param table Profiler information table model
   */
  public void addInfoTable(String key, TableModel table) {

    // Create additional components
    JComponent component = makeRowsPanel(key, table);

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

    // Clear selection table
    this.selectedPanel = null;
    this.selectedTable = null;
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
   * Profiler: Added cost information panel
   *
   * @param key Cost information identification string
   * @param table Cost information table model
   * @return Profiler: Cost Information Panel
   */
  private JComponent makeRowsPanel(String key, TableModel table) {
    // Profiler: Cost Information Panel
    NodePanel rows = new NodePanel(key);
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
    JLabel label = new JLabel(key);
    label.setOpaque(false);
    panelName.add(label);

    // Profiler: Cost Information Panel
    JPanel panelTable = new JPanel();
    panelTable.setLayout(new BoxLayout(panelTable, BoxLayout.Y_AXIS));
    panelTable.setOpaque(false);
    panelTable.setBorder(new EmptyBorder(5, 40, 5, 40));

    // Profiler: Cost information table
    // Cell placement
    JTable tableInfo = new JStripeTable(this.model.getTableColumnAlignments(), SwingConstants.LEFT);
    tableInfo.setModel(table);

    tableInfo.setAutoCreateColumnsFromModel(false);
    tableInfo.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    tableInfo.setColumnSelectionAllowed(false);
    tableInfo.addMouseListener(this);
    // Table change event
    ProfilerInfoListSelectionListener listener = new ProfilerInfoListSelectionListener(tableInfo);
    tableInfo.getSelectionModel().addListSelectionListener(listener);

    // Table column width setting
    DefaultTableColumnModel columnModel = (DefaultTableColumnModel) tableInfo.getColumnModel();
    model.setTableColumnWidth(columnModel);

    // Draw table headers and rows separately
    JTableHeader tableHeader = tableInfo.getTableHeader();

    tableInfo.setShowGrid(false);
    tableInfo.setIntercellSpacing(new Dimension(0, 0));

    // sort
    //        tableCostInfo.setAutoCreateRowSorter(true);
    //        int index = 1;
    //        tableCostInfo.getRowSorter().setSortKeys(
    //            Arrays.asList(new RowSorter.SortKey(index, SortOrder.DESCENDING)));

    // Profiler: Border setting of cost information table
    LineBorder border = new LineBorder(Color.GRAY, 1, false);
    tableInfo.setBorder(border);
    tableHeader.setBorder(border);

    // Profiler: Added to Cost Information Panel
    panelTable.add(tableHeader);
    panelTable.add(tableInfo);

    tableInfo.validate();
    tableInfo.updateUI();

    // Profiler: Setting the action listener for the cost information expansion button
    button.addActionListener(new ProfilerInfoExpandAction(panelTable));

    // Profiler: Add name panel and body panel to cost information panel
    rows.add(panelName);
    rows.add(panelTable);

    // set the table
    rows.setTableInfo(tableInfo);

    // Set the context menu
    tableInfo.setComponentPopupMenu(this.costinfoPopupMenu);

    return rows;
  }

  /**
   * Cost Information Node Panel
   *
   * @author RIKEN
   */
  private class NodePanel extends JPanel {
    /** Serial number */
    private static final long serialVersionUID = 1L;

    /** Cost information identification string */
    private String key;
    /** Cost information table */
    private JTable tableInfo;

    /**
     * Constructor
     *
     * @param key Cost information key string
     */
    public NodePanel(String key) {
      this.key = key;
    }

    /**
     * Get the cost information identification string
     *
     * @return Cost information identification string
     */
    @SuppressWarnings("unused")
    public String getKey() {
      return key;
    }

    /**
     * Set the cost information identification string
     *
     * @param key Cost information identification string
     */
    @SuppressWarnings("unused")
    public void setKey(String key) {
      this.key = key;
    }

    /**
     * Get the cost information table
     *
     * @return Cost information table
     */
    public JTable getTableInfo() {
      return tableInfo;
    }

    /**
     * Set the cost information table
     *
     * @param table Cost information table
     */
    public void setTableInfo(JTable table) {
      this.tableInfo = table;
    }
  }

  /**
   * Profiler: Get cost information table model
   *
   * @return Profiler: Cost Information Table Model
   */
  public ProfilerTableBaseModel getModel() {
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
  }

  /** Clear the model. */
  @Override
  public void clearModel() {
    // Model clear
    model.clearModel();
  }

  /**
   * Cost Information Panel Expand Button Action Listener
   *
   * @author RIKEN
   */
  private class ProfilerInfoExpandAction implements ActionListener {
    /** Profiler: Cost Information Table Panel */
    private JPanel panelRow;

    /**
     * Constructor
     *
     * @param panel Profiler to switch the display: Cost information panel
     */
    public ProfilerInfoExpandAction(JPanel panel) {
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
      // Profiler: Toggle display of cost information panel
      if (panelRow.isVisible()) {
        // Profiler: Hide cost information panel
        btn.setIcon(collapse_icon);
        panelRow.setVisible(false);
      } else {
        // Profiler: Show cost information panel
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
      // Selection panel
      this.selectedPanel = panel;
      // Change the background color of the selected column to the selected color
      setSelectedBackgroud(this.selectedPanel);

      if (panel != null) {
        JTable table = panel.getTableInfo();
        int selection = table.getSelectedRow();
        if (selection < 0) {
          // Select the first line.
          table.setRowSelectionInterval(0, 0);
        }
        // Convert to the number of rows in the table model
        int modelRow = table.convertRowIndexToModel(selection);
        if (modelRow >= 0) {
          Object cell = table.getModel().getValueAt(modelRow, 0);
          if (cell != null && cell instanceof ProfilerBaseData) {
            // Set the selection profiler data
            this.model.setSelectedInfo((ProfilerBaseData) cell);
          }
        }
        // Set the selection table
        this.selectedTable = table;
      }
      //
      ITabComponent parent = this.getParentComponent();
      if (parent instanceof AnalysisView) {
        ((AnalysisView) parent).changeAnalisysTab();
      }

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
   * @param event Mouse event information
   */
  @Override
  public void mousePressed(MouseEvent event) {}

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

  /**
   * Cost information table selection change listener
   *
   * @author RIKEN
   */
  private class ProfilerInfoListSelectionListener implements ListSelectionListener {
    /** Listener target table */
    private JTable table;

    /**
     * Constructor
     *
     * @param table Listener target table
     */
    public ProfilerInfoListSelectionListener(JTable table) {
      this.table = table;
    }

    /**
     * Profiler: Cost information table selection change event.
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
      if (cell instanceof ProfilerBaseData) {
        // Set the selection profiler data
        ProfilerTablePanel.this.model.setSelectedInfo((ProfilerBaseData) cell);
      }
      // Set the selection table
      ProfilerTablePanel.this.selectedTable = table;
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
   * @param popupMenu Context menu
   */
  public void setPopupMenu(ProfilerPopupMenu popupMenu) {
    this.costinfoPopupMenu = popupMenu;

    // Additional information
    this.actionEdit = popupMenu.getActionAnalysisInformation();

    // Open the relevant part
    actionOpenAnalysis = (ViewOpenAnalysisLineAction) popupMenu.getActionOpenAnalysisLine();
  }

  /**
   * Get the selected block
   *
   * @return selection block
   */
  @Override
  public IBlock getSelectedBlock() {
    ProfilerBaseData value = this.model.getSelectedInfo();
    return value.getBlock();
  }

  /**
   * Get additional selection information
   *
   * @return Selectable additional information
   */
  @Override
  public IInformation getSelectedInformation() {
    IInformation[] infos = getSelectedInformations();
    if (infos == null) return null;
    return infos[0];
  }

  /**
   * Get additional selection information
   *
   * @return Select additional information range
   */
  public IInformation[] getSelectedInformations() {
    ProfilerBaseData value = this.model.getSelectedInfo();
    IBlock[] blocks = value.getBlocks();
    if (blocks == null) return null;
    List<IInformation> list = new ArrayList<IInformation>();
    for (IBlock block : blocks) {
      if (block instanceof IInformation) {
        list.add((IInformation) block);
      } else {
        return null;
      }
    }
    if (list.size() <= 0) return null;
    return list.toArray(new IInformation[0]);
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
    setSelectedBackgroud(this.selectedPanel);
  }

  /**
   * Set profiler properties
   *
   * @param properties Profiler properties
   */
  public void setProfilerProperties(ProfilerProperties properties) {
    if (this.model == null) return;
    this.model.setProfilerProperties(properties);
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
    // Open the corresponding line of cost information
    if (event.getSource() == this.btnOpenFile) {
      ProfilerBaseData value = this.model.getSelectedInfo();
      if (value == null) return;
      if (value.getAreas() != null) {
        this.actionOpenAnalysis.viewSelectedAreas(value.getAreas());
      } else if (value.getCodeLine() != null && value.getCodeLine().getSourceFile() != null) {
        this.actionOpenAnalysis.clearSourceBlock();
        this.actionOpenAnalysis.viewSourceLine(value.getCodeLine());
      }
    }
    // Edit additional information
    else if (event.getSource() == this.btnEdit) {
      ProfilerBaseData value = this.model.getSelectedInfo();
      String text = this.model.getSelectedText();
      if (value == null) return;
      this.actionEdit.editInformations(value, text);
    } else if (event.getSource() == this.btnPulldown) {
      menuVisibledColumns = makeVisibledColumnMenu();
      menuVisibledColumns.show(this.btnPulldown, 0, this.btnPulldown.getHeight());
    }
    // sort
    // else if (event.getSource() == this.btnSort) {
    //	Icon icon = null;
    //	if (this.viewSort) {
    //		icon = ResourceUtils.getIcon("sort_gray.gif");
    //		this.viewSort = false;
    //	} else {
    //		icon = ResourceUtils.getIcon("sort.gif");
    //		this.viewSort = true;
    //	}
    //	this.model.setViewSort(this.viewSort);
    //	this.refreshPanel();
    //	this.btnSort.setIcon(icon);
    // }
  }

  /** Copy the selection to the clipboard. */
  @Override
  public void copyClipboard() {
    if (this.selectedTable == null) return;
    String text = SwingUtils.toCsvOfSeletedRows(this.selectedTable);
    if (text == null) return;

    // copy to clipboard
    SwingUtils.copyClipboard(text);
  }

  /**
   * Create a pull-down menu for switching the display of table columns.
   *
   * @return Display switching pull-down menu
   */
  private JPopupMenu makeVisibledColumnMenu() {
    String[] columns = this.model.getHeaderColumns();
    boolean[] visibled = this.model.getVisibledColumns();
    JPopupMenu menu = new JPopupMenu();
    for (int i = 0; i < columns.length; i++) {
      if (columns[i].isEmpty()) continue;
      final int col = i;
      JCheckBoxMenuItem checkItem = new JCheckBoxMenuItem(columns[i]);
      checkItem.setSelected(visibled[i]);
      checkItem.addActionListener(
          new ActionListener() {
            public void actionPerformed(ActionEvent event) {
              JCheckBoxMenuItem item = (JCheckBoxMenuItem) event.getSource();
              boolean checked = item.isSelected();
              ProfilerTablePanel.this.model.setVisibledColumns(col, checked);
            }
          });
      menu.add(checkItem);
    }

    return menu;
  }

  /** Whether there is data that can be exported */
  @Override
  public boolean isExportable() {
    if (this.model == null) return false;
    return (!this.model.isEmpty());
  }

  /**
   * Change the background color of the selection panel to the selection color
   *
   * @param panel Selection panel
   */
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
