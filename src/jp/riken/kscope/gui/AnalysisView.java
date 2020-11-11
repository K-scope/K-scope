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
import java.awt.Dimension;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.swing.event.ChangeListener;
import jp.riken.kscope.action.AnalysisTabChangeAction;
import jp.riken.kscope.action.AnalysisTraceAction;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.common.TRACE_DIR;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.Keyword;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.menu.MainMenu;
import jp.riken.kscope.menu.ProfilerPopupMenu;
import jp.riken.kscope.model.ProfilerTableBaseModel;
import jp.riken.kscope.model.TraceResultModel;
import jp.riken.kscope.properties.ProfilerProperties;
import jp.riken.kscope.properties.SourceProperties;

/**
 * Parsing view class. <br>
 * Place tabs for reference lists, traces, and search results.
 *
 * @author RIKEN
 */
public class AnalysisView extends ClosableTabbedPane implements PropertyChangeListener {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Variable characteristic list panel */
  private VariableTablePanel panelVariable;
  /** Additional information panel */
  private InformationPanel panelInformation;
  /** Calculation count panel */
  private OperandTablePanel panelOperand;
  /** Request Byte / FLOP calculation result panel */
  private RequiredByteFlopPanel panelRequiredByteFlop;
  /** Property table panel */
  private PropertiesTablePanel panelPropertiesTable;
  /** Console panel */
  private ConsolePanel panelConsole;
  /** Error location panel */
  private ErrorInfoPanel panelError;
  /** Search Results Panel */
  private SearchResultPanel panelSearchResult;
  /** Reference list panel */
  private ReferencePanel panelReference;
  /** Variable Effectiveness Panel */
  private ScopePanel panelScope;
  /** Trace panel */
  private TraceResultPanel panelTrace;
  /** Profiler: Information Panel */
  private List<ProfilerTablePanel> panelProfilerList;
  /** Profiler: Measurement interval information table panel */
  private ProfilerMeasurePanel panelProfilerMeasure;

  // Information required when creating a new trace panel.
  /** Main menu */
  private MainMenu mainMenu;
  /** Source settings properties */
  private SourceProperties propertiesSource;
  /** Profiler Properties */
  private ProfilerProperties propertiesProfiler;
  /** Trace close action */
  private AnalysisTraceAction closeTraceAction;

  /** Constructor */
  public AnalysisView() {
    super(FRAME_VIEW.ANALYSIS_VIEW);
    initGUI();
  }

  /**
   * Initialize. <br>
   * Place tabs for reference lists, traces, and search results.
   */
  private void initGUI() {
    try {

      // Add analysis tab
      // Additional information
      panelInformation = new InformationPanel(ANALYSIS_PANEL.INFORMATION);
      panelInformation.setParentComponent(this);
      this.addTab(ANALYSIS_PANEL.INFORMATION.getTabName(), panelInformation);

      // search results
      panelSearchResult = new SearchResultPanel(ANALYSIS_PANEL.SEARCHRESULT);
      panelSearchResult.setParentComponent(this);
      this.addTab(ANALYSIS_PANEL.SEARCHRESULT.getTabName(), panelSearchResult);

      // Variable characteristic list panel
      panelVariable = new VariableTablePanel(ANALYSIS_PANEL.VALIABLE);
      panelVariable.setParentComponent(this);
      this.addTab(ANALYSIS_PANEL.VALIABLE.getTabName(), panelVariable);

      // Calculation count
      panelOperand = new OperandTablePanel(ANALYSIS_PANEL.OPERAND);
      panelOperand.setParentComponent(this);
      this.addTab(ANALYSIS_PANEL.OPERAND.getTabName(), panelOperand);

      // Request Byte / FLOP calculation result panel
      panelRequiredByteFlop = new RequiredByteFlopPanel(ANALYSIS_PANEL.REQUIRED);
      panelRequiredByteFlop.setParentComponent(this);

      // Declaration / Definition / Reference
      panelReference = new ReferencePanel(ANALYSIS_PANEL.REFERENCE);
      panelReference.setParentComponent(this);
      this.addTab(ANALYSIS_PANEL.REFERENCE.getTabName(), panelReference);

      // Trace
      panelTrace = new TraceResultPanel(ANALYSIS_PANEL.TRACE);
      panelTrace.setParentComponent(this);
      this.addTab(ANALYSIS_PANEL.TRACE.getTabName(), panelTrace);

      // Variable Effectiveness Panel
      panelScope = new ScopePanel(ANALYSIS_PANEL.SCOPE);
      panelScope.setParentComponent(this);
      this.addTab(ANALYSIS_PANEL.SCOPE.getTabName(), panelScope);

      // Profiler panel list
      if (panelProfilerList == null) {
        panelProfilerList = new ArrayList<ProfilerTablePanel>();
      }
      panelProfilerList.clear();
      // Profiler cost information (procedure) panel
      {
        ProfilerTablePanel panelProfiler = new ProfilerTablePanel(ANALYSIS_PANEL.COST_PROCEDURE);
        panelProfiler.setParentComponent(this);
        panelProfilerList.add(panelProfiler);
      }
      // Profiler cost information (loop) panel
      {
        ProfilerTablePanel panelProfiler = new ProfilerTablePanel(ANALYSIS_PANEL.COST_LOOP);
        panelProfiler.setParentComponent(this);
        panelProfilerList.add(panelProfiler);
      }
      // Profiler cost information (line) panel
      {
        ProfilerTablePanel panelProfiler = new ProfilerTablePanel(ANALYSIS_PANEL.COST_LINE);
        panelProfiler.setParentComponent(this);
        panelProfilerList.add(panelProfiler);
      }
      // Profiler: Call graph information
      {
        ProfilerTablePanel panelProfiler = new ProfilerTablePanel(ANALYSIS_PANEL.CALLGRAPH);
        panelProfiler.setParentComponent(this);
        panelProfilerList.add(panelProfiler);
      }
      // Profiler: Event counter information: Eprof_CACHE
      {
        ProfilerTablePanel panelProfiler =
            new ProfilerTablePanel(ANALYSIS_PANEL.EVENTCOUNTER_CACHE);
        panelProfiler.setParentComponent(this);
        panelProfilerList.add(panelProfiler);
      }
      // Profiler: Event counter information: Eprof_INSTRUCTIONS
      {
        ProfilerTablePanel panelProfiler =
            new ProfilerTablePanel(ANALYSIS_PANEL.EVENTCOUNTER_INSTRUCTIONS);
        panelProfiler.setParentComponent(this);
        panelProfilerList.add(panelProfiler);
      }
      // Profiler: Event counter information: Eprof_MEM_ACCESS
      {
        ProfilerTablePanel panelProfiler =
            new ProfilerTablePanel(ANALYSIS_PANEL.EVENTCOUNTER_MEM_ACCESS);
        panelProfiler.setParentComponent(this);
        panelProfilerList.add(panelProfiler);
      }
      // Profiler: Event counter information: Eprof_PERFORMANCE
      {
        ProfilerTablePanel panelProfiler =
            new ProfilerTablePanel(ANALYSIS_PANEL.EVENTCOUNTER_PERFORMANCE);
        panelProfiler.setParentComponent(this);
        panelProfilerList.add(panelProfiler);
      }
      // Profiler: Event counter information: Eprof_STATISTICS
      {
        ProfilerTablePanel panelProfiler =
            new ProfilerTablePanel(ANALYSIS_PANEL.EVENTCOUNTER_STATISTICS);
        panelProfiler.setParentComponent(this);
        panelProfilerList.add(panelProfiler);
      }
      // Profiler: Measurement interval information table panel
      panelProfilerMeasure = new ProfilerMeasurePanel(ANALYSIS_PANEL.EPROF_MEASURE);
      panelProfilerMeasure.setParentComponent(this);
      // this.addTab(ANALYSIS_PANEL.TIMERINFO.getTabName(),  panelTimerTable);

      // Property panel
      panelPropertiesTable = new PropertiesTablePanel(ANALYSIS_PANEL.PROPARTIES);
      panelPropertiesTable.setParentComponent(this);
      this.addTab(ANALYSIS_PANEL.PROPARTIES.getTabName(), panelPropertiesTable);

      // Error location panel
      panelError = new ErrorInfoPanel(ANALYSIS_PANEL.ERROR);
      panelError.setParentComponent(this);
      this.addTab(ANALYSIS_PANEL.ERROR.getTabName(), panelError);

      // Console panel
      panelConsole = new ConsolePanel(ANALYSIS_PANEL.CONSOLE);
      panelConsole.setParentComponent(this);
      // this.addTab(ANALYSIS_PANEL.CONSOLE.getTabName(),  panelConsole);

      this.setMaximumSize(new Dimension(400, 100));
      this.setMinimumSize(new Dimension(400, 50));
      this.setPreferredSize(new Dimension(400, 100));
      this.setSize(new Dimension(400, 50));

      // Set the initial display to the additional information tab.
      this.setSelectedIndex(0);

    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /** Close tab */
  @Override
  public void closeTabComponent() {
    int index = this.getSelectedIndex();
    // close tab
    closeTab(index);
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
    if (tab instanceof IAnalisysComponent) {
      ((IAnalisysComponent) tab).closeTab();
    }
    this.remove(index);

    // Reset the trace keyword
    this.closeTraceAction.setTraceKeywords();
  }

  /**
   * Get additional information panel
   *
   * @return Additional information panel
   */
  public InformationPanel getPanelInformation() {
    return panelInformation;
  }

  /**
   * Get the reference list panel
   *
   * @return Reference list panel
   */
  public ReferencePanel getPanelReference() {
    return this.panelReference;
  }

  /**
   * Get the search results panel
   *
   * @return Search results panel
   */
  public SearchResultPanel getPanelSearchResult() {
    return this.panelSearchResult;
  }

  /**
   * Get the variable characteristic list panel
   *
   * @return Variable characteristics list panel
   */
  public VariableTablePanel getPanelVariable() {
    return panelVariable;
  }

  /**
   * Get the variable effective area panel
   *
   * @return Variable Effectiveness Panel
   */
  public ScopePanel getPanelScope() {
    return this.panelScope;
  }

  /**
   * Get the console panel
   *
   * @return console panel
   */
  public ConsolePanel getPanelConsole() {
    return panelConsole;
  }

  /**
   * Get the error location panel
   *
   * @return Error location panel
   */
  public ErrorInfoPanel getPanelError() {
    return panelError;
  }

  /**
   * Get the property table panel
   *
   * @return property table panel
   */
  public PropertiesTablePanel getPanelPropertiesTable() {
    return panelPropertiesTable;
  }

  /**
   * Get profiler panel
   *
   * @param type Panel identifier
   * @return Profiler panel
   */
  public ProfilerTablePanel getPanelProfiler(ANALYSIS_PANEL type) {
    for (ProfilerTablePanel panel : this.panelProfilerList) {
      if (panel.getEnumPanel() == type) {
        return panel;
      }
    }
    return null;
  }

  /**
   * Get the measurement interval information table panel
   *
   * @return Measurement interval information table panel
   */
  public ProfilerMeasurePanel getPanelProfilerMeasure() {
    return this.panelProfilerMeasure;
  }

  /**
   * Get the currently selected analysis information panel.
   *
   * @return Selective analysis information panel
   */
  public IAnalisysComponent getSelectedPanel() {
    int index = this.getSelectedIndex();
    //
    if (index < 0) {
      return null;
    }
    IAnalisysComponent tab = (IAnalisysComponent) this.getComponentAt(index);

    return tab;
  }

  /**
   * Activate the designated analysis information panel. <br>
   * Open if closed
   *
   * @param panel Selective analysis information panel
   */
  public void setSelectedPanel(ANALYSIS_PANEL panel) {

    Component viewpanel = null;
    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      Component comp = this.getComponentAt(i);
      if (comp instanceof IAnalisysComponent) {
        if (((IAnalisysComponent) comp).getEnumPanel() == panel) {
          viewpanel = comp;
          this.setSelectedIndex(i);
          return;
        }
      }
    }

    // The tab is not displayed.
    if (viewpanel == null) {
      /** Variable characteristic list panel */
      if (panelVariable.getEnumPanel() == panel) {
        viewpanel = panelVariable;
      }
      /** Additional information panel */
      if (panelInformation.getEnumPanel() == panel) {
        viewpanel = panelInformation;
      }
      /** Calculation count panel */
      if (panelOperand.getEnumPanel() == panel) {
        viewpanel = panelOperand;
      }
      /** Request Byte / FlOP calculation result panel */
      if (panelRequiredByteFlop.getEnumPanel() == panel) {
        viewpanel = panelRequiredByteFlop;
      }
      /** Property table panel */
      if (panelPropertiesTable.getEnumPanel() == panel) {
        viewpanel = panelPropertiesTable;
      }
      /** Console panel */
      if (panelConsole.getEnumPanel() == panel) {
        viewpanel = panelConsole;
      }
      /** Error location panel */
      if (panelError.getEnumPanel() == panel) {
        viewpanel = panelError;
      }
      /** Search Results Panel */
      if (panelSearchResult.getEnumPanel() == panel) {
        viewpanel = panelSearchResult;
      }
      /** Reference list panel */
      if (panelReference.getEnumPanel() == panel) {
        viewpanel = panelReference;
      }
      /** Variable Effectiveness Panel */
      if (panelScope.getEnumPanel() == panel) {
        viewpanel = panelScope;
      }
      // Trace
      if (ANALYSIS_PANEL.TRACE == panel) {
        TraceResultPanel panelTrace = createTraceResultPanel();
        viewpanel = panelTrace;
      }
      /** Profiler: Cost Information Panel (Procedure) */
      ProfilerTablePanel profilerpanel = getPanelProfiler(panel);
      if (profilerpanel != null) {
        viewpanel = profilerpanel;
      }
      /** Profiler: Measurement interval information table panel */
      if (panelProfilerMeasure.getEnumPanel() == panel) {
        viewpanel = panelProfilerMeasure;
      }
    }
    if (viewpanel == null) return;

    // The tab is not displayed, so add it
    this.addTab(panel.getTabName(), viewpanel);
    this.setSelectedIndex(this.getTabCount() - 1);

    // Set display properties
    if (viewpanel instanceof IAnalisysComponent) {
      ((IAnalisysComponent) viewpanel).setSourceProperties(this.propertiesSource);
    }
    // Set profiler properties
    if (viewpanel instanceof ProfilerTablePanel) {
      ((ProfilerTablePanel) viewpanel).setProfilerProperties(this.propertiesProfiler);
    }

    return;
  }

  /**
   * Set an action listener on the panel. <br>
   * Assign the created action listener to the menu bar to the panel button.
   *
   * @param menu Main menu
   */
  public void setActionListener(MainMenu menu) {

    /** Panel list */
    IAnalisysComponent[] listPanel = {
      panelVariable,
      panelInformation,
      panelOperand,
      panelRequiredByteFlop,
      panelPropertiesTable,
      panelConsole,
      panelError,
      panelSearchResult,
      panelReference,
      panelScope,
      panelProfilerMeasure,
      panelTrace
    };
    for (IAnalisysComponent panel : listPanel) {
      if (panel != null) {
        panel.setActionListener(menu);
      }
    }

    // Profiler panel
    for (ProfilerTablePanel panel : this.panelProfilerList) {
      panel.setActionListener(menu);
    }

    // Trace close action
    this.closeTraceAction = menu.getActionAnalysisTrace(TRACE_DIR.REFRESH);

    this.mainMenu = menu;
  }

  /** Clear analysis information */
  public void clearModels() {

    /** Additional information panel */
    this.panelInformation.clearModel();
    /** Variable characteristic list panel */
    this.panelVariable.clearModel();
    /** Calculation count panel */
    this.panelOperand.clearModel();
    /** Request B / F calculation result panel */
    this.panelRequiredByteFlop.clearModel();
    /** Property table panel */
    this.panelPropertiesTable.clearModel();
    /** Error location panel */
    this.panelError.clearModel();
    /** Search Results Panel */
    this.panelSearchResult.clearModel();
    /** Reference list panel */
    this.panelReference.clearModel();
    /** Variable Effectiveness Panel */
    this.panelScope.clearModel();
    /** Profiler panel */
    for (ProfilerTablePanel panel : this.panelProfilerList) {
      panel.clearModel();
    }
    /** Profiler: Measurement interval information table panel */
    panelProfilerMeasure.clearModel();

    // Clear the trace result
    clearTrace();
  }

  /**
   * Clear the trace result. <br>
   * Close all trace tabs.
   */
  public void clearTrace() {

    // Close the trace result panel
    int count = this.getTabCount();
    boolean blankPanel = false;
    for (int i = count - 1; i >= 0; i--) {
      Component comp = this.getComponentAt(i);
      if (comp instanceof TraceResultPanel) {
        TraceResultPanel panel = (TraceResultPanel) comp;
        if (!blankPanel) {
          // The first trace panel is a blank panel.
          panel.clearModel();
          blankPanel = true;
          setTracePanelTabname(i, panel.getModel());
        } else {
          this.closeTab(i);
        }
      }
    }
  }

  /**
   * Get the calculation count panel
   *
   * @return Calculation count panel
   */
  public OperandTablePanel getPanelOperand() {
    return this.panelOperand;
  }

  /**
   * Request Byte / FLOP calculation result panel
   *
   * @return Request Byte / FLOP calculation result panel
   */
  public RequiredByteFlopPanel getPanelRequiredByteFlop() {
    return this.panelRequiredByteFlop;
  }

  /**
   * Get the index of the specified analysis information panel. <br>
   * Returns -1 if closed
   *
   * @param panel Specified analysis information panel identifier
   * @return tab index
   */
  public int getTabIndex(ANALYSIS_PANEL panel) {

    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      Component comp = this.getComponentAt(i);
      if (comp instanceof IAnalisysComponent) {
        if (((IAnalisysComponent) comp).getEnumPanel() == panel) {
          return i;
        }
      }
    }
    return -1;
  }

  /**
   * Get selected source code line information
   *
   * @return Selected source code line information
   */
  public CodeLine getSelectedCodeLine() {
    IAnalisysComponent tab = (IAnalisysComponent) this.getSelectedComponent();
    return tab.getSelectedCodeLine();
  }

  /**
   * Get the selected block
   *
   * @return selection block
   */
  public IBlock getSelectedBlock() {
    IAnalisysComponent tab = (IAnalisysComponent) this.getSelectedComponent();
    return tab.getSelectedBlock();
  }

  /**
   * Get additional selection information
   *
   * @return Selectable additional information
   */
  public IInformation getSelectedInformation() {
    IAnalisysComponent tab = (IAnalisysComponent) this.getSelectedComponent();
    return tab.getSelectedInformation();
  }

  /**
   * Display trace results
   *
   * @param modelTrace Trace result model
   */
  public void viewAnalysisTrace(TraceResultModel modelTrace) {

    if (modelTrace == null) return;

    int count = this.getTabCount();
    int index = -1;
    TraceResultPanel panelTrace = null;
    for (int i = 0; i < count; i++) {
      Component comp = this.getComponentAt(i);
      if (comp instanceof TraceResultPanel) {
        TraceResultPanel panel = (TraceResultPanel) comp;
        TraceResultModel panelModel = panel.getModel();
        // Is it a blank model?
        if (panelModel.getTraceWord() == null || panelModel.getRootBlock() == null) {
          index = i;
          panelTrace = panel;
          break;
        }
        // Check if they are the same trace model
        if (panelModel.equalsTrace(modelTrace)) {
          index = i;
          panelTrace = panel;
          break;
        }
      }
    }

    if (panelTrace == null) {
      int lastindex = -1;
      for (int i = 0; i < count; i++) {
        Component comp = this.getComponentAt(i);
        if (comp instanceof TraceResultPanel) {
          lastindex = i;
        }
      }
      // Trace panel insertion position
      lastindex++;
      // Blank model, same trace model does not exist, so create a new trace panel
      panelTrace = createTraceResultPanel();
      if (lastindex > 0 && lastindex <= count - 1) {
        this.insertTab(ANALYSIS_PANEL.TRACE.getTabName(), panelTrace, lastindex);
        index = lastindex;
      } else {
        this.addTab(ANALYSIS_PANEL.TRACE.getTabName(), panelTrace);
        index = this.getTabCount() - 1;
      }
    }

    // Set the trace model
    panelTrace.setModel(modelTrace);

    // Set the trace result tab name
    setTracePanelTabname(index, modelTrace);

    // Activate the Trace tab
    this.setSelectedIndex(index);
  }

  /**
   * Create a new trace panel
   *
   * @return Create Trace Panel
   */
  private TraceResultPanel createTraceResultPanel() {
    TraceResultPanel panelTrace = new TraceResultPanel(ANALYSIS_PANEL.TRACE);

    // Action listener settings
    panelTrace.setActionListener(this.mainMenu);
    // Source property settings
    panelTrace.setSourceProperties(this.propertiesSource);

    return panelTrace;
  }

  /**
   * Set the trace result tab name. <br>
   * Trace variables and line numbers
   *
   * @param index Tab index
   * @param modelTrace Trace model
   */
  private void setTracePanelTabname(int index, TraceResultModel modelTrace) {
    if (index >= this.getTabCount()) return;

    // Trace variables and line numbers
    String msg = null;
    if (modelTrace != null) {
      String word = modelTrace.getTraceWord();
      IBlock block = modelTrace.getRootBlock();
      if (word != null && block != null) {
        String name = block.toString();
        if (block instanceof Procedure) {
          name = ((Procedure) block).get_name();
        }
        msg = "( " + word + ":" + name + ")";
      }
    }
    String tabname = ANALYSIS_PANEL.TRACE.getTabName();
    if (msg != null) {
      tabname += " " + msg;
    }

    // Set the tab name
    setTabTitle(index, tabname);
  }

  /**
   * Currently selected analysis information: Get the trace panel. <br>
   * Returns null if the trace panel is unselected.
   *
   * @return Selective analysis information trace panel
   */
  public TraceResultPanel getSelectedTracePanel() {
    int index = this.getSelectedIndex();
    IAnalisysComponent tab = (IAnalisysComponent) this.getComponentAt(index);
    if (tab.getEnumPanel() == ANALYSIS_PANEL.TRACE) {
      return (TraceResultPanel) tab;
    }

    return null;
  }

  /**
   * Currently selected analysis information: Profiler: Get cost information panel. <br>
   * Returns null if the profiler panel is not selected.
   *
   * @return Select Profiler: Cost Information Panel
   */
  public ProfilerTablePanel getSelectedProfilerCostPanel() {
    int index = this.getSelectedIndex();
    IAnalisysComponent tab = (IAnalisysComponent) this.getComponentAt(index);
    if (tab.getEnumPanel() == ANALYSIS_PANEL.COST_LINE
        || tab.getEnumPanel() == ANALYSIS_PANEL.COST_LOOP
        || tab.getEnumPanel() == ANALYSIS_PANEL.COST_PROCEDURE) {
      return (ProfilerTablePanel) tab;
    }

    return null;
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
      propertiesSource = (SourceProperties) event.getNewValue();

      // Set the source display properties on the Analysis View tab.
      int count = this.getTabCount();
      for (int i = 0; i < count; i++) {
        Component comp = this.getComponentAt(i);
        if (comp instanceof IAnalisysComponent) {
          ((IAnalisysComponent) comp).setSourceProperties(propertiesSource);
        }
      }
    }
    // Change profiler properties
    else if (event.getNewValue() instanceof ProfilerProperties) {
      ProfilerProperties newProp = (ProfilerProperties) event.getNewValue();
      int newvalue = newProp.getCostinfoMaxCount();
      if (this.propertiesProfiler != null) {
        int oldvalue = this.propertiesProfiler.getCostinfoMaxCount();
        if (newvalue == oldvalue) {
          return;
        }
      }
      this.propertiesProfiler =
          (ProfilerProperties) ((ProfilerProperties) event.getNewValue()).clone();
      // Set profiler properties on the profiler tab.
      int count = this.getTabCount();
      for (int i = 0; i < count; i++) {
        Component comp = this.getComponentAt(i);
        if (comp instanceof ProfilerTablePanel) {
          ((ProfilerTablePanel) comp).setProfilerProperties(propertiesProfiler);
        }
      }
    }
  }

  /**
   * Get the keyword list of trace results
   *
   * @return Trace keyword list
   */
  public Keyword[] getTraceKeywords() {

    // Keyword list
    List<Keyword> list = new ArrayList<Keyword>();

    /***** Get the keyword list for the Active Trace tab ******
     * // Get the keyword list of trace results
     * int count = this.getTabCount ();
     * for (int i = 0; i <count; i ++) {
     * Component comp = this.getComponentAt (i);
     * if (comp instanceof TraceResultPanel) {
     * TraceResultPanel panel = (TraceResultPanel) comp;
     * Keyword [] words = panel.getTraceKeywords ();
     * if (words! = null) {
     * list.addAll (Arrays.asList (words));
     * }
     * }
     * }
     ****************************************/
    IAnalisysComponent comp = getSelectedPanel();
    if (comp instanceof TraceResultPanel) {
      TraceResultPanel panel = (TraceResultPanel) comp;
      Keyword[] words = panel.getTraceKeywords();
      if (words != null) {
        list.addAll(Arrays.asList(words));
      }
    }
    if (list.size() <= 0) return null;

    return list.toArray(new Keyword[0]);
  }

  /**
   * Get the trace panel model
   *
   * @return Trace panel model
   */
  public TraceResultModel[] getTraceResultModels() {

    List<TraceResultModel> list = new ArrayList<TraceResultModel>();
    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      Component comp = this.getComponentAt(i);
      if (comp instanceof TraceResultPanel) {
        TraceResultPanel panel = (TraceResultPanel) comp;
        TraceResultModel panelModel = panel.getModel();
        // Get a non-blank model
        if (panelModel.getTraceWord() != null && panelModel.getRootBlock() != null) {
          list.add(panelModel);
        }
      }
    }
    if (list.size() <= 0) return null;

    return list.toArray(new TraceResultModel[0]);
  }

  /**
   * Get the keyword list of search results
   *
   * @return Search keyword list
   */
  public Keyword[] getSearchKeywords() {

    // Keyword list
    List<Keyword> list = new ArrayList<Keyword>();

    // Get the keyword list of search results
    int count = this.getTabCount();
    for (int i = 0; i < count; i++) {
      Component comp = this.getComponentAt(i);
      if (comp instanceof SearchResultPanel) {
        SearchResultPanel panel = (SearchResultPanel) comp;
        Keyword[] words = panel.getSearchKeywords();
        if (words != null) {
          list.addAll(Arrays.asList(words));
        }
      }
    }
    if (list.size() <= 0) return null;

    return list.toArray(new Keyword[0]);
  }

  /**
   * Analysis tab change event
   *
   * @param action Tab change action
   */
  public void addTabChangeListener(AnalysisTabChangeAction action) {
    this.addChangeListener(action);
  }

  /** Performs processing associated with changing the analysis tab. */
  public void changeAnalisysTab() {
    ChangeListener[] listeners = this.getChangeListeners();
    if (listeners == null || listeners.length <= 0) return;
    for (ChangeListener l : listeners) {
      if (l instanceof AnalysisTabChangeAction) {
        ((AnalysisTabChangeAction) l).changeAnalisysTab();
      }
    }
  }

  /**
   * Set profiler pop-up menu
   *
   * @param profilerPopup Profiler pop-up menu
   */
  public void setProfilerPopupMenu(ProfilerPopupMenu profilerPopup) {
    /** Profiler panel */
    for (ProfilerTablePanel panel : this.panelProfilerList) {
      panel.setPopupMenu(profilerPopup);
    }
  }

  /**
   * Get a profiler model
   *
   * @return Profiler model
   */
  public ProfilerTableBaseModel[] getProfilerModels() {
    List<ProfilerTableBaseModel> list = new ArrayList<ProfilerTableBaseModel>();
    /** Profiler panel */
    for (ProfilerTablePanel panel : this.panelProfilerList) {
      if (panel.getModel() != null) {
        list.add(panel.getModel());
      }
    }
    if (list.size() <= 0) return null;
    return list.toArray(new ProfilerTableBaseModel[0]);
  }

  /** Clear profiler information */
  public void clearProfilerInfo() {
    /** Profiler panel */
    for (ProfilerTablePanel panel : this.panelProfilerList) {
      if (panel.getModel() != null) {
        panel.clearModel();
      }
    }
  }

  /** Copy the selection to the clipboard. */
  public void copyClipboard() {
    IAnalisysComponent panel = getSelectedPanel();
    panel.copyClipboard();
  }

  /**
   * Close the designated analysis information panel.
   *
   * @param panel Close Analysis Information Panel
   */
  public void closeTab(ANALYSIS_PANEL panel) {
    int count = this.getTabCount();
    for (int i = count - 1; i >= 0; i--) {
      Component comp = this.getComponentAt(i);
      if (comp instanceof IAnalisysComponent) {
        if (((IAnalisysComponent) comp).getEnumPanel() == panel) {
          this.closeTab(i);
        }
      }
    }
  }
}
