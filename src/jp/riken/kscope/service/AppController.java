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
package jp.riken.kscope.service;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import javax.swing.JOptionPane;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.action.AnalysisVariableAction;
import jp.riken.kscope.action.WindowProgressAction;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.common.FILTER_TYPE;
import jp.riken.kscope.common.PROFILERINFO_TYPE;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.ErrorInfo;
import jp.riken.kscope.data.Keyword;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.dialog.ProfilerLegendDialog;
import jp.riken.kscope.gui.AnalysisView;
import jp.riken.kscope.gui.ConsolePanel;
import jp.riken.kscope.gui.ExploreView;
import jp.riken.kscope.gui.IAnalisysComponent;
import jp.riken.kscope.gui.ISourceBargraph;
import jp.riken.kscope.gui.LanguageTreePanel;
import jp.riken.kscope.gui.MainFrame;
import jp.riken.kscope.gui.ProfilerTablePanel;
import jp.riken.kscope.gui.SourceView;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.FileTreeModel;
import jp.riken.kscope.model.InformationModel;
import jp.riken.kscope.model.LanguageTreeModel;
import jp.riken.kscope.model.ModuleTreeModel;
import jp.riken.kscope.model.OperandTableModel;
import jp.riken.kscope.model.ProjectModel;
import jp.riken.kscope.model.PropertiesTableModel;
import jp.riken.kscope.model.ReferenceModel;
import jp.riken.kscope.model.RequiredBFModel;
import jp.riken.kscope.model.ScopeModel;
import jp.riken.kscope.model.SearchResultModel;
import jp.riken.kscope.model.VariableTableModel;
import jp.riken.kscope.profiler.ProfilerInfo;
import jp.riken.kscope.properties.ApplicationProperties;
import jp.riken.kscope.properties.KeywordProperties;
import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.properties.OperationProperties;
import jp.riken.kscope.properties.ProfilerProperties;
import jp.riken.kscope.properties.ProgramProperties;
import jp.riken.kscope.properties.ProjectProperties;
import jp.riken.kscope.properties.RemoteBuildProperties;
import jp.riken.kscope.properties.RequiredBFProperties;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.properties.VariableMemoryProperties;

/**
 * Application controller class
 *
 * @author RIKEN
 */
public class AppController implements PropertyChangeListener {

  /** main frame */
  private MainFrame mainframe;

  /** Project information */
  private ProjectModel projectModel;

  /** Project settings */
  private ProjectProperties propertiesProject;

  /** Source view settings */
  private SourceProperties propertiesSource;

  /** Highlight settings */
  private KeywordProperties propertiesKeyword;

  /** External tool settings properties */
  private ProgramProperties propertiesProgram;

  /** Arithmetic count property */
  private OperationProperties propertiesOperand;

  /** Profiler Properties */
  private ProfilerProperties propertiesProfiler;

  /** Request Byte / FLOP configuration property */
  private RequiredBFProperties propertiesMemory;

  // Remote build properties
  private RemoteBuildProperties rb_properties = null;

  /** Application Properties */
  private ApplicationProperties propertiesApplication;

  /** Variable access destination memory setting */
  private VariableMemoryProperties propertiesVariable;

  /** Fortran database */
  private Fortran fortranLanguage;

  /** Thread task */
  private FutureService<Integer> threadFuture;

  /** Structure tree filter */
  private List<FILTER_TYPE> listLanguageFilter;

  /** Last access folder */
  private String lastAccessFolder;

  /** Profiler information class */
  private ProfilerInfo profilerInfo;

  /** Final variable characteristic list information block */
  private List<IBlock> lastBlocks;

  /** Final variable characteristic list Information variable list */
  private List<VariableDefinition> lastVars;

  /** Variable characteristic list action (for update) */
  private AnalysisVariableAction actionVariable = null;

  /**
   * Constructor
   *
   * @throws Exception
   */
  public AppController() throws Exception {
    if (this.rb_properties == null) {
      this.rb_properties = new RemoteBuildProperties();
    }
  }

  /**
   * Initialize.
   *
   * @throws Exception Initial startup error
   */
  public void initialize() throws Exception {
    // Initialize.
    initialize(this.mainframe);
  }

  /**
   * Initialize.
   *
   * @param frame mainframe
   * @throws Exception Initial startup error
   */
  public void initialize(MainFrame frame) throws Exception {
    this.mainframe = frame;

    // Project information
    projectModel = new ProjectModel();

    /** Generate property settings: Do not clear if created. */
    createProperties(false);

    /** Register property setting change listener */
    // Explorer View: Source View Settings Properties
    {
      ExploreView view = mainframe.getPanelExplorerView();
      propertiesSource.addPropertyChangeListener(view);
    }
    // Source View: Source View Settings Properties
    {
      SourceView view = mainframe.getPanelSourceView();
      propertiesSource.addPropertyChangeListener(view);
      propertiesKeyword.addPropertyChangeListener(view);
      propertiesProfiler.addPropertyChangeListener(view);
      propertiesMemory.addPropertyChangeListener(view);
      propertiesVariable.addPropertyChangeListener(view);
    }

    // Additional information model: External tool setting properties
    InformationModel modelInformation = this.getInformationModel();
    if (modelInformation != null) {
      propertiesProgram.addPropertyChangeListener(modelInformation);
      modelInformation.setPropertiesExtension(propertiesProgram);
    }

    // Trace Panel: Source View Settings Properties
    AnalysisView analysis = mainframe.getPanelAnalysisView();
    propertiesSource.addPropertyChangeListener(analysis);
    propertiesSource.firePropertyChange();
    // Profiler Panel: Profiler Properties
    propertiesProfiler.addPropertyChangeListener(analysis);
    propertiesProfiler.firePropertyChange();

    // Structure tree filter default settings
    FILTER_TYPE[] filters = KscopeProperties.LANGUGE_DEFAULTFILTERS;
    this.setListLanguageFilter(filters);

    // Create a Fortran database
    fortranLanguage = new Fortran();
    // Profiler information class
    profilerInfo = new ProfilerInfo();

    // Close the profiler legend dialog
    ProfilerLegendDialog dialog = this.getMainframe().getDialogProfilerLegend();
    if (dialog != null) {
      dialog.setVisible(false);
    }

    // Final variable characteristic list information
    lastBlocks = null;
    lastVars = null;
  }

  /**
   * Generate property settings.
   *
   * @param clear true = Clear property settings
   * @throws Exception Property read error
   */
  private void createProperties(boolean clear) throws Exception {
    // Clear property settings
    if (clear) {
      clearProperties();
    }
    /** Property settings */
    if (this.propertiesProject == null) {
      this.propertiesProject = new ProjectProperties();
    }
    if (this.propertiesSource == null) {
      this.propertiesSource = new SourceProperties();
    }
    if (this.propertiesKeyword == null) {
      this.propertiesKeyword = new KeywordProperties();
    }
    if (this.propertiesProgram == null) {
      this.propertiesProgram = new ProgramProperties();
    }
    if (this.propertiesOperand == null) {
      this.propertiesOperand = new OperationProperties();
    }
    if (this.propertiesApplication == null) {
      this.propertiesApplication = new ApplicationProperties();
    }

    // Copy the menu display selection
    this.mainframe.getMenuMain().clearSelectedMenu();
    if (this.propertiesProfiler == null) {
      this.propertiesProfiler = new ProfilerProperties(this.propertiesProfiler);
    } else {
      this.propertiesProfiler.setVisibleProperties(this.propertiesProfiler);
    }
    // Bar graph color settings
    PROFILERINFO_TYPE.setProfilerProperties(this.propertiesProfiler);
    // Request Byte / FLOP setting property: If it has been generated to keep the default setting,
    // do not new.
    if (this.propertiesMemory == null) {
      this.propertiesMemory = new RequiredBFProperties();
      // Set the default settings.
      RequiredBFProperties defaultProperties = new RequiredBFProperties();
      this.propertiesMemory.setDefaultProperties(defaultProperties);
    }
    // Request Byte / FLOP variable setting data
    this.propertiesVariable = new VariableMemoryProperties(this.propertiesMemory);
  }

  /**
   * Get the mainframe
   *
   * @return mainframe mainframe
   */
  public MainFrame getMainframe() {
    return mainframe;
  }

  /**
   * Get project settings
   *
   * @return Project settings
   */
  public ProjectProperties getPropertiesProject() {
    return propertiesProject;
  }

  /**
   * Get source view settings.
   *
   * @return Source view settings
   */
  public SourceProperties getPropertiesSource() {
    return propertiesSource;
  }

  /**
   * Get highlight settings
   *
   * @return highlight settings
   */
  public KeywordProperties getPropertiesKeyword() {
    return propertiesKeyword;
  }

  /**
   * Get external tool settings properties
   *
   * @return External tool settings properties
   */
  public ProgramProperties getPropertiesExtension() {
    return propertiesProgram;
  }

  /**
   * Get the operation count property
   *
   * @return operation count property
   */
  public OperationProperties getPropertiesOperation() {
    return this.propertiesOperand;
  }

  /**
   * Get profiler count property
   *
   * @return Profiler properties
   */
  public ProfilerProperties getPropertiesProfiler() {
    return this.propertiesProfiler;
  }

  /**
   * Get the request Byte / FLOP configuration property
   *
   * @return Request Byte / FLOP configuration property
   */
  public RequiredBFProperties getPropertiesMemory() {
    return this.propertiesMemory;
  }

  /**
   * Get application properties
   *
   * @return application properties
   */
  public ApplicationProperties getPropertiesApplication() {
    return this.propertiesApplication;
  }

  /**
   * Get variable access destination memory
   *
   * @return Variable access destination memory
   */
  public VariableMemoryProperties getPropertiesVariable() {
    return propertiesVariable;
  }

  /**
   * Get RemoteBuildProperties properties
   *
   * @return RemoteBuildProperties
   */
  public RemoteBuildProperties getRBproperties() {
    return rb_properties;
  }

  /**
   * Get project information
   *
   * @return projectModel Project information
   */
  public ProjectModel getProjectModel() {
    return projectModel;
  }

  /** Update property settings. */
  public void updateProperties() {
    // Project settings
    propertiesProject.firePropertyChange();
    // Source view settings
    propertiesSource.firePropertyChange();
    // Highlight settings
    propertiesKeyword.firePropertyChange();
    // External tool settings properties
    propertiesProgram.firePropertyChange();
    // Profiler configuration properties
    propertiesProfiler.firePropertyChange();
    // Request Byte / FLOP configuration property
    propertiesMemory.firePropertyChange();
    // Variable access destination memory setting
    propertiesVariable.firePropertyChange();

    return;
  }

  /**
   * Get additional information model
   *
   * @return Additional information model
   */
  public InformationModel getInformationModel() {
    if (this.mainframe.getPanelAnalysisView() == null) return null;
    if (this.mainframe.getPanelAnalysisView().getPanelInformation() == null) return null;
    return this.mainframe.getPanelAnalysisView().getPanelInformation().getModel();
  }

  /**
   * Get the property table model
   *
   * @return property table model
   */
  public PropertiesTableModel getPropertiesTableModel() {
    if (this.mainframe.getPanelAnalysisView() == null) return null;
    if (this.mainframe.getPanelAnalysisView().getPanelPropertiesTable() == null) return null;
    return this.mainframe.getPanelAnalysisView().getPanelPropertiesTable().getModel();
  }

  /**
   * Get the error information model
   *
   * @return Error information model
   */
  public ErrorInfoModel getErrorInfoModel() {
    if (this.mainframe.getPanelAnalysisView() == null) return null;
    if (this.mainframe.getPanelAnalysisView().getPanelError() == null) return null;
    // Error information model
    ErrorInfoModel model = this.mainframe.getPanelAnalysisView().getPanelError().getModel();
    if (model == null) return null;

    // Set the project folder
    if (this.getProjectModel() != null) {
      model.setProjectFolder(this.getProjectModel().getProjectFolder());
    }
    return model;
  }

  /**
   * Get variable characteristic information list model
   *
   * @return Variable characteristic information list model
   */
  public VariableTableModel getVariableTableModel() {
    // Variable characteristic information list model
    VariableTableModel model = this.mainframe.getPanelAnalysisView().getPanelVariable().getModel();
    return model;
  }

  /**
   * Get reference list model
   *
   * @return Reference list model
   */
  public ReferenceModel getReferenceModel() {
    // Reference list model
    ReferenceModel model = this.mainframe.getPanelAnalysisView().getPanelReference().getModel();
    return model;
  }

  /**
   * Get the search result model
   *
   * @return Search result model
   */
  public SearchResultModel getSearchResultModel() {
    // Search list model
    SearchResultModel model =
        this.mainframe.getPanelAnalysisView().getPanelSearchResult().getModel();
    return model;
  }

  /**
   * Get the variable scope model
   *
   * @return Variable scope model
   */
  public ScopeModel getScopeModel() {
    // Variable scope model
    ScopeModel model = this.mainframe.getPanelAnalysisView().getPanelScope().getModel();
    return model;
  }

  /**
   * Get the Fortran database
   *
   * @return Fortran database
   */
  public Fortran getFortranLanguage() {
    return fortranLanguage;
  }

  /**
   * Set up a Fortran database
   *
   * @param value Fortran database
   */
  public void setFortranLanguage(Fortran value) {
    this.fortranLanguage = value;
  }

  /**
   * View source files in Source Files view
   *
   * @param file Source file
   * @throws Exception File open error
   */
  public void openSourceFile(SourceFile file) throws Exception {
    openSourceFile(new CodeLine(file, file.getPath()));
  }

  /**
   * View source files in Source Files view
   *
   * @param line source file
   * @throws Exception File open error
   */
  public void openSourceFile(CodeLine line) throws Exception {

    // open the source file
    this.getMainframe().getPanelSourceView().viewSource(line);

    // Apply property settings such as keyword settings.
    this.updateProperties();

    // Set profile bar graph to source view
    setProfilerBargraph();
  }

  /**
   * Select and display the specified line in the source file view
   *
   * @param lines Select display line information
   */
  public void setSelectedBlock(CodeLine[] lines) {
    // open the source file
    this.getMainframe().getPanelSourceView().setSelectedBlock(lines);
  }

  /**
   * Get the structure tree model
   *
   * @return Structural tree model
   */
  public LanguageTreeModel getLanguageTreeModel() {
    // Structural panel
    LanguageTreePanel panel = this.mainframe.getPanelExplorerView().getPanelLanguageTree();
    if (panel == null) {
      // Create a structure panel because it does not exist
      this.mainframe.getPanelExplorerView().createLanguageTreePanel();
      // Apply the filter
      applyLanguageTreeFilter();
    }
    // Structural tree model
    LanguageTreeModel model =
        this.mainframe.getPanelExplorerView().getPanelLanguageTree().getModel();

    return model;
  }

  /**
   * Get the module tree model
   *
   * @return Module tree model
   */
  public ModuleTreeModel getModuleTreeModel() {
    // Module tree model
    ModuleTreeModel model = this.mainframe.getPanelExplorerView().getPanelModuleTree().getModel();

    return model;
  }

  /**
   * Get the source tree model
   *
   * @return Sourcetree model
   */
  public FileTreeModel getSourceTreeModel() {
    // Error information model
    FileTreeModel model = this.mainframe.getPanelExplorerView().getPanelSourceTree().getModel();

    // Set the project folder
    if (this.getProjectModel() != null) {
      model.setProjectFolder(this.getProjectModel().getProjectFolder());
    }
    return model;
  }

  /**
   * Get the XML tree model
   *
   * @return Xml tree model
   */
  public FileTreeModel getXmlTreeModel() {
    // Error information model
    FileTreeModel model = this.mainframe.getPanelExplorerView().getPanelXmlTree().getModel();

    // Set the project folder
    if (this.getProjectModel() != null) {
      model.setProjectFolder(this.getProjectModel().getProjectFolder());
    }
    return model;
  }

  /** Clear the Fortran database */
  public void clearFortranLanguage() {
    this.fortranLanguage = new Fortran();
    // Clear variable access destination memory property
    this.propertiesVariable.clearVariableMemory();
  }

  /**
   * Receive a notification of the end of the thread task and display a message box. <br/>
   * <pre>
   * Exit code: SUCCESS_RESULT = Normal termination
   * CANCEL_RESULT = Interruption due to cancellation
   * CANCEL_RESULT = Abnormal termination
   * null = end without message box
   * </ pre>
   * @param result Exit code
   */
  public void finishThreadFuture(Integer result) {

    // Close the progress dialog
    WindowProgressAction progress = new WindowProgressAction(this);
    progress.closeProgressDialog();
    // Clear the progress bar
    Application.status.setProgressStart(false);

    // console
    ConsolePanel console = this.getMainframe().getPanelAnalysisView().getPanelConsole();
    if (console != null) {
      console.flush();
    }

    if (result == null) {
      return;
    }

    if (result == Constant.ERROR_RESULT) {
      String errmsg = null;
      if (this.threadFuture != null) {
        errmsg = this.threadFuture.getMessage();
      }
      // Exited due to an error.
      String msg = Message.getString("appcontroller.thread.message.error");
      if (errmsg != null) {
        msg += "\n[" + errmsg + "]";
      }
      JOptionPane.showMessageDialog(
          this.mainframe,
          msg,
          Message.getString("appcontroller.thread.title"), // Processing message
          JOptionPane.INFORMATION_MESSAGE);
    } else if (result == Constant.CANCEL_RESULT) {
      JOptionPane.showMessageDialog(
          this.mainframe,
          Message.getString(
              "appcontroller.thread.message.cancel"), // It was interrupted due to cancellation.
          Message.getString("appcontroller.thread.title"), // Processing message
          JOptionPane.INFORMATION_MESSAGE);
    } else if (result != null) {
      JOptionPane.showMessageDialog(
          this.mainframe,
          Message.getString("appcontroller.thread.message.success"), // Execution finished.
          Message.getString("appcontroller.thread.title"), // Processing message
          JOptionPane.INFORMATION_MESSAGE);
    }
  }

  /**
   * Get a thread task
   *
   * @return threadFuture Thread task
   */
  public FutureService<Integer> getThreadFuture() {
    return threadFuture;
  }

  /**
   * Set up a thread task
   *
   * @param threadFuture Thread task
   */
  public void setThreadFuture(FutureService<Integer> threadFuture) {
    this.threadFuture = threadFuture;
  }

  /**
   * Property change notification. <br>
   * Notify and receive thread task end notification by PropertyChangeSupport
   *
   * @param event Event information
   */
  @Override
  public void propertyChange(PropertyChangeEvent event) {

    if (Constant.PROPERTYNAME_THREADDONE.equals(event.getPropertyName())) {
      // Thread end event
      Integer result = (Integer) event.getNewValue();

      // Receive the thread task end notification and display a message box
      finishThreadFuture(result);
    }
  }

  /**
   * Check the end status of thread tasks
   *
   * @return true = thread task finished
   */
  public boolean isThreadTaskDone() {
    if (this.threadFuture == null) {
      return true;
    }
    return this.threadFuture.isDone();
  }

  /**
   * Activate the Analyze View Specify tab
   *
   * @param panel Selection tab
   */
  public void setSelectedAnalysisPanel(ANALYSIS_PANEL panel) {
    this.getMainframe().getPanelAnalysisView().setSelectedPanel(panel);
  }

  /**
   * Get the arithmetic count table model
   *
   * @return Arithmetic count table model
   */
  public OperandTableModel getOperandTableModel() {
    // Arithmetic count table model
    OperandTableModel model = this.mainframe.getPanelAnalysisView().getPanelOperand().getModel();

    return model;
  }

  /**
   * Get the request Byte / FLOP calculation result model
   *
   * @return Request Byte / FLOP calculation result model
   */
  public RequiredBFModel getRequiredByteFlopModel() {
    // Request Byte / FLOP calculation result model
    RequiredBFModel model =
        this.mainframe.getPanelAnalysisView().getPanelRequiredByteFlop().getModel();

    return model;
  }

  /**
   * Clear the project
   *
   * @throws Exception Project clear error
   */
  public void clearProject() throws Exception {
    // Generate property settings
    boolean clear = false;

    // Since clear is set to false when TODO Project Open, the default property is not deleted.
    // IsValidProject is false because project_folder is null in Default (K-scope reads default
    // parameter when started).
    if (this.projectModel != null && this.projectModel.isVaildProject()) {
      clear = true;
    }
    createProperties(clear);
    // Initialize
    initialize();
  }

  /** Clear properties */
  private void clearProperties() {
    /** Clear property settings: Generate with initialize */
    this.propertiesProject = null;
    this.propertiesSource = null;
    this.propertiesKeyword = null;
    this.propertiesProgram = null;
    this.propertiesOperand = null;
    this.propertiesApplication = null;
    this.propertiesProfiler = null;
    this.propertiesMemory = null;
    this.propertiesVariable = null;
    // this.propertiesSSH = null;
  }

  public void setRBproperties(RemoteBuildProperties rb_properties) {
    this.rb_properties = rb_properties;
  }

  /** Set trace keywords */
  public void setTraceKeywords() {
    // Get the keyword list
    Keyword[] words = this.getMainframe().getPanelAnalysisView().getTraceKeywords();
    // Set keywords
    this.getMainframe().getPanelSourceView().setSearchWords(words);
  }

  /** Set search keywords */
  public void setSearchKeywords() {
    // Get the keyword list
    Keyword[] words = this.getMainframe().getPanelAnalysisView().getSearchKeywords();
    // Set keywords
    this.getMainframe().getPanelSourceView().setSearchWords(words);
  }

  /**
   * Get the structure tree filter list
   *
   * @return Structure tree filter list
   */
  public List<FILTER_TYPE> getListLanguageFilter() {
    return listLanguageFilter;
  }

  /**
   * Set the structure tree filter list
   *
   * @param list Structure tree filter list
   */
  public void setListLanguageFilter(FILTER_TYPE[] list) {
    this.listLanguageFilter = new ArrayList<FILTER_TYPE>();
    this.listLanguageFilter.addAll(java.util.Arrays.asList(list));

    // Set a filter in the structure tree
    applyLanguageTreeFilter();
  }

  /**
   * Add a structure tree filter
   *
   * @param filter Structure tree filter
   */
  public void addListLanguageFilter(FILTER_TYPE filter) {
    if (filter == null) return;
    if (this.listLanguageFilter == null) {
      this.listLanguageFilter = new ArrayList<FILTER_TYPE>();
    }
    if (this.listLanguageFilter.contains(filter)) return;
    this.listLanguageFilter.add(filter);

    // Set a filter in the structure tree
    applyLanguageTreeFilter();
  }

  /**
   * Remove the structure tree filter
   *
   * @param filter Structure tree filter
   */
  public void removeListLanguageFilter(FILTER_TYPE filter) {
    if (filter == null) return;
    if (this.listLanguageFilter == null) return;
    if (this.listLanguageFilter.size() <= 0) return;

    // Delete filter
    this.listLanguageFilter.remove(filter);

    // Set a filter in the structure tree
    applyLanguageTreeFilter();
  }

  /** Apply a filter to the structure tree */
  public void applyLanguageTreeFilter() {
    FILTER_TYPE[] filters = null;
    if (this.listLanguageFilter != null) {
      filters = this.listLanguageFilter.toArray(new FILTER_TYPE[0]);
    }
    this.mainframe.getPanelExplorerView().setLanguageTreeFilter(filters);
  }

  /**
   * Get the last access folder
   *
   * @return Last access folder
   */
  public String getLastAccessFolder() {
    return lastAccessFolder;
  }

  /**
   * Set the last access folder
   *
   * @param folder Last access folder
   */
  public void setLastAccessFolder(File folder) {
    if (folder == null) {
      this.lastAccessFolder = null;
      return;
    }
    // Check if it is a folder
    if (folder.isDirectory()) {
      this.lastAccessFolder = folder.getAbsolutePath();
    } else {
      this.lastAccessFolder = folder.getParent();
    }
  }

  /**
   * Set the last access folder
   *
   * @param folder Last access folder
   */
  public void setLastAccessFolder(String folder) {
    this.lastAccessFolder = folder;
  }

  /**
   * Profiler information class
   *
   * @return Profiler information class
   */
  public ProfilerInfo getProfilerInfo() {
    return profilerInfo;
  }

  /**
   * Profiler information class
   *
   * @param info Profiler information class
   */
  public void setProfilerInfo(ProfilerInfo info) {
    this.profilerInfo = info;
  }

  /** Set profile bar graph to source view */
  public void setProfilerBargraph() {
    PROFILERINFO_TYPE type = null;
    // Get the display profiler information type from the View tab
    IAnalisysComponent panel = this.getMainframe().getPanelAnalysisView().getSelectedPanel();
    if (panel == null) return;
    if (panel.getEnumPanel() == ANALYSIS_PANEL.COST_PROCEDURE) {
      type = PROFILERINFO_TYPE.COST_PROCEDURE;
    } else if (panel.getEnumPanel() == ANALYSIS_PANEL.COST_LOOP) {
      type = PROFILERINFO_TYPE.COST_LOOP;
    } else if (panel.getEnumPanel() == ANALYSIS_PANEL.COST_LINE) {
      type = PROFILERINFO_TYPE.COST_LINE;
    }
    if (type == null) return;

    this.setProfilerBargraph(type);

    return;
  }

  /**
   * Set profile bar graph to source view
   *
   * @param type Profiler information type
   */
  public void setProfilerBargraph(PROFILERINFO_TYPE type) {
    if (type == null) return;
    ProfilerTablePanel panel = this.mainframe.getPanelAnalysisView().getSelectedProfilerCostPanel();
    if (panel == null) return;
    if (panel.getModel() == null) return;
    ISourceBargraph[] bargraph = panel.getModel().getSelectedBargraph();
    if (bargraph == null) return;
    this.mainframe.getPanelSourceView().setProfilerBargraph(bargraph);

    return;
  }

  /** Clear profiler information */
  public void clearProfilerInfo() {
    if (this.profilerInfo != null) {
      this.profilerInfo.clearProfilerData();
    }
    this.mainframe.getPanelAnalysisView().clearProfilerInfo();
    this.mainframe.getPanelSourceView().clearBargraphData();
  }

  /**
   * Get final variable characteristic list information
   *
   * @return Block of selection tree
   */
  public List<IBlock> getLastVariableBlocks() {
    return lastBlocks;
  }

  /**
   * Get final variable characteristic list information
   *
   * @return Variable list
   */
  public List<VariableDefinition> getLastVariableVars() {
    return lastVars;
  }

  /**
   * Set the final variable characteristic list
   *
   * @param blocks List of final variable characteristics: blocks
   * @param vars Final variable characteristic list: Variable list
   */
  public void setLastVariable(List<IBlock> blocks, List<VariableDefinition> vars) {
    lastBlocks = blocks;
    lastVars = vars;
  }

  /**
   * Set variable characteristic list action
   *
   * @param actionVar Variable characteristic list Action
   */
  public void setActionVariable(AnalysisVariableAction actionVar) {
    this.actionVariable = actionVar;
  }

  /**
   * Update the analysis view of the additional information display by editing the additional
   * information. Update the variable characteristic information list.
   */
  public void refreshInformation() {
    if (this.actionVariable != null) {
      this.actionVariable.refresh();
    }
    // Request Byte / FLOP calculation result model
    RequiredBFModel requiredModel = this.getRequiredByteFlopModel();
    requiredModel.notifyModel();
  }

  /**
   * Set error information
   *
   * @param error Error information
   */
  public void setErrorInfo(ErrorInfo error) {
    // Error information model
    ErrorInfoModel errorModel = this.getErrorInfoModel();
    errorModel.addErrorInfo(error);
  }
}
