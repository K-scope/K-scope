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

package jp.riken.kscope.menu;

import java.awt.event.ActionListener;
import java.util.EventListener;
import java.util.List;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;
import javax.swing.tree.DefaultMutableTreeNode;
import jp.riken.kscope.Message;
import jp.riken.kscope.action.ActionBase;
import jp.riken.kscope.action.AllAnalysisMemoryAction;
import jp.riken.kscope.action.AnalysisMemoryAction;
import jp.riken.kscope.action.AnalysisOperandAction;
import jp.riken.kscope.action.AnalysisReferenceAction;
import jp.riken.kscope.action.AnalysisScopeAction;
import jp.riken.kscope.action.AnalysisTraceAction;
import jp.riken.kscope.action.AnalysisVariableAction;
import jp.riken.kscope.action.EditClipboardCopyAction;
import jp.riken.kscope.action.EditInformationEditAction;
import jp.riken.kscope.action.ErrorOpenFileAction;
import jp.riken.kscope.action.FileExitAction;
import jp.riken.kscope.action.FileExportAnalysisAction;
import jp.riken.kscope.action.FileExportExploreAction;
import jp.riken.kscope.action.FileExportSourceFileAction;
import jp.riken.kscope.action.FileOpenSourceFileAction;
import jp.riken.kscope.action.FileProjectCloseAction;
import jp.riken.kscope.action.FileProjectNewAction;
import jp.riken.kscope.action.FileProjectOpenAction;
import jp.riken.kscope.action.FileProjectSaveAction;
import jp.riken.kscope.action.HelpVersionAction;
import jp.riken.kscope.action.ProfilerAddEprofAction;
import jp.riken.kscope.action.ProfilerClearAction;
import jp.riken.kscope.action.ProfilerOpenFileAction;
import jp.riken.kscope.action.ProfilerSaveFileAction;
import jp.riken.kscope.action.ProfilerSaveFolderAction;
import jp.riken.kscope.action.ProfilerViewBargraphAction;
import jp.riken.kscope.action.ProfilerViewRulerAction;
import jp.riken.kscope.action.ProjectAddFileAction;
import jp.riken.kscope.action.ProjectAddFolderAction;
import jp.riken.kscope.action.ProjectBuildAction;
import jp.riken.kscope.action.ProjectClearLanguageAction;
import jp.riken.kscope.action.ProjectDeleteFileAction;
import jp.riken.kscope.action.ProjectPropertyAction;
import jp.riken.kscope.action.ProjectRebuildAction;
import jp.riken.kscope.action.ProjectSettingKeywordAction;
import jp.riken.kscope.action.ProjectSettingOperationAction;
import jp.riken.kscope.action.ProjectSettingProfilerAction;
import jp.riken.kscope.action.ProjectSettingProjectAction;
import jp.riken.kscope.action.ProjectSettingRequiredBFAction;
import jp.riken.kscope.action.ProjectSettingToolsAction;
import jp.riken.kscope.action.ProjectSettingViewAction;
import jp.riken.kscope.action.SearchFindAction;
import jp.riken.kscope.action.SearchGrepAction;
import jp.riken.kscope.action.SearchResultAction;
import jp.riken.kscope.action.SearchTreeAction;
import jp.riken.kscope.action.ThreadCancelAction;
import jp.riken.kscope.action.TreeCollapseAllAction;
import jp.riken.kscope.action.TreeExpandAllAction;
import jp.riken.kscope.action.TreeExpandSelectAction;
import jp.riken.kscope.action.ViewCloseAction;
import jp.riken.kscope.action.ViewLangugeFilterAction;
import jp.riken.kscope.action.ViewOpenAnalysisLineAction;
import jp.riken.kscope.action.ViewOpenExploreBlockAction;
import jp.riken.kscope.action.ViewOpenLanguageTreeAction;
import jp.riken.kscope.action.WindowProfilerLegendAction;
import jp.riken.kscope.action.WindowProgressAction;
import jp.riken.kscope.action.WindowSourceAction;
import jp.riken.kscope.action.WindowViewAction;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.common.FILTER_TYPE;
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.common.TRACE_DIR;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.model.LanguageTreeModel;
import jp.riken.kscope.model.TraceResultModel;
import jp.riken.kscope.properties.ProfilerProperties;
import jp.riken.kscope.service.AppController;

/**
 * Main menu bar class
 *
 * @author RIKEN
 */
public class MainMenu extends JMenuBar implements MenuListener {

  /** Serial number */
  private static final long serialVersionUID = 1L;
  /** Project menu */
  private JMenu menuProject;
  /** Project: XML file deletion */
  private JMenuItem menuProjectDeleteXmlFile;
  /** Structural analysis cancel menu */
  private JMenuItem menuProjectCancel;
  /** Window menu */
  private JMenu menuWindow;
  /** Window: Progress */
  private JMenuItem menuWindowProgress;
  /** Window: Explorer View */
  private JMenu menuWindowExplore;
  /** Window: Source view */
  private JMenu menuWindowSource;
  /** Application controller */
  private AppController controller;
  /** Window: Explorer View: Structure */
  private JMenuItem menuWindowExploreLanguage;
  /** Window: Analysis View: Trace */
  private JMenuItem menuWindowAnalysisTrace;
  /** Window: Analysis view */
  private JMenu menuWindowAnalysis;
  /** Display */
  private JMenu menuView;
  /** Display: Structural filter */
  private JMenu menuViewFilter;
  /** Profile bar graph display */
  private JCheckBoxMenuItem menuProfilerBarVisibled;
  /** Profiler cost ruler display */
  private JCheckBoxMenuItem menuProfilerRulerVisibled;
  /** Additional information editing action */
  private EditInformationEditAction actionEditInformation;
  /** Analytical information export action */
  private FileExportAnalysisAction actionExportAnalysis;
  /** Action to open the error part */
  private ErrorOpenFileAction actionErrorOpenFile;
  /** Analysis result Action to open the relevant part */
  private ViewOpenAnalysisLineAction actionOpenAnalysisLine;
  /** Analysis: Variable Characteristic List Action */
  private AnalysisVariableAction actionAnalysisVariable;
  /** Analysis: Arithmetic Count Action */
  private AnalysisOperandAction actionAnalysisOperand;
  /** All storage action */
  private TreeCollapseAllAction actionTreeCollapseAll;
  /** New Structure Tree Action */
  private ViewOpenLanguageTreeAction actionOpenLanguageTree;

  /** Constructor */
  public MainMenu() {
    // Create a menu.
    initialize();
  }

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public MainMenu(AppController controller) {
    this.controller = controller;
    // Create a menu.
    initialize();
  }

  /** Create a menu. */
  private void initialize() {

    // File
    JMenu menuFile = new JMenu(Message.getString("mainmenu.file")); // File
    this.add(menuFile);
    menuFile.addMenuListener(this);

    // File: New project
    JMenuItem menuFileProjectNew =
        new JMenuItem(Message.getString("mainmenu.file.newproject")); // Create a new project
    menuFileProjectNew.addActionListener(new FileProjectNewAction(this.controller));

    menuFile.add(menuFileProjectNew);
    // File: Open the project
    JMenuItem menuFileProjectOpen =
        new JMenuItem(Message.getString("mainmenu.file.openproject")); // open the project
    menuFile.add(menuFileProjectOpen);
    menuFileProjectOpen.addActionListener(new FileProjectOpenAction(this.controller));

    // File: Close project
    JMenuItem menuFileProjectClose =
        new JMenuItem(Message.getString("mainmenu.file.closeproject")); // close the project
    menuFile.add(menuFileProjectClose);
    menuFileProjectClose.addActionListener(new FileProjectCloseAction(this.controller));

    // File: Save project
    JMenuItem menuFileProjectSave =
        new JMenuItem(Message.getString("mainmenu.file.saveproject")); // Save project
    menuFile.add(menuFileProjectSave);
    menuFileProjectSave.addActionListener(new FileProjectSaveAction(this.controller));

    // Separator
    menuFile.addSeparator();

    // File: Export
    JMenu menuFileExport = new JMenu(Message.getString("mainmenu.file.export")); // export
    menuFile.add(menuFileExport);

    // File: Export: Structural Information (TEXT)
    JMenuItem menuFileExportLanguage =
        new JMenuItem(
            Message.getString("mainmenu.file.export.structure")); // Structural information (TEXT)
    menuFileExport.add(menuFileExportLanguage);
    menuFileExportLanguage.addActionListener(new FileExportExploreAction(this.controller));

    // File: Export: Analytical information
    JMenuItem menuFileExportAnalysis =
        new JMenuItem(Message.getString("mainmenu.file.export.analysis")); // Analytical information
    menuFileExport.add(menuFileExportAnalysis);
    actionExportAnalysis = new FileExportAnalysisAction(this.controller);
    menuFileExportAnalysis.addActionListener(actionExportAnalysis);

    // File: Export: Source file
    JMenuItem menuFileExportSourceFile =
        new JMenuItem(Message.getString("mainmenu.file.export.source")); // source file
    menuFileExport.add(menuFileExportSourceFile);
    menuFileExportSourceFile.addActionListener(new FileExportSourceFileAction(controller));

    menuFile.add(menuFileExport);

    // File: Open the source file with an external tool
    JMenuItem menuFileOpenSourceFile =
        new JMenuItem(Message.getString("mainmenu.file.program")); // Open with an external tool
    menuFile.add(menuFileOpenSourceFile);
    menuFileOpenSourceFile.addActionListener(
        new FileOpenSourceFileAction(this.controller, FRAME_VIEW.EXPLORE_VIEW));

    // Separator
    menuFile.addSeparator();

    // File: Finish
    JMenuItem menuFileExit = new JMenuItem(Message.getString("mainmenu.file.close")); // end
    menuFile.add(menuFileExit);
    menuFileExit.addActionListener(new FileExitAction(this.controller));

    // Edit
    JMenu menuEdit = new JMenu(Message.getString("mainmenu.edit")); // edit
    this.add(menuEdit);
    menuEdit.addMenuListener(this);
    // Edit: Cut
    JMenuItem menuEditCut = new JMenuItem(Message.getString("mainmenu.edit.cut")); // Cut out
    menuEdit.add(menuEditCut);
    // Edit: Since the cut function is unused, set it to DisEnable.
    menuEditCut.setEnabled(false);

    // Edit: Copy
    JMenuItem menuEditCopy = new JMenuItem(Message.getString("mainmenu.edit.copy")); // copy
    menuEdit.add(menuEditCopy);
    menuEditCopy.addActionListener(
        new EditClipboardCopyAction(this.controller, FRAME_VIEW.SOURCE_VIEW));
    // Edit: Paste
    JMenuItem menuEditPaste = new JMenuItem(Message.getString("mainmenu.edit.paste")); // pasting
    menuEdit.add(menuEditPaste);
    // Edit: Since the paste function is unused, set it to DisEnable.
    menuEditPaste.setEnabled(false);

    // Separator
    menuEdit.addSeparator();

    // Edit: Edit additional information
    JMenuItem menuEditInformationEdit =
        new JMenuItem(Message.getString("mainmenu.edit.info")); // Edit additional information
    menuEdit.add(menuEditInformationEdit);
    actionEditInformation = new EditInformationEditAction(this.controller, FRAME_VIEW.EXPLORE_VIEW);
    menuEditInformationEdit.addActionListener(actionEditInformation);

    // Search
    JMenu menuSearch = new JMenu(Message.getString("mainmenu.search")); // search
    this.add(menuSearch);
    menuSearch.addMenuListener(this);
    // Search: Source search
    JMenuItem menuSearchFind =
        new JMenuItem(Message.getString("mainmenu.search.source")); // Source search
    menuSearch.add(menuSearchFind);
    menuSearchFind.addActionListener(new SearchFindAction(this.controller));
    // Search: File search ...
    JMenuItem menuSearchGrep =
        new JMenuItem(Message.getString("mainmenu.search.file")); // File search
    menuSearch.add(menuSearchGrep);
    menuSearchGrep.addActionListener(new SearchGrepAction(this.controller));
    // Search: Tree search ...
    JMenuItem menuSearchTree =
        new JMenuItem(Message.getString("mainmenu.search.tree")); // Tree search
    menuSearch.add(menuSearchTree);
    menuSearchTree.addActionListener(new SearchTreeAction(this.controller));
    // Separator
    menuSearch.addSeparator();
    // Search: Search before
    JMenuItem menuSearchUp =
        new JMenuItem(Message.getString("mainmenu.search.backward")); // Search before
    menuSearch.add(menuSearchUp);
    menuSearchUp.addActionListener(new SearchResultAction(this.controller, TRACE_DIR.UP));
    // Search: Find Next
    JMenuItem menuSearchBackward =
        new JMenuItem(Message.getString("mainmenu.search.forward")); // Find next
    menuSearch.add(menuSearchBackward);
    menuSearchBackward.addActionListener(new SearchResultAction(this.controller, TRACE_DIR.DOWN));
    // Search: Refresh
    JMenuItem menuSearchRefresh =
        new JMenuItem(Message.getString("mainmenu.search.refresh")); // Update search results
    menuSearch.add(menuSearchRefresh);
    menuSearchRefresh.addActionListener(new SearchResultAction(this.controller, TRACE_DIR.REFRESH));
    // Search: Clear search results
    JMenuItem menuSearchClear =
        new JMenuItem(Message.getString("mainmenu.search.clear")); // Clear search results
    menuSearch.add(menuSearchClear);
    menuSearchClear.addActionListener(new SearchResultAction(this.controller, TRACE_DIR.END));

    // Project
    this.menuProject = new JMenu(Message.getString("mainmenu.project")); // project
    this.add(menuProject);
    menuProject.addMenuListener(this);

    // Project: Perform structural analysis
    JMenuItem menuProjectBuild =
        new JMenuItem(
            Message.getString("mainmenu.project.startanalysis")); // Structural analysis execution
    menuProject.add(menuProjectBuild);
    menuProjectBuild.addActionListener(new ProjectBuildAction((this.controller)));

    // Project: Cancel structural analysis
    menuProjectCancel =
        new JMenuItem(
            Message.getString("mainmenu.project.endanalysis")); // Cancel structural analysis
    menuProject.add(menuProjectCancel);
    menuProjectCancel.addActionListener(new ThreadCancelAction((this.controller)));

    // Project: Clear structural analysis
    JMenuItem menuProjectClear =
        new JMenuItem(
            Message.getString("mainmenu.project.clearanalysis")); // Clear structural analysis
    menuProject.add(menuProjectClear);
    menuProjectClear.addActionListener(new ProjectClearLanguageAction((this.controller)));

    // Project: Re-execute structural analysis
    JMenuItem menuProjectRebuild =
        new JMenuItem(
            Message.getString(
                "mainmenu.project.restertanalysis")); // Re-execute structural analysis
    menuProject.add(menuProjectRebuild);
    menuProjectRebuild.addActionListener(new ProjectRebuildAction((this.controller)));

    // Separator
    menuProject.addSeparator();
    // Project: Add folder ...
    JMenuItem menuProjectAddXmlFolder =
        new JMenuItem(Message.getString("mainmenu.project.addxmlfolder")); // Add folder ...
    menuProject.add(menuProjectAddXmlFolder);
    menuProjectAddXmlFolder.addActionListener(new ProjectAddFolderAction(this.controller, null));
    // Project: Add file ...
    JMenuItem menuProjectAddXmlFile =
        new JMenuItem(Message.getString("mainmenu.project.addxmlfile")); // Add file ...
    menuProject.add(menuProjectAddXmlFile);
    menuProjectAddXmlFile.addActionListener(new ProjectAddFileAction(this.controller, null));
    // Project: Delete files ...
    this.menuProjectDeleteXmlFile =
        new JMenuItem(Message.getString("mainmenu.project.removexmlfile")); // Delete files ...
    menuProject.add(menuProjectDeleteXmlFile);
    menuProjectDeleteXmlFile.addActionListener(new ProjectDeleteFileAction(this.controller, null));
    // Separator
    menuProject.addSeparator();

    // Project: Properties
    JMenuItem menuProjectProperty =
        new JMenuItem(Message.getString("mainmenu.project.property")); // Properties
    menuProject.add(menuProjectProperty);
    menuProjectProperty.addActionListener(new ProjectPropertyAction(this.controller));

    // Project: Settings
    JMenu menuProjectSetting =
        new JMenu(Message.getString("mainmenu.project.config")); // Configuration
    menuProject.add(menuProjectSetting);

    // Project: Settings: Project
    JMenuItem menuProjectSettingProject =
        new JMenuItem(Message.getString("mainmenu.project.config.project")); // Project
    menuProjectSetting.add(menuProjectSettingProject);
    menuProjectSettingProject.addActionListener(new ProjectSettingProjectAction(this.controller));

    // Project: Settings: Keywords
    JMenuItem menuProjectSettingKeyword =
        new JMenuItem(Message.getString("mainmenu.project.config.keyword")); // keyword
    menuProjectSetting.add(menuProjectSettingKeyword);
    menuProjectSettingKeyword.addActionListener(new ProjectSettingKeywordAction(this.controller));

    // Project: Settings: Source view
    JMenuItem menuProjectSettingView =
        new JMenuItem(Message.getString("mainmenu.project.config.display")); // Source view
    menuProjectSetting.add(menuProjectSettingView);
    menuProjectSettingView.addActionListener(new ProjectSettingViewAction(this.controller));

    // Project: Settings: Calculation count
    JMenuItem menuProjectSettingOperation =
        new JMenuItem(Message.getString("mainmenu.project.config.operation")); // Calculation count
    menuProjectSetting.add(menuProjectSettingOperation);
    menuProjectSettingOperation.addActionListener(
        new ProjectSettingOperationAction(this.controller));

    // Project: Settings: External tools
    JMenuItem menuProjectSettingTools =
        new JMenuItem(Message.getString("mainmenu.project.config.program")); // External tools
    menuProjectSetting.add(menuProjectSettingTools);
    menuProjectSettingTools.addActionListener(new ProjectSettingToolsAction(this.controller));

    // Project: Settings: Profiler settings
    JMenuItem menuProjectSettingProfiler =
        new JMenuItem(Message.getString("mainmenu.project.config.profiler")); // Profiler
    menuProjectSetting.add(menuProjectSettingProfiler);
    menuProjectSettingProfiler.addActionListener(new ProjectSettingProfilerAction(this.controller));

    // Project: Settings: Request Bye / FLOP settings
    JMenuItem menuProjectSettingRequiredBF =
        new JMenuItem(
            Message.getString("mainmenu.project.config.requiredbf")); // Request Bye / FLOP
    menuProjectSetting.add(menuProjectSettingRequiredBF);
    menuProjectSettingRequiredBF.addActionListener(
        new ProjectSettingRequiredBFAction(this.controller));

    // Analysis
    JMenu menuAnalysis = new JMenu(Message.getString("mainmenu.analysis")); // analysis
    this.add(menuAnalysis);
    menuAnalysis.addMenuListener(this);

    // Analysis: List of variable characteristics
    JMenuItem menuAnalysisVariable =
        new JMenuItem(
            Message.getString(
                "mainmenu.analysis.valiableproperty")); // List of variable characteristics
    menuAnalysis.add(menuAnalysisVariable);
    actionAnalysisVariable = new AnalysisVariableAction(this.controller);
    menuAnalysisVariable.addActionListener(actionAnalysisVariable);
    this.controller.setActionVariable(actionAnalysisVariable);

    // Analysis: Calculation count
    JMenuItem menuAnalysisCount =
        new JMenuItem(Message.getString("mainmenu.analysis.operation")); // Calculation count
    actionAnalysisOperand = new AnalysisOperandAction(this.controller, FRAME_VIEW.EXPLORE_VIEW);
    menuAnalysis.add(menuAnalysisCount);
    menuAnalysisCount.addActionListener(actionAnalysisOperand);

    // Analysis: Variable scope
    JMenuItem menuAnalysisValid =
        new JMenuItem(Message.getString("mainmenu.analysis.valiablescope")); // Variable valid area
    menuAnalysis.add(menuAnalysisValid);
    menuAnalysisValid.addActionListener(
        new AnalysisScopeAction(this.controller, FRAME_VIEW.EXPLORE_VIEW));

    // Separator
    menuAnalysis.addSeparator();

    // Analysis: Declaration / Definition / Reference
    JMenuItem menuAnalysisReference =
        new JMenuItem(
            Message.getString(
                "mainmenu.analysis.dec-def-ref")); // Declaration / Definition / Reference
    menuAnalysis.add(menuAnalysisReference);
    menuAnalysisReference.addActionListener(
        new AnalysisReferenceAction(this.controller, FRAME_VIEW.EXPLORE_VIEW));

    // Separator
    menuAnalysis.addSeparator();
    // Analysis: Trace: Start
    JMenuItem menuAnalysisTraceStart =
        new JMenuItem(Message.getString("mainmenu.analysis.starttrace")); // Trace: Start
    menuAnalysis.add(menuAnalysisTraceStart);
    menuAnalysisTraceStart.addActionListener(
        new AnalysisTraceAction(this.controller, TRACE_DIR.START));

    // Analysis: Trace: Update
    JMenuItem menuAnalysisTraceRefresh =
        new JMenuItem(Message.getString("mainmenu.analysis.updatetrace")); // Trace: Update
    menuAnalysis.add(menuAnalysisTraceRefresh);
    menuAnalysisTraceRefresh.addActionListener(
        new AnalysisTraceAction(this.controller, TRACE_DIR.REFRESH));

    // Analysis: Trace: Clear
    JMenuItem menuAnalysisTraceClear =
        new JMenuItem(Message.getString("mainmenu.analysis.cleartrace")); // Trace: Clear
    menuAnalysis.add(menuAnalysisTraceClear);
    menuAnalysisTraceClear.addActionListener(
        new AnalysisTraceAction(this.controller, TRACE_DIR.END));

    // Separator
    menuAnalysis.addSeparator();

    // Analysis: Trace: Previous
    JMenuItem menuAnalysisUp =
        new JMenuItem(Message.getString("mainmenu.analysis.up")); // Trace: Up
    menuAnalysis.add(menuAnalysisUp);
    menuAnalysisUp.addActionListener(new AnalysisTraceAction(this.controller, TRACE_DIR.UP));

    // Analysis: Trace: Next
    JMenuItem menuAnalysisDown =
        new JMenuItem(Message.getString("mainmenu.analysis.down")); // Trace: Down
    menuAnalysis.add(menuAnalysisDown);
    menuAnalysisDown.addActionListener(new AnalysisTraceAction(this.controller, TRACE_DIR.DOWN));

    // Analysis: Trace: In
    JMenuItem menuAnalysisInside =
        new JMenuItem(Message.getString("mainmenu.analysis.in")); // Trace: In
    menuAnalysis.add(menuAnalysisInside);
    menuAnalysisInside.addActionListener(new AnalysisTraceAction(this.controller, TRACE_DIR.IN));

    // Analysis: Trace: Out
    JMenuItem menuAnalysisOutside =
        new JMenuItem(Message.getString("mainmenu.analysis.out")); // Trace: Out
    menuAnalysis.add(menuAnalysisOutside);
    menuAnalysisOutside.addActionListener(new AnalysisTraceAction(this.controller, TRACE_DIR.OUT));

    // Analysis: Trace: Forward
    JMenuItem menuAnalysisForward =
        new JMenuItem(Message.getString("mainmenu.analysis.forward")); // Trace: Forward
    menuAnalysis.add(menuAnalysisForward);
    menuAnalysisForward.addActionListener(
        new AnalysisTraceAction(this.controller, TRACE_DIR.FORWARD));

    // Separator
    menuAnalysis.addSeparator();

    // Analysis: Access destination setting
    JMenuItem menuAnalysisMemoryAccess =
        new JMenuItem(
            Message.getString("mainmenu.analysis.access")); // Variable access destination setting
    menuAnalysis.add(menuAnalysisMemoryAccess);
    menuAnalysisMemoryAccess.addActionListener(
        new AnalysisMemoryAction(
            this.controller,
            AnalysisMemoryAction.ACTION_MODE.ACCESS_SETTING,
            FRAME_VIEW.EXPLORE_VIEW));

    // Analysis: Request Byte / FLOP calculation
    JMenuItem menuAnalysisMemoryCalculate =
        new JMenuItem(
            Message.getString("mainmenu.analysis.calculate")); // Request Byte / FLOP calculation
    menuAnalysis.add(menuAnalysisMemoryCalculate);
    menuAnalysisMemoryCalculate.addActionListener(
        new AnalysisMemoryAction(
            this.controller,
            AnalysisMemoryAction.ACTION_MODE.MEMORY_CALCULATE,
            FRAME_VIEW.EXPLORE_VIEW));

    // Analysis: Request Byte / FLOP calculation (2014/4/8 added ohichi)
    JMenuItem menuAllAMC = new JMenuItem(Message.getString("mainmenu.analysis.allcalculate"));
    menuAnalysis.add(menuAllAMC);
    menuAllAMC.addActionListener(new AllAnalysisMemoryAction((this.controller)));

    // Profiler
    JMenu menuProfiler = new JMenu(Message.getString("mainmenu.project.profiler"));
    this.add(menuProfiler);
    menuProfiler.addMenuListener(this);

    // Profiler: Loading Profiler
    JMenuItem menuProfilerOpen = new JMenuItem(Message.getString("mainmenu.profiler.read"));
    menuProfiler.add(menuProfilerOpen);
    menuProfilerOpen.addActionListener(new ProfilerOpenFileAction(this.controller));

    // Profiler: Clear Profiler
    JMenuItem menuProfilerClear = new JMenuItem(Message.getString("mainmenu.profiler.clear"));
    menuProfiler.add(menuProfilerClear);
    menuProfilerClear.addActionListener(new ProfilerClearAction(this.controller));

    // Profiler: Cost display
    this.menuProfilerBarVisibled =
        new JCheckBoxMenuItem(Message.getString("mainmenu.profiler.cost"));
    menuProfiler.add(menuProfilerBarVisibled);
    menuProfilerBarVisibled.addActionListener(new ProfilerViewBargraphAction(this.controller));

    // Profiler: Cost ruler display
    this.menuProfilerRulerVisibled =
        new JCheckBoxMenuItem(Message.getString("mainmenu.profiler.costruler"));
    menuProfiler.add(menuProfilerRulerVisibled);
    menuProfilerRulerVisibled.addActionListener(new ProfilerViewRulerAction(this.controller));

    // Separator
    menuProfiler.addSeparator();

    // Profiler: Measurement interval setting
    JMenuItem menuProfilerAddEprofArea =
        new JMenuItem(Message.getString("mainmenu.profiler.set-mesuermentrange"));
    menuProfiler.add(menuProfilerAddEprofArea);
    menuProfilerAddEprofArea.addActionListener(
        new ProfilerAddEprofAction(this.controller, FRAME_VIEW.EXPLORE_VIEW));
    // Profiler: Overwrite measurement section
    JMenuItem menuProfilerSavefile =
        new JMenuItem(Message.getString("mainmenu.profiler.save-mesuermentrange"));
    menuProfiler.add(menuProfilerSavefile);
    menuProfilerSavefile.addActionListener(new ProfilerSaveFileAction(this.controller));
    // Profiler: Save measurement section folder
    JMenuItem menuProfilerSavefolder =
        new JMenuItem(Message.getString("mainmenu.profiler.savefolder-mesuermentrange"));
    menuProfiler.add(menuProfilerSavefolder);
    menuProfilerSavefolder.addActionListener(new ProfilerSaveFolderAction(this.controller));

    // Display
    menuView = new JMenu(Message.getString("mainmenu.view")); // display
    this.add(menuView);
    menuView.addMenuListener(this);

    // Display: Tree storage / expansion
    JMenu menuViewTreeNode =
        new JMenu(Message.getString("mainmenu.view.collapse-expand")); // Tree storage / expansion
    menuView.add(menuViewTreeNode);
    // Display: Tree storage / expansion: All storage
    JMenuItem menuViewTreeNodeAllCollapse =
        new JMenuItem(
            Message.getString("mainmenu.view.collapse-expand.collapse-all")); // All stored
    menuViewTreeNode.add(menuViewTreeNodeAllCollapse);
    this.actionTreeCollapseAll = new TreeCollapseAllAction(this.controller);
    menuViewTreeNodeAllCollapse.addActionListener(this.actionTreeCollapseAll);

    // Display: Tree storage / expansion: All storage
    JMenuItem menuViewTreeNodeAllExpand =
        new JMenuItem(Message.getString("mainmenu.view.collapse-expand.expand-all")); // Expand all
    menuViewTreeNode.add(menuViewTreeNodeAllExpand);
    menuViewTreeNodeAllExpand.addActionListener(new TreeExpandAllAction(this.controller));
    // Display: Tree storage / expansion: Expansion
    JMenuItem menuViewTreeNodeExpand =
        new JMenuItem(
            Message.getString("mainmenu.view.collapse-expand.selective")); // Selective expansion
    menuViewTreeNode.add(menuViewTreeNodeExpand);
    menuViewTreeNodeExpand.addActionListener(new TreeExpandSelectAction(this.controller));

    // View: New Structure Tree
    JMenuItem menuViewOpenTree =
        new JMenuItem(Message.getString("mainmenu.view.newtree")); // New structure tree
    menuView.add(menuViewOpenTree);
    this.actionOpenLanguageTree = new ViewOpenLanguageTreeAction(this.controller);
    menuViewOpenTree.addActionListener(this.actionOpenLanguageTree);

    // View: Open file
    JMenuItem menuViewOpenFile =
        new JMenuItem(Message.getString("mainmenu.view.openfile")); // open the file
    menuView.add(menuViewOpenFile);
    menuViewOpenFile.addActionListener(new ViewOpenExploreBlockAction(this.controller));
    // Display: Close
    JMenuItem menuViewClose =
        new JMenuItem(Message.getString("mainmenu.view.closefile")); // close the file
    menuView.add(menuViewClose);
    menuViewClose.addActionListener(new ViewCloseAction(this.controller));
    // Display: Close all
    JMenuItem menuViewAllClose =
        new JMenuItem(Message.getString("mainmenu.view.close-all-file")); // close all
    menuView.add(menuViewAllClose);
    menuViewAllClose.addActionListener(new ViewCloseAction(this.controller, true));

    // Separator
    menuView.addSeparator();
    // Display: Structural filter
    menuViewFilter = new JMenu(Message.getString("mainmenu.view.filter")); // Structural filter
    menuView.add(menuViewFilter);
    // View: Structural Filter: Show All
    JCheckBoxMenuItem menuViewFilterAll =
        new JCheckBoxMenuItem(Message.getString("mainmenu.view.filter.viewall")); // Show all
    menuViewFilter.add(menuViewFilterAll);
    menuViewFilterAll.addActionListener(
        new ViewLangugeFilterAction(this.controller, FILTER_TYPE.ALL));
    // Display: Structural filter: Subroutines / functions
    JCheckBoxMenuItem menuViewFilterSubroutine =
        new JCheckBoxMenuItem(
            Message.getString(
                "mainmenu.view.filter.subroutine-function")); // Subroutines / functions
    menuViewFilter.add(menuViewFilterSubroutine);
    menuViewFilterSubroutine.addActionListener(
        new ViewLangugeFilterAction(this.controller, FILTER_TYPE.PROCEDURE));
    // Display: Structural filter: CALL statement / function call statement
    JCheckBoxMenuItem menuViewFilterCall =
        new JCheckBoxMenuItem(
            Message.getString(
                "mainmenu.view.filter.call")); // CALL statement / function call statement
    menuViewFilter.add(menuViewFilterCall);
    menuViewFilterCall.addActionListener(
        new ViewLangugeFilterAction(this.controller, FILTER_TYPE.PROCEDUREUSAGE));
    // Display: Structural filter: DO statement
    JCheckBoxMenuItem menuViewFilterDo =
        new JCheckBoxMenuItem(Message.getString("mainmenu.view.filter.do")); // DO statement
    menuViewFilter.add(menuViewFilterDo);
    menuViewFilterDo.addActionListener(
        new ViewLangugeFilterAction(this.controller, FILTER_TYPE.REPETITION));
    // Display: Structural filter: IF statement
    JCheckBoxMenuItem menuViewFilterIf = new JCheckBoxMenuItem(FILTER_TYPE.SELECTION_IF.getName());
    menuViewFilter.add(menuViewFilterIf);
    menuViewFilterIf.addActionListener(
        new ViewLangugeFilterAction(this.controller, FILTER_TYPE.SELECTION_IF));
    // Display: Structural filter: SELECT statement
    JCheckBoxMenuItem menuViewFilterSelect =
        new JCheckBoxMenuItem(FILTER_TYPE.SELECTION_SELECT.getName());
    menuViewFilter.add(menuViewFilterSelect);
    menuViewFilterSelect.addActionListener(
        new ViewLangugeFilterAction(this.controller, FILTER_TYPE.SELECTION_SELECT));
    // Display: Structural filter: Assignment statement
    JCheckBoxMenuItem menuViewFilterAssignment =
        new JCheckBoxMenuItem(
            Message.getString("mainmenu.view.filter.expression-array")); // Assignment statement
    menuViewFilter.add(menuViewFilterAssignment);
    menuViewFilterAssignment.addActionListener(
        new ViewLangugeFilterAction(this.controller, FILTER_TYPE.SUBSTITUTION));
    // Display: Structural filter: Flow control statement
    JCheckBoxMenuItem menuViewFilterFlow =
        new JCheckBoxMenuItem(
            Message.getString("mainmenu.view.filter.other-flow-control")); // Flow control statement
    menuViewFilter.add(menuViewFilterFlow);
    menuViewFilterFlow.addActionListener(
        new ViewLangugeFilterAction(this.controller, FILTER_TYPE.FLOW));
    // Display: Structural filter: Directive statement
    JMenu menuViewFilterDirective =
        new JMenu(Message.getString("mainmenu.view.filter.directive")); // Directive statement
    JCheckBoxMenuItem menuViewFilterDirectiveOmp =
        new JCheckBoxMenuItem(Message.getString("mainmenu.view.filter.openmp")); // OPENMP
    JCheckBoxMenuItem menuViewFilterDirectiveOcl =
        new JCheckBoxMenuItem(Message.getString("mainmenu.view.filter.ocl")); // OCL
    menuViewFilterDirective.add(menuViewFilterDirectiveOmp);
    menuViewFilterDirective.add(menuViewFilterDirectiveOcl);
    menuViewFilter.add(menuViewFilterDirective);
    menuViewFilterDirectiveOmp.addActionListener(
        new ViewLangugeFilterAction(this.controller, FILTER_TYPE.DIRECTIVE_OPENML));
    menuViewFilterDirectiveOcl.addActionListener(
        new ViewLangugeFilterAction(this.controller, FILTER_TYPE.DIRECTIVE_OCL));

    // Separator
    menuViewFilter.addSeparator();

    // Display: Structural filter: Default
    JMenuItem menuViewFilterDefault =
        new JMenuItem(Message.getString("mainmenu.view.filter.default")); // return to default
    menuViewFilter.add(menuViewFilterDefault);
    menuViewFilterDefault.addActionListener(
        new ViewLangugeFilterAction(this.controller, FILTER_TYPE.DEFAULT));

    // window
    menuWindow = new JMenu(Message.getString("mainmenu.window")); // window
    this.add(menuWindow);
    menuWindow.addMenuListener(this);

    // Window: Explorer view
    menuWindowExplore = new JMenu(Message.getString("mainmenu.window.explore")); // Explorer view
    menuWindow.add(menuWindowExplore);
    // Window: Explorer View: Structure
    menuWindowExploreLanguage =
        new JMenuItem(Message.getString("mainmenu.window.explore.structure")); // Construction
    menuWindowExplore.add(menuWindowExploreLanguage);
    menuWindowExploreLanguage.addActionListener(
        new WindowViewAction(this.controller, EXPLORE_PANEL.LANGUAGE));

    // Window: Explorer View: Module
    JMenuItem menuWindowExploreModule =
        new JMenuItem(Message.getString("mainmenu.window.explore.module")); // module
    menuWindowExplore.add(menuWindowExploreModule);
    menuWindowExploreModule.addActionListener(
        new WindowViewAction(this.controller, EXPLORE_PANEL.MODULE));

    // Window: Explorer View: Source
    JMenuItem menuWindowExploreSource =
        new JMenuItem(Message.getString("mainmenu.window.explore.source")); // Source
    menuWindowExplore.add(menuWindowExploreSource);
    menuWindowExploreSource.addActionListener(
        new WindowViewAction(this.controller, EXPLORE_PANEL.SOURCE));

    // Window: Explorer View: XML
    JMenuItem menuWindowExploreXml =
        new JMenuItem(Message.getString("mainmenu.window.explore.xml")); // XML
    menuWindowExplore.add(menuWindowExploreXml);
    menuWindowExploreXml.addActionListener(
        new WindowViewAction(this.controller, EXPLORE_PANEL.XML));

    // Window: Source view
    menuWindowSource = new JMenu(Message.getString("mainmenu.window.source")); // Source view
    menuWindow.add(menuWindowSource);

    // Window: Source View: Source File
    // Add the file that is open in the menuSelected event.

    // Window: Analysis view
    menuWindowAnalysis = new JMenu(Message.getString("mainmenu.window.analysis")); // Analysis view
    menuWindow.add(menuWindowAnalysis);
    // Window: Analysis view: Additional information
    JMenuItem menuWindowAnalysisInformation =
        new JMenuItem(
            Message.getString("mainmenu.window.analysis.information")); // Additional information
    menuWindowAnalysis.add(menuWindowAnalysisInformation);
    menuWindowAnalysisInformation.addActionListener(
        new WindowViewAction(this.controller, ANALYSIS_PANEL.INFORMATION));

    // Window: Analysis view: Search results
    JMenuItem menuWindowAnalysisSearch =
        new JMenuItem(Message.getString("mainmenu.window.analysis.search")); // search results
    menuWindowAnalysis.add(menuWindowAnalysisSearch);
    menuWindowAnalysisSearch.addActionListener(
        new WindowViewAction(this.controller, ANALYSIS_PANEL.SEARCHRESULT));

    // Window: Analysis view: List of variable characteristics
    JMenuItem menuWindowAnalysisVariable =
        new JMenuItem(
            Message.getString(
                "mainmenu.analysis.valiableproperty")); // List of variable characteristics
    menuWindowAnalysis.add(menuWindowAnalysisVariable);
    menuWindowAnalysisVariable.addActionListener(
        new WindowViewAction(this.controller, ANALYSIS_PANEL.VALIABLE));

    // Window: Analysis view: Calculation count
    JMenuItem menuWindowAnalysisCount =
        new JMenuItem(Message.getString("mainmenu.analysis.operation")); // Calculation count
    menuWindowAnalysis.add(menuWindowAnalysisCount);
    menuWindowAnalysisCount.addActionListener(
        new WindowViewAction(this.controller, ANALYSIS_PANEL.OPERAND));

    // Window: Analysis view: Request B / F calculation result
    JMenuItem menuWindowAnalysisRequired =
        new JMenuItem(
            Message.getString(
                "mainmenu.window.analysis.byteflop")); // Request B / F calculation result
    menuWindowAnalysis.add(menuWindowAnalysisRequired);
    menuWindowAnalysisRequired.addActionListener(
        new WindowViewAction(this.controller, ANALYSIS_PANEL.REQUIRED));

    // Window: Analysis view: Reference list
    JMenuItem menuWindowAnalysisReference =
        new JMenuItem(Message.getString("mainmenu.analysis.dec-def-ref")); // Reference list
    menuWindowAnalysis.add(menuWindowAnalysisReference);
    menuWindowAnalysisReference.addActionListener(
        new WindowViewAction(this.controller, ANALYSIS_PANEL.REFERENCE));

    // Window: Analysis View: Trace
    menuWindowAnalysisTrace =
        new JMenuItem(Message.getString("mainmenu.window.analysis.trace")); // trace
    menuWindowAnalysis.add(menuWindowAnalysisTrace);
    menuWindowAnalysisTrace.addActionListener(
        new WindowViewAction(this.controller, ANALYSIS_PANEL.TRACE));

    // Window: Analysis view: Variable scope
    JMenuItem menuWindowAnalysisScope =
        new JMenuItem(Message.getString("mainmenu.analysis.valiablescope")); // Variable valid area
    menuWindowAnalysis.add(menuWindowAnalysisScope);
    menuWindowAnalysisScope.addActionListener(
        new WindowViewAction(this.controller, ANALYSIS_PANEL.SCOPE));

    // Window: Analysis view: Structural information replacement
    JMenuItem menuWindowReplace =
        new JMenuItem(
            Message.getString(
                "mainmenu.window.analysis.structureinfo")); // Structural information replacement
    menuWindowAnalysis.add(menuWindowReplace);
    menuWindowReplace.addActionListener(
        new WindowViewAction(this.controller, ANALYSIS_PANEL.REPLACE));

    // Window: Analysis View: Properties
    JMenuItem menuWindowAnalysisProparty =
        new JMenuItem(Message.getString("mainmenu.project.property")); // Analysis properties
    menuWindowAnalysis.add(menuWindowAnalysisProparty);
    menuWindowAnalysisProparty.addActionListener(
        new WindowViewAction(this.controller, ANALYSIS_PANEL.PROPARTIES));

    // Window: Analysis view: Error location
    JMenuItem menuWindowAnalysisError =
        new JMenuItem(Message.getString("mainmenu.window.analysis.error")); // Error location
    menuWindowAnalysis.add(menuWindowAnalysisError);
    menuWindowAnalysisError.addActionListener(
        new WindowViewAction(this.controller, ANALYSIS_PANEL.ERROR));

    // Window: Analysis View: Console
    JMenuItem menuWindowAnalysisConsole =
        new JMenuItem(Message.getString("mainmenu.window.analysis.console")); // console
    menuWindowAnalysis.add(menuWindowAnalysisConsole);
    menuWindowAnalysisConsole.addActionListener(
        new WindowViewAction(this.controller, ANALYSIS_PANEL.CONSOLE));

    // Window: Analysis view: Cost information
    {
      JMenu menuViewProfilerInfo =
          new JMenu(Message.getString("mainmenu.window.analysis.profiler-info"));
      menuWindowAnalysis.add(menuViewProfilerInfo);
      // Window: Analysis View: Cost Information: Procedure
      JMenuItem menuWindowAnalysisCostProcedure =
          new JMenuItem(ANALYSIS_PANEL.COST_PROCEDURE.getTabName());
      menuViewProfilerInfo.add(menuWindowAnalysisCostProcedure);
      menuWindowAnalysisCostProcedure.addActionListener(
          new WindowViewAction(this.controller, ANALYSIS_PANEL.COST_PROCEDURE));
      // Window: Analysis View: Cost Information: Loop
      JMenuItem menuWindowAnalysisCostLoop = new JMenuItem(ANALYSIS_PANEL.COST_LOOP.getTabName());
      menuViewProfilerInfo.add(menuWindowAnalysisCostLoop);
      menuWindowAnalysisCostLoop.addActionListener(
          new WindowViewAction(this.controller, ANALYSIS_PANEL.COST_LOOP));
      // Window: Analysis View: Cost Information: Line
      JMenuItem menuWindowAnalysisCostLine = new JMenuItem(ANALYSIS_PANEL.COST_LINE.getTabName());
      menuViewProfilerInfo.add(menuWindowAnalysisCostLine);
      menuWindowAnalysisCostLine.addActionListener(
          new WindowViewAction(this.controller, ANALYSIS_PANEL.COST_LINE));
      // Window: Analysis view: Call graph information
      JMenuItem menuWindowAnalysisCallGraph = new JMenuItem(ANALYSIS_PANEL.CALLGRAPH.getTabName());
      menuViewProfilerInfo.add(menuWindowAnalysisCallGraph);
      menuWindowAnalysisCallGraph.addActionListener(
          new WindowViewAction(this.controller, ANALYSIS_PANEL.CALLGRAPH));
      // Window: Analysis View: Profiler: Event Counter Information: Eprof: CACHE
      JMenuItem menuWindowAnalysisEprofCache =
          new JMenuItem(ANALYSIS_PANEL.EVENTCOUNTER_CACHE.getTabName());
      menuViewProfilerInfo.add(menuWindowAnalysisEprofCache);
      menuWindowAnalysisEprofCache.addActionListener(
          new WindowViewAction(this.controller, ANALYSIS_PANEL.EVENTCOUNTER_CACHE));
      // Window: Analysis View: Profiler: Event Counter Information: Eprof: INSTRUCTIONS
      JMenuItem menuWindowAnalysisEprofInstructions =
          new JMenuItem(ANALYSIS_PANEL.EVENTCOUNTER_INSTRUCTIONS.getTabName());
      menuViewProfilerInfo.add(menuWindowAnalysisEprofInstructions);
      menuWindowAnalysisEprofInstructions.addActionListener(
          new WindowViewAction(this.controller, ANALYSIS_PANEL.EVENTCOUNTER_INSTRUCTIONS));
      // Window: Analysis View: Profiler: Event Counter Information: Eprof: MEM_ACCESS
      JMenuItem menuWindowAnalysisEprofMemaccess =
          new JMenuItem(ANALYSIS_PANEL.EVENTCOUNTER_MEM_ACCESS.getTabName());
      menuViewProfilerInfo.add(menuWindowAnalysisEprofMemaccess);
      menuWindowAnalysisEprofMemaccess.addActionListener(
          new WindowViewAction(this.controller, ANALYSIS_PANEL.EVENTCOUNTER_MEM_ACCESS));
      // Window: Analysis View: Profiler: Event Counter Information: Eprof: PERFORMANCE
      JMenuItem menuWindowAnalysisEprofPerformance =
          new JMenuItem(ANALYSIS_PANEL.EVENTCOUNTER_PERFORMANCE.getTabName());
      menuViewProfilerInfo.add(menuWindowAnalysisEprofPerformance);
      menuWindowAnalysisEprofPerformance.addActionListener(
          new WindowViewAction(this.controller, ANALYSIS_PANEL.EVENTCOUNTER_PERFORMANCE));
      // Window: Analysis View: Profiler: Event Counter Information: Eprof: STATISTICS
      JMenuItem menuWindowAnalysisEprofStatistics =
          new JMenuItem(ANALYSIS_PANEL.EVENTCOUNTER_STATISTICS.getTabName());
      menuViewProfilerInfo.add(menuWindowAnalysisEprofStatistics);
      menuWindowAnalysisEprofStatistics.addActionListener(
          new WindowViewAction(this.controller, ANALYSIS_PANEL.EVENTCOUNTER_STATISTICS));
      // Window: Analysis view: Timer information
      JMenuItem menuWindowAnalysisEprofMeasure =
          new JMenuItem(ANALYSIS_PANEL.EPROF_MEASURE.getTabName());
      menuViewProfilerInfo.add(menuWindowAnalysisEprofMeasure);
      menuWindowAnalysisEprofMeasure.addActionListener(
          new WindowViewAction(this.controller, ANALYSIS_PANEL.EPROF_MEASURE));
    }

    // Window: Progress
    menuWindowProgress =
        new JMenuItem(Message.getString("mainmenu.window.analysis.progress")); // progress
    menuWindow.add(menuWindowProgress);
    menuWindowProgress.addActionListener(new WindowProgressAction(this.controller));

    // Window: Profiler Legend
    JMenuItem menuWindowSourcePreview =
        new JMenuItem(
            Message.getString("mainmenu.window.analysis.profiler-legend")); // Profiler legend
    menuWindow.add(menuWindowSourcePreview);
    menuWindowSourcePreview.addActionListener(new WindowProfilerLegendAction(this.controller));

    // help
    JMenu menuHelp = new JMenu(Message.getString("mainmenu.help")); // help
    this.add(menuHelp);
    JMenuItem menuHelpVersion =
        new JMenuItem(Message.getString("mainmenu.help.about")); // version information...
    menuHelp.add(menuHelpVersion);
    menuHelpVersion.addActionListener(new HelpVersionAction(this.controller));

    // Hide menu
    // Open the error part
    actionErrorOpenFile = new ErrorOpenFileAction(this.controller);
    actionOpenAnalysisLine = new ViewOpenAnalysisLineAction(this.controller);

    // Initialize menu selection state
    this.clearSelectedMenu();
  }

  /**
   * Analytical information export action
   *
   * @return Analysis information export action
   */
  public FileExportAnalysisAction getActionExportAnalysis() {
    return actionExportAnalysis;
  }

  /**
   * Menu selection action <br>
   * Display menu and switch enable.
   *
   * @param event Event information
   */
  @Override
  public void menuSelected(MenuEvent event) {

    // Window: Source view
    if (event.getSource() == this.menuWindow) {
      // Get a list of open files
      SourceFile[] list = this.controller.getMainframe().getPanelSourceView().getOpenedSourceFile();
      // Toggle source view enable
      menuWindowSource.setEnabled((list != null));

      // Clear display source file
      menuWindowSource.removeAll();

      // Add source file name
      if (list != null) {
        for (SourceFile file : list) {
          // Window: Source View: Source File
          JMenuItem menuWindowSourceFile = new JMenuItem(file.getFile().getName());
          WindowSourceAction action = new WindowSourceAction(this.controller, file);
          menuWindowSourceFile.addActionListener(action);
          menuWindowSource.add(menuWindowSourceFile);
        }
      }

      // Structure menu
      {
        int pos = getMenuItemPos(menuWindowExplore, menuWindowExploreLanguage);
        if (pos >= 0) {
          // Get a list of open structure tabs
          LanguageTreeModel[] models =
              this.controller.getMainframe().getPanelExplorerView().getLanguageModels();
          // Add procedure name on the Structure tab
          if (models != null && models.length > 0) {
            menuWindowExplore.remove(menuWindowExploreLanguage);
            menuWindowExploreLanguage =
                new JMenu(Message.getString("mainmenu.window.explore.structure")); // Construction
            menuWindowExplore.insert(menuWindowExploreLanguage, pos);
            for (LanguageTreeModel model : models) {
              // Window: Explorer View: Structure
              DefaultMutableTreeNode root = model.getRootNode();
              String menuname = EXPLORE_PANEL.LANGUAGE.getTabName();
              if (root != null && root.getChildCount() > 0) {
                menuname = root.toString();
                DefaultMutableTreeNode child = (DefaultMutableTreeNode) root.getChildAt(0);
                Object obj = child.getUserObject();
                if (obj != null && obj instanceof Procedure) {
                  menuname = ((Procedure) obj).get_name();
                }
              }

              JMenuItem menuLanguage = new JMenuItem(menuname);
              WindowViewAction action = new WindowViewAction(this.controller, model);
              menuLanguage.addActionListener(action);
              menuWindowExploreLanguage.add(menuLanguage);
            }
          } else {
            menuWindowExplore.remove(menuWindowExploreLanguage);
            menuWindowExploreLanguage =
                new JMenuItem(
                    Message.getString("mainmenu.window.explore.structure")); // Construction
            menuWindowExplore.insert(menuWindowExploreLanguage, pos);
            menuWindowExploreLanguage.addActionListener(
                new WindowViewAction(this.controller, EXPLORE_PANEL.LANGUAGE));
          }
        }
      }
      // Trace menu
      {
        int pos = getMenuItemPos(menuWindowAnalysis, menuWindowAnalysisTrace);
        if (pos >= 0) {
          // Get a list of open trace tabs
          TraceResultModel[] models =
              this.controller.getMainframe().getPanelAnalysisView().getTraceResultModels();
          // Add procedure name on the trace tab
          if (models != null && models.length > 0) {
            menuWindowAnalysis.remove(menuWindowAnalysisTrace);
            menuWindowAnalysisTrace =
                new JMenu(Message.getString("mainmenu.window.analysis.trace")); // trace
            menuWindowAnalysis.insert(menuWindowAnalysisTrace, pos);
            for (TraceResultModel model : models) {
              IBlock block = model.getRootBlock();
              String word = model.getTraceWord();
              String menuname = ANALYSIS_PANEL.TRACE.getTabName();
              if (word != null && block != null) {
                if (block instanceof Procedure) {
                  menuname = word + " : " + ((Procedure) block).get_name();
                } else {
                  menuname = word + " : " + block.toString();
                }
              }
              JMenuItem menuTrace = new JMenuItem(menuname);
              WindowViewAction action = new WindowViewAction(this.controller, model);
              menuTrace.addActionListener(action);
              menuWindowAnalysisTrace.add(menuTrace);
            }
          } else {
            menuWindowAnalysis.remove(menuWindowAnalysisTrace);
            menuWindowAnalysisTrace = new JMenuItem(ANALYSIS_PANEL.TRACE.getTabName());
            menuWindowAnalysis.insert(menuWindowAnalysisTrace, pos);
            menuWindowAnalysisTrace.addActionListener(
                new WindowViewAction(this.controller, ANALYSIS_PANEL.TRACE));
          }
        }
      }
    }
    // Display: Structural filter
    if (event.getSource() == this.menuView) {
      // Get structure tree filter
      List<FILTER_TYPE> filters = this.controller.getListLanguageFilter();
      if (filters != null && filters.size() > 0) {
        int count = this.menuViewFilter.getMenuComponentCount();
        for (int i = 0; i < count; i++) {
          Object obj = this.menuViewFilter.getMenuComponent(i);
          if (!(obj instanceof JCheckBoxMenuItem)) continue;
          JCheckBoxMenuItem submenu = (JCheckBoxMenuItem) obj;
          ActionListener[] actions = submenu.getActionListeners();
          if (actions == null) continue;
          for (ActionListener action : actions) {
            if (action instanceof ViewLangugeFilterAction) {
              FILTER_TYPE filter = ((ViewLangugeFilterAction) action).getFilter();
              boolean checked = filters.contains(filter);
              submenu.setSelected(checked);
            }
          }
        }
      }
    }

    // Check if the action is executable
    JMenu menu = (JMenu) event.getSource();
    validateAction(menu);
  }

  /**
   * Check if the action is executable
   *
   * @param menu Check menu
   */
  private void validateAction(JMenu menu) {

    int count = menu.getMenuComponentCount();
    int disenabled = 0;
    for (int i = 0; i < count; i++) {
      Object obj = menu.getMenuComponent(i);
      if (obj instanceof JMenu) {
        validateAction((JMenu) obj);
      }
      if (!(obj instanceof JMenuItem)) continue;
      JMenuItem submenu = (JMenuItem) obj;
      ActionListener[] actions = submenu.getActionListeners();
      if (actions == null) continue;
      for (ActionListener action : actions) {
        if (action instanceof ActionBase) {
          boolean enabled = ((ActionBase) action).validateAction();
          submenu.setEnabled(enabled);
          if (!enabled) {
            disenabled++;
          }
        }
      }
    }
    // If all are disabled, disable the parent menu as well
    menu.setEnabled((count != disenabled));
  }

  /**
   * Get the position of the menu item. <br>
   * Returns -1 if the menu item does not exist.
   *
   * @param menu Parent menu
   * @param item Menu item
   * @return Menu item position
   */
  private int getMenuItemPos(JMenu menu, JMenuItem item) {
    int count = menu.getMenuComponentCount();
    if (count <= 0) return -1;
    for (int i = 0; i < count; i++) {
      if (menu.getMenuComponent(i) == item) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Menu deselection action
   *
   * @param event Event information
   */
  @Override
  public void menuDeselected(MenuEvent event) {}

  /**
   * Menu cancel action
   *
   * @param event Event information
   */
  @Override
  public void menuCanceled(MenuEvent event) {}

  /**
   * Get the action to open the error location
   *
   * @return Action to open the error part
   */
  public EventListener getActionErrorOpenFile() {
    return this.actionErrorOpenFile;
  }

  /**
   * Get the action to open the analysis result
   *
   * @return Analysis result Action to open the relevant part
   */
  public EventListener getActionOpenAnalysisLine() {
    return this.actionOpenAnalysisLine;
  }

  /**
   * Get additional information edit action
   *
   * @return Additional information editing action
   */
  public EditInformationEditAction getActionEditInformation() {
    return actionEditInformation;
  }

  /**
   * Get the arithmetic count action
   *
   * @return Arithmetic count action
   */
  public AnalysisOperandAction getActionAnalysisOperand() {
    return this.actionAnalysisOperand;
  }

  /**
   * Get a trace action
   *
   * @param dir Trace direction
   * @return Trace action
   */
  public AnalysisTraceAction getActionAnalysisTrace(TRACE_DIR dir) {
    return new AnalysisTraceAction(this.controller, dir);
  }

  /**
   * Get a new structure tree action
   *
   * @return New structure tree action
   */
  public ViewOpenLanguageTreeAction getActionOpenLanguageTree() {
    return actionOpenLanguageTree;
  }

  /**
   * Get search result action
   *
   * @param dir Search result direction
   * @return Search result action
   */
  public SearchResultAction getActionSearchResult(TRACE_DIR dir) {
    return new SearchResultAction(this.controller, dir);
  }

  /** Initialize the menu display (combo box) */
  public void clearSelectedMenu() {
    // Profiler: Cost display
    if (menuProfilerBarVisibled != null) {
      boolean visibleBar = ProfilerProperties.INITIALIZE_VISIBLE_BARGRAPH;
      menuProfilerBarVisibled.setSelected(visibleBar);
    }
    // Profiler: Cost ruler display
    if (menuProfilerRulerVisibled != null) {
      boolean visibleRuler = ProfilerProperties.INITIALIZE_VISIBLE_RULER;
      menuProfilerRulerVisibled.setSelected(visibleRuler);
    }
  }
}
