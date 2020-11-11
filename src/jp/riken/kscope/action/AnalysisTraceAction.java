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
package jp.riken.kscope.action;

import java.awt.event.ActionEvent;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.swing.tree.DefaultMutableTreeNode;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.common.KEYWORD_TYPE;
import jp.riken.kscope.common.TRACE_DIR;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.dialog.TraceChooserDialog;
import jp.riken.kscope.gui.TraceResultPanel;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.TraceResultModel;
import jp.riken.kscope.service.AnalysisTraceService;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.utils.FileUtils;

/**
 * Trace action
 *
 * @author RIKEN
 */
public class AnalysisTraceAction extends ActionBase {

  /** Trace direction */
  private TRACE_DIR tracedir;

  /**
   * Constructor
   *
   * @param controller Application controller
   * @param dir Trace direction
   */
  public AnalysisTraceAction(AppController controller, TRACE_DIR dir) {
    super(controller);
    this.tracedir = dir;
  }

  /**
   * Check if the action is executable. <br>
   * Check before executing the action and switch the menu enable. <br>
   *
   * @return true = Action can be executed
   */
  @Override
  public boolean validateAction() {

    if (tracedir == TRACE_DIR.START) {
      // Start tracing
      // Get the selected line of source code
      CodeLine line = this.controller.getMainframe().getPanelSourceView().getSelectedCodeLine();
      if (line == null) return false;

      // Selected string
      String statement = line.getStatement();
      if (statement == null || statement.isEmpty()) return false;
    } else if (tracedir == TRACE_DIR.UP
        || tracedir == TRACE_DIR.DOWN
        || tracedir == TRACE_DIR.IN
        || tracedir == TRACE_DIR.OUT
        || tracedir == TRACE_DIR.FORWARD
        || tracedir == TRACE_DIR.END
        || tracedir == TRACE_DIR.REFRESH) {
      // Trace
      TraceResultModel[] models =
          this.controller.getMainframe().getPanelAnalysisView().getTraceResultModels();
      if (models == null || models.length <= 0) return false;
      for (TraceResultModel model : models) {
        if (model.getTraceWord() == null || model.getTraceWord().isEmpty()) {
          return false;
        }
      }
    }

    return true;
  }

  /**
   * Action occurrence event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {

    // Status message
    final String message = Message.getString("mainmenu.window.analysis.trace"); // trace

    if (tracedir == TRACE_DIR.START) {
      // Status message
      Application.status.setMessageMain(
          message + Message.getString("analysistraceaction.trace-start.status")); // :start

      // Start tracing from the selected string.
      startTrace();
    } else if (tracedir == TRACE_DIR.UP) {
      // Status message
      Application.status.setMessageMain(
          message + Message.getString("analysistraceaction.trace-up.satatus")); // :up

      // Trace: Up
      traceUp();
    } else if (tracedir == TRACE_DIR.DOWN) {
      // Status message
      Application.status.setMessageMain(
          message + Message.getString("analysistraceaction.trace-down.status")); // :down

      // Trace: Go down
      traceDown();
    } else if (tracedir == TRACE_DIR.IN) {
      // Status message
      Application.status.setMessageMain(
          message + Message.getString("analysistraceaction.trace-in.status")); // : Inn

      // Trace: Do an in
      traceIn();
    } else if (tracedir == TRACE_DIR.OUT) {
      // Status message
      Application.status.setMessageMain(
          message + Message.getString("analysistraceaction.trace-out.status")); // :out

      // Trace: Do out
      traceOut(false);
    } else if (tracedir == TRACE_DIR.FORWARD) {
      // Status message
      Application.status.setMessageMain(
          message + Message.getString("analysistraceaction.trace-forword.status")); // :forward

      // Trace: Forward
      traceOut(true);
    } else if (tracedir == TRACE_DIR.REFRESH) {
      // Status message
      Application.status.setMessageMain(
          message + Message.getString("analysistraceaction.trace-refresh.status")); // :refresh

      // Activate the Trace tab
      if (this.controller.getMainframe().getPanelAnalysisView().getSelectedTracePanel() == null) {
        this.controller
            .getMainframe()
            .getPanelAnalysisView()
            .setSelectedPanel(ANALYSIS_PANEL.TRACE);
      }
    }

    if (tracedir != TRACE_DIR.END) {
      // Get the keyword list
      setTraceKeywords();
    } else {
      // Status message
      Application.status.setMessageMain(
          message + Message.getString("analysistraceaction.trace-clear.status")); // :clear

      // Clear the trace result
      clearTrace();
      // Remove trace keywords from source view
      this.controller.getMainframe().getPanelSourceView().clearSearchWords(KEYWORD_TYPE.TRACE);
    }
  }

  /** Start tracing from the selected string. */
  private void startTrace() {

    // Get the selected line of source code
    CodeLine line = this.controller.getMainframe().getPanelSourceView().getSelectedCodeLine();
    if (line == null) return;

    // Change the source file to the relative path of the project folder
    SourceFile srcfile = line.getSourceFile();
    if (srcfile.getFile().isAbsolute()) {
      File projectfolder = this.controller.getProjectModel().getProjectFolder();
      String relpath = FileUtils.getRelativePath(srcfile.getFile(), projectfolder);
      if (relpath != null && !relpath.isEmpty()) {
        line = new CodeLine(line);
        SourceFile relfile = new SourceFile(relpath);
        relfile.setFileType(srcfile.getFileType());
        line.setSourceFile(relfile);
      }
    }

    // Selected string
    String statement = line.getStatement();
    if (statement == null || statement.isEmpty()) return;

    // Fortran database
    Fortran fortran = this.controller.getFortranLanguage();
    // Error information model
    ErrorInfoModel errorModel = this.controller.getErrorInfoModel();

    // Analysis: Trace service
    AnalysisTraceService service = new AnalysisTraceService(fortran);
    service.setErrorInfoModel(errorModel);
    // Set the trace target variable name
    service.setTraceWord(statement);

    // Get the trace result
    TraceResultModel modelTrace = service.analysisTraceStart(line);
    if (modelTrace == null) {
      // Status message
      Application.status.setMessageMain(
          Message.getString(
              "analysistraceaction.trace-start-no-target.status")); // Trace: Start [Not Target]
      return;
    }

    // Add a trace path
    IBlock[] paths = {getRootBlock(modelTrace)};
    modelTrace.setTracePath(paths);

    // Display the trace result
    this.controller.getMainframe().getPanelAnalysisView().viewAnalysisTrace(modelTrace);
  }

  /** Clear the trace result */
  private void clearTrace() {
    // Clear the trace result
    // Close all trace tabs.
    this.controller.getMainframe().getPanelAnalysisView().clearTrace();
  }

  /** Trace: Up */
  private void traceUp() {
    // Search for the above node in the trace panel
    TraceResultPanel panel =
        this.controller.getMainframe().getPanelAnalysisView().getSelectedTracePanel();
    if (panel == null) return;

    // Trace: Up
    panel.traceUp();
  }

  /** Trace: Go down */
  private void traceDown() {
    // Search for the above node in the trace panel
    TraceResultPanel panel =
        this.controller.getMainframe().getPanelAnalysisView().getSelectedTracePanel();
    if (panel == null) return;

    // Trace: Go down
    panel.traceDown();
  }

  /** Trace: Do an in */
  private void traceIn() {
    // Select trace panel
    TraceResultPanel panel =
        this.controller.getMainframe().getPanelAnalysisView().getSelectedTracePanel();
    if (panel == null) return;
    // Select trace model
    TraceResultModel model = panel.getModel();
    // Trace variable
    String statement = model.getTraceWord();
    // Current selection block
    IBlock selectedBlock = model.getSelectedBlock();
    if (statement == null || selectedBlock == null) return;

    // Fortran database
    Fortran fortran = this.controller.getFortranLanguage();
    // Error information model
    ErrorInfoModel errorModel = this.controller.getErrorInfoModel();

    // Analysis: Trace service
    AnalysisTraceService service = new AnalysisTraceService(fortran);
    service.setErrorInfoModel(errorModel);
    // Set the trace target variable name
    service.setTraceWord(statement);

    // Trace: Get in result
    TraceResultModel[] modelTraces = service.analysisTraceIn(selectedBlock);
    if (modelTraces == null || modelTraces.length <= 0 || modelTraces[0] == null) {
      // Status message
      Application.status.setMessageMain(
          Message.getString(
              "analysistraceaction.trace-in-no-target.status")); // Trace: In [Not Target]
      return;
    }

    // Trace destination
    TraceResultModel selectTrace = modelTraces[0];
    if (modelTraces.length > 1) {
      // Display the trace destination selection dialog and select the trace destination.
      selectTrace = showTraceChooserDialog(modelTraces);
    }
    if (selectTrace == null) return;

    // Add a trace path
    List<IBlock> list = new ArrayList<IBlock>();
    IBlock[] paths = model.getTracePath();
    if (paths != null && paths.length > 0) {
      list.addAll(Arrays.asList(paths));
    }
    list.add(selectedBlock);
    list.add(getRootBlock(selectTrace));
    selectTrace.setTracePath(list.toArray(new IBlock[0]));

    // Display the trace result
    this.controller.getMainframe().getPanelAnalysisView().viewAnalysisTrace(selectTrace);
  }

  /**
   * Trace: Do out
   *
   * @param forward Perform a forward.
   */
  private void traceOut(boolean forward) {

    // Select trace panel
    TraceResultPanel panel =
        this.controller.getMainframe().getPanelAnalysisView().getSelectedTracePanel();
    if (panel == null) return;
    // Select trace model
    TraceResultModel model = panel.getModel();
    // Trace variable
    String statement = model.getTraceWord();
    // Current root block
    IBlock rootBlock = model.getRootBlock();
    if (statement == null || rootBlock == null) return;
    // Trace path
    IBlock[] tracePath = null;
    if (forward) {
      // Trace: Forward
      tracePath = model.getTracePath();
    }

    // Fortran database
    Fortran fortran = this.controller.getFortranLanguage();
    // Error information model
    ErrorInfoModel errorModel = this.controller.getErrorInfoModel();

    // Analysis: Trace service
    AnalysisTraceService service = new AnalysisTraceService(fortran);
    service.setErrorInfoModel(errorModel);
    // Set the trace target variable name
    service.setTraceWord(statement);

    // Trace: Get out result
    TraceResultModel[] modelTraces = service.analysisTraceOut(rootBlock, tracePath);
    if (modelTraces == null || modelTraces.length <= 0 || modelTraces[0] == null) {
      // Status message
      Application.status.setMessageMain(
          Message.getString(
              "analysistraceaction.trace-out-no-target.status")); // Trace: Out [Not applicable]
      return;
    }

    // Trace destination
    TraceResultModel selectTrace = modelTraces[0];
    if (modelTraces.length > 1) {
      // Display the trace destination selection dialog and select the trace destination.
      selectTrace = showTraceChooserDialog(modelTraces);
    }
    if (selectTrace == null) return;

    // Add a trace path
    List<IBlock> list = new ArrayList<IBlock>();
    if (forward) {
      // Trace: Forward
      tracePath = model.getTracePath();
      if (tracePath != null && tracePath.length > 0) {
        list.addAll(Arrays.asList(tracePath));
        if (list.size() > 2) {
          // Delete the last node
          list.remove(list.size() - 1);
          list.remove(list.size() - 1);
        }
      }
    } else {
      list.add(getRootBlock(selectTrace));
    }
    // list.add(getRootBlock(selectTrace));
    selectTrace.setTracePath(list.toArray(new IBlock[0]));

    // Display the trace result
    this.controller.getMainframe().getPanelAnalysisView().viewAnalysisTrace(selectTrace);
  }

  /**
   * Display the trace destination selection dialog
   *
   * @return Trace result model
   */
  private TraceResultModel showTraceChooserDialog(TraceResultModel[] traces) {
    TraceChooserDialog dialog = new TraceChooserDialog(this.controller.getMainframe(), true);
    dialog.setTraceResultModel(traces);

    // Setting the action to open the relevant part
    dialog.setViewOpenAnalysisLineAction(new ViewOpenAnalysisLineAction(this.controller));

    // Open the trace selection dialog
    int result = dialog.showDialog();
    if (result == Constant.CANCEL_DIALOG) return null;

    return dialog.getTraceResultModel();
  }

  /** Set trace keywords */
  public void setTraceKeywords() {
    // Get the keyword list
    this.controller.setTraceKeywords();
  }

  /**
   * Get the root block
   *
   * @param modelTrace Trace model
   * @return Root block
   */
  private IBlock getRootBlock(TraceResultModel modelTrace) {
    if (modelTrace == null) return null;
    DefaultMutableTreeNode root = modelTrace.getRootNode();
    if (root == null) return null;
    if (root.getUserObject() == null) return null;
    if (root.getUserObject() instanceof IBlock) {
      return (IBlock) root.getUserObject();
    }

    return null;
  }
}
