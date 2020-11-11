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

import java.awt.Frame;
import java.awt.event.ActionEvent;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeNode;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.dialog.EprofStatementDialog;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.model.ProfilerMeasureModel;
import jp.riken.kscope.properties.ProfilerProperties;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.ProfilerService;

/**
 * Profiler: Eprof measurement interval insertion action class
 *
 * @author RIKEN
 */
public class ProfilerAddEprofAction extends ActionBase {

  /** Measurement interval acquisition destination view */
  private FRAME_VIEW view;

  /**
   * Constructor
   *
   * @param controller Application controller
   * @param view Measurement interval acquisition destination view
   */
  public ProfilerAddEprofAction(AppController controller, FRAME_VIEW view) {
    super(controller);
    this.view = view;
  }

  /**
   * Check if the action is executable. <br>
   * Check before executing the action and switch the menu enable. <br>
   *
   * @return true = Action can be executed
   */
  @Override
  public boolean validateAction() {
    if (this.view == FRAME_VIEW.SOURCE_VIEW) {
      CodeLine line = this.controller.getMainframe().getPanelSourceView().getSelectedCodeLine();
      return (line != null);
    } else if (this.view == FRAME_VIEW.EXPLORE_VIEW) {
      IBlock[] blocks = this.controller.getMainframe().getPanelExplorerView().getSelectedBlocks();
      if (blocks == null || blocks.length <= 0) return false;
      // Check if they are the same file
      SourceFile srcfile = null;
      for (IBlock block : blocks) {
        SourceFile file = block.getStartCodeLine().getSourceFile();
        if (srcfile == null) {
          srcfile = file;
        } else if (!srcfile.equals(file)) {
          return false;
        }
        // Measurement interval cannot be set in subroutines and function declaration statements
        if (block instanceof Procedure) {
          return false;
        }
      }
      // Check if they are in the same hierarchy
      int length = -1;
      DefaultMutableTreeNode[] nodes =
          this.controller.getMainframe().getPanelExplorerView().getSelectedNodes();
      for (DefaultMutableTreeNode node : nodes) {
        TreeNode[] paths = node.getPath();
        if (paths == null) {
          return false;
        } else if (length == -1) {
          length = paths.length;
        } else if (length != paths.length) {
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
    final String message = event.getActionCommand();
    Application.status.setMessageMain(message);
    // Get the parent Frame.
    Frame frame = getWindowAncestor(event);

    // Display the group name input dialog
    ProfilerProperties properties = this.controller.getPropertiesProfiler();
    EprofStatementDialog dialog = new EprofStatementDialog(frame, true);
    dialog.setProperties(properties);
    int result = dialog.showDialog();
    if (result != Constant.OK_DIALOG) {
      Application.status.setMessageMain(
          message + Message.getString("action.common.cancel.status")); // Cancel
      return;
    }

    // group name
    String goupname = dialog.getGroupname();
    String number = dialog.getNumber();
    String level = dialog.getLevel();

    // Profiler service
    ProfilerService service = new ProfilerService();
    service.setErrorInfoModel(this.controller.getErrorInfoModel());
    // Measurement interval model
    ProfilerMeasureModel model =
        this.controller.getMainframe().getPanelAnalysisView().getPanelProfilerMeasure().getModel();
    service.setMeasureModel(model);
    // Profiler information
    service.setProfilerInfo(this.controller.getProfilerInfo());
    // Project folder
    service.setProjectFolder(this.controller.getProjectModel().getProjectFolder());

    if (this.view == FRAME_VIEW.SOURCE_VIEW) {
      CodeLine line = this.controller.getMainframe().getPanelSourceView().getSelectedArea();
      service.addProfilerMeasureInfo(line, goupname, number, level);
    } else if (this.view == FRAME_VIEW.EXPLORE_VIEW) {
      IBlock[] blocks = this.controller.getMainframe().getPanelExplorerView().getSelectedBlocks();
      if (blocks != null && blocks.length > 0) {
        service.addProfilerMeasureInfo(blocks, goupname, number, level);
      }
    }

    // Activate the measurement interval tab
    this.controller
        .getMainframe()
        .getPanelAnalysisView()
        .setSelectedPanel(ANALYSIS_PANEL.EPROF_MEASURE);

    Application.status.setMessageMain(
        message + Message.getString("action.common.done.status")); // Done
  }
}
