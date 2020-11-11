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
import java.io.File;
import javax.swing.tree.DefaultMutableTreeNode;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.dialog.InformationDialog;
import jp.riken.kscope.information.TextInfo;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.language.Program;
import jp.riken.kscope.model.InformationModel;
import jp.riken.kscope.service.AppController;

/**
 * Additional information editing action class
 *
 * @author RIKEN
 */
public class EditInformationEditAction extends ActionBase {

  /** Additional information acquisition destination view */
  private FRAME_VIEW view;

  /**
   * Constructor
   *
   * @param controller Application controller
   * @param view Additional information acquisition destination view
   */
  public EditInformationEditAction(AppController controller, FRAME_VIEW view) {
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
    return isSelectedInformation();
    /*        IInformation info = getSelectedInformation();
    if (info == null) return false;
    return true;*/
    // 2012/10/30 Marge yabe
  }

  /**
   * Additional information editing event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {

    // Execution check
    if (!validateAction()) return;

    // Status message
    final String message = Message.getString("mainmenu.edit.info"); // Edit additional information
    Application.status.setMessageMain(message);

    // Get additional selection information
    IInformation infoNode = getSelectedInformation();
    if (infoNode == null) return;

    // Edit additional information
    if (!editInformation(infoNode)) {
      // Cancel editing of additional information
      Application.status.setMessageMain(
          message + Message.getString("action.common.cancel.status")); // :Cancel
      return;
    }

    // Activate the additional information panel
    this.controller
        .getMainframe()
        .getPanelAnalysisView()
        .setSelectedPanel(ANALYSIS_PANEL.INFORMATION);
    Application.status.setMessageMain(
        message + Message.getString("action.common.done.status")); // : Done
    return;
  }

  /**
   * Get additional selection information
   *
   * @return Selectable additional information
   */
  private IInformation getSelectedInformation() {
    IInformation info = null;
    if (this.view == FRAME_VIEW.EXPLORE_VIEW) {
      DefaultMutableTreeNode[] node =
          this.controller.getMainframe().getPanelExplorerView().getSelectedNodes();
      if (node == null) return null;
      Object sobj = node[0].getUserObject();
      Object eobj = node[node.length - 1].getUserObject();
      if (sobj == eobj) {
        if (sobj instanceof IInformation) {
          info = (IInformation) sobj;
        }
      } else {
        if (sobj != null && eobj != null) {
          if (sobj instanceof IInformation && eobj instanceof IInformation) {
            // Get the additional information class from multiple ranges of additional information.
            info =
                getProgramInformation(
                    new IInformation[] {(IInformation) sobj, (IInformation) eobj});
            /*Program fortran = this.controller.getFortranLanguage();
            if (fortran != null) {
                info = fortran.getInformation((IInformation) sobj, (IInformation) eobj);
            }*/
            // 2012/10/30 Marge yabe
          }
        }
      }
    } else if (this.view == FRAME_VIEW.ANALYSIS_VIEW) {
      info = this.controller.getMainframe().getPanelAnalysisView().getSelectedInformation();
      if (info == null) return null;
    }
    return info;
  }

  /**
   * Get additional selection information
   *
   * @return Selectable additional information
   */
  private boolean isSelectedInformation() {
    if (this.view == FRAME_VIEW.EXPLORE_VIEW) {
      DefaultMutableTreeNode[] node =
          this.controller.getMainframe().getPanelExplorerView().getSelectedNodes();
      if (node == null) return false;
      Object sobj = node[0].getUserObject();
      Object eobj = node[node.length - 1].getUserObject();
      if (sobj != null && eobj != null) {
        if (sobj instanceof IInformation && eobj instanceof IInformation) {
          return true;
        }
      } else if (sobj != null) {
        if (sobj instanceof IInformation) {
          return true;
        }
      }
      return false;
    } else if (this.view == FRAME_VIEW.ANALYSIS_VIEW) {
      IInformation info =
          this.controller.getMainframe().getPanelAnalysisView().getSelectedInformation();
      if (info == null) return false;
      return true;
    }

    return false;
  }

  /**
   * Edit additional information
   *
   * @param infoNode Additional information
   * @return Whether to edit additional information
   */
  public boolean editInformation(IInformation infoNode) {
    return editInformation(infoNode, null, true);
  }

  /**
   * Edit additional information
   *
   * @param infoNode Additional information
   * @param editable Whether additional information can be edited
   * @return Whether to edit additional information
   */
  /*
  public boolean editInformation(IInformation infoNode, boolean editable) {
      return editInformation(infoNode, null, editable);
  }
   */

  /**
   * Edit additional information
   *
   * @param infoNode Additional information
   * @param addinfo Additional information
   * @return Whether to edit additional information
   */
  public boolean editInformation(IInformation infoNode, String addinfo) {
    return editInformation(infoNode, addinfo, true);
  }

  /**
   * Edit additional information
   *
   * @param infoNode Additional information
   * @param addinfo Additional information
   * @param editable Whether additional information can be edited
   * @return Whether to edit additional information
   */
  public boolean editInformation(IInformation infoNode, String addinfo, boolean editable) {
    if (infoNode == null) return false;

    // Status message
    final String message = Message.getString("mainmenu.edit.info"); // Edit additional information
    Application.status.setMessageMain(message);

    // main frame
    Frame frame = this.controller.getMainframe();

    // Project folder
    File projectFolder = this.controller.getProjectModel().getProjectFolder();

    // Additional information
    TextInfo info = infoNode.getInformation();
    if (info == null) {
      info = new TextInfo();
    }
    // Additional information
    String content = info.getContent();
    if (content == null) {
      content = "";
    }
    if (addinfo != null && !addinfo.isEmpty()) {
      if (content != null && !content.isEmpty()) {
        content += "\n";
      }
      content += addinfo;
    }
    // Display the additional information edit dialog.
    InformationDialog dialog = new InformationDialog(frame, true);
    dialog.setProjectFolder(projectFolder);
    dialog.setInformation(content);
    dialog.setBlockName(infoNode.toString());
    dialog.setEditable(editable);

    // Display a dialog
    int result = dialog.showDialog();
    if (result == Constant.CANCEL_DIALOG) {
      // Cancel editing of additional information
      Application.status.setMessageMain(
          message + Message.getString("action.common.cancel.status")); // :Cancel
      return false;
    }

    // Set the updated additional information
    content = dialog.getInformation();
    info.setContent(content);
    // Set additional information on the node
    infoNode.setInformation(info);

    // Additional information panel model settings
    InformationModel infoModel =
        this.controller.getMainframe().getPanelAnalysisView().getPanelInformation().getModel();
    // Project folder settings
    infoModel.setProjectFolder(projectFolder);

    // Additional information settings
    infoModel.setTitle(infoNode.toString());
    infoModel.setInformation(infoNode, info);

    // Redraw the explorer view
    this.controller.getMainframe().getPanelExplorerView().fireSelectNodeChanged();

    // Update variable characteristic list information
    this.controller.refreshInformation();

    Application.status.setMessageMain(
        message + Message.getString("action.common.done.status")); // : Done
    return true;
  }

  /**
   * Get the additional information class from multiple ranges of additional information. Generate
   * and return InformationBlock of additional information range in Program class. <br>
   * If there is only one additional information range, the first additional information is
   * returned.
   *
   * @param infos Additional information range
   * @return Additional information
   */
  private IInformation getProgramInformation(IInformation[] infos) {
    if (infos == null) return null;
    Program fortran = this.controller.getFortranLanguage();
    if (fortran == null) return null;
    if (infos.length == 0) return null;
    // single
    if (infos.length == 1) {
      return infos[0];
    }
    // Multiple ranges
    IInformation start = infos[0];
    IInformation end = infos[infos.length - 1];
    if (start != null && end != null) {
      IInformation info = fortran.getInformation(start, end);
      return info;
    }

    return start;
  }
}
