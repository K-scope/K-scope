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
import java.util.ArrayList;
import java.util.List;
import javax.swing.JOptionPane;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.data.FILE_TYPE;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.model.FileTreeModel;
import jp.riken.kscope.model.ProjectModel;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.ProjectService;

/**
 * XML file deletion action class
 *
 * @author RIKEN
 */
public class ProjectDeleteFileAction extends ActionBase {

  /** Action Explorer Panel */
  private EXPLORE_PANEL panelExplore;

  /**
   * Constructor
   *
   * @param controller Application controller
   * @param panel Action explorer panel
   */
  public ProjectDeleteFileAction(AppController controller, EXPLORE_PANEL panel) {
    super(controller);
    panelExplore = panel;
  }

  /**
   * Check if the action is executable. <br>
   * Check before executing the action and switch the menu enable. <br>
   *
   * @return true = Action can be executed
   */
  @Override
  public boolean validateAction() {

    // Menu enable switching
    // Project information
    ProjectModel project = this.controller.getProjectModel();
    if (project == null) return false;
    // Have you added the XML file to your project?
    if (project.getListSelectedFile() == null || project.getListSelectedFile().size() <= 0) {
      // XML file unregistered
      return false;
    }

    if (project.getFileType() == FILE_TYPE.XCODEML_XML && panelExplore == EXPLORE_PANEL.XML) {
      return true;
    }
    if (project.getFileType() == FILE_TYPE.FORTRANLANG && panelExplore == EXPLORE_PANEL.SOURCE) {
      return true;
    }
    boolean enable = false;
    if (panelExplore == null || panelExplore == EXPLORE_PANEL.UNKNOWN) {
      // Action from mainframe
      EXPLORE_PANEL activePanel =
          this.controller.getMainframe().getPanelExplorerView().getSelectedEnumPanel();
      if (project.getFileType() == FILE_TYPE.XCODEML_XML) {
        // Enable only if the XML tree panel is displayed.
        enable = (activePanel == EXPLORE_PANEL.XML);
      }
      //            if (project.getFileType() == FILE_TYPE.FORTRANLANG) {
      // // Enable only when the source tree panel is displayed.
      //                enable = (activePanel == EXPLORE_PANEL.SOURCE);
      //            }
    }
    return enable;
  }

  /**
   * Action occurrence event
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {
    // Get the parent Frame.
    Frame frame = getWindowAncestor(event);
    // Project information
    ProjectModel project = this.controller.getProjectModel();
    // Status message
    String message =
        Message.getString("projectdeletefileaction.xml.delete.status"); // XML file deletion
    if (project.getFileType() == FILE_TYPE.FORTRANLANG) {
      message =
          Message.getString("projectdeletefileaction.source.delete.status"); // Delete source file
    }
    Application.status.setMessageMain(message);

    // Select selected file (select file OR get child files of selected folder)
    SourceFile[] selectedFiles = null;
    String title =
        Message.getString(
            "projectdeletefileaction.xml.confirmdialog.title"); // Delete XML file / folder
    String confirm =
        Message.getString(
            "projectdeletefileaction.xml.confirmdialog.message"); // Delete the selected XML file /
                                                                  // folder, are you sure?
    if (project.getFileType() == FILE_TYPE.XCODEML_XML) {
      selectedFiles =
          this.controller
              .getMainframe()
              .getPanelExplorerView()
              .getPanelXmlTree()
              .getSelectChildSourceFiles();
    } else if (project.getFileType() == FILE_TYPE.FORTRANLANG) {
      selectedFiles =
          this.controller
              .getMainframe()
              .getPanelExplorerView()
              .getPanelSourceTree()
              .getSelectChildSourceFiles();
      title =
          Message.getString(
              "projectdeletefileaction.source.confirmdialog.title"); // Delete source files /
                                                                     // folders
      confirm =
          Message.getString(
              "projectdeletefileaction.source.confirmdialog.message"); // Delete the selected source
                                                                       // file / folder, are you
                                                                       // sure?
    }
    if (selectedFiles == null) {
      JOptionPane.showMessageDialog(
          frame,
          Message.getString(
              "projectdeletefileaction.selectdialog.message"), // Select the file / folder you want
                                                               // to delete.
          Message.getString("dialog.common.error"), // error
          JOptionPane.ERROR_MESSAGE);

      Application.status.setMessageMain(
          message + Message.getString("action.common.error.status")); // error
      return;
    }

    // Display a confirmation message.
    int option =
        JOptionPane.showConfirmDialog(
            frame,
            confirm, // Delete the selected file / folder, are you sure?
            title, // Delete files / folders
            JOptionPane.OK_CANCEL_OPTION,
            JOptionPane.WARNING_MESSAGE);
    if (option != JOptionPane.OK_OPTION) {
      Application.status.setMessageMain(
          message + Message.getString("action.common.cancel.status")); // Cancel
      return;
    }

    // Project management service
    ProjectService service = new ProjectService(project);
    List<SourceFile> list = java.util.Arrays.asList(selectedFiles);
    service.deleteProjectSelectedFile(list);

    // XML file list after deletion from the project
    list = project.getListSelectedFile();

    // Display the selected XML file in the XML tree.
    List<SourceFile> xmlfiles = new ArrayList<SourceFile>();
    List<SourceFile> srcfiles = new ArrayList<SourceFile>();
    if (list != null && list.size() > 0) {
      for (SourceFile file : list) {
        if (FILE_TYPE.isFortranFile(file.getFile())) {
          srcfiles.add(file);
        } else if (FILE_TYPE.isXcodemlFile(file.getFile())) {
          xmlfiles.add(file);
        }
      }
    }
    FileTreeModel treeModel = null;
    List<SourceFile> setfiles = null;
    if (project.getFileType() == FILE_TYPE.XCODEML_XML) {
      // Display the selected XML file in the XML tree.
      treeModel = this.controller.getXmlTreeModel();
      setfiles = xmlfiles;
    } else if (project.getFileType() == FILE_TYPE.FORTRANLANG) {
      // Add Fortran file
      treeModel = this.controller.getSourceTreeModel();
      setfiles = srcfiles;
    }
    if (treeModel != null) {
      if (setfiles != null && setfiles.size() > 0) {
        treeModel.setSourceFile(setfiles.toArray(new SourceFile[0]));
      } else {
        treeModel.clearTreeModel();
      }
    }

    // Close the closed file in Source view
    for (int i = 0; i < selectedFiles.length; i++) {
      String filename = selectedFiles[i].getPath();
      this.controller.getMainframe().getPanelSourceView().closeSourceFile(filename);
    }

    Application.status.setMessageMain(
        message + Message.getString("action.common.done.status")); // Done

    return;
  }
}
