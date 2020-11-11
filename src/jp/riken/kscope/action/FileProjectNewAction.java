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
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Callable;
import javax.swing.JOptionPane;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.data.FILE_TYPE;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.dialog.FileProjectNewDialog;
import jp.riken.kscope.gui.ConsolePanel;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.FileTreeModel;
import jp.riken.kscope.model.LanguageTreeModel;
import jp.riken.kscope.model.ModuleTreeModel;
import jp.riken.kscope.model.ProjectModel;
import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.properties.ProjectProperties;
import jp.riken.kscope.properties.RemoteBuildProperties;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.FutureService;
import jp.riken.kscope.service.LanguageService;
import jp.riken.kscope.service.ProjectMakeService;
import jp.riken.kscope.service.ProjectService;
import jp.riken.kscope.xcodeml.XcodeMLParserStax;

/**
 * New project action
 *
 * @author RIKEN
 */
public class FileProjectNewAction extends ActionBase {
  /** Class that generates intermediate code */
  private ProjectMakeService makeService;
  /** Project clear action */
  private ProjectClearLanguageAction clearAction;
  /** Database construction service */
  private LanguageService languageService;
  /** Project construction service */
  private ProjectService projectService;

  private Boolean debug = (System.getenv("DEBUG") != null);

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public FileProjectNewAction(AppController controller) {
    super(controller);
  }

  /**
   * Check if the action is executable. <br>
   * Check before executing the action and switch the menu enable. <br>
   *
   * @return true = Action can be executed
   */
  @Override
  public boolean validateAction() {
    // Check the execution status of the thread task
    return this.controller.isThreadTaskDone();
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
    // Check if have opened project we need to save and close
    FileProjectSaveAction save_action = new FileProjectSaveAction(this.controller);
    if (save_action.validateAction()) {
      int option =
          JOptionPane.showConfirmDialog(
              frame,
              Message.getString(
                  "fileprojectsaveaction.save.confirm.dialog.message"), // Do you want to save the
                                                                        // project?
              Message.getString(
                  "mainmenu.file.closeproject"), // close the project, // save the project
              JOptionPane.OK_CANCEL_OPTION,
              JOptionPane.WARNING_MESSAGE);

      if (option == JOptionPane.OK_OPTION) {
        save_action.saveProject(frame);
      }
      FileProjectCloseAction close_action = new FileProjectCloseAction(this.controller);
      try {
        close_action.closeProject();
      } catch (Exception e) {
        System.err.println("Error closing project");
        e.printStackTrace();
      }
    }

    final String message = Message.getString("mainmenu.file.newproject"); // Create a new project
    // RemoteBuildProperties rb_properties = null;
    Application.status.setMessageMain(message);

    // Last access folder
    String currentFolder = this.controller.getLastAccessFolder();
    // Read default value of use_sshconnect
    ProjectProperties pproperties = this.controller.getPropertiesProject();
    // rb_properties = this.controller.getRBproperties();

    // Display the new project dialog.
    FileProjectNewDialog dialog =
        new FileProjectNewDialog(frame, true, pproperties, this.controller);
    dialog.setLastAccessFolder(currentFolder);
    // Set the exclusion path name
    dialog.addExcludeName(KscopeProperties.SETTINGS_FOLDER);
    // Display the string if there is a make command in the project properties
    dialog.setBuildCommand(
        this.controller
            .getPropertiesProject()
            .getPropertyValue(ProjectProperties.BUILD_COMMAND)
            .getValue());
    // Title, Makefile, Save flag setting deleted at 2013/05/30 by @hira
    // Display the character string if there is a title setting in the project property
    dialog.setProjectTitle(
        this.controller
            .getPropertiesProject()
            .getPropertyValue(ProjectProperties.PRJ_TITLE)
            .getValue());
    // Show the Makefile path if it exists in the project properties
    // dialog.setMakefilePath(this.controller.getPropertiesProject().getPropertyValue(ProjectProperties.MAKEFILE_PATH).getValue());
    // Set whether to save the project immediately after creating the project
    // dialog.setSaveFlag (this.controller.getPropertiesApplication (). getSaveProjectAfterCreate
    // ());

    int result = dialog.showDialog();

    if (result != Constant.OK_DIALOG) {
      Application.status.setMessageMain(
          message + Message.getString("action.common.cancel.status")); // :Cancel
      return;
    }

    // Last access folder
    this.controller.setLastAccessFolder(new File(dialog.getProjectFolder()));
    // Whether to generate intermediate code
    boolean genCode = dialog.isGenerateIntermediateCode();

    String rs_file = dialog.remoteSettingsFile(); // RB settings file from dialog
    // Set Project property
    pproperties.getPropertyValue(RemoteBuildProperties.SETTINGS_FILE).setValue(rs_file);

    // Selected source
    boolean selectedXml = dialog.isSelectedXml();
    FILE_TYPE type = FILE_TYPE.XCODEML_XML;
    if (!genCode && !selectedXml) {
      // Fortran source file
      type = FILE_TYPE.FORTRANLANG;
    }
    try {
      // String makeCom = dialog.getMakeCommand();  // make command as set in New Project dialog.
      // Full path if executable file.
      // String makefilePath = dialog.getMakefilePath(); // path to makefile as set in New Project
      // dialog. If set " " (space), makefilePath = " ".
      String build_command =
          dialog.getBuildCommand(); // build command as set in text field in New Project dialog.
      String clean_command =
          dialog.getCleanCommand(); // clean command as set in text field in New Project dialog.

      // close the project
      FileProjectCloseAction closeAction = new FileProjectCloseAction(this.controller);
      closeAction.clearProject();

      // Project information
      ProjectModel project = this.controller.getProjectModel();
      // Create a new project
      projectService = new ProjectService(project);
      projectService.createProject(
          dialog.getPeojectTitle(),
          new File(dialog.getProjectFolder()),
          dialog.getProjectXmlList(),
          type);

      // Create an error model
      ErrorInfoModel errorModel = this.controller.getErrorInfoModel();
      projectService.setErrorInfoModel(errorModel);

      // Clear the Fortran database
      clearAction = new ProjectClearLanguageAction(this.controller);
      clearAction.clearFortranLanguage();

      /** Structural analysis related information setting */
      // Fortran database
      Fortran fortran = this.controller.getFortranLanguage();
      // Create XML parser
      XcodeMLParserStax xmlParser = new XcodeMLParserStax();
      // Sourcetree model
      FileTreeModel fileModel = this.controller.getSourceTreeModel();
      // Structural tree model
      LanguageTreeModel languageModel = this.controller.getLanguageTreeModel();
      // Module tree model
      ModuleTreeModel moduleModel = this.controller.getModuleTreeModel();
      // Class that builds and searches the database
      languageService = new LanguageService(fortran);
      // Parser settings
      languageService.setPerser(xmlParser);
      // Set the structure tree model
      languageService.setLanguageTreeModel(languageModel);
      // Set up the module tree model
      languageService.setModuleTreeModel(moduleModel);
      // Set the error information model.
      languageService.setErrorInfoModel(errorModel);
      // Set the source tree model.
      languageService.setSourceTreeModel(fileModel);
      // Set the project folder
      languageService.setProjectFolder(this.controller.getProjectModel().getProjectFolder());

      /** Property information setting Required when saving a file */
      // Setting keyword properties
      projectService.setPropertiesKeyword(this.controller.getPropertiesKeyword());
      // External tool property settings
      projectService.setPropertiesExtension(this.controller.getPropertiesExtension());
      // Arithmetic count property setting
      projectService.setPropertiesOperand(this.controller.getPropertiesOperation());
      // Source view property settings
      projectService.setPropertiesSource(this.controller.getPropertiesSource());
      // Profiler property settings
      projectService.setPropertiesProfiler(this.controller.getPropertiesProfiler());

      // Request Byte / FLOP configuration property
      projectService.setPropertiesMemory(this.controller.getPropertiesMemory());

      projectService.setRBproperties(this.controller.getRBproperties());

      // Make related information
      File work = null;

      // Project property settings
      // TODO: check if need to move this below if-statement.
      projectService.setPropertiesProject(this.controller.getPropertiesProject());
      // Generate intermediate code
      if (genCode) {
        // Show console
        this.controller.setSelectedAnalysisPanel(ANALYSIS_PANEL.CONSOLE);

        work = project.getProjectFolder();

        // Project property settings
        ProjectProperties pp = this.controller.getPropertiesProject();
        pp.setBuildCommand(build_command);
        pp.setCleanCommand(clean_command);

        if (rs_file != null
            && rs_file.length() > 0) { // Set command line options for remote build command
          // rb_properties = this.controller.getRBproperties();
          pproperties.setLocalPath(work.getAbsolutePath());
          pproperties.setSettingsFile(rs_file);
          // pproperties.setRemoteProperties(rb_properties);
        }
      }
      // Do not generate intermediate code
      else {
        if (project.getListSelectedFile().size() < 1) {
          Application.status.setMessageMain(
              message + Message.getString("action.common.error.status")); // :error
          String filetype = "XML";
          if (type == FILE_TYPE.FORTRANLANG) filetype = "Fortran";
          String msg =
              Message.getString(
                  "fileprojectnewaction.createprojecterr.dialog.message",
                  filetype); // There is no filetype file in the specified folder. ...
          JOptionPane.showMessageDialog(
              frame,
              msg,
              Message.getString(
                  "fileprojectnewaction.createprojecterr.dialog.title"), // Project creation error
              JOptionPane.ERROR_MESSAGE);
          return;
        }
      }
      // Set PRJ_TITLE property of ProjectProperties to value from txtProjectTitle TextField
      this.controller.getPropertiesProject().setProjectTitle(dialog.getPeojectTitle());

      if (this.debug) {
        // TODO: check why rb_properties.getRemoteService() is null
        if (dialog.remoteBuild())
          System.out.println(
              "Calling execMake build_command="
                  + build_command
                  + ". Use remote service "
                  + pproperties.getRemoteService());
        else System.out.println("Calling execMake build_command=" + build_command + ".");
      }

      /** New execution */
      execMake(
          build_command,
          work,
          dialog.getProjectXmlList(),
          project,
          dialog.isBuild(),
          dialog.isSave(),
          genCode,
          (project.getFileType() == FILE_TYPE.XCODEML_XML));

      // NO MORE NEED IN REBUILD FLAG. "REBUILD" MENU DEPENDS ONLY ON GENXML AND FULL_PROJECT
      // PROPERTIES
      // Flag that project can be rebuilt
      // if (genCode) this.controller.getPropertiesProject().setRebuildFlag(true);

      // Set the project folder in the source view
      this.controller
          .getMainframe()
          .getPanelSourceView()
          .setProjectFolder(project.getProjectFolder());
      if (dialog.isBuild()) {
        this.controller
            .getMainframe()
            .getPanelExplorerView()
            .setSelectedPanel(EXPLORE_PANEL.LANGUAGE);
      } else {
        if (project.getFileType() == FILE_TYPE.XCODEML_XML) {
          // Activate the XML tab
          this.controller.getMainframe().getPanelExplorerView().setSelectedPanel(EXPLORE_PANEL.XML);
        } else if (project.getFileType() == FILE_TYPE.FORTRANLANG) {
          // Activate the Source tab
          this.controller
              .getMainframe()
              .getPanelExplorerView()
              .setSelectedPanel(EXPLORE_PANEL.SOURCE);
        }
      }

    } catch (Exception ex) {

      ex.printStackTrace();
      // Error message
      JOptionPane.showMessageDialog(
          frame,
          Message.getString(
              "fileprojectnewaction.newprojecterr.dialog.message"), // New project creation error
          message + Message.getString("dialog.common.error"), // error
          JOptionPane.ERROR_MESSAGE);

      // Status message
      Application.status.setMessageMain(
          message + Message.getString("action.common.error.status")); // :error
    }

    // Application.status.setMessageMain (message + Message.getString
    // ("action.common.done.status")); //: Done

  }

  /**
   * Create a new project
   *
   * @param build_command
   * @param work
   * @param xmls
   * @param model
   * @param build
   * @param save
   * @param make
   * @param mode
   */
  private void execMake(
      String build_command,
      File work,
      List<File> xmls,
      ProjectModel model,
      boolean build,
      boolean save,
      boolean make,
      boolean mode) {
    final String message = Message.getString("mainmenu.file.newproject"); // Create a new project
    makeService = new ProjectMakeService(work, this.controller);
    final ConsolePanel console =
        this.controller.getMainframe().getPanelAnalysisView().getPanelConsole();
    console.clearConsole();
    OutputStream out = console.getOutputStream();
    makeService.setOutputStream(out);
    final boolean bMake = make;
    final boolean bBuild = build;
    final boolean bSave = save;
    final boolean bModeXML = mode;
    final File prjFolder = model.getProjectFolder();
    final File settingFolder =
        new File(prjFolder.getAbsoluteFile() + File.separator + KscopeProperties.SETTINGS_FOLDER);
    final ProjectModel prjModel = model;
    final List<File> sourceFiles = xmls;
    final String build_c = build_command;

    // Create a thread task service.
    FutureService<Integer> future =
        new FutureService<Integer>(
            /** Thread call class */
            new Callable<Integer>() {
              /** Perform thread execution */
              @Override
              public Integer call() {
                try {
                  // Analyze execution
                  if (bMake) {
                    console.disable_horizontal_scroll = true; // disable scroll
                    boolean result = makeService.executeMakeCommand();
                    console.disable_horizontal_scroll = false; // enable scroll
                    if (!result) {
                      return Constant.CANCEL_RESULT;
                    }
                  }
                  Application.status.setMessageStatus("Set files...");
                  // Add intermediate code or Fortran files to the source list
                  FILE_TYPE filter = FILE_TYPE.XCODEML_XML;
                  FileTreeModel treeModel = controller.getXmlTreeModel();
                  if (!bModeXML) {
                    filter = FILE_TYPE.FORTRANLANG;
                    treeModel = controller.getSourceTreeModel();
                  }
                  SourceFile[] srcs =
                      projectService.getSourceFiles(
                          sourceFiles.toArray(new File[sourceFiles.size()]), filter, true);
                  if (debug != null) {
                    System.out.println(
                        "Check source files in in kscope/action/FileProjectNewAction.execMake()"
                            + " call :");
                    for (File fs : sourceFiles) {
                      System.out.println(fs.toString());
                    }
                  }
                  if (srcs == null) {
                    System.err.println(
                        "No XML files for project in FileProjectNewAction:"
                            + " kscope/action/FileProjectNewAction.execMake call.");
                    System.err.println(
                        "sourceFiles="
                            + sourceFiles
                            + " build_command="
                            + build_c
                            + " filer="
                            + filter);
                    return Constant.ERROR_RESULT;
                  } else if (srcs.length < 1) return Constant.ERROR_RESULT;
                  ArrayList<SourceFile> ls = new ArrayList<SourceFile>(Arrays.asList(srcs));

                  prjModel.setListXmlFile(ls);
                  languageService.setSourceFiles(ls.toArray(new SourceFile[0]));
                  // Update XML tree
                  if (treeModel != null) {
                    treeModel.setProjectFolder(prjFolder);
                    treeModel.setSourceFile(ls.toArray(new SourceFile[0]));
                  }

                  if (bBuild) {
                    if (languageService.canParse()) {
                      languageService.parseSourceFile();
                    } else {
                      return Constant.ERROR_RESULT;
                    }
                  }
                  if (bSave) {
                    if (projectService.existAllProperties()) {
                      projectService.saveProject(prjFolder);
                      languageService.writeLanguage(settingFolder);
                    } else {
                      return Constant.ERROR_RESULT;
                    }
                  }

                  return Constant.SUCCESS_RESULT;
                } catch (Exception e) {
                  e.printStackTrace();
                  return Constant.ERROR_RESULT;
                }
              }
            }) {
          /**
           * Thread execution completed. <br>
           * Perform post-processing when canceled.
           */
          @Override
          protected void done() {
            // Check if the end is due to cancellation.
            if (this.isCancelled()) {
              Application.status.setMessageMain(
                  message + Message.getString("action.common.cancel.status")); // :Cancel
            } else {
              Application.status.setMessageMain(
                  message + Message.getString("action.common.done.status")); // : Done
            }
            // Stop service execution
            if (makeService != null) {
              makeService.cancelRunning();
            }
            if (languageService != null) {
              languageService.cancelRunning();
            }

            super.done();
          }
        };
    // Clear status message
    Application.status.setMessageStatus(null);

    // Register the controller as a listener in the thread task: To call when the thread is
    // completed
    future.addPropertyChangeListener(this.controller);
    this.controller.setThreadFuture(future);

    // Display the progress dialog
    WindowProgressAction progress = new WindowProgressAction(this.controller);
    progress.showProgressDialog();
    Application.status.setProgressStart(true);

    // Thread start
    new Thread(future).start();
  }
}
