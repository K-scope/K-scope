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
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import javax.swing.JOptionPane;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.data.FILE_TYPE;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.exception.LanguageException;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.FileTreeModel;
import jp.riken.kscope.model.LanguageTreeModel;
import jp.riken.kscope.model.ModuleTreeModel;
import jp.riken.kscope.model.ProjectModel;
import jp.riken.kscope.properties.KscopeProperties;
// import jp.riken.kscope.properties.VariableMemoryProperties;
import jp.riken.kscope.service.AnalysisMemoryService;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.FutureService;
import jp.riken.kscope.service.LanguageService;
import jp.riken.kscope.service.ProjectService;
import jp.riken.kscope.utils.SwingUtils;
import jp.riken.kscope.xcodeml.XcodeMLParserStax;

/**
 * Action to open project
 *
 * @author RIKEN
 */
public class FileProjectOpenAction extends ActionBase {

  private static boolean debug = (System.getenv("DEBUG") != null);
  private static boolean debug_l2 = false;

  /** Class that builds and searches the database */
  private LanguageService serviceLanguage;
  /** Variable access destination memory service */
  private AnalysisMemoryService serviceMemory;
  /** Project clear action */
  private ProjectClearLanguageAction clearAction;

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public FileProjectOpenAction(AppController controller) {
    super(controller);
    if (debug) debug_l2 = (System.getenv("DEBUG").equalsIgnoreCase("high"));
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
    String message = Message.getString("mainmenu.file.openproject"); // open the project
    Application.status.setMessageMain(message);

    // Get the parent Frame.
    Frame frame = getWindowAncestor(event);

    // Last access folder
    String currentFolder = this.controller.getLastAccessFolder();
    if (currentFolder == null) {
      currentFolder = System.getProperty("user.dir");
    }

    // Display the project folder selection dialog.
    File selected =
        SwingUtils.showOpenProjectDialog(
            frame,
            Message.getString("dialog.common.selectproject.title"), // Select project folder
            currentFolder);
    if (selected == null) {
      Application.status.setMessageMain(
          message + Message.getString("action.common.cancel.status")); // :Cancel
      return;
    }
    // Is it a project configuration file?
    File projectFolder = null; // Project folder
    File projectFile = null; // Project configuration file
    if (selected.isFile() && KscopeProperties.PROJECT_FILE.equalsIgnoreCase(selected.getName())) {
      projectFolder = selected.getParentFile();
      projectFile = selected;
      // Last access folder
      this.controller.setLastAccessFolder(projectFolder);
    } else {
      projectFolder = selected;
      projectFile =
          new File(selected.getAbsolutePath() + File.separator + KscopeProperties.PROJECT_FILE);
      // Last access folder
      this.controller.setLastAccessFolder(projectFolder.getParentFile());
    }

    // Check the project folder
    // Project configuration file
    if (!projectFile.exists()) {
      // Error message
      JOptionPane.showMessageDialog(
          frame,
          Message.getString(
              "dialog.common.selectproject.notprojecterr.message"), // Not a project folder.
          Message.getString("dialog.common.error"), // error
          JOptionPane.ERROR_MESSAGE);
      return;
    }

    try {
      // Clear the project
      FileProjectCloseAction actionClose = new FileProjectCloseAction(this.controller);
      actionClose.clearProject();

      // Project model
      ProjectModel modelProject = this.controller.getProjectModel();
      // Project service
      ProjectService service = new ProjectService(modelProject);
      // Keyword properties
      service.setPropertiesKeyword(this.controller.getPropertiesKeyword());
      // External tool properties
      service.setPropertiesExtension(this.controller.getPropertiesExtension());
      // Arithmetic count property
      service.setPropertiesOperand(this.controller.getPropertiesOperation());
      // Source view settings properties
      service.setPropertiesSource(this.controller.getPropertiesSource());
      // Profiler property setting property
      service.setPropertiesProfiler(this.controller.getPropertiesProfiler());
      // Project properties
      service.setPropertiesProject(this.controller.getPropertiesProject());
      // Request Byte / FLOP configuration property
      service.setPropertiesMemory(this.controller.getPropertiesMemory());

      service.setRBproperties(this.controller.getRBproperties());
      // Error model
      service.setErrorInfoModel(this.controller.getErrorInfoModel());

      // open the project
      service.openProject(projectFolder);

      // Display the selected XML file in the XML tree.
      List<SourceFile> listSource = modelProject.getListSelectedFile();
      List<SourceFile> xmlfiles = new ArrayList<SourceFile>();
      List<SourceFile> srcfiles = new ArrayList<SourceFile>();
      if (listSource != null && listSource.size() > 0) {
        for (SourceFile file : listSource) {
          if (FILE_TYPE.isFortranFile(file.getFile())) {
            srcfiles.add(file);
          } else if (FILE_TYPE.isXcodemlFile(file.getFile())) {
            xmlfiles.add(file);
          }
        }
      }

      // Set the project folder in the source view
      this.controller
          .getMainframe()
          .getPanelSourceView()
          .setProjectFolder(modelProject.getProjectFolder());

      // Update properties
      this.controller.updateProperties();
      // Set the title in the project property
      this.controller.getPropertiesProject().setProjectTitle(modelProject.getProjectTitle());

      // Deserialize the Language class
      // settings folder
      File settingsFolder =
          new File(
              projectFolder.getAbsoluteFile() + File.separator + KscopeProperties.SETTINGS_FOLDER);
      readLanguage(
          settingsFolder, xmlfiles.toArray(new SourceFile[0]), srcfiles.toArray(new SourceFile[0]));
      if (xmlfiles.size() <= 0) {
        this.controller
            .getMainframe()
            .getPanelExplorerView()
            .setSelectedPanel(EXPLORE_PANEL.SOURCE);
        this.controller
            .getErrorInfoModel()
            .addErrorInfo(
                Message.getString(
                    "fileprojectopenaction.build.noxmlerr.errinfo")); // Since the XML file does not
                                                                      // exist, new structural
                                                                      // analysis cannot be
                                                                      // executed.
      }

    } catch (Exception e) {
      if (debug) e.printStackTrace();
      // Error message
      JOptionPane.showMessageDialog(
          frame,
          Message.getString(
              "fileprojectopenaction.openproject.openerr.dialog.message"), // Project open error
          message + Message.getString("dialog.common.error"), // error
          JOptionPane.ERROR_MESSAGE);

      // Status message
      Application.status.setMessageMain(
          message + Message.getString("action.common.error.status")); // :error
    }
  }

  /**
   * Deserialize the Language class
   *
   * @param folder Language class serialized folder
   * @param xmlfiles XML file list
   * @param srcfiles Source file list
   */
  public void readLanguage(
      final File folder, final SourceFile[] xmlfiles, final SourceFile[] srcfiles) {
    // Status message
    final String message = Message.getString("mainmenu.file.openproject"); // open the project

    // Clear the Fortran database
    clearAction = new ProjectClearLanguageAction(this.controller);
    clearAction.clearFortranLanguage();

    // Fortran database
    Fortran fortran = this.controller.getFortranLanguage();
    // Error information model
    ErrorInfoModel errorModel = this.controller.getErrorInfoModel();
    // Sourcetree model
    final FileTreeModel fileModel = this.controller.getSourceTreeModel();
    // Xml tree model
    final FileTreeModel xmlModel = this.controller.getXmlTreeModel();
    // Structural tree model
    LanguageTreeModel languageModel = this.controller.getLanguageTreeModel();
    // Module tree model
    ModuleTreeModel moduleModel = this.controller.getModuleTreeModel();
    // Create XML parser
    XcodeMLParserStax xmlParser = new XcodeMLParserStax();

    ProjectModel model = this.controller.getProjectModel();
    final File prjFolder = model.getProjectFolder();

    // Structural analysis service
    serviceLanguage = new LanguageService(xmlfiles, fortran, xmlParser);
    // Set the error information model.
    serviceLanguage.setErrorInfoModel(errorModel);
    // Set the source tree model.
    serviceLanguage.setSourceTreeModel(fileModel);
    // Set up an Xml tree model.
    serviceLanguage.setXmlTreeModel(xmlModel);
    // Set the structure tree model
    serviceLanguage.setLanguageTreeModel(languageModel);
    // Set the module tree model
    serviceLanguage.setModuleTreeModel(moduleModel);

    // Variable access destination memory service
    serviceMemory = new AnalysisMemoryService();
    // Variable access destination memory setting
    serviceMemory.setPropertiesVariableMemory(this.controller.getPropertiesVariable());

    // Create a thread task service.
    FutureService<Integer> future =
        new FutureService<Integer>(
            /** Thread call class */
            new Callable<Integer>() {
              /** Perform thread execution */
              @Override
              public Integer call() {
                try {
                  // Tree update
                  Application.status.setMessageStatus("Set files...");
                  xmlModel.setProjectFolder(prjFolder);
                  xmlModel.setSourceFile(xmlfiles);
                  fileModel.setProjectFolder(prjFolder);
                  fileModel.setSourceFile(srcfiles);

                  // Deserialize execution
                  serviceLanguage.readLanguage(folder);
                  controller.setFortranLanguage(serviceLanguage.getFortranLanguage());

                  // Get the variables set in the access destination memory
                  serviceMemory.createVariableMemoryProperties(
                      serviceLanguage.getFortranLanguage());

                  return Constant.SUCCESS_RESULT;

                } catch (LanguageException lang_ex) {
                  return Constant.ERROR_RESULT;
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
            // A new object has been created for the Fortran parsing result storage database.
            // Set in the application controller
            Fortran value = serviceLanguage.getFortranLanguage();
            if (value != null) {
              controller.setFortranLanguage(value);
            }

            // Check if the end is due to cancellation.
            if (this.isCancelled()) {
              // Clear the Fortran database
              clearAction.clearFortranLanguage();
              Application.status.setMessageMain(
                  message + Message.getString("action.common.cancel.status")); // :Cancel
            } else {
              Application.status.setMessageMain(
                  message + Message.getString("action.common.done.status")); // : Done
            }

            // Stop service execution
            if (serviceLanguage != null) {
              serviceLanguage.cancelRunning();
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
    WindowProgressAction progress = new WindowProgressAction(FileProjectOpenAction.this.controller);
    progress.showProgressDialog();
    Application.status.setProgressStart(true);

    // Thread start
    new Thread(future).start();
  }
}
