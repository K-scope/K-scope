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
import java.io.IOException;
import java.util.concurrent.Callable;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.Constant;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.dialog.FileExportSourceFileDialog;
import jp.riken.kscope.model.ProjectModel;
import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.FutureService;
import jp.riken.kscope.utils.FileUtils;
import jp.riken.kscope.utils.StringUtils;

/**
 * Source file export action.
 *
 * @author RIKEN
 */
public class FileExportSourceFileAction extends ActionBase {
  /** Project folder */
  private File prjFolder;
  /** Source folder */
  private File srcFolder;
  /** Output folder */
  private File outFolder;
  /** Status message */
  private String message = "";

  /**
   * Constructor
   *
   * @param controller Application controller
   */
  public FileExportSourceFileAction(AppController controller) {
    super(controller);
    message =
        Message.getString("fileexportsourcefileaction.exportsource.status"); // Source file export
  }

  /**
   * Check if the action is executable. <br>
   * Check before executing the action and switch the menu enable. <br>
   *
   * @return true = Action can be executed
   */
  @Override
  public boolean validateAction() {
    // The project has been created
    ProjectModel model = this.controller.getProjectModel();
    if (model == null) return false;
    if (model.getProjectTitle() == null) return false;
    if (model.getProjectFolder() == null || !model.getProjectFolder().exists()) return false;

    return true;
  }

  @Override
  public void actionPerformed(ActionEvent event) {
    // Get the parent Frame.
    Frame frame = getWindowAncestor(event);
    Application.status.setMessageMain(message);

    FileExportSourceFileDialog dialog = new FileExportSourceFileDialog(frame, true);
    dialog.setProjectFolder(controller.getProjectModel().getProjectFolder());
    dialog.setExcludeFile(controller.getPropertiesApplication().getSourceExportExclude());

    int res = dialog.showDialog();
    if (res != Constant.OK_DIALOG) {
      Application.status.setMessageMain(
          message + Message.getString("action.common.cancel.status")); // :Cancel
      return;
    }

    // Get output folder
    outFolder = new File(dialog.getOutputFolder());
    // Copy destination directory check
    if (!outFolder.exists()) return;
    if (!outFolder.isDirectory()) return;

    // Project path
    String prjRoot = this.controller.getProjectModel().getProjectFolder().getAbsolutePath();
    // Project folder
    prjFolder = new File(prjRoot);
    // Source root path (if the source is outside the project folder)
    String sourceRoot = this.controller.getSourceTreeModel().getRootFolder();
    // Source root folder
    srcFolder = null;
    if (!StringUtils.isNullOrEmpty(sourceRoot) && !FileUtils.isChildPath(prjRoot, sourceRoot)) {
      srcFolder = new File(sourceRoot);
    }

    // Excluded file pattern string
    String exclude = dialog.getExcludeFilePattern();

    // Application system exclusion path list
    String[] excludePaths =
        new String[] {
          FileUtils.joinFilePath(prjFolder, KscopeProperties.SETTINGS_FOLDER).getAbsolutePath(),
          FileUtils.joinFilePath(prjFolder, KscopeProperties.PROJECT_FILE).getAbsolutePath()
        };

    Application.status.setMessageMain(
        message + Message.getString("action.common.process.status")); // :processing

    copyFiles(exclude, excludePaths, dialog.isExportOtherFile());
  }

  /**
   * Copy the source file.
   *
   * @param exclude Excluded file extension comma separated list
   * @param excludePaths Exclude file path list
   * @param other true = Copy other than the source file
   */
  private void copyFiles(String exclude, String[] excludePaths, boolean other) {
    final String excludeStr = exclude;
    final String[] excludePathArr = excludePaths;
    final boolean otherFlag = other;

    // Create a thread task service.
    FutureService<Integer> future =
        new FutureService<Integer>(
            /** Thread call class */
            new Callable<Integer>() {
              /** Perform thread execution */
              @Override
              public Integer call() {
                try {
                  Application.status.setProgressStart(true);
                  File[] cpFiles = null;
                  if (otherFlag) {
                    // Scan under the project folder
                    File[] files = FileUtils.getChildren(prjFolder, excludeStr, excludePathArr);

                    // If the source folder is not under the project folder, scan under the source
                    // folder as well
                    if (srcFolder != null
                        && !FileUtils.isChildPath(
                            prjFolder.getAbsolutePath(), srcFolder.getAbsolutePath())) {
                      File[] srcFiles =
                          FileUtils.getChildren(srcFolder, excludeStr, excludePathArr);
                      if (srcFiles != null && srcFiles.length > 0) {
                        if (files != null && files.length > 0) {
                          cpFiles = new File[files.length + srcFiles.length];
                          for (int i = 0; i < files.length; i++) {
                            cpFiles[i] = files[i];
                          }
                          for (int i = files.length; i < cpFiles.length; i++) {
                            cpFiles[i] = srcFiles[i - files.length];
                          }
                        } else {
                          cpFiles = srcFiles;
                        }
                      } else {
                        cpFiles = files;
                      }
                    } else {
                      cpFiles = files;
                    }
                  } else {
                    SourceFile[] sourceFiles = controller.getSourceTreeModel().getAllSourceFiles();
                    cpFiles = new File[sourceFiles.length];
                    for (int i = 0; i < sourceFiles.length; i++) {
                      cpFiles[i] = sourceFiles[i].getFile();
                    }
                  }

                  // Exit if copy target does not exist
                  if (cpFiles == null) return Constant.SUCCESS_RESULT;
                  if (cpFiles.length < 1) return Constant.SUCCESS_RESULT;

                  File outPrj = FileUtils.joinFilePath(outFolder, prjFolder.getName());
                  if (srcFolder == null || otherFlag) {
                    outPrj.mkdir();
                  }

                  // Source file folder Copy destination root
                  File outSrc = null;
                  if (srcFolder != null) {
                    outSrc = FileUtils.joinFilePath(outFolder, srcFolder.getName());
                    outSrc.mkdir();
                  }

                  int cnt = 0;
                  for (File f : cpFiles) {
                    if (FileUtils.isChildPath(prjFolder.getAbsolutePath(), f.getAbsolutePath())) {
                      String relPath = FileUtils.getRelativePath(f, prjFolder);
                      if (relPath.startsWith(".")) relPath = relPath.substring(2);

                      File newPath = FileUtils.joinFilePath(outPrj, relPath);
                      try {
                        FileUtils.copyFile(f, outPrj, newPath);
                        cnt++;
                        Application.status.setMessageStatus(
                            "Exporting (" + cnt + "/" + cpFiles.length + ")");
                      } catch (IOException e) {
                        // TODO Auto-generated catch block
                        e.printStackTrace();
                      }
                    } else if (FileUtils.isChildPath(
                        srcFolder.getAbsolutePath(), f.getAbsolutePath())) {
                      String relPath = FileUtils.getRelativePath(f, srcFolder);
                      if (relPath.startsWith(".")) relPath = relPath.substring(2);

                      File newPath = FileUtils.joinFilePath(outSrc, relPath);
                      try {
                        FileUtils.copyFile(f, outSrc, newPath);
                        cnt++;
                        Application.status.setMessageStatus(
                            "Exporting (" + cnt + "/" + cpFiles.length + ")");
                      } catch (IOException e) {
                        // TODO Auto-generated catch block
                        e.printStackTrace();
                      }
                    }
                  }
                  Application.status.setProgressStart(false);
                  return Constant.SUCCESS_RESULT;
                } catch (Exception e) {
                  e.printStackTrace();
                  Application.status.setMessageMain(
                      message + Message.getString("action.common.error.status")); // :error
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

    // Thread start
    new Thread(future).start();
  }
}
