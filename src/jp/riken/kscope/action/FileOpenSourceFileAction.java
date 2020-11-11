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
// import java.util.Arrays;
import java.util.List;
import javax.swing.JOptionPane;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.Program;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.properties.ProgramProperties;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Action class to open source file
 *
 * @author RIKEN
 */
public class FileOpenSourceFileAction extends ActionBase {

  /** Source file source view */
  private FRAME_VIEW view;

  /**
   * Constructor
   *
   * @param controller Application controller
   * @param view Source file source view
   */
  public FileOpenSourceFileAction(AppController controller, FRAME_VIEW view) {
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

    CodeLine line = getSelectedCodeLine();
    if (line == null) return false;
    SourceFile source = line.getSourceFile();
    if (source == null) return false;
    ProgramProperties properties = this.controller.getPropertiesExtension();
    Program program = properties.getMatchProgram(source.getPath());

    return program != null;
  }

  /**
   * Get the currently selected line of code
   *
   * @return line of selection code
   */
  private CodeLine getSelectedCodeLine() {

    CodeLine line = null;
    if (this.view == FRAME_VIEW.SOURCE_VIEW) {
      line = this.controller.getMainframe().getPanelSourceView().getSelectedCodeLine();
    } else if (this.view == FRAME_VIEW.EXPLORE_VIEW) {
      CodeLine[] lines =
          this.controller.getMainframe().getPanelExplorerView().getSelectedCodeLines();
      if (lines != null && lines.length > 0) {
        line = lines[0];
      }
    }
    return line;
  }

  /**
   * Event to open source file
   *
   * @param event Event information
   */
  @Override
  public void actionPerformed(ActionEvent event) {

    // Status message
    final String message = Message.getString("mainmenu.file.program"); // Open with an external tool
    Application.status.setMessageMain(message);

    // Execution check
    CodeLine line = getSelectedCodeLine();
    if (line == null) {
      this.controller
          .getErrorInfoModel()
          .addErrorInfo(
              Message.getString(
                  "fileopensourcefileaction.openfile.notarget.errinfo")); // No file is selected.
      Application.status.setMessageMain(
          message + Message.getString("action.common.error.status")); // :error
      return;
    }
    SourceFile source = line.getSourceFile();
    int startline = line.getStartLine();
    if (source == null || source.getFile() == null) {
      this.controller
          .getErrorInfoModel()
          .addErrorInfo(
              line,
              Message.getString(
                  "fileopensourcefileaction.openfile.filenotget.errinfo")); // Could not get the
                                                                            // file.
      Application.status.setMessageMain(
          message + ":" + Message.getString("action.common.error.status")); // :error
      return;
    }
    File file = source.getFile();
    if (!file.isAbsolute()) {
      file =
          new File(
              this.controller.getProjectModel().getProjectFolder().getAbsolutePath()
                  + File.separator
                  + file.getPath());
    }
    if (!file.exists()) {
      this.controller
          .getErrorInfoModel()
          .addErrorInfo(
              line,
              Message.getString(
                  "fileopensourcefileaction.openfile.notexist.errinfo")); // The file does not
                                                                          // exist.
      Application.status.setMessageMain(
          message + ":" + Message.getString("action.common.error.status")); // :error
      return;
    }

    ProgramProperties properties = this.controller.getPropertiesExtension();
    Program program = properties.getMatchProgram(source.getPath());
    if (program == null) {
      this.controller
          .getErrorInfoModel()
          .addErrorInfo(
              line,
              Message.getString(
                  "fileopensourcefileaction.openfile.noprogram.errinfo")); // No external tools have
                                                                           // been set.
      Application.status.setMessageMain(
          message + Message.getString("action.common.error.status")); // :error
      return;
    }

    String option = program.getOption();
    String filename = file.getAbsolutePath();
    // Start argument
    String[] args = getProgramArguments(option, filename, startline);
    if (args != null && args.length > 0) {
      // If the startup argument is set, the source file will be included in the startup argument.
      filename = null;
    }
    String programname = program.getExename();
    if (program.isRelation()) {
      programname = null;
    }
    // Executing an external program
    String errMsg = SwingUtils.processOpenProgram(filename, programname, args);
    if (errMsg != null && !errMsg.isEmpty()) {
      JOptionPane.showMessageDialog(
          this.controller.getMainframe(),
          errMsg,
          Message.getString("dialog.common.error"), // error
          JOptionPane.ERROR_MESSAGE);
      Application.status.setMessageMain(
          message + Message.getString("action.common.error.status")); // :error
      return;
    }
    Application.status.setMessageMain(
        message + Message.getString("action.common.done.status")); // Done
    return;
  }

  /**
   * Get startup arguments
   *
   * @param option Startup argument
   * @param filename Startup source file name
   * @param startline line number
   * @return Start argument
   */
  private String[] getProgramArguments(String option, String filename, int startline) {
    if (option == null || option.isEmpty()) return null;

    List<String> args = new ArrayList<String>();
    String[] opts = StringUtils.tokenizerDelimit(option, " ");
    if (opts == null) return null;
    boolean isadd = false;
    for (String arg : opts) {
      int posfile = arg.indexOf("%F");
      int posline = arg.indexOf("%L");
      if (posfile >= 0) {
        arg = arg.replaceAll("%F", filename);
        isadd = true;
      }
      if (posline >= 0) {
        arg = arg.replaceAll("%L", String.valueOf(startline));
      }
      args.add(arg);
    }
    if (!isadd) {
      args.add(filename);
    }
    if (args.size() <= 0) return null;

    return args.toArray(new String[0]);
  }
}
