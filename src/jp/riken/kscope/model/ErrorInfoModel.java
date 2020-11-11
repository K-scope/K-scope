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

package jp.riken.kscope.model;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Observable;
import java.util.concurrent.CopyOnWriteArrayList;
import javax.swing.SwingUtilities;
import javax.swing.table.DefaultTableModel;
import jp.riken.kscope.Message;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.ErrorInfo;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.utils.FileUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Error information model
 *
 * @author RIKEN
 */
public class ErrorInfoModel extends Observable {

  /** Table header list The first column is CodeLine information. */
  private String[] HEADER_COLUMNS = {
    "",
    Message.getString("settingprojectdialog.column_header.message"), // message
    Message.getString("mainmenu.file"), // File
    Message.getString("languageservice.properties.linenumber")
  }; // line number

  /** Title */
  private String title;

  /** Project folder */
  private File projectFolder = null;

  /** Error information list */
  private List<ErrorInfo> listError = null;

  /** Constructor */
  public ErrorInfoModel() {
    super();

    // Create a table model
    createTableModel();
  }

  /** Notify model changes */
  private void notifyModel() {
    SwingUtilities.invokeLater(
        new Runnable() {
          @Override
          public void run() {
            setChanged();
            notifyObservers();
            clearChanged();
          }
        });
  }

  /**
   * Get the table model
   *
   * @return table model
   */
  public DefaultTableModel getTableModel() {
    return createTableModel();
  }

  /** Create a table model */
  private DefaultTableModel createTableModel() {
    // Create a table model
    DefaultTableModel tableModel = new DefaultTableModel();
    tableModel.setColumnIdentifiers(HEADER_COLUMNS);

    // Create a table model from the error list.
    if (listError == null) return tableModel;
    CopyOnWriteArrayList<ErrorInfo> copyList = new CopyOnWriteArrayList<ErrorInfo>(listError);
    for (ErrorInfo error : copyList) {
      CodeLine line = error.getCodeLine();
      String message = error.getMessage();
      String filename = null;
      if (line != null) {
        if (line.getSourceFile() != null && line.getSourceFile().getFile() != null) {
          File file = line.getSourceFile().getFile();
          filename = getRelativePath(file);
          if (filename == null) filename = line.getStrSourceFile();
        } else {
          filename = line.getStrSourceFile();
        }
      }

      String no = null;
      if (line != null) {
        no = line.getLineno();
      }

      // Create table row array
      Object[] row = {line, message, filename, no};

      // Add table row
      tableModel.addRow(row);
    }

    return tableModel;
  }

  /**
   * Get the header column list.
   *
   * @return Header column list
   */
  public String[] getHeaderColumns() {
    return HEADER_COLUMNS;
  }

  /**
   * Add error message
   *
   * @param lineInfo Error location information
   * @param message Error message
   */
  public void addErrorInfo(CodeLine lineInfo, String message) {

    // Add an error message row to the table model
    addTableRow(lineInfo, message);

    // Notify model changes
    notifyModel();
  }

  /**
   * Add error message
   *
   * @param infos Error information list
   */
  public void addErrorInfos(ErrorInfo[] infos) {
    if (infos == null) return;

    // Add an error message row to the table model
    addTableRows(infos);

    // Notify model changes
    notifyModel();
  }

  /**
   * Add error message to error information list
   *
   * @param lineInfo Error location information
   * @param message Error message
   */
  private void addTableRow(CodeLine lineInfo, String message) {
    if (this.listError == null) {
      this.listError = new ArrayList<ErrorInfo>();
    }
    listError.add(new ErrorInfo(lineInfo, message));
  }

  /**
   * Add error information list to error information list
   *
   * @param infos Error information list
   */
  private void addTableRows(ErrorInfo[] infos) {
    if (infos == null) return;
    if (this.listError == null) {
      this.listError = new ArrayList<ErrorInfo>();
    }
    listError.addAll(Arrays.asList(infos));
  }

  /**
   * Add error information to the error information list
   *
   * @param info Error information
   */
  private void addTableRow(ErrorInfo info) {
    if (info == null) return;
    if (this.listError == null) {
      this.listError = new ArrayList<ErrorInfo>();
    }
    listError.add(info);
  }

  /**
   * Add error message
   *
   * @param message Error message
   */
  public void addErrorInfo(String message) {

    // Add the error message to the error information list
    addTableRow(null, message);

    // Notify model changes
    notifyModel();
  }

  /**
   * Add error message
   *
   * @param ex Error occurrence exception
   */
  public void addErrorInfo(Exception ex) {
    String message = ex.getMessage();
    if (message == null) {
      message = ex.toString();
    }
    // Add the error message to the error information list
    addTableRow(null, message);

    // Notify model changes
    notifyModel();
  }

  /**
   * Add error message
   *
   * @param file Error file
   * @param message Error message
   */
  public void addErrorInfo(SourceFile file, String message) {
    addErrorInfo(file, message, 0);
  }

  /**
   * Add error message
   *
   * @param file Error file
   * @param message Error message
   * @param lineno Error line number
   */
  public void addErrorInfo(SourceFile file, String message, int lineno) {

    String fn = null;
    if (file != null) {
      fn = file.getPath();
    }
    CodeLine line = new CodeLine(file, lineno, lineno, fn);

    // Add the error message to the error information list
    addTableRow(line, message);

    // Notify model changes
    notifyModel();
  }

  /**
   * Add error message
   *
   * @param lineInfos Error location information
   * @param messages error messages
   */
  public void setErrorInfoList(CodeLine lineInfos[], String messages[]) {
    // Create a table model
    createTableModel();

    for (int i = 0; i < lineInfos.length; i++) {
      // Add an error message row to the table model
      addTableRow(lineInfos[i], messages[i]);
    }

    // Notify model changes
    notifyModel();
  }

  /** Clear the table model. */
  public void clearErrorList() {
    if (this.listError == null) {
      this.listError = new ArrayList<ErrorInfo>();
    }
    this.listError.clear();

    // Notify model changes
    notifyModel();
  }

  /**
   * Get the error information list
   *
   * @return Error information list
   */
  public List<ErrorInfo> getErrorList() {
    return this.listError;
  }

  /**
   * Get the number of error information list
   *
   * @return Number of error information lists
   */
  public int getErrorListCount() {
    if (this.listError == null) return 0;
    return this.listError.size();
  }

  /**
   * Get the title
   *
   * @return title
   */
  public String getTitle() {
    return title;
  }

  /**
   * Set the title
   *
   * @param title Title
   */
  public void setTitle(String title) {
    this.title = title;
  }

  /**
   * Get the relative path from the project folder
   *
   * @param file Target file
   * @return Relative path
   */
  private String getRelativePath(File file) {
    if (file == null) return null;
    if (this.projectFolder == null) {
      return file.getAbsolutePath();
    }

    // Get the relative path
    return FileUtils.getRelativePath(file, this.projectFolder);
  }

  /**
   * Set the project folder
   *
   * @param folder Project folder
   */
  public void setProjectFolder(File folder) {
    this.projectFolder = folder;
  }

  /**
   * Output table information to a file.
   *
   * @param file Output file
   */
  public void writeFile(File file) {

    // Error table model
    DefaultTableModel tableModel = createTableModel();

    if (tableModel == null) return;

    try {
      // File output
      PrintWriter pw = new PrintWriter(new BufferedWriter(new FileWriter(file)));

      // Table data
      String buf = SwingUtils.toCsv(tableModel);
      // File output
      pw.print(buf);

      pw.close();

    } catch (IOException ex) {
      ex.printStackTrace();
    }
  }

  /**
   * Whether the model is empty
   *
   * @return Whether it is empty (true: empty, false: with data)
   */
  public boolean isEmpty() {
    if (this.listError == null) return true;
    return (this.listError.size() < 1);
  }

  /**
   * Add error message
   *
   * @param error Error information
   */
  public void addErrorInfo(ErrorInfo error) {
    if (error == null) return;

    // Add an error message row to the table model
    addTableRow(error);

    // Notify model changes
    notifyModel();
  }
}
