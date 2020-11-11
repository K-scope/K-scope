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
import java.util.List;
import java.util.Observable;
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;
import jp.riken.kscope.Message;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.profiler.ProfilerMeasureInfo;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Measurement timer information table model
 *
 * @author RIKEN
 */
public class ProfilerMeasureModel extends Observable {

  /** Table header list */
  private String[] HEADER_COLUMNS = {
    "",
    Message.getString("eprofstatementdialog.dialog.desc"), // Measurement interval setting
    Message.getString("profilercosttablemodel.header_columns.filename"), // file name
    Message.getString("profilercosttablemodel.header_columns.linenumber")
  }; // line number

  /** Table column size -1 = Hide */
  private int[] HEADER_COLUMNS_PREFERREDWIDTH = {-1, 240, 300, 80};
  /**
   * Minimum table column size. <br>
   * -1 = Hide
   */
  private int[] HEADER_COLUMNS_MINWIDTH = {-1, 80, 80, 40};

  /** Title */
  private String title;

  /** Measurement section information */
  private ProfilerMeasureInfo measureInfo;

  /** Constructor */
  public ProfilerMeasureModel() {
    super();
  }

  /** Notify model changes */
  private void notifyModel() {
    this.setChanged();
    this.notifyObservers();
    this.clearChanged();
  }

  /**
   * Get variable effective area default table model
   *
   * @return Variable scope default table model
   */
  public DefaultTableModel getScopeDefaultTableModel() {
    // Create a table model
    // Create a table model
    DefaultTableModel tableModel = new DefaultTableModel();
    tableModel.setColumnIdentifiers(HEADER_COLUMNS);
    return tableModel;
  }

  /**
   * Set measurement section information
   *
   * @param info Measurement section information
   */
  public void setMeasureInfo(ProfilerMeasureInfo info) {
    this.measureInfo = info;

    // Notify model changes
    notifyModel();
  }

  /**
   * Get measurement section information
   *
   * @return Measurement section information
   */
  public ProfilerMeasureInfo getMeasureInfo() {
    return this.measureInfo;
  }

  /** Clear the table model. */
  public void clearModel() {
    // Clear table model
    if (this.measureInfo != null) {
      this.measureInfo.clearMeasureInfo();
    }
    this.measureInfo = null;
    this.title = null;

    // Notify model changes
    notifyModel();
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
   * Output table information to a file.
   *
   * @param file Output file
   */
  public void writeFile(File file) {

    try {

      // Timer information
      if (this.measureInfo == null) return;

      // File output
      PrintWriter pw = new PrintWriter(new BufferedWriter(new FileWriter(file)));

      // Output the table
      String buf = SwingUtils.toCsv(getTableModel());
      pw.print(buf);

      pw.close();
    } catch (IOException ex) {
      ex.printStackTrace();
    }
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
    DefaultTableModel tableModel = getScopeDefaultTableModel();
    if (this.measureInfo == null) return tableModel;

    // Get timer data
    List<ProfilerMeasureInfo.MeasureData> list = this.measureInfo.getMeasureList();
    if (list == null || list.size() <= 0) return tableModel;
    for (ProfilerMeasureInfo.MeasureData data : list) {
      // Create table row array
      Object[] cols = new Object[HEADER_COLUMNS.length];
      cols[0] = data;
      cols[1] = data.toStringParam();
      CodeLine code = data.getMeasureArea();
      if (code != null) {
        cols[2] = code.getSourceFile().getPath();
        cols[3] = code.getLineno();
      }
      // Add table row
      tableModel.addRow(cols);
    }

    return tableModel;
  }

  /**
   * Set column width
   *
   * @param columnModel Table column model
   */
  public void setTableColumnWidth(DefaultTableColumnModel columnModel) {
    for (int i = 0; i < columnModel.getColumnCount(); i++) {
      // Get column
      TableColumn column = columnModel.getColumn(i);
      if (HEADER_COLUMNS_PREFERREDWIDTH.length >= i) {
        if (HEADER_COLUMNS_PREFERREDWIDTH[i] >= 0) {
          column.setPreferredWidth(HEADER_COLUMNS_PREFERREDWIDTH[i]);
        } else {
          column.setMinWidth(0);
          column.setMaxWidth(0);
          column.setPreferredWidth(0);
          column.setResizable(false);
        }
      }
      if (HEADER_COLUMNS_MINWIDTH.length >= i) {
        if (HEADER_COLUMNS_MINWIDTH[i] >= 0) {
          column.setMinWidth(HEADER_COLUMNS_MINWIDTH[i]);
        }
      }
    }
  }

  /**
   * Deletion of measurement interval data
   *
   * @param data Deleted measurement interval data
   */
  public void removeMeasureData(ProfilerMeasureInfo.MeasureData data) {
    if (data == null) return;
    if (this.measureInfo == null) return;
    this.measureInfo.removeMeasureData(data);

    // Notify model changes
    notifyModel();
  }

  /**
   * Whether the model is empty
   *
   * @return Whether it is empty (true: empty, false: with data)
   */
  public boolean isEmpty() {
    if (this.measureInfo == null) return true;
    return (this.measureInfo.getMeasureDataCount() < 1);
  }
}
