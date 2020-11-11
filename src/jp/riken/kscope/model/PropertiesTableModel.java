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
import java.util.Observable;
import javax.swing.table.DefaultTableModel;
import jp.riken.kscope.Message;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Property table model
 *
 * @author RIKEN
 */
public class PropertiesTableModel extends Observable {

  /** Table header list */
  private String[] HEADER_COLUMNS = {
    Message.getString("propertiestablemodel.header_columns.name"), // item
    Message.getString("settingprojectdialog.column_header.value")
  }; // value

  /** Title */
  private String title;

  /** Property table model */
  private DefaultTableModel tableModel;

  /** Constructor */
  public PropertiesTableModel() {
    super();

    // Create a table model
    createTableModel();
  }

  /** Notify model changes */
  private void notifyModel() {
    this.setChanged();
    this.notifyObservers();
    this.clearChanged();
  }

  /**
   * Get the table model
   *
   * @return table model
   */
  public DefaultTableModel getTableModel() {
    return tableModel;
  }

  /** Create a table model */
  private void createTableModel() {
    // Create a table model
    tableModel = new DefaultTableModel();
    tableModel.setColumnIdentifiers(HEADER_COLUMNS);
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
   * Add a table row
   *
   * @param item Item name
   * @param value value
   */
  public void addProperties(String item, String value) {
    String[] row = {item, value};
    tableModel.addRow(row);

    // Notify model changes
    notifyModel();
  }

  /**
   * Add a table row
   *
   * @param item Item name
   * @param value value
   */
  public void setProperties(String item[], String value[]) {
    // Create a table model
    createTableModel();
    if (item == null || item.length <= 0) return;

    int count = item.length;
    for (int i = 0; i < count; i++) {
      String[] row = {item[i], value[i]};
      tableModel.addRow(row);
    }

    // Notify model changes
    notifyModel();
  }

  /** Clear the table model. */
  public void clearProperties() {
    // Create a table model
    createTableModel();
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
    if (this.tableModel == null) return;
    if (isEmpty()) return;

    try {
      // File output
      PrintWriter pw = new PrintWriter(new BufferedWriter(new FileWriter(file)));

      // Table data
      String buf = SwingUtils.toCsv(this.tableModel);
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
   * @return Whether it is empty (ture: empty, false: with data)
   */
  public boolean isEmpty() {
    if (this.tableModel == null) return true;
    return (this.tableModel.getRowCount() < 1);
  }
}
