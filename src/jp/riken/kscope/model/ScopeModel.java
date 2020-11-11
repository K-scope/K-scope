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
import java.util.List;
import java.util.Observable;
import javax.swing.table.DefaultTableModel;
import jp.riken.kscope.Message;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Variable scope model
 *
 * @author RIKEN
 */
public class ScopeModel extends Observable {

  /** Table header list */
  private String[] HEADER_COLUMNS = {
    Message.getString("mainmenu.analysis.valiablescope")
  }; // Variable valid area

  /** Title */
  private String title;

  /** Variable scope information */
  private List<String> listScope;

  /** Constructor */
  public ScopeModel() {
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
   * Set variable effective area data
   *
   * @param list Variable scope data
   */
  public void setScope(String[] list) {
    // Clear table model
    this.listScope = new ArrayList<String>();

    if (list != null) {
      this.listScope.addAll(java.util.Arrays.asList(list));
    }

    // Notify model changes
    notifyModel();
  }

  /** Clear the table model. */
  public void clear() {
    // Clear table model
    this.listScope = new ArrayList<String>();
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

      // Block operation count
      if (this.listScope == null || this.listScope.size() <= 0) return;

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
    if (this.listScope == null) return tableModel;

    for (String area : this.listScope) {
      // Create table row array
      Object[] row = new Object[HEADER_COLUMNS.length];
      row[0] = area;
      // Add table row
      tableModel.addRow(row);
    }

    return tableModel;
  }

  /**
   * Whether the model is empty
   *
   * @return Whether it is empty (true: empty, false: with data)
   */
  public boolean isEmpty() {
    if (this.listScope == null) return true;
    return (this.listScope.size() < 1);
  }
}
