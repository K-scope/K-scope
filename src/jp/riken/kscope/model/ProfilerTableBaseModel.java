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
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;
import jp.riken.kscope.common.PROFILERINFO_TYPE;
import jp.riken.kscope.gui.ISourceBargraph;
import jp.riken.kscope.profiler.ProfilerBaseData;
import jp.riken.kscope.properties.ProfilerProperties;
import jp.riken.kscope.utils.SwingUtils;

/**
 * Profiler: Information-based model
 *
 * @author RIKEN
 */
public abstract class ProfilerTableBaseModel extends Observable {

  /** Profiler information type */
  private PROFILERINFO_TYPE enumInfo;
  /** Profiler Properties */
  private ProfilerProperties properties;

  /**
   * Constructor
   *
   * @param type Profiler information type
   */
  public ProfilerTableBaseModel(PROFILERINFO_TYPE type) {
    this.enumInfo = type;
  }

  /**
   * Get profiler information type
   *
   * @return Profiler information type
   */
  public PROFILERINFO_TYPE getEnumInfo() {
    return enumInfo;
  }

  /**
   * Set profiler information type
   *
   * @param type Profiler information type
   */
  public void setEnumInfo(PROFILERINFO_TYPE type) {
    this.enumInfo = type;
  }

  /** Clear the table model. */
  public abstract void clearModel();

  /**
   * Set selection profile information
   *
   * @param info Selection profile information
   */
  public abstract void setSelectedInfo(ProfilerBaseData info);

  /**
   * Get selection profile information
   *
   * @return Selection profile information
   */
  public abstract ProfilerBaseData getSelectedInfo();

  /**
   * Get the title
   *
   * @return title
   */
  public abstract String getTitle();

  /**
   * Set the title
   *
   * @param title Title
   */
  public abstract void setTitle(String title);

  /**
   * Get the number of profile information maps
   *
   * @return Number of profile information maps
   */
  public abstract int getInfoMapCount();

  /**
   * Get profile information map key name
   *
   * @param index Map index
   * @return Cost information map key name
   */
  public abstract String getInfoMapKey(int index);

  /**
   * Get profile information subtitle
   *
   * @param index Map index
   * @return Subtitle
   */
  public abstract String getSubTitle(int index);

  /**
   * Get the table model
   *
   * @param index Cost information map index
   * @return table model
   */
  public abstract DefaultTableModel getInfoTableModel(int index);

  /**
   * Get the header recommended column width list.
   *
   * @return Header recommended column width
   */
  protected abstract int[] getHeaderColumnsPreferredWidth();

  /**
   * Get the header minimum column width list.
   *
   * @return Header minimum column width
   */
  protected abstract int[] getHeaderColumnsMinWidth();

  /**
   * Set profiler data
   *
   * @param key Profiler data key
   * @param infos Profiler data
   */
  public abstract void setProfilerData(String key, ProfilerBaseData[] infos);

  /**
   * Get profile bar graph data
   *
   * @return Profile bar graph data
   */
  public abstract ISourceBargraph[] getSelectedBargraph();

  /**
   * Set column width
   *
   * @param columnModel Table column model
   */
  public void setTableColumnWidth(DefaultTableColumnModel columnModel) {
    boolean[] visibled = getVisibledColumns();
    int[] preferredWidth = getHeaderColumnsPreferredWidth();
    int[] minWidth = getHeaderColumnsMinWidth();
    for (int i = 0; i < columnModel.getColumnCount(); i++) {
      // Get column
      TableColumn column = columnModel.getColumn(i);
      if (preferredWidth.length >= i) {
        if (preferredWidth[i] >= 0 && visibled[i]) {
          column.setPreferredWidth(preferredWidth[i]);
        } else {
          column.setMinWidth(0);
          column.setMaxWidth(0);
          column.setPreferredWidth(0);
          column.setResizable(false);
        }
      }
      if (minWidth.length > i) {
        if (minWidth[i] >= 0) {
          column.setMinWidth(minWidth[i]);
        }
      }
    }
  }

  /**
   * Output table information to a file.
   *
   * @param file Output file
   */
  public void writeFile(File file) {

    try {
      boolean[] visibled = getVisibledColumns();
      // File output
      PrintWriter pw = new PrintWriter(new BufferedWriter(new FileWriter(file)));
      int mapsize = getInfoMapCount();
      if (mapsize > 0) {
        for (int i = 0; i < mapsize; i++) {
          // key string
          String key = getSubTitle(i);
          pw.println(key);

          // Get table model
          DefaultTableModel table = this.getInfoTableModel(i);

          // Output the table
          String buf = SwingUtils.toCsv(table, visibled);
          pw.print(buf);
          pw.println();
        }
      }
      pw.close();
    } catch (IOException ex) {
      ex.printStackTrace();
    }
  }

  /**
   * Set profiler properties
   *
   * @param properties Profiler properties
   */
  public void setProfilerProperties(ProfilerProperties properties) {
    this.properties = properties;
    // Notify model changes
    notifyModel();
  }

  /**
   * Get profiler properties
   *
   * @return properties Profiler properties
   */
  public ProfilerProperties getProfilerProperties() {
    return this.properties;
  }

  /**
   * Get text data of selected profile data
   *
   * @return Selected text data
   */
  public abstract String getSelectedText();

  /**
   * Get the header column list.
   *
   * @return Header column list
   */
  public abstract String[] getHeaderColumns();

  /**
   * Get the display status of the header column
   *
   * @return Header column display status list
   */
  public abstract boolean[] getVisibledColumns();

  /**
   * Set the display state of the header column
   *
   * @param col Header column number
   * @param checked Display status
   */
  public abstract void setVisibledColumns(int col, boolean checked);

  /** Notify model changes */
  protected abstract void notifyModel();

  /**
   * Get column placement
   *
   * @return Column placement
   */
  public abstract int[] getTableColumnAlignments();

  /**
   * Whether the model is empty
   *
   * @return Whether it is empty (true: empty, false: with data)
   */
  public boolean isEmpty() {
    return (getInfoMapCount() < 1);
  }

  /**
   * Returns whether View is in sorted state
   *
   * @param sort
   */
  public void setViewSort(boolean sort) {
    return;
  }
}
