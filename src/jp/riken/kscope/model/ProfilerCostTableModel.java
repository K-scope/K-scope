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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import javax.swing.SwingConstants;
import javax.swing.table.DefaultTableModel;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.PROFILERINFO_TYPE;
import jp.riken.kscope.gui.ISourceBargraph;
import jp.riken.kscope.profiler.ProfilerBaseData;
import jp.riken.kscope.profiler.ProfilerDprofData;
import jp.riken.kscope.properties.ProfilerProperties;
import jp.riken.kscope.utils.StringUtils;

/**
 * Profiler: Cost Information Model
 *
 * @author RIKEN
 */
public class ProfilerCostTableModel extends ProfilerTableBaseModel {

  /**
   * Table header list (5 columns): Cost information (procedure). <br>
   * The first column is cost information.
   */
  private String[] HEADER_COLUMNS_PROCEDURE = {
    "",
    Message.getString("profilercallgraphmodel.header_columns.sampling"), // Number of samples
    Message.getString(
        "profilercallgraphmodel.header_columns.total-percentage"), // Percentage of the total (%)
    Message.getString("profileinfo_type.enum.procedure"), // procedure
    Message.getString("profilercosttablemodel.header_columns.filename"), // file name
    Message.getString("profilercosttablemodel.header_columns.linenumber")
  }; // line number
  /**
   * Table header list (5 columns): Cost information (loop). <br>
   * The first column is cost information.
   */
  private String[] HEADER_COLUMNS_LOOP = {
    "",
    Message.getString("profilercallgraphmodel.header_columns.sampling"), // Number of samples
    Message.getString(
        "profilercallgraphmodel.header_columns.total-percentage"), // Percentage of the total (%)
    Message.getString("profileinfo_type.enum.loop"), // loop
    Message.getString("profilercosttablemodel.header_columns.filename"), // file name
    Message.getString("profilercosttablemodel.header_columns.linenumber")
  }; // line number
  /**
   * Table header list (5 columns): Cost information (line). <br>
   * The first column is cost information.
   */
  private String[] HEADER_COLUMNS_LINE = {
    "",
    Message.getString("profilercallgraphmodel.header_columns.sampling"), // Number of samples
    Message.getString(
        "profilercallgraphmodel.header_columns.total-percentage"), // Percentage of the total (%)
    Message.getString("profileinfo_type.enum.line"), // line
    Message.getString("profilercosttablemodel.header_columns.filename"), // file name
    Message.getString("profilercosttablemodel.header_columns.linenumber")
  }; // line number
  /** Table column display status */
  private boolean[] visibledcolumns = {false, true, true, true, true, true};
  /** Table column size -1 = Hide */
  private int[] HEADER_COLUMNS_PREFERREDWIDTH = {-1, 120, 140, 240, 160, 80};

  /**
   * Minimum table column size. <br>
   * -1 = Hide
   */
  private int[] HEADER_COLUMNS_MINWIDTH = {-1, 80, 80, 80, 80, 80};

  /** Table column arrangement. <br> */
  private int[] COLUMNS_ALIGNMENTS = {
    SwingConstants.LEFT,
    SwingConstants.RIGHT,
    SwingConstants.RIGHT,
    SwingConstants.LEFT,
    SwingConstants.LEFT,
    SwingConstants.LEFT
  };

  /** Title */
  private String title;

  /**
   * Profiler: Cost Information Map Key: A unique string that describes the profiler process and
   * thread Value: Procedure, loop, line cost information list
   */
  private Map<String, List<ProfilerDprofData>> mapCostInfo;
  /** Select cost information */
  private ProfilerBaseData selectedCostInfo;
  /** View sort state */
  private boolean viewSort = false;

  /**
   * Constructor
   *
   * @param type Profiler information type
   */
  public ProfilerCostTableModel(PROFILERINFO_TYPE type) {
    super(type);
  }

  /** Notify model changes */
  @Override
  protected void notifyModel() {
    this.setChanged();
    this.notifyObservers();
    this.clearChanged();
  }

  /**
   * Get the number of cost information maps
   *
   * @return Number of cost information maps
   */
  @Override
  public int getInfoMapCount() {
    if (this.mapCostInfo == null) {
      return 0;
    }
    return this.mapCostInfo.size();
  }

  /**
   * Get cost information map key name
   *
   * @param index Map index
   * @return Cost information map key name
   */
  @Override
  public String getInfoMapKey(int index) {
    if (this.mapCostInfo == null) {
      return null;
    }
    if (this.mapCostInfo.size() <= index) return null;
    Set<String> keySet = this.mapCostInfo.keySet();
    int i = 0;
    for (String key : keySet) {
      if (i == index) {
        return key;
      }
      i++;
    }
    return null;
  }

  /**
   * Get the cost information list
   *
   * @param index Map index
   * @return Cost information list
   */
  public List<ProfilerDprofData> getInfoMapValue(int index) {
    return getInfoMap(getInfoMapKey(index));
  }

  /**
   * Get the cost information list
   *
   * @param key Map key
   * @return Cost information list
   */
  public List<ProfilerDprofData> getInfoMap(String key) {
    if (this.mapCostInfo == null) {
      return null;
    }
    if (key == null) return null;
    return this.mapCostInfo.get(key);
  }

  /**
   * Get the table model
   *
   * @return table model
   */
  public DefaultTableModel getDefaultTableModel() {
    // Create a table model
    String[] header = getHeaderColumns();
    DefaultTableModel tableModel = new DefaultTableModel(header, 0);
    return tableModel;
  }

  /**
   * Get the header column list.
   *
   * @return Header column list
   */
  @Override
  public String[] getHeaderColumns() {
    // Create a table model
    String[] header = HEADER_COLUMNS_PROCEDURE;
    if (this.getEnumInfo() == PROFILERINFO_TYPE.COST_PROCEDURE) {
      header = HEADER_COLUMNS_PROCEDURE;
    } else if (this.getEnumInfo() == PROFILERINFO_TYPE.COST_LOOP) {
      header = HEADER_COLUMNS_LOOP;
    } else if (this.getEnumInfo() == PROFILERINFO_TYPE.COST_LINE) {
      header = HEADER_COLUMNS_LINE;
    }
    return header;
  }

  /**
   * Get the header recommended column width list.
   *
   * @return Header recommended column width
   */
  @Override
  protected int[] getHeaderColumnsPreferredWidth() {
    return HEADER_COLUMNS_PREFERREDWIDTH;
  }

  /**
   * Get the header minimum column width list.
   *
   * @return Header minimum column width
   */
  @Override
  protected int[] getHeaderColumnsMinWidth() {
    return HEADER_COLUMNS_MINWIDTH;
  }

  /**
   * Add cost information
   *
   * @param key Cost information key
   * @param info Cost information
   */
  public void addCostInfo(String key, ProfilerDprofData info) {

    if (key == null) return;
    if (info == null) return;

    // Generate cost information map
    if (this.mapCostInfo == null) {
      this.mapCostInfo = new TreeMap<String, List<ProfilerDprofData>>();
    }
    List<ProfilerDprofData> list = this.mapCostInfo.get(key);
    if (list == null) {
      list = new ArrayList<ProfilerDprofData>();
      this.mapCostInfo.put(key, list);
    }
    list.add(info);

    // Notify model changes
    notifyModel();
  }

  /**
   * Set profiler data
   *
   * @param key Cost information key
   * @param infos Cost information list
   */
  @Override
  public void setProfilerData(String key, ProfilerBaseData[] infos) {

    if (key == null) return;
    // Generate cost information map
    if (this.mapCostInfo == null) {
      this.mapCostInfo = new TreeMap<String, List<ProfilerDprofData>>();
    }

    // If the cost information list is null, delete the cost information
    if (infos == null) {
      if (this.mapCostInfo.containsKey(key)) {
        this.mapCostInfo.remove(key);
      }
      return;
    }

    List<ProfilerDprofData> list = null;
    if (this.mapCostInfo.containsKey(key)) {
      list = this.mapCostInfo.get(key);
    } else {
      list = new ArrayList<ProfilerDprofData>();
      this.mapCostInfo.put(key, list);
    }
    list.clear();
    for (ProfilerBaseData data : infos) {
      if (data instanceof ProfilerDprofData) {
        list.add((ProfilerDprofData) data);
      }
    }

    // Notify model changes
    notifyModel();
  }

  /** Clear the table model. */
  @Override
  public void clearModel() {
    // Clear cost information map
    if (this.mapCostInfo != null) {
      this.mapCostInfo = new TreeMap<String, List<ProfilerDprofData>>();
    }
    // Clear title
    this.title = null;
    // Clear selected profiler data
    this.selectedCostInfo = null;
    // Notify model changes
    notifyModel();
  }

  /**
   * Get the title
   *
   * @return title
   */
  @Override
  public String getTitle() {
    return title;
  }

  /**
   * Set the title
   *
   * @param title Title
   */
  @Override
  public void setTitle(String title) {
    this.title = title;
  }

  /**
   * Set selection cost information
   *
   * @param info Select cost information
   */
  @Override
  public void setSelectedInfo(ProfilerBaseData info) {
    this.selectedCostInfo = info;
  }

  /**
   * Get selection cost information
   *
   * @return Selection cost information
   */
  @Override
  public ProfilerBaseData getSelectedInfo() {
    return this.selectedCostInfo;
  }

  /**
   * Get the table model
   *
   * @param index Cost information map index
   * @return table model
   */
  public DefaultTableModel getInfoTableModel(int index) {
    return getCostInfoTableModel(this.getInfoMapKey(index));
  }

  /**
   * Get the table model
   *
   * @param key Cost information identification string
   * @return table model
   */
  public DefaultTableModel getCostInfoTableModel(String key) {
    if (key == null) return null;
    // Create a table model
    DefaultTableModel tableModel = getDefaultTableModel();
    List<ProfilerDprofData> list = this.mapCostInfo.get(key);

    int maxcount = 0;
    if (this.getProfilerProperties() != null) {
      maxcount = this.getProfilerProperties().getCostinfoMaxCount();
    }
    final String INDENT = "    "; // Nest indent blank 4
    for (ProfilerDprofData info : list) {
      Object[] cols = new Object[tableModel.getColumnCount()];
      // First column is ProfilerCostInfo: Hidden
      cols[0] = info;
      cols[1] = (int) info.getSampling();
      float value =
          new BigDecimal(String.valueOf(info.getRatio() * 100))
              .setScale(ProfilerProperties.COST_RATIO_SCALE, BigDecimal.ROUND_HALF_UP)
              .floatValue();
      String format = "%.0" + ProfilerProperties.COST_RATIO_SCALE + "f";
      cols[2] = String.format(format, value);
      // Symbol name
      String name = info.getSymbol();
      int nest = info.getNestLevel();
      // Nested symbol names
      name = StringUtils.repeat(INDENT, nest) + name;
      cols[3] = String.valueOf(name);
      if (info.getCodeLine().getSourceFile() != null) {
        cols[4] = info.getCodeLine().getSourceFile().getFile().getName();
      }
      if (info.getCodeLine() != null) {
        cols[5] = info.getCodeLine().getLineno();
      }
      tableModel.addRow(cols);

      // Maximum number of displayed lines (If the initial value is 0, the maximum number of
      // displayed lines is not set)
      maxcount--;
      if (maxcount == 0) {
        break;
      }
    }

    return tableModel;
  }

  /**
   * Get profile information subtitle
   *
   * @param index Map index
   * @return Subtitle
   */
  @Override
  public String getSubTitle(int index) {
    String key = getInfoMapKey(index);
    return key;
  }

  /**
   * Get profile bar graph data
   *
   * @return Profile bar graph data
   */
  @Override
  public ISourceBargraph[] getSelectedBargraph() {
    // Get selected profiler data
    if (mapCostInfo == null) return null;

    List<ProfilerDprofData> selected = null;
    if (this.selectedCostInfo == null) {
      // Since it is in the unselected state, get the first profile data
      selected = getInfoMapValue(0);
    } else {
      // Get the list to which the selected profile data belongs
      Set<String> keySet = this.mapCostInfo.keySet();
      MAP_LABEL:
      for (String key : keySet) {
        List<ProfilerDprofData> datas = this.mapCostInfo.get(key);
        for (ProfilerDprofData data : datas) {
          if (data == this.selectedCostInfo) {
            selected = datas;
            break MAP_LABEL;
          }
        }
      }
    }
    if (selected == null) return null;

    return selected.toArray(new ISourceBargraph[0]);
  }

  /**
   * Get text data of selected profile data
   *
   * @return Selected text data
   */
  @Override
  public String getSelectedText() {
    if (this.selectedCostInfo == null) return null;
    if (!(this.selectedCostInfo instanceof ProfilerDprofData)) return null;
    ProfilerDprofData info = (ProfilerDprofData) this.selectedCostInfo;
    StringBuffer buf = new StringBuffer();
    // Header: Exclude the first column because it is a data column
    String[] header = getHeaderColumns();
    for (int i = 1; i < header.length; i++) {
      if (visibledcolumns[i]) {
        buf.append(header[i]);
        buf.append(", ");
      }
    }
    buf.delete(buf.length() - 2, buf.length());
    buf.append("\n");

    // Number of samples
    if (visibledcolumns[1]) {
      buf.append(info.getSampling());
      buf.append(", ");
    }
    // Percentage of the total (%)
    if (visibledcolumns[2]) {
      float value =
          new BigDecimal(String.valueOf(info.getRatio() * 100))
              .setScale(ProfilerProperties.COST_RATIO_SCALE, BigDecimal.ROUND_HALF_UP)
              .floatValue();
      String format = "%.0" + ProfilerProperties.COST_RATIO_SCALE + "f";
      String text = String.format(format, value);
      buf.append(text);
      buf.append(", ");
    }
    // Symbol name
    if (visibledcolumns[3]) {
      buf.append(info.getSymbol());
      buf.append(", ");
    }
    // file name
    if (visibledcolumns[4]) {
      if (info.getCodeLine().getSourceFile() != null) {
        buf.append(info.getCodeLine().getSourceFile().getFile().getName());
      }
      buf.append(", ");
    }
    // line number
    if (visibledcolumns[5]) {
      if (info.getCodeLine() != null) {
        buf.append(info.getCodeLine().getLineno());
      }
      buf.append(", ");
    }
    buf.delete(buf.length() - 2, buf.length());
    return buf.toString();
  }

  /**
   * Get the display status of the header column
   *
   * @return Header column display status list
   */
  @Override
  public boolean[] getVisibledColumns() {
    return this.visibledcolumns;
  }

  /**
   * Set the display state of the header column
   *
   * @param col Header column number
   * @param checked Display status
   */
  @Override
  public void setVisibledColumns(int col, boolean checked) {
    if (this.visibledcolumns.length <= col) return;
    this.visibledcolumns[col] = checked;
    this.notifyModel();
  }

  /**
   * Get table column placement
   *
   * @return Table column placement
   */
  @Override
  public int[] getTableColumnAlignments() {
    return this.COLUMNS_ALIGNMENTS;
  }

  /**
   * Set the sort flag
   *
   * @return void
   */
  @Override
  public void setViewSort(boolean sort) {
    this.viewSort = sort;

    ProfilerCostTableModelComparator comp = null;
    if (this.viewSort) {
      comp =
          new ProfilerCostTableModelComparator(ProfilerCostTableModelComparator.SORT_MODE.BY_LINE);
    } else {
      comp =
          new ProfilerCostTableModelComparator(ProfilerCostTableModelComparator.SORT_MODE.BY_COST);
    }
    for (Map.Entry<String, List<ProfilerDprofData>> e : mapCostInfo.entrySet()) {
      Collections.sort(e.getValue(), comp);
    }

    notifyModel();
  }
}
