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
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import javax.swing.SwingConstants;
import javax.swing.table.DefaultTableModel;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.common.PROFILERINFO_TYPE;
import jp.riken.kscope.gui.ISourceBargraph;
import jp.riken.kscope.profiler.ProfilerBaseData;
import jp.riken.kscope.profiler.ProfilerDprofData;
import jp.riken.kscope.properties.ProfilerProperties;
import jp.riken.kscope.utils.StringUtils;

/**
 * Profiler: Call Graph Information Model
 *
 * @author RIKEN
 */
public class ProfilerCallGraphModel extends ProfilerTableBaseModel {

  /**
   * Table header list (4 columns): Call graph information. <br>
   * The first column is cost information.
   */
  private String[] HEADER_COLUMNS_CALLGRAPH = {
    "",
    Message.getString("profilercallgraphmodel.header_columns.sampling"), // Number of samples
    Message.getString(
        "profilercallgraphmodel.header_columns.total-percentage"), // Percentage of the total (%)
    Message.getString("profilercallgraphmodel.header_columns.symbol"),
  }; // Symbol name
  /** Table column display status */
  private boolean[] visibledcolumns = {false, true, true, true};
  /** Table column size -1 = Hide */
  private int[] HEADER_COLUMNS_PREFERREDWIDTH = {-1, 120, 140, 480};

  /**
   * Minimum table column size. <br>
   * -1 = Hide
   */
  private int[] HEADER_COLUMNS_MINWIDTH = {-1, 80, 80, 80};
  /** Table column arrangement. <br> */
  private int[] COLUMNS_ALIGNMENTS = {
    SwingConstants.LEFT, SwingConstants.RIGHT, SwingConstants.RIGHT, SwingConstants.LEFT
  };

  /** Title */
  private String title;

  /**
   * Profiler: Call Graph Map Key: Unique string that describes the profiler process and thread =
   * file name Value: Call graph symbol information list
   */
  private Map<String, List<ProfilerDprofData>> mapInfo;
  /** Selected call graph information */
  private ProfilerBaseData selectedInfo;
  /** Display Call Graph Panel Identifier */
  private ANALYSIS_PANEL enumPanel;

  /**
   * Constructor
   *
   * @param type Profiler information type
   */
  public ProfilerCallGraphModel(PROFILERINFO_TYPE type) {
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
   * Get the number of call graph information maps
   *
   * @return Number of call graph information maps
   */
  @Override
  public int getInfoMapCount() {
    if (this.mapInfo == null) {
      return 0;
    }
    return this.mapInfo.size();
  }

  /**
   * Get the call graph information map key name
   *
   * @param index Call graph index
   * @return Call graph information map key name
   */
  @Override
  public String getInfoMapKey(int index) {
    if (this.mapInfo == null) {
      return null;
    }
    if (this.mapInfo.size() <= index) return null;
    Set<String> keySet = this.mapInfo.keySet();
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
   * Get the call graph information list
   *
   * @param index Map index
   * @return Call graph information list
   */
  public List<ProfilerDprofData> getInfoMapValue(int index) {
    return getInfoMap(getInfoMapKey(index));
  }

  /**
   * Get the call graph information list
   *
   * @param key Map key
   * @return Call graph information list
   */
  public List<ProfilerDprofData> getInfoMap(String key) {
    if (this.mapInfo == null) {
      return null;
    }
    if (key == null) return null;
    return this.mapInfo.get(key);
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
    String[] header = HEADER_COLUMNS_CALLGRAPH;
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
   * Add call graph information
   *
   * @param key Call graph information key
   * @param info Call graph information
   */
  public void addInfo(String key, ProfilerDprofData info) {

    if (key == null) return;
    if (info == null) return;

    // Generate cost information map
    if (this.mapInfo == null) {
      this.mapInfo = new TreeMap<String, List<ProfilerDprofData>>();
    }
    List<ProfilerDprofData> list = this.mapInfo.get(key);
    if (list == null) {
      list = new ArrayList<ProfilerDprofData>();
      this.mapInfo.put(key, list);
    }
    list.add(info);

    // Notify model changes
    notifyModel();
  }

  /**
   * Set profiler data
   *
   * @param key Profiler data key
   * @param infos Profiler data
   */
  /**
   * Set call graph information
   *
   * @param key Call graph information key
   * @param infos Call graph information list
   */
  @Override
  public void setProfilerData(String key, ProfilerBaseData[] infos) {

    if (key == null) return;
    // Generate cost information map
    if (this.mapInfo == null) {
      this.mapInfo = new TreeMap<String, List<ProfilerDprofData>>();
    }

    // If the cost information list is null, delete the cost information
    if (infos == null) {
      if (this.mapInfo.containsKey(key)) {
        this.mapInfo.remove(key);
      }
      return;
    }

    List<ProfilerDprofData> list = null;
    if (this.mapInfo.containsKey(key)) {
      list = this.mapInfo.get(key);
    } else {
      list = new ArrayList<ProfilerDprofData>();
      this.mapInfo.put(key, list);
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
    if (this.mapInfo != null) {
      this.mapInfo = new TreeMap<String, List<ProfilerDprofData>>();
    }
    // Clear title
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
   * Set selection call graph information
   *
   * @param info Selected call graph information
   */
  public void setSelectedInfo(ProfilerBaseData info) {
    this.selectedInfo = info;
  }

  /**
   * Get selected call graph information
   *
   * @return Select call graph information
   */
  public ProfilerBaseData getSelectedInfo() {
    return this.selectedInfo;
  }

  /**
   * Get display cost panel identifier
   *
   * @return Display cost panel identifier
   */
  public ANALYSIS_PANEL getEnumPanel() {
    return enumPanel;
  }

  /**
   * Set display cost panel identifier
   *
   * @param panel Display cost panel identifier
   */
  public void setEnumPanel(ANALYSIS_PANEL panel) {
    this.enumPanel = panel;
  }

  /**
   * Get the table model
   *
   * @param index Call graph information map index
   * @return table model
   */
  public DefaultTableModel getInfoTableModel(int index) {
    return getInfoTableModel(this.getInfoMapKey(index));
  }

  /**
   * Get the table model
   *
   * @param key Cost information identification string
   * @return table model
   */
  public DefaultTableModel getInfoTableModel(String key) {
    if (key == null) return null;
    // Create a table model
    DefaultTableModel tableModel = getDefaultTableModel();
    List<ProfilerDprofData> list = this.mapInfo.get(key);

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
      String text = String.format(format, value);
      cols[2] = text;
      // Symbol name
      String name = info.getSymbol();
      int nest = info.getNestLevel();
      // Nested symbol names
      name = StringUtils.repeat(INDENT, nest) + name;
      cols[3] = String.valueOf(name);
      tableModel.addRow(cols);
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
    return null;
  }

  /**
   * Get text data of selected profile data
   *
   * @return Selected text data
   */
  @Override
  public String getSelectedText() {
    if (this.selectedInfo == null) return null;
    if (!(this.selectedInfo instanceof ProfilerDprofData)) return null;
    ProfilerDprofData info = (ProfilerDprofData) this.selectedInfo;
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
    if (visibledcolumns[3]) {
      // Symbol name
      buf.append(info.getSymbol());
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
}
