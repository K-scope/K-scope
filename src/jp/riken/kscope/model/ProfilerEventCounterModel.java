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

import java.text.DecimalFormat;
import java.util.ArrayList;
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
import jp.riken.kscope.profiler.ProfilerEprofData;
import jp.riken.kscope.profiler.eprof.HardwareMonitorInfo;
import jp.riken.kscope.profiler.eprof.HardwarePaTable;

/**
 * Profiler: Event counter information model
 *
 * @author RIKEN
 */
public class ProfilerEventCounterModel extends ProfilerTableBaseModel {

  /**
   * Table header list (5 columns): Hardware monitor information (PA information) table: Cache table
   * <br>
   * The first column is profiler information.
   */
  private String[] HEADER_COLUMNS_CACHE = {
    "",
    Message.getString("profilereventcountermodel.header_columns_cache.threadnum"), // Thread number
    Message.getString(
        "profilereventcountermodel.header_columns_cache.elapsedtime"), // elapsed time (s)
    Message.getString("profilereventcountermodel.header_columns_cache.usertime"), // User time (s)
    Message.getString(
        "profilereventcountermodel.header_columns_cache.num-instruction-exe"), // Number of
                                                                               // instruction
                                                                               // executions
    Message.getString(
        "profilereventcountermodel.header_columns_cache.num-load-store"), // Number of load / store
                                                                          // instructions
    Message.getString(
        "profilereventcountermodel.header_columns_cache.num-prefetch"), // Number of prefetch
                                                                        // instructions
    Message.getString(
        "profilereventcountermodel.header_columns_cache.num-SIMD-load-store"), // SIMD load / store
                                                                               // instructions
    Message.getString(
        "profilereventcountermodel.header_columns_cache.num-L1-cachemisses"), // Number of L1 data
                                                                              // cache misses
    Message.getString(
        "profilereventcountermodel.header_columns_cache.num-L2-cachedemandmisses"), // L2 cache
                                                                                    // demand number
                                                                                    // of misses
    Message.getString(
        "profilereventcountermodel.header_columns_cache.num-L2-cacheprefetchmisses"), // L2 cache
                                                                                      // prefetch
                                                                                      // number of
                                                                                      // misses
    Message.getString("profilereventcountermodel.header_columns_cache.num-dataaccessMDTLBmisses")
  }; // Number of data access MDTLB mistakes
  /**
   * Table header list (5 columns): Hardware monitor information (PA information) table:
   * Instructions table <br>
   * The first column is profiler information.
   */
  private String[] HEADER_COLUMNS_INSTRUCTIONS = {
    "",
    Message.getString("profilereventcountermodel.header_columns_cache.threadnum"), // Thread number
    Message.getString(
        "profilereventcountermodel.header_columns_cache.elapsedtime"), // elapsed time (s)
    Message.getString("profilereventcountermodel.header_columns_cache.usertime"), // User time (s)
    Message.getString(
        "profilereventcountermodel.header_columns_cache.num-instruction-exe"), // Number of
                                                                               // instruction
                                                                               // executions
    Message.getString(
        "profilereventcountermodel.header_columns_cache.num-load-store"), // Number of load / store
                                                                          // instructions
    Message.getString(
        "profilereventcountermodel.header_columns_instructions.num-floating-point"), // Number of
                                                                                     // floating
                                                                                     // point
                                                                                     // arithmetic
                                                                                     // instructions
    Message.getString(
        "profilereventcountermodel.header_columns_instructions.num-high-speed"), // Number of
                                                                                 // high-speed
                                                                                 // arithmetic
                                                                                 // instructions
    Message.getString(
        "profilereventcountermodel.header_columns_cache.num-SIMD-load-store"), // SIMD load / store
                                                                               // instructions
    Message.getString(
        "profilereventcountermodel.header_columns_instructions.num-SIMD-floating-point"), // Number
                                                                                          // of SIMD
                                                                                          // floating point arithmetic instructions
    Message.getString("profilereventcountermodel.header_columns_instructions.num-SIMD-high-speed")
  }; // Number of SIMD high-speed arithmetic instructions
  /**
   * Table header list (5 columns): Hardware monitor information (PA information) table: MEM_access
   * table <br>
   * The first column is profiler information.
   */
  private String[] HEADER_COLUMNS_MEM_ACCESS = {
    "",
    Message.getString("profilereventcountermodel.header_columns_cache.threadnum"), // Thread number
    Message.getString(
        "profilereventcountermodel.header_columns_cache.elapsedtime"), // elapsed time (s)
    Message.getString("profilereventcountermodel.header_columns_cache.usertime"), // User time (s)
    Message.getString(
        "profilereventcountermodel.header_columns_cache.num-instruction-exe"), // Number of
                                                                               // instruction
                                                                               // executions
    Message.getString(
        "profilereventcountermodel.header_columns_cache.num-load-store"), // Number of load / store
                                                                          // instructions
    Message.getString(
        "profilereventcountermodel.header_columns_cache.num-prefetch"), // Number of prefetch
                                                                        // instructions
    Message.getString(
        "profilereventcountermodel.header_columns_cache.num-SIMD-load-store"), // SIMD load / store
                                                                               // instructions
    Message.getString(
        "profilereventcountermodel.header_columns_cache.num-L2-cachedemandmisses"), // L2 cache
                                                                                    // demand number
                                                                                    // of misses
    Message.getString(
        "profilereventcountermodel.header_columns_cache.num-L2-cacheprefetchmisses"), // L2 cache
                                                                                      // prefetch
                                                                                      // number of
                                                                                      // misses
    Message.getString(
        "profilereventcountermodel.header_columns_mem.num-L2-cachedemandmiss"), // L2 cache demand
                                                                                // Miss writeback
                                                                                // number
    Message.getString("profilereventcountermodel.header_columns_mem.num-L2-prefetchmiss")
  }; // "L2 cache prefetch miswriteback count"
  /**
   * Table header list (5 columns): Hardware monitor information (PA information) table: Performance
   * table <br>
   * The first column is profiler information.
   */
  private String[] HEADER_COLUMNS_PERFORMANCE = {
    "",
    Message.getString("profilereventcountermodel.header_columns_cache.threadnum"), // Thread number
    Message.getString(
        "profilereventcountermodel.header_columns_cache.elapsedtime"), // elapsed time (s)
    Message.getString("profilereventcountermodel.header_columns_cache.usertime"), // User time (s)
    Message.getString(
        "profilereventcountermodel.header_columns_performance.num-cycles"), // number of cycles
    Message.getString(
        "profilereventcountermodel.header_columns_performance.num-cycles0"), // Number of
    // instruction
    // completions 0 Number
    // of cycles
    Message.getString(
        "profilereventcountermodel.header_columns_performance.num-cycles1"), // Number of completed
    // instructions 1
    // Number of cycles
    Message.getString(
        "profilereventcountermodel.header_columns_performance.floating-point.num-cycles0"), // Floating point arithmetic: Number of instruction completions 0 Number of cycles
    Message.getString(
        "profilereventcountermodel.header_columns_performance.waitmem.num-cycles0"), // Waiting for
    // memory
    // access data:
    // Number of
    // completed
    // instructions
    // 0 Number of
    // cycles
    Message.getString(
        "profilereventcountermodel.header_columns_performance.num-cycles-L2-cachemiss"), // Number
    // of L2
    // Cass
    // Smith
    // waiting
    // cycles
    Message.getString(
        "profilereventcountermodel.header_columns_performance.CSE-empty.num-cycles0"), // CSE empty:
    // number of
    // completed
    // instructions 0 number of cycles
    Message.getString(
        "profilereventcountermodel.header_columns_performance.CSE-empty-store.num-cycles0")
  }; // CSE empty / store port full: number of completed instructions 0 number of cycles
  /**
   * Table header list (5 columns): Hardware monitor information (PA information) table: Statistics
   * table <br>
   * The first column is profiler information.
   */
  private String[] HEADER_COLUMNS_STATISTICS = {
    "",
    Message.getString("profilereventcountermodel.header_columns_cache.threadnum"), // Thread number
    Message.getString(
        "profilereventcountermodel.header_columns_cache.elapsedtime"), // elapsed time (s)
    Message.getString("profilereventcountermodel.header_columns_cache.usertime"), // User time (s)
    Message.getString(
        "profilereventcountermodel.header_columns_cache.num-instruction-exe"), // Number of
                                                                               // instruction
                                                                               // executions
    Message.getString(
        "profilereventcountermodel.header_columns_instructions.num-floating-point"), // Number of
                                                                                     // floating
                                                                                     // point
                                                                                     // arithmetic
                                                                                     // instructions
    Message.getString(
        "profilereventcountermodel.header_columns_instructions.num-high-speed"), // Number of
                                                                                 // high-speed
                                                                                 // arithmetic
                                                                                 // instructions
    Message.getString(
        "profilereventcountermodel.header_columns_cache.num-SIMD-load-store"), // SIMD load / store
                                                                               // instructions
    Message.getString(
        "profilereventcountermodel.header_columns_instructions.num-SIMD-floating-point"), // Number
                                                                                          // of SIMD
                                                                                          // floating point arithmetic instructions
    Message.getString(
        "profilereventcountermodel.header_columns_instructions.num-SIMD-high-speed"), // Number of
                                                                                      // SIMD
                                                                                      // high-speed
                                                                                      // arithmetic
                                                                                      // instructions
    Message.getString(
        "profilereventcountermodel.header_columns_statistics.datatransfer-r"), // Data transfer
                                                                               // amount between
                                                                               // memory CPU (r)
    Message.getString("profilereventcountermodel.header_columns_statistics.datatransfer-w")
  }; // Data transfer amount between memory CPU (w)
  /** Table column display status */
  private boolean[] visibledcolumns = {
    false, true, true, true, true, true, true, true, true, true, true, true
  };
  /** Table column arrangement. <br> */
  private int[] COLUMNS_ALIGNMENTS = {
    SwingConstants.LEFT,
    SwingConstants.RIGHT,
    SwingConstants.RIGHT,
    SwingConstants.RIGHT,
    SwingConstants.RIGHT,
    SwingConstants.RIGHT,
    SwingConstants.RIGHT,
    SwingConstants.RIGHT,
    SwingConstants.RIGHT,
    SwingConstants.RIGHT,
    SwingConstants.RIGHT,
    SwingConstants.RIGHT
  };

  // Table column size -1 = Hide
  /** Table column size: Hardware monitor information (PA information) Table: Cache table */
  private int[] HEADER_COLUMNS_PREFERREDWIDTH_CACHE = {
    -1, 100, 100, 100, 140, 140, 140, 160, 160, 180, 180, 180
  };
  /** Table column size: Hardware monitor information (PA information) table: Instructions table */
  private int[] HEADER_COLUMNS_PREFERREDWIDTH_INSTRUCTIONS = {
    -1, 100, 100, 100, 140, 140, 140, 140, 160, 180, 180
  };
  /** Table column size: Hardware monitor information (PA information) table: MEM_access table */
  private int[] HEADER_COLUMNS_PREFERREDWIDTH_MEM_ACCESS = {
    -1, 100, 100, 100, 140, 140, 140, 160, 180, 180, 260, 260
  };
  /** Table column size: Hardware monitor information (PA information) Table: Performance table */
  private int[] HEADER_COLUMNS_PREFERREDWIDTH_PERFORMANCE = {
    -1, 100, 100, 100, 140, 140, 140, 240, 300, 200, 200, 300
  };
  /** Table column size: Hardware monitor information (PA information) Table: Statistics table */
  private int[] HEADER_COLUMNS_PREFERREDWIDTH_STATISTICS = {
    -1, 100, 100, 100, 140, 140, 100, 160, 180, 140, 180, 200
  };

  /**
   * Minimum table column size. <br>
   * -1 = Hide
   */
  private int[] HEADER_COLUMNS_MINWIDTH = {-1, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80};

  /** Title */
  private String title;

  /**
   * Profiler: Event Counter Information Map Key: A unique string that describes the profiler
   * process and thread Value: Event counter information list
   */
  private Map<String, List<ProfilerEprofData>> mapInfo;
  /** Selected event counter information */
  private ProfilerBaseData selectedInfo;
  /** Maximum decimal point display: Switching to exponential notation */
  private double MAX_EXPONENT = 1000000.0;
  /**
   * Constructor
   *
   * @param type Profiler information type
   */
  public ProfilerEventCounterModel(PROFILERINFO_TYPE type) {
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
   * Get the number of event counter information maps. For Eprof, the number of maps x the number of
   * profiler information lists
   *
   * @return Number of event counter information maps
   */
  @Override
  public int getInfoMapCount() {
    if (this.mapInfo == null) {
      return 0;
    }

    Set<String> keySet = this.mapInfo.keySet();
    int count = 0;
    for (String key : keySet) {
      List<ProfilerEprofData> list = this.mapInfo.get(key);
      if (list == null) continue;
      count += list.size();
    }
    return count;
  }

  /**
   * Get event counter information map key name For Eprof, the key name is map key +'/' + counter
   * group name
   *
   * @param index Map index
   * @return Event counter information map key name
   */
  @Override
  public String getInfoMapKey(int index) {
    if (this.mapInfo == null) {
      return null;
    }
    if (this.getInfoMapCount() <= index) return null;
    Set<String> keySet = this.mapInfo.keySet();
    int i = 0;
    for (String key : keySet) {
      List<ProfilerEprofData> list = this.mapInfo.get(key);
      if (list == null) continue;
      for (ProfilerEprofData data : list) {
        if (i == index) {
          // Counter group name
          String name = data.getSymbol();
          return key + "/" + name;
        }
        i++;
      }
    }
    return null;
  }

  /**
   * Get the event counter information list
   *
   * @param index Map index
   * @return Event counter information list
   */
  public List<ProfilerEprofData> getInfoMapValue(int index) {
    return getInfoMap(getInfoMapKey(index));
  }

  /**
   * Get the event counter information list For Eprof, the key name is map key +'/' + counter group
   * name
   *
   * @param key Map key
   * @return Event counter information list
   */
  public List<ProfilerEprofData> getInfoMap(String key) {
    if (this.mapInfo == null) {
      return null;
    }
    if (key == null) return null;
    String[] keys = key.split("/");
    if (keys == null || keys.length != 2) return null;
    List<ProfilerEprofData> list = this.mapInfo.get(keys[0]);
    if (list == null) return null;
    // In the case of Eprof, there should be only one event counter information.
    List<ProfilerEprofData> result = new ArrayList<ProfilerEprofData>();
    for (ProfilerEprofData data : list) {
      String name = data.getSymbol();
      if (name.equals(keys[1])) {
        result.add(data);
      }
    }
    return result;
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
    // Header column list
    String[] header = HEADER_COLUMNS_STATISTICS;
    if (this.getEnumInfo() == PROFILERINFO_TYPE.EVENTCOUNTER_CACHE) {
      header = HEADER_COLUMNS_CACHE;
    } else if (this.getEnumInfo() == PROFILERINFO_TYPE.EVENTCOUNTER_INSTRUCTIONS) {
      header = HEADER_COLUMNS_INSTRUCTIONS;
    } else if (this.getEnumInfo() == PROFILERINFO_TYPE.EVENTCOUNTER_MEM_ACCESS) {
      header = HEADER_COLUMNS_MEM_ACCESS;
    } else if (this.getEnumInfo() == PROFILERINFO_TYPE.EVENTCOUNTER_PERFORMANCE) {
      header = HEADER_COLUMNS_PERFORMANCE;
    } else if (this.getEnumInfo() == PROFILERINFO_TYPE.EVENTCOUNTER_STATISTICS) {
      header = HEADER_COLUMNS_STATISTICS;
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
    // Header column list
    int[] header = HEADER_COLUMNS_PREFERREDWIDTH_STATISTICS;
    if (this.getEnumInfo() == PROFILERINFO_TYPE.EVENTCOUNTER_CACHE) {
      header = HEADER_COLUMNS_PREFERREDWIDTH_CACHE;
    } else if (this.getEnumInfo() == PROFILERINFO_TYPE.EVENTCOUNTER_INSTRUCTIONS) {
      header = HEADER_COLUMNS_PREFERREDWIDTH_INSTRUCTIONS;
    } else if (this.getEnumInfo() == PROFILERINFO_TYPE.EVENTCOUNTER_MEM_ACCESS) {
      header = HEADER_COLUMNS_PREFERREDWIDTH_MEM_ACCESS;
    } else if (this.getEnumInfo() == PROFILERINFO_TYPE.EVENTCOUNTER_PERFORMANCE) {
      header = HEADER_COLUMNS_PREFERREDWIDTH_PERFORMANCE;
    } else if (this.getEnumInfo() == PROFILERINFO_TYPE.EVENTCOUNTER_STATISTICS) {
      header = HEADER_COLUMNS_PREFERREDWIDTH_STATISTICS;
    }
    return header;
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
   * Add event counter information
   *
   * @param key Event counter information key
   * @param info Event counter information
   */
  public void addInfo(String key, ProfilerEprofData info) {

    if (key == null) return;
    if (info == null) return;

    // Generate event counter information map
    if (this.mapInfo == null) {
      this.mapInfo = new TreeMap<String, List<ProfilerEprofData>>();
    }
    List<ProfilerEprofData> list = this.mapInfo.get(key);
    if (list == null) {
      list = new ArrayList<ProfilerEprofData>();
      this.mapInfo.put(key, list);
    }
    list.add(info);

    // Notify model changes
    notifyModel();
  }

  /**
   * Set event counter information
   *
   * @param key Event counter information key
   * @param infos Event counter information list
   */
  @Override
  public void setProfilerData(String key, ProfilerBaseData[] infos) {

    if (key == null) return;
    // Generate event counter information map
    if (this.mapInfo == null) {
      this.mapInfo = new TreeMap<String, List<ProfilerEprofData>>();
    }

    // If the event counter information list is null, delete the event counter information
    if (infos == null) {
      if (this.mapInfo.containsKey(key)) {
        this.mapInfo.remove(key);
      }
      return;
    }

    List<ProfilerEprofData> list = null;
    if (this.mapInfo.containsKey(key)) {
      list = this.mapInfo.get(key);
    } else {
      list = new ArrayList<ProfilerEprofData>();
      this.mapInfo.put(key, list);
    }
    list.clear();
    for (ProfilerBaseData data : infos) {
      if (data instanceof ProfilerEprofData) {
        list.add((ProfilerEprofData) data);
      }
    }

    // Notify model changes
    notifyModel();
  }

  /** Clear the table model. */
  @Override
  public void clearModel() {
    // Clear event counter information map
    if (this.mapInfo != null) {
      this.mapInfo = new TreeMap<String, List<ProfilerEprofData>>();
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
   * Set selection event counter information
   *
   * @param info Select event counter information
   */
  @Override
  public void setSelectedInfo(ProfilerBaseData info) {
    this.selectedInfo = info;
  }

  /**
   * Get selection event counter information
   *
   * @return Select event counter information
   */
  @Override
  public ProfilerBaseData getSelectedInfo() {
    return this.selectedInfo;
  }

  /**
   * Get the table model
   *
   * @param index Event counter information map index
   * @return table model
   */
  public DefaultTableModel getInfoTableModel(int index) {
    return getInfoTableModel(this.getInfoMapKey(index));
  }

  /**
   * Get the table model
   *
   * @param key Event counter information identification string
   * @return table model
   */
  public DefaultTableModel getInfoTableModel(String key) {
    if (key == null) return null;
    // Create a table model
    DefaultTableModel tableModel = getDefaultTableModel();
    List<ProfilerEprofData> list = getInfoMap(key);

    // In the case of Eprof, there should be only one event counter information.
    for (ProfilerEprofData info : list) {
      HardwareMonitorInfo hardwareInfo = info.getHardwareInfo();
      if (hardwareInfo == null) continue;
      List<HardwarePaTable> paInfo = hardwareInfo.getPaInfo();
      if (paInfo == null) continue;
      for (HardwarePaTable pa : paInfo) {
        int columncount = tableModel.getColumnCount();
        Object[] cols = new Object[columncount];
        // First column is ProfilerEprofData: Hide
        cols[0] = info;
        // Thread number
        cols[1] = pa.getThreadno();
        // Hardware monitor information (PA information) table
        double[] patable = pa.getPaTable();
        for (int i = 0; i < patable.length; i++) {
          if (i + 2 >= columncount) break;
          String text = formatDouble(patable[i]);
          cols[i + 2] = text;
        }
        tableModel.addRow(cols);
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
    // File name + Group name
    String[] keys = key.split("/");
    if (keys == null || keys.length != 2) return null;
    // For Eprof, group name
    String subtitle = keys[1];
    if (this.mapInfo.size() > 1) {
      // If multiple files exist, add the file name.
      subtitle += ":" + keys[0];
    }
    return subtitle;
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
    if (!(this.selectedInfo instanceof ProfilerEprofData)) return null;
    ProfilerEprofData info = (ProfilerEprofData) this.selectedInfo;
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

    HardwareMonitorInfo hardwareInfo = info.getHardwareInfo();
    if (hardwareInfo == null) return null;
    List<HardwarePaTable> paInfo = hardwareInfo.getPaInfo();
    if (paInfo == null) return null;
    for (HardwarePaTable pa : paInfo) {
      if (visibledcolumns[1]) {
        // Thread number
        buf.append(pa.getThreadno());
        buf.append(", ");
      }
      // Hardware monitor information (PA information) table
      double[] patable = pa.getPaTable();
      for (int i = 0; i < patable.length; i++) {
        if (visibledcolumns[i + 2]) {
          String text = formatDouble(patable[i]);
          buf.append(text);
          buf.append(", ");
        }
      }
      buf.delete(buf.length() - 2, buf.length());
      buf.append("\n");
    }
    return buf.toString();
  }

  /**
   * Format double values into display text.
   *
   * @param value double value
   * @return Formatted text
   */
  private String formatDouble(double value) {
    String text = null;
    if (value >= MAX_EXPONENT) {
      // exponential notation
      DecimalFormat decimal = new DecimalFormat("0.000E00");
      text = decimal.format(value);
    } else {
      // Decimal point display
      DecimalFormat decimal = new DecimalFormat("#.###");
      text = decimal.format(value);
    }
    return text;
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
