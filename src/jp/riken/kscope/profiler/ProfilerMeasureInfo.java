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

package jp.riken.kscope.profiler;

import java.util.ArrayList;
import java.util.List;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.IBlock;

/**
 * Detailed profiler measurement interval information class
 *
 * @author RIKEN
 */
public class ProfilerMeasureInfo {

  /** Measurement interval data list */
  private List<MeasureData> listMeasure;

  /** Constructor */
  public ProfilerMeasureInfo() {
    this.listMeasure = new ArrayList<MeasureData>();
  }

  /**
   * Add measurement interval data
   *
   * @param data Measurement interval data
   * @return Additional measurement interval data
   */
  public MeasureData addMeasureData(MeasureData data) {
    this.listMeasure.add(data);
    return data;
  }

  /**
   * Add measurement interval data
   *
   * @param code Measurement interval
   * @param name Group name
   * @return Additional measurement interval data
   */
  public MeasureData addMeasureData(CodeLine code, String name) {
    MeasureData data = new MeasureData(name, code);
    this.listMeasure.add(data);
    return data;
  }

  /**
   * Add measurement interval data
   *
   * @param code Measurement interval
   * @param name Group name
   * @param number Detail number
   * @param level Priority level
   * @return Additional measurement interval data
   */
  public MeasureData addMeasureData(CodeLine code, String name, String number, String level) {
    MeasureData data = new MeasureData(name, code);
    if (number != null) {
      data.setNumber(number);
    }
    if (level != null) {
      data.setLevel(level);
    }
    this.listMeasure.add(data);
    return data;
  }

  /**
   * Add measurement interval data
   *
   * @param blocks Measurement interval {start block to end block}
   * @param name Group name
   * @return Additional measurement interval data
   */
  public MeasureData addMeasureData(IBlock[] blocks, String name) {
    MeasureData data = new MeasureData(name, blocks);
    this.listMeasure.add(data);
    return data;
  }

  /**
   * Add measurement interval data
   *
   * @param blocks Measurement interval {start block to end block}
   * @param name Group name
   * @param number Detail number
   * @param level Priority level
   * @return Additional measurement interval data
   */
  public MeasureData addMeasureData(IBlock[] blocks, String name, String number, String level) {
    MeasureData data = new MeasureData(name, blocks);
    if (number != null) {
      data.setNumber(number);
    }
    if (level != null) {
      data.setLevel(level);
    }
    this.listMeasure.add(data);
    return data;
  }
  /**
   * Add measurement interval data list
   *
   * @param list Measurement interval data list
   */
  public void addMeasureData(List<MeasureData> list) {
    this.listMeasure.addAll(list);
  }

  /**
   * Set the measurement interval data list
   *
   * @param list Measurement interval data list
   */
  public void setMeasureList(List<MeasureData> list) {
    this.listMeasure = new ArrayList<MeasureData>();
    this.listMeasure.addAll(list);
  }

  /**
   * Get the measurement interval data list
   *
   * @return Measurement interval data list
   */
  public List<MeasureData> getMeasureList() {
    return this.listMeasure;
  }

  /**
   * Get the number of measurement interval data lists
   *
   * @return Number of measurement interval data lists
   */
  public int getMeasureDataCount() {
    if (this.listMeasure == null) return 0;
    return this.listMeasure.size();
  }

  /**
   * Get measurement interval data
   *
   * @param id index
   * @return Measurement interval data
   */
  public MeasureData getMeasureData(int id) {
    if (this.listMeasure == null) return null;
    if (this.listMeasure.size() <= id) return null;
    return this.listMeasure.get(id);
  }

  /** Clear the measurement interval data list. */
  public void clearMeasureInfo() {
    if (this.listMeasure == null) return;
    this.listMeasure = new ArrayList<MeasureData>();
  }

  /**
   * Delete data from the measurement interval data list
   *
   * @param id Delete index
   */
  public void removeMeasureData(int id) {
    if (this.listMeasure == null) return;
    if (this.listMeasure.size() <= id) return;
    this.listMeasure.remove(id);
  }

  /**
   * Delete data from the measurement interval data list
   *
   * @param data Deleted measurement interval data
   */
  public void removeMeasureData(MeasureData data) {
    if (this.listMeasure == null) return;
    this.listMeasure.remove(data);
  }

  /**
   * Check if the same group name exists from the measurement interval data list
   *
   * @param name Group name
   * @return true = Same group name exists
   */
  public boolean containsMeasureData(String name) {
    if (this.listMeasure == null) return false;
    if (name == null) return false;
    for (MeasureData data : this.listMeasure) {
      String group = data.getGroupname();
      if (group.equals(name)) {
        return true;
      }
    }
    return false;
  }

  /** Measurement interval data class */
  public class MeasureData {
    /** group name */
    private String groupname;
    /** Detail number */
    private String number;
    /** Priority level */
    private String level;
    /** Measurement interval insertion line */
    private CodeLine measureCodeLine;
    /** Measurement interval insertion block */
    private IBlock[] measureBlocks;

    /**
     * Constructor
     *
     * @param code Measurement interval insertion line information
     */
    public MeasureData(CodeLine code) {
      this.measureCodeLine = code;
    }

    /**
     * Constructor
     *
     * @param blocks Measurement interval insertion block {start block ~ end block}
     */
    public MeasureData(IBlock[] blocks) {
      this.measureBlocks = blocks;
    }

    /**
     * Constructor
     *
     * @param name Group name
     * @param code Measurement interval insertion line information
     */
    public MeasureData(String name, CodeLine code) {
      this.groupname = name;
      this.measureCodeLine = code;
    }

    /**
     * Constructor
     *
     * @param name Group name
     * @param blocks Measurement interval insertion block {start block ~ end block}
     */
    public MeasureData(String name, IBlock[] blocks) {
      this.groupname = name;
      this.measureBlocks = blocks;
    }

    /**
     * Get the group name
     *
     * @return group name
     */
    public String getGroupname() {
      return groupname;
    }

    /**
     * Get the group name
     *
     * @param name Group name
     */
    public void setGroupname(String name) {
      this.groupname = name;
    }

    /**
     * Get the measurement interval insertion line
     *
     * @return Measurement interval insertion line
     */
    public CodeLine getMeasureCodeLine() {
      return this.measureCodeLine;
    }

    /**
     * Set the measurement interval insertion line.
     *
     * @param code Measurement interval insertion line
     */
    public void setMeasureCodeLine(CodeLine code) {
      this.measureCodeLine = code;
    }

    /**
     * Get the measurement interval insertion block
     *
     * @return Measurement interval insertion block
     */
    public IBlock[] getMeasureBlocks() {
      return this.measureBlocks;
    }

    /**
     * Set the measurement interval insertion block.
     *
     * @param blocks Measurement interval insertion block {start block ~ end block}
     */
    public void setMeasureBlocks(IBlock[] blocks) {
      this.measureBlocks = blocks;
    }

    /**
     * Detail number
     *
     * @return Detail number
     */
    public String getNumber() {
      return number;
    }

    /**
     * Detail number
     *
     * @param number Detail number
     */
    public void setNumber(String number) {
      this.number = number;
    }

    /**
     * Priority level
     *
     * @return Priority level
     */
    public String getLevel() {
      return level;
    }

    /**
     * Priority level
     *
     * @param level Priority level
     */
    public void setLevel(String level) {
      this.level = level;
    }

    /**
     * Get the measurement interval insertion range. Get the insertion range from the measurement
     * interval insertion line and measurement interval insertion block.
     *
     * @return Measurement interval insertion range
     */
    public CodeLine getMeasureArea() {
      if (this.measureCodeLine != null) {
        return this.measureCodeLine;
      } else if (this.measureBlocks != null) {
        // Start block
        CodeLine startcode =
            new CodeLine(
                this.measureBlocks[0].getStartCodeLine(), this.measureBlocks[0].getEndCodeLine());
        // End block
        CodeLine endtcode =
            new CodeLine(
                this.measureBlocks[this.measureBlocks.length - 1].getStartCodeLine(),
                this.measureBlocks[this.measureBlocks.length - 1].getEndCodeLine());
        CodeLine area = new CodeLine(startcode, endtcode);
        return area;
      }

      return null;
    }

    /**
     * Output parameters as a string
     *
     * @return parameter
     */
    public String toStringParam() {
      StringBuffer buf = new StringBuffer();
      /** group name */
      if (this.groupname != null && !this.groupname.isEmpty()) {
        String rep_name = this.groupname;
        // Delete 2012/05/21: If you enclose it in double quotes, do not add double quotes.
        //              if (!(rep_name.startsWith("\"") && rep_name.endsWith("\""))) {
        //                  rep_name = "\"" + rep_name + "\"";
        //              }
        buf.append(rep_name);
      }
      /** Detail number */
      if (this.number != null && !this.number.isEmpty()) {
        if (buf.length() > 0) buf.append(", ");
        buf.append(this.number);
      }
      /** Priority level */
      if (this.level != null && !this.level.isEmpty()) {
        if (buf.length() > 0) buf.append(", ");
        buf.append(this.level);
      }
      return buf.toString();
    }
  }
}
