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

package jp.riken.kscope.common;

import java.awt.Color;
import jp.riken.kscope.Message;
import jp.riken.kscope.properties.ProfilerProperties;

/**
 * Profiler information type
 *
 * @author RIKEN
 */
public enum PROFILERINFO_TYPE {
  /** Cost information (procedure): DPROF */
  COST_PROCEDURE(
      Message.getString("analysis_panel.enum.costinfo-procedure"), // Cost information: Procedure
      Message.getString("profileinfo_type.enum.procedure")), // procedure
  /** Cost information (loop): DPROF */
  COST_LOOP(
      Message.getString("analysis_panel.enum.costinfo-loop"), // Cost information: Loop
      Message.getString("profileinfo_type.enum.loop")), // loop
  /** Cost information (line): DPROF */
  COST_LINE(
      Message.getString("analysis_panel.enum.costinfo-line"), // Cost information: Line
      Message.getString("profileinfo_type.enum.line")), // line
  /** Call graph: DPROF */
  CALLGRAPH(
      Message.getString("analysis_panel.enum.callgraph"), // Call graph
      Message.getString("analysis_panel.enum.callgraph")), // Call graph
  /**
   * Eprof: Event counter information: Hardware monitor information (PA information) table = Cache
   * table
   */
  EVENTCOUNTER_CACHE(
      Message.getString("profileinfo_type.enum.eprof-cache"), // EProf:Cache
      Message.getString("profileinfo_type.enum.cache")), // Cache
  /**
   * Eprof: Event counter information: Hardware monitor information (PA information) table =
   * Instructions table
   */
  EVENTCOUNTER_INSTRUCTIONS(
      Message.getString("profileinfo_type.enum.eprof-instructions"), // EProf:Instructions
      Message.getString("profileinfo_type.enum.instructions")), // Instructions
  /**
   * Eprof: Event counter information: Hardware monitor information (PA information) table =
   * MEM_access table
   */
  EVENTCOUNTER_MEM_ACCESS(
      Message.getString("profileinfo_type.enum.eprof-mem"), // EProf:MEM_access
      Message.getString("profileinfo_type.enum.mem")), // MEM_access
  /**
   * Eprof: Event counter information: Hardware monitor information (PA information) table =
   * Performance table
   */
  EVENTCOUNTER_PERFORMANCE(
      Message.getString("profileinfo_type.enum.eprof-performance"), // EProf:Performance
      Message.getString("profileinfo_type.enum.performance")), // Performance
  /**
   * Eprof: Event counter information: Hardware monitor information (PA information) table =
   * Statistics table
   */
  EVENTCOUNTER_STATISTICS(
      Message.getString("profileinfo_type.enum.eprof-statistics"), // EProf:Statistics
      Message.getString("profileinfo_type.enum.statistics")); // Statistics

  /** Profiler information name */
  private String name;
  /** Profiler abbreviated information name */
  private String shortname;
  /** Color of bar graph display of cost information */
  private Color barColor;

  /**
   * Constructor
   *
   * @param name Profiler information name
   */
  private PROFILERINFO_TYPE(String name, String shortname) {
    this.name = name;
    this.shortname = shortname;
  }

  /**
   * Set the color of the bar graph display of cost information
   *
   * @param properties Profiler properties
   */
  public static void setProfilerProperties(ProfilerProperties properties) {
    if (properties == null) return;
    COST_PROCEDURE.setBarColor(properties.getCostinfoBarcolorProcedure());
    COST_LOOP.setBarColor(properties.getCostinfoBarcolorLoop());
    COST_LINE.setBarColor(properties.getCostinfoBarcolorLine());
  }

  /**
   * Get profiler information name
   *
   * @return name Profiler information name
   */
  public String getName() {
    return name;
  }

  /**
   * Get profiler abbreviated information name
   *
   * @return name Profiler short information name
   */
  public String getShortName() {
    return this.shortname;
  }

  /**
   * Color of bar graph display of cost information
   *
   * @return Color of bar graph display of cost information
   */
  public Color getBarColor() {
    return barColor;
  }

  /**
   * Color of bar graph display of cost information
   *
   * @param color Color of bar graph display of cost information
   */
  public void setBarColor(Color color) {
    this.barColor = color;
  }
}
