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

import jp.riken.kscope.Message;

/**
 * Identification string in the analysis information panel
 *
 * @author RIKEN
 */
public enum ANALYSIS_PANEL {
  // Panel list
  /** Additional information panel */
  INFORMATION(
      Message.getString("mainmenu.window.analysis.information"),
      "information.csv"), // Additional information
  /** Search Results Panel */
  SEARCHRESULT(
      Message.getString("mainmenu.window.analysis.search"), "search.csv"), // search results
  /** Variable characteristic list panel */
  VALIABLE(
      Message.getString("mainmenu.analysis.valiableproperty"),
      "variable.csv"), // List of variable characteristics
  /** Declaration / Definition / Reference Panel */
  REFERENCE(
      Message.getString("mainmenu.analysis.dec-def-ref"),
      "reference.csv"), // Declaration / Definition / Reference
  /** Trace panel */
  TRACE(Message.getString("mainmenu.window.analysis.trace"), "trace.csv"), // trace
  /** Calculation count panel */
  OPERAND(Message.getString("mainmenu.analysis.operation"), "count.csv"), // Calculation count
  /** Request Byte / FLOP calculation result panel */
  REQUIRED(
      Message.getString("mainmenu.window.analysis.byteflop"),
      "required.csv"), // Request Byte / FLOP calculation result panel
  /** Property Panel */
  PROPARTIES(Message.getString("mainmenu.project.property"), "proparty.csv"), // Properties
  /** Error location panel */
  ERROR(Message.getString("mainmenu.window.analysis.error"), "error.csv"), // Error location
  /** Console panel */
  CONSOLE(Message.getString("mainmenu.window.analysis.console"), "console.txt"), // console
  /** Variable Effectiveness Panel */
  SCOPE(Message.getString("mainmenu.analysis.valiablescope"), "scope.csv"), // Variable valid area
  /** Replacement result panel */
  REPLACE(
      Message.getString("mainmenu.window.analysis.structureinfo"),
      "replaceResult.csv"), // Structural information replacement result
  /** Profiler: Cost Information: Procedure Panel */
  COST_PROCEDURE(
      Message.getString("analysis_panel.enum.costinfo-procedure"),
      "cost_procedure.csv"), // Cost information: Procedure
  /** Profiler: Cost Information: Loop Panel */
  COST_LOOP(
      Message.getString("analysis_panel.enum.costinfo-loop"),
      "cost_loop.csv"), // Cost information: Loop
  /** Profiler: Cost Information: Line Panel */
  COST_LINE(
      Message.getString("analysis_panel.enum.costinfo-line"),
      "cost_line.csv"), // Cost information: Line
  /** Profiler: Call Graph Information Panel */
  CALLGRAPH(
      Message.getString("analysis_panel.enum.callgraph"),
      "callgraph.csv"), // Call graph information
  /**
   * Eprof: Event counter information: Hardware monitor information (PA information) table = Cache
   * table panel
   */
  EVENTCOUNTER_CACHE(
      Message.getString("analysis_panel.enum.detail-cache"), "cache.csv"), // More info: Cache
  /**
   * Eprof: Event counter information: Hardware monitor information (PA information) table =
   * Instructions table panel
   */
  EVENTCOUNTER_INSTRUCTIONS(
      Message.getString("analysis_panel.enum.detail-instructions"),
      "instructions.csv"), // More info: Instructions
  /**
   * Eprof: Event counter information: Hardware monitor information (PA information) table =
   * MEM_access table panel
   */
  EVENTCOUNTER_MEM_ACCESS(
      Message.getString("analysis_panel.enum.detail-mem"),
      "mem_access.csv"), // Detailed information: MEM_access
  /**
   * Eprof: Event counter information: Hardware monitor information (PA information) table =
   * Performance table panel
   */
  EVENTCOUNTER_PERFORMANCE(
      Message.getString("analysis_panel.enum.detail-performance"),
      "performance.csv"), // Detailed information: Performance
  /**
   * Eprof: Event counter information: Hardware monitor information (PA information) table =
   * Statistics table panel
   */
  EVENTCOUNTER_STATISTICS(
      Message.getString("analysis_panel.enum.detail-statistics"),
      "statistics.csv"), // More info: Statistics
  /** Detailed profiler: Measurement interval */
  EPROF_MEASURE(
      Message.getString("analysis_panel.enum.mesuermentrange"),
      "eprof_measure.csv"); // Measurement section

  /** Tab name */
  private String tabname;
  /** Export default file name */
  private String filename;

  /**
   * Constructor
   *
   * @param tabname Tab name
   * @param filename Export default file name
   */
  private ANALYSIS_PANEL(String tabname, String filename) {
    this.tabname = tabname;
    this.filename = filename;
  }

  /**
   * Get the tab name
   *
   * @return tab name
   */
  public String getTabName() {
    return this.tabname;
  }

  /**
   * Get the export default file name
   *
   * @return Export default file name
   */
  public String getFilename() {
    return this.filename;
  }
}
