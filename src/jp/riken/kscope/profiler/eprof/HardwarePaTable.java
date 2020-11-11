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
package jp.riken.kscope.profiler.eprof;

/**
 * Hardware monitor information (PA information) table
 *
 * @author RIKEN
 */
public class HardwarePaTable {
  /** Thread number */
  private int threadno;
  /** Hardware monitor information (PA information) table */
  private double[] paTable;

  /** Constructor */
  public HardwarePaTable() {}

  /**
   * Thread number
   *
   * @return thread number
   */
  public int getThreadno() {
    return threadno;
  }

  /**
   * Thread number
   *
   * @param no thread number
   */
  public void setThreadno(int no) {
    this.threadno = no;
  }
  /**
   * Hardware monitor information (PA information) table
   *
   * @return Hardware monitor information (PA information) table
   */
  public double[] getPaTable() {
    return paTable;
  }

  /**
   * Hardware monitor information (PA information) table
   *
   * @param table Hardware monitor information (PA information) table
   */
  public void setPaTable(double[] table) {
    this.paTable = table;
  }
}
