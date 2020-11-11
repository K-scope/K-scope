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
package jp.riken.kscope.profiler.dprof;

/**
 * Hold offset information
 *
 * @author RIKEN
 */
public class OffSetInfo {
  private int lineInfo;
  private int loopInfo;
  private int callGraphInfo;
  private int mpiFuncElapsTimeInfo;
  private int comInfo;
  private int symbolInfo;

  /**
   * Get line information
   *
   * @return Line information
   */
  public int getLineInfo() {
    return lineInfo;
  }

  /**
   * Get loop information
   *
   * @return Loop information
   */
  public int getLoopInfo() {
    return loopInfo;
  }

  /**
   * Get call graph information
   *
   * @return Call graph information
   */
  public int getCallGraphInfo() {
    return callGraphInfo;
  }

  /**
   * Get MPI function elapsed time information
   *
   * @return MPI function elapsed time information
   */
  public int getMpiFuncElapsTimeInfo() {
    return mpiFuncElapsTimeInfo;
  }

  /**
   * Get communication information
   *
   * @return Communication information
   */
  public int getComInfo() {
    return comInfo;
  }

  /**
   * Get symbol information
   *
   * @return Symbol information
   */
  public int getSymbolInfo() {
    return symbolInfo;
  }

  /**
   * Set line information
   *
   * @param lineInfo Line information to set
   */
  public void setLineInfo(int lineInfo) {
    this.lineInfo = lineInfo;
  }

  /**
   * Set loop information
   *
   * @param loopInfo Loop information to set
   */
  public void setLoopInfo(int loopInfo) {
    this.loopInfo = loopInfo;
  }

  /**
   * Set call graph information
   *
   * @param callGraphInfo Call graph information to be set
   */
  public void setCallGraphInfo(int callGraphInfo) {
    this.callGraphInfo = callGraphInfo;
  }

  /**
   * MPI function Set elapsed time information
   *
   * @param mpiFuncElapsTimeInfo MPI function elapsed time information to be set
   */
  public void setMpiFuncElapsTimeInfo(int mpiFuncElapsTimeInfo) {
    this.mpiFuncElapsTimeInfo = mpiFuncElapsTimeInfo;
  }

  /**
   * Set communication information
   *
   * @param comInfo Communication information to be set
   */
  public void setComInfo(int comInfo) {
    this.comInfo = comInfo;
  }

  /**
   * Set symbol information
   *
   * @param symbolInfo Symbol information to set
   */
  public void setSymbolInfo(int symbolInfo) {
    this.symbolInfo = symbolInfo;
  }
}
