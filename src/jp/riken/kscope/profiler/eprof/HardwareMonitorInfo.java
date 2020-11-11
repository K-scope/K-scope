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

import java.util.List;

/**
 * Hardware monitor information
 *
 * @author RIKEN
 */
public class HardwareMonitorInfo {
  /** Number of measurement threads */
  private int threadCount;
  /** Hardware monitor information (PA information) table list */
  private List<HardwarePaTable> paInfo;

  /** Constructor */
  public HardwareMonitorInfo() {}

  /**
   * Number of measurement threads
   *
   * @return Number of measurement threads
   */
  public int getThreadCount() {
    return threadCount;
  }

  /**
   * Number of measurement threads
   *
   * @param count Number of measurement threads
   */
  public void setThreadCount(int count) {
    this.threadCount = count;
  }

  /**
   * Hardware monitor information (PA information) table list
   *
   * @return Hardware monitor information (PA information) table list
   */
  public List<HardwarePaTable> getPaInfo() {
    return paInfo;
  }

  /**
   * Hardware monitor information (PA information) table list
   *
   * @param list Hardware monitor information (PA information) table list
   */
  public void setPaInfo(List<HardwarePaTable> list) {
    this.paInfo = list;
  }
}
