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
 * Event counter group
 *
 * @author RIKEN
 */
public class EventCounterGroup {

  /** Counter group name */
  private String groupname;
  /** Counter detail number */
  private int detailno;
  /** Basic information */
  private BaseInfo baseInfo;
  /** MPI information */
  private MpiInfo mpiInfo;
  /** Hardware monitor information */
  private HardwareMonitorInfo hardwareInfo;

  /** Constructor */
  public EventCounterGroup() {}

  /**
   * Counter group name
   *
   * @return Counter group name
   */
  public String getGroupname() {
    return groupname;
  }

  /**
   * Counter group name
   *
   * @param name Counter group name
   */
  public void setGroupname(String name) {
    this.groupname = name;
  }

  /**
   * Counter detail number
   *
   * @return Counter detail number
   */
  public int getDetailno() {
    return detailno;
  }

  /**
   * Counter detail number
   *
   * @param no Counter detail number
   */
  public void setDetailno(int no) {
    this.detailno = no;
  }

  /**
   * Basic information
   *
   * @return Basic information
   */
  public BaseInfo getBaseInfo() {
    return baseInfo;
  }

  /**
   * Basic information
   *
   * @param info Basic information
   */
  public void setBaseInfo(BaseInfo info) {
    this.baseInfo = info;
  }

  /**
   * MPI information
   *
   * @return MPI information
   */
  public MpiInfo getMpiInfo() {
    return mpiInfo;
  }

  /**
   * MPI information
   *
   * @param info MPI information
   */
  public void setMpiInfo(MpiInfo info) {
    this.mpiInfo = info;
  }

  /**
   * Hardware monitor information
   *
   * @return Hardware monitor information
   */
  public HardwareMonitorInfo getHardwareInfo() {
    return hardwareInfo;
  }

  /**
   * Hardware monitor information
   *
   * @param info Hardware monitor information
   */
  public void setHardwareInfo(HardwareMonitorInfo info) {
    this.hardwareInfo = info;
  }
}
