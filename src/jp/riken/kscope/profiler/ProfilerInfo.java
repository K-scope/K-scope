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
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import jp.riken.kscope.common.PROFILERINFO_TYPE;
import jp.riken.kscope.gui.ISourceBargraph;

/**
 * Profiler information class
 *
 * @author RIKEN
 */
public class ProfilerInfo {
  /** Profiler: Cost Information (Procedure) */
  private Map<String, List<ProfilerDprofData>> costProcedure;
  /** Profiler: Cost Information (Loop) */
  private Map<String, List<ProfilerDprofData>> costLoop;
  /** Profiler: Cost Information (Line) */
  private Map<String, List<ProfilerDprofData>> costLine;
  /** Profiler: Call Graph Information */
  private Map<String, List<ProfilerDprofData>> callgraph;
  /** Profiler: Event counter information */
  private Map<String, List<ProfilerEprofData>> eventCounter;
  /** Detailed profiler measurement section information */
  private ProfilerMeasureInfo measureInfo;

  /**
   * Profiler: Cost information (procedure)
   *
   * @param key Profiler key string
   * @return Profiler: Cost Information (Procedure)
   */
  public ProfilerDprofData[] getCostProcedure(String key) {
    return getMapProfilerDprofData(this.costProcedure, key);
  }
  /**
   * Profiler: Cost information (procedure)
   *
   * @param key Profiler key string
   * @param data Profiler: Cost information (procedure)
   */
  public void putCostProcedure(String key, ProfilerDprofData[] data) {
    if (this.costProcedure == null) {
      this.costProcedure = new TreeMap<String, List<ProfilerDprofData>>();
    }
    putMapProfilerDprofData(this.costProcedure, key, data);
  }
  /**
   * Profiler: Cost information (loop)
   *
   * @param key Profiler key string
   * @return Profiler: Cost Information (Loop)
   */
  public ProfilerDprofData[] getCostLoop(String key) {
    return getMapProfilerDprofData(this.costLoop, key);
  }
  /**
   * Profiler: Cost information (loop)
   *
   * @param key Profiler key string
   * @param data Profiler: Cost information (loop)
   */
  public void putCostLoop(String key, ProfilerDprofData[] data) {
    if (this.costLoop == null) {
      this.costLoop = new TreeMap<String, List<ProfilerDprofData>>();
    }
    putMapProfilerDprofData(this.costLoop, key, data);
  }
  /**
   * Profiler: Cost information (line)
   *
   * @param key Profiler key string
   * @return Profiler: Cost Information (Line)
   */
  public ProfilerDprofData[] getCostLine(String key) {
    return getMapProfilerDprofData(this.costLine, key);
  }
  /**
   * Profiler: Cost information (line)
   *
   * @param key Profiler key string
   * @param data Profiler: Cost information (line)
   */
  public void putCostLine(String key, ProfilerDprofData[] data) {
    if (this.costLine == null) {
      this.costLine = new TreeMap<String, List<ProfilerDprofData>>();
    }
    putMapProfilerDprofData(this.costLine, key, data);
  }
  /**
   * Profiler: Call graph information
   *
   * @param key Profiler key string
   * @return Profiler: Call graph information
   */
  public ProfilerDprofData[] getCallgraph(String key) {
    return getMapProfilerDprofData(this.callgraph, key);
  }
  /**
   * Profiler: Call graph information
   *
   * @param key Profiler key string
   * @param data Profiler: Call graph information
   */
  public void putCallgraph(String key, ProfilerDprofData[] data) {
    if (this.callgraph == null) {
      this.callgraph = new TreeMap<String, List<ProfilerDprofData>>();
    }
    putMapProfilerDprofData(this.callgraph, key, data);
  }
  /**
   * Profiler: Event counter information
   *
   * @param key Profiler key string
   * @return Profiler: Event counter information
   */
  public ProfilerEprofData[] getEventCounter(String key) {
    return getMapProfilerEprofData(this.eventCounter, key);
  }
  /**
   * Profiler: Event counter information
   *
   * @param key Profiler key string
   * @param data Profiler: Event counter information
   */
  public void putEventCounter(String key, ProfilerEprofData[] data) {
    if (this.eventCounter == null) {
      this.eventCounter = new TreeMap<String, List<ProfilerEprofData>>();
    }
    putMapProfilerEprofData(this.eventCounter, key, data);
  }

  /**
   * Get ProfilerDprofData [] from Map
   *
   * @param map Profiler Map data
   * @param key key string
   * @return ProfilerDprofData []
   */
  private ProfilerDprofData[] getMapProfilerDprofData(
      Map<String, List<ProfilerDprofData>> map, String key) {
    if (map == null) return null;
    List<ProfilerDprofData> list = map.get(key);
    if (list == null) return null;
    return list.toArray(new ProfilerDprofData[0]);
  }

  /**
   * Add ProfilerDprofData [] to Map
   *
   * @param map Profiler Map data
   * @param key key string
   * @param data Additional profile data
   * @return Additional profile data
   */
  private List<ProfilerDprofData> putMapProfilerDprofData(
      Map<String, List<ProfilerDprofData>> map, String key, ProfilerDprofData[] data) {
    if (map == null) return null;
    List<ProfilerDprofData> list = null;
    if (data != null) {
      list = new ArrayList<ProfilerDprofData>();
      list.addAll(Arrays.asList(data));
    }
    return map.put(key, list);
  }

  /**
   * Get ProfilerEprofData [] from Map
   *
   * @param map Profiler Map data
   * @param key key string
   * @return ProfilerEprofData []
   */
  private ProfilerEprofData[] getMapProfilerEprofData(
      Map<String, List<ProfilerEprofData>> map, String key) {
    if (map == null) return null;
    List<ProfilerEprofData> list = map.get(key);
    if (list == null) return null;
    return list.toArray(new ProfilerEprofData[0]);
  }

  /**
   * Add ProfilerEprofData [] to Map
   *
   * @param map Profiler Map data
   * @param key key string
   * @param data Additional profile data
   * @return Additional profile data
   */
  private List<ProfilerEprofData> putMapProfilerEprofData(
      Map<String, List<ProfilerEprofData>> map, String key, ProfilerEprofData[] data) {
    if (map == null) return null;
    List<ProfilerEprofData> list = null;
    if (data != null) {
      list = new ArrayList<ProfilerEprofData>();
      list.addAll(Arrays.asList(data));
    }
    return map.put(key, list);
  }

  /** Clear profiler data. */
  public void clearProfilerData() {
    if (this.costProcedure != null) {
      this.costProcedure.clear();
    }
    if (this.costLoop != null) {
      this.costLoop.clear();
    }
    if (this.costLine != null) {
      this.costLine.clear();
    }
    if (this.callgraph != null) {
      this.callgraph.clear();
    }
    if (this.eventCounter != null) {
      this.eventCounter.clear();
    }
  }

  /**
   * Get all profile bar graph data
   *
   * @return Profile bar graph data
   */
  public ISourceBargraph[] getBargraphData() {

    List<ISourceBargraph> list = new ArrayList<ISourceBargraph>();
    {
      PROFILERINFO_TYPE type = PROFILERINFO_TYPE.COST_PROCEDURE;
      ISourceBargraph[] data = getBargraphData(type);
      if (data != null) {
        list.addAll(Arrays.asList(data));
      }
    }
    {
      PROFILERINFO_TYPE type = PROFILERINFO_TYPE.COST_LOOP;
      ISourceBargraph[] data = getBargraphData(type);
      if (data != null) {
        list.addAll(Arrays.asList(data));
      }
    }
    {
      PROFILERINFO_TYPE type = PROFILERINFO_TYPE.COST_LINE;
      ISourceBargraph[] data = getBargraphData(type);
      if (data != null) {
        list.addAll(Arrays.asList(data));
      }
    }
    if (list.size() <= 0) return null;

    return list.toArray(new ISourceBargraph[0]);
  }

  /**
   * Get profiler information type profile bar graph data
   *
   * @param type Profiler information type
   * @return Profile bar graph data
   */
  public ISourceBargraph[] getBargraphData(PROFILERINFO_TYPE type) {
    if (type == null) return null;
    List<ISourceBargraph> list = new ArrayList<ISourceBargraph>();
    if (type == PROFILERINFO_TYPE.COST_PROCEDURE) {
      if (this.costProcedure != null) {
        Set<String> keySet = this.costProcedure.keySet();
        for (String key : keySet) {
          List<ProfilerDprofData> dprofs = this.costProcedure.get(key);
          if (dprofs != null) {
            list.addAll(dprofs);
          }
        }
      }
    } else if (type == PROFILERINFO_TYPE.COST_LOOP) {
      if (this.costLoop != null) {
        Set<String> keySet = this.costLoop.keySet();
        for (String key : keySet) {
          List<ProfilerDprofData> dprofs = this.costLoop.get(key);
          if (dprofs != null) {
            list.addAll(dprofs);
          }
        }
      }
    } else if (type == PROFILERINFO_TYPE.COST_LINE) {
      if (this.costLine != null) {
        Set<String> keySet = this.costLine.keySet();
        for (String key : keySet) {
          List<ProfilerDprofData> dprofs = this.costLine.get(key);
          if (dprofs != null) {
            list.addAll(dprofs);
          }
        }
      }
    }
    if (list.size() <= 0) return null;

    return list.toArray(new ISourceBargraph[0]);
  }
  /**
   * Get detailed profiler measurement interval information
   *
   * @return Detailed profiler measurement section information
   */
  public ProfilerMeasureInfo getMeasureInfo() {
    return this.measureInfo;
  }
  /**
   * Set detailed profiler measurement interval information
   *
   * @param info Detailed profiler measurement section information
   */
  public void setMeasureInfo(ProfilerMeasureInfo info) {
    this.measureInfo = info;
  }
}
