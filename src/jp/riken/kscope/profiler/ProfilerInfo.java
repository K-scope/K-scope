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
 * プロファイラ情報クラス
 * @author riken
 */
public class ProfilerInfo {
    /** プロファイラ:コスト情報(手続) */
    private Map<String, List<ProfilerDprofData>> costProcedure;
    /** プロファイラ:コスト情報(ループ) */
    private Map<String, List<ProfilerDprofData>> costLoop;
    /** プロファイラ:コスト情報(ライン) */
    private Map<String, List<ProfilerDprofData>> costLine;
    /** プロファイラ:コールグラフ情報 */
    private Map<String, List<ProfilerDprofData>> callgraph;
    /** プロファイラ:イベントカウンタ情報 */
    private Map<String, List<ProfilerEprofData>> eventCounter;
    /** 詳細プロファイラ測定区間情報 */
    private ProfilerMeasureInfo measureInfo;

    /**
     * プロファイラ:コスト情報(手続)
     * @param key		プロファイラキー文字列
     * @return プロファイラ:コスト情報(手続)
     */
    public ProfilerDprofData[] getCostProcedure(String key) {
        return getMapProfilerDprofData(this.costProcedure, key);
    }
    /**
     * プロファイラ:コスト情報(手続)
     * @param key  プロファイラキー文字列
     * @param data プロファイラ:コスト情報(手続)
     */
    public void putCostProcedure(String key, ProfilerDprofData[] data) {
        if (this.costProcedure == null) {
            this.costProcedure = new TreeMap<String, List<ProfilerDprofData>>();
        }
        putMapProfilerDprofData(this.costProcedure, key, data);
    }
    /**
     * プロファイラ:コスト情報(ループ)
     * @param key		プロファイラキー文字列
     * @return プロファイラ:コスト情報(ループ)
     */
    public ProfilerDprofData[] getCostLoop(String key) {
        return getMapProfilerDprofData(this.costLoop, key);
    }
    /**
     * プロファイラ:コスト情報(ループ)
     * @param key  プロファイラキー文字列
     * @param data プロファイラ:コスト情報(ループ)
     */
    public void putCostLoop(String key, ProfilerDprofData[] data) {
        if (this.costLoop == null) {
            this.costLoop = new TreeMap<String, List<ProfilerDprofData>>();
        }
        putMapProfilerDprofData(this.costLoop, key, data);
    }
    /**
     * プロファイラ:コスト情報(ライン)
     * @param key		プロファイラキー文字列
     * @return プロファイラ:コスト情報(ライン)
     */
    public ProfilerDprofData[] getCostLine(String key) {
        return getMapProfilerDprofData(this.costLine, key);
    }
    /**
     * プロファイラ:コスト情報(ライン)
     * @param key  プロファイラキー文字列
     * @param data プロファイラ:コスト情報(ライン)
     */
    public void putCostLine(String key, ProfilerDprofData[] data) {
        if (this.costLine == null) {
            this.costLine = new TreeMap<String, List<ProfilerDprofData>>();
        }
        putMapProfilerDprofData(this.costLine, key, data);
    }
    /**
     * プロファイラ:コールグラフ情報
     * @param key  プロファイラキー文字列
     * @return プロファイラ:コールグラフ情報
     */
    public ProfilerDprofData[] getCallgraph(String key) {
        return getMapProfilerDprofData(this.callgraph, key);
    }
    /**
     * プロファイラ:コールグラフ情報
     * @param key  プロファイラキー文字列
     * @param data プロファイラ:コールグラフ情報
     */
    public void putCallgraph(String key, ProfilerDprofData[] data) {
        if (this.callgraph == null) {
            this.callgraph = new TreeMap<String, List<ProfilerDprofData>>();
        }
        putMapProfilerDprofData(this.callgraph, key, data);
    }
    /**
     * プロファイラ:イベントカウンタ情報
     * @param key  プロファイラキー文字列
     * @return プロファイラ:イベントカウンタ情報
     */
    public ProfilerEprofData[] getEventCounter(String key) {
        return getMapProfilerEprofData(this.eventCounter, key);
    }
    /**
     * プロファイラ:イベントカウンタ情報
     * @param key  プロファイラキー文字列
     * @param data プロファイラ:イベントカウンタ情報
     */
    public void putEventCounter(String key, ProfilerEprofData[] data) {
        if (this.eventCounter == null) {
            this.eventCounter = new TreeMap<String, List<ProfilerEprofData>>();
        }
        putMapProfilerEprofData(this.eventCounter, key, data);
    }

    /**
     * MapからProfilerDprofData[]を取得する
     * @param map		プロファイラMapデータ
     * @param key		キー文字列
     * @return			ProfilerDprofData[]
     */
    private ProfilerDprofData[] getMapProfilerDprofData(Map<String, List<ProfilerDprofData>> map, String key) {
        if (map == null) return null;
        List<ProfilerDprofData> list = map.get(key);
        if (list == null) return null;
        return list.toArray(new ProfilerDprofData[0]);
    }

    /**
     * MapにProfilerDprofData[]を追加する
     * @param map		プロファイラMapデータ
     * @param key		キー文字列
     * @param data		追加プロファイルデータ
     * @return    追加プロファイルデータ
     */
    private List<ProfilerDprofData> putMapProfilerDprofData(
                        Map<String, List<ProfilerDprofData>> map,
                        String key,
                        ProfilerDprofData[] data) {
        if (map == null) return null;
        List<ProfilerDprofData> list = null;
        if (data != null) {
            list = new ArrayList<ProfilerDprofData>();
            list.addAll(Arrays.asList(data));
        }
        return map.put(key, list);
    }

    /**
     * MapからProfilerEprofData[]を取得する
     * @param map		プロファイラMapデータ
     * @param key		キー文字列
     * @return			ProfilerEprofData[]
     */
    private ProfilerEprofData[] getMapProfilerEprofData(Map<String, List<ProfilerEprofData>> map, String key) {
        if (map == null) return null;
        List<ProfilerEprofData> list = map.get(key);
        if (list == null) return null;
        return list.toArray(new ProfilerEprofData[0]);
    }


    /**
     * MapにProfilerEprofData[]を追加する
     * @param map		プロファイラMapデータ
     * @param key		キー文字列
     * @param data		追加プロファイルデータ
     * @return    追加プロファイルデータ
     */
    private List<ProfilerEprofData> putMapProfilerEprofData(
                        Map<String, List<ProfilerEprofData>> map,
                        String key,
                        ProfilerEprofData[] data) {
        if (map == null) return null;
        List<ProfilerEprofData> list = null;
        if (data != null) {
            list = new ArrayList<ProfilerEprofData>();
            list.addAll(Arrays.asList(data));
        }
        return map.put(key, list);
    }

    /**
     * プロファイラデータをクリアする.
     */
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
     * すべてのプロファイルバーグラフデータを取得する
     * @return	プロファイルバーグラフデータ
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
     * プロファイラ情報タイプのプロファイルバーグラフデータを取得する
     * @param  type   プロファイラ情報タイプ
     * @return	プロファイルバーグラフデータ
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
        }
        else if (type == PROFILERINFO_TYPE.COST_LOOP) {
            if (this.costLoop != null) {
                Set<String> keySet = this.costLoop.keySet();
                for (String key : keySet) {
                    List<ProfilerDprofData> dprofs = this.costLoop.get(key);
                    if (dprofs != null) {
                        list.addAll(dprofs);
                    }
                }
            }
        }
        else if (type == PROFILERINFO_TYPE.COST_LINE) {
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
     * 詳細プロファイラ測定区間情報を取得する
     * @return 		詳細プロファイラ測定区間情報
     */
    public ProfilerMeasureInfo getMeasureInfo() {
        return this.measureInfo;
    }
    /**
     * 詳細プロファイラ測定区間情報を設定する
     * @param info 詳細プロファイラ測定区間情報
     */
    public void setMeasureInfo(ProfilerMeasureInfo info) {
        this.measureInfo = info;
    }
}


