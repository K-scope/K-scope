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
 * プロファイラ情報タイプ
 * @author RIKEN
 */
public enum PROFILERINFO_TYPE {
    /** コスト情報(手続):DPROF */
    COST_PROCEDURE(
    		Message.getString("analysis_panel.enum.costinfo-procedure"), //コスト情報:手続
    		Message.getString("profileinfo_type.enum.procedure")),  //手続
    /** コスト情報(ループ):DPROF */
    COST_LOOP(
    		Message.getString("analysis_panel.enum.costinfo-loop"), //コスト情報:ループ
    		Message.getString("profileinfo_type.enum.loop")), //ループ
    /** コスト情報(ライン):DPROF */
    COST_LINE(
    		Message.getString("analysis_panel.enum.costinfo-line"), //コスト情報:ライン
    		Message.getString("profileinfo_type.enum.line")), //ライン
    /** コールグラフ:DPROF */
    CALLGRAPH(
    		Message.getString("analysis_panel.enum.callgraph"), //コールグラフ
    		Message.getString("analysis_panel.enum.callgraph")), //コールグラフ
    /** Eprof:イベントカウンタ情報:ハードウェアモニタ情報（ＰＡ情報）テーブル=Cacheのテーブル */
    EVENTCOUNTER_CACHE(
    		Message.getString("profileinfo_type.enum.eprof-cache"), //EProf:Cache
    		Message.getString("profileinfo_type.enum.cache")), //Cache
    /** Eprof:イベントカウンタ情報:ハードウェアモニタ情報（ＰＡ情報）テーブル=Instructionsのテーブル */
    EVENTCOUNTER_INSTRUCTIONS(
    		Message.getString("profileinfo_type.enum.eprof-instructions"), //EProf:Instructions
    		Message.getString("profileinfo_type.enum.instructions")), //Instructions
    /** Eprof:イベントカウンタ情報:ハードウェアモニタ情報（ＰＡ情報）テーブル=MEM_accessのテーブル */
    EVENTCOUNTER_MEM_ACCESS(
    		Message.getString("profileinfo_type.enum.eprof-mem"), //EProf:MEM_access
    		Message.getString("profileinfo_type.enum.mem")), //MEM_access
    /** Eprof:イベントカウンタ情報:ハードウェアモニタ情報（ＰＡ情報）テーブル=Performanceのテーブル */
    EVENTCOUNTER_PERFORMANCE(
    		Message.getString("profileinfo_type.enum.eprof-performance"), //EProf:Performance
    		Message.getString("profileinfo_type.enum.performance")), //Performance
    /** Eprof:イベントカウンタ情報:ハードウェアモニタ情報（ＰＡ情報）テーブル=Statisticsのテーブル */
    EVENTCOUNTER_STATISTICS(
    		Message.getString("profileinfo_type.enum.eprof-statistics"), //EProf:Statistics
    		Message.getString("profileinfo_type.enum.statistics")); //Statistics

    /** プロファイラ情報名 */
    private String name;
    /** プロファイラ短縮情報名 */
    private String shortname;
    /** コスト情報の棒グラフ表示の色 */
    private Color barColor;

    /**
     * コンストラクタ
     * @param name		プロファイラ情報名
     */
    private PROFILERINFO_TYPE(String name, String shortname) {
        this.name = name;
        this.shortname = shortname;
    }

    /**
     * コスト情報の棒グラフ表示の色を設定する
     * @param properties			プロファイラプロパティ
     */
    public static void setProfilerProperties(ProfilerProperties properties) {
        if (properties == null) return;
        COST_PROCEDURE.setBarColor(properties.getCostinfoBarcolorProcedure());
        COST_LOOP.setBarColor(properties.getCostinfoBarcolorLoop());
        COST_LINE.setBarColor(properties.getCostinfoBarcolorLine());
    }


    /**
     * プロファイラ情報名を取得する
     * @return name		プロファイラ情報名
     */
    public String getName() {
        return name;
    }

    /**
     * プロファイラ短縮情報名を取得する
     * @return name		プロファイラ短縮情報名
     */
    public String getShortName() {
        return this.shortname;
    }

    /**
     * コスト情報の棒グラフ表示の色
     * @return コスト情報の棒グラフ表示の色
     */
    public Color getBarColor() {
        return barColor;
    }


    /**
     * コスト情報の棒グラフ表示の色
     * @param color コスト情報の棒グラフ表示の色
     */
    public void setBarColor(Color color) {
        this.barColor = color;
    }
}


