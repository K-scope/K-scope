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
 * 分析情報パネルの識別文字列
 * @author RIKEN
 */
public enum ANALYSIS_PANEL {
    // パネル一覧
    /** 付加情報パネル */
    INFORMATION(Message.getString("mainmenu.window.analysis.information"), "information.csv"), //付加情報
    /**  検索結果パネル   */
    SEARCHRESULT(Message.getString("mainmenu.window.analysis.search"), "search.csv"), //検索結果
    /** 変数特性一覧パネル */
    VARIABLE(Message.getString("mainmenu.analysis.variableproperty"), "variable.csv"), //変数特性一覧
    /** 宣言・定義・参照パネル */
    REFERENCE(Message.getString("mainmenu.analysis.dec-def-ref"), "reference.csv"), //宣言・定義・参照
    /** トレースパネル */
    TRACE(Message.getString("mainmenu.window.analysis.trace"), "trace.csv"), //トレース
    /** 演算カウントパネル */
    OPERAND(Message.getString("mainmenu.analysis.operation"), "count.csv"), //演算カウント
    /** 要求Byte/FLOP算出結果パネル */
    REQUIRED(Message.getString("mainmenu.window.analysis.byteflop"), "required.csv"), // 要求Byte/FLOP算出結果パネル
    /** プロパティパネル */
    PROPARTIES(Message.getString("mainmenu.project.property"), "proparty.csv"), //プロパティ
    /** エラー箇所パネル */
    ERROR(Message.getString("mainmenu.window.analysis.error"), "error.csv"), //エラー箇所
    /** コンソールパネル */
    CONSOLE(Message.getString("mainmenu.window.analysis.console"), "console.txt"), //コンソール
    /** 変数有効域パネル */
    SCOPE(Message.getString("mainmenu.analysis.variablescope"), "scope.csv"), //変数有効域
    /** 差替結果パネル */
    REPLACE(Message.getString("mainmenu.window.analysis.structureinfo"), "replaceResult.csv"), //構造情報差替結果
    /** プロファイラ:コスト情報:手続パネル */
    COST_PROCEDURE(Message.getString("analysis_panel.enum.costinfo-procedure"), "cost_procedure.csv"), //コスト情報：手続
    /** プロファイラ:コスト情報:ループパネル */
    COST_LOOP(Message.getString("analysis_panel.enum.costinfo-loop"), "cost_loop.csv"), //コスト情報：ループ
    /** プロファイラ:コスト情報:ラインパネル */
    COST_LINE(Message.getString("analysis_panel.enum.costinfo-line"), "cost_line.csv"), //コスト情報：ライン
    /** プロファイラ:コールグラフ情報パネル */
    CALLGRAPH(Message.getString("analysis_panel.enum.callgraph"), "callgraph.csv"), //コールグラフ情報
    /** Eprof:イベントカウンタ情報:ハードウェアモニタ情報（ＰＡ情報）テーブル=Cacheのテーブルパネル */
    EVENTCOUNTER_CACHE(Message.getString("analysis_panel.enum.detail-cache"), "cache.csv"), //詳細情報：Cache
    /** Eprof:イベントカウンタ情報:ハードウェアモニタ情報（ＰＡ情報）テーブル=Instructionsのテーブルパネル */
    EVENTCOUNTER_INSTRUCTIONS(Message.getString("analysis_panel.enum.detail-instructions"), "instructions.csv"), //詳細情報：Instructions
    /** Eprof:イベントカウンタ情報:ハードウェアモニタ情報（ＰＡ情報）テーブル=MEM_accessのテーブルパネル */
    EVENTCOUNTER_MEM_ACCESS(Message.getString("analysis_panel.enum.detail-mem"), "mem_access.csv"), //詳細情報：MEM_access
    /** Eprof:イベントカウンタ情報:ハードウェアモニタ情報（ＰＡ情報）テーブル=Performanceのテーブルパネル */
    EVENTCOUNTER_PERFORMANCE(Message.getString("analysis_panel.enum.detail-performance"), "performance.csv"), //詳細情報：Performance
    /** Eprof:イベントカウンタ情報:ハードウェアモニタ情報（ＰＡ情報）テーブル=Statisticsのテーブルパネル */
    EVENTCOUNTER_STATISTICS(Message.getString("analysis_panel.enum.detail-statistics"), "statistics.csv"), //詳細情報：Statistics
    /** 詳細プロファイラ:測定区間 */
    EPROF_MEASURE(Message.getString("analysis_panel.enum.mesuermentrange"), "eprof_measure.csv"); //測定区間

    /** タブ名 */
    private String tabname;
    /** エクスポートデフォルトファイル名 */
    private String filename;

    /**
     * コンストラクタ
     * @param tabname        タブ名
     * @param filename        エクスポートデフォルトファイル名
     */
    private ANALYSIS_PANEL(String tabname, String filename) {
        this.tabname = tabname;
        this.filename = filename;
    }

    /**
     * タブ名を取得する
     * @return        タブ名
     */
    public String getTabName() {
        return this.tabname;
    }

    /**
     * エクスポートデフォルトファイル名を取得する
     * @return        エクスポートデフォルトファイル名
     */
    public String getFilename() {
        return this.filename;
    }

}
