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
package jp.riken.kscope.profiler.common;

/**
 * プロファイラ定数クラス
 * @author RIKEN
 */
public class ProfConstant {

    // 測定時オプションのビット列
    // 測定時オプション:DPROF
    /** 通信情報 */
    public static final int DPRF_COLL_OPT_COM = 0x00000001;
    /** PA 測定 */
    public static final int DPRF_COLL_OPT_PA = 0x00000002;
    /** サンプリング */
    public static final int DPRF_COLL_OPT_SAMPLING = 0x00000004;
    /** 時系列 */
    public static final int DPRF_COLL_OPT_REALTIME = 0x00000010;
    /** 指定区間 */
    public static final int DPRF_COLL_OPT_PA_RANGE = 0x00000080;
    /** イベント指定(非公開) */
    public static final int DPRF_COLL_OPT_PA_EVENT = 0x00000100;
    /** コールグラフ */
    public static final int DPRF_COLL_OPT_CALLGRAPH = 0x00000200;
    /** MPI経過時間 */
    public static final int DPRF_COLL_OPT_MPIELAPS = 0x00000400;
    /** サンプリング指定区間(引数無し) */
    public static final int DPRF_COLL_OPT_SAMP_RANGE = 0x00000800;
    /** USERFUNC ON */
    public static final int DPRF_COLL_OPT_USERFUNC = 0x00001000;
    /** サンプリング指定区間(引数有り) */
    public static final int DPRF_COLL_OPT_SAMP_RANGE_COST = 0x00002000;
    /** 収集時に-Inompiall指定 */
    public static final int DPRF_COLL_OPT_ST_COM = 0x00004000;
    /** SLEEP ON */
    public static final int DPRF_COLL_OPT_SLEEP = 0x00010000;

    // 実行形態のビット列
    /** 逐次 */
    public static final int EXEC_KIND_SERIAL      =0x1000;
    /** MPI */
    public static final int EXEC_KIND_MPI         =0x2000;
    /** XPF */
    public static final int EXEC_KIND_XPF         =0x4000;
    /** FULLモード */
    public static final int EXEC_KIND_FULL        =0x0100;
    /** LIMITEDモード */
    public static final int EXEC_KIND_LIMITED     =0x0200;
    /** 自動並列 */
    public static final int EXEC_KIND_AUTO        =0x0010;
    /** OpenMP */
    public static final int EXEC_KIND_OMP         =0x0020;
    /** 不明 */
    public static final int EXEC_KIND_UNKNOWN     =0x0000;

    // 測定時オプションのビット列
    // 測定時オプション:EPROF
    /** MPI情報     */
    public static final int  EPRF_COLL_OPT_MPI         =    0x00000001;
    /** MPI情報なし */
    public static final int  EPRF_COUNTER_OPT_NOMPI    =    0x00000002;
    /** PA 測定     */
    public static final int  EPRF_COLL_OPT_PA          =    0x00000004;
    /** イベント指定(非公開) */
    public static final int  EPRF_COLL_OPT_PA_EVENT    =    0x00000100;

}
