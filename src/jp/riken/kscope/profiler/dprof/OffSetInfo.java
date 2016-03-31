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
 * オフセット情報を保持する
 *
 * @author RIKEN
 *
 */
public class OffSetInfo {
    private int lineInfo;
    private int loopInfo;
    private int callGraphInfo;
    private int mpiFuncElapsTimeInfo;
    private int comInfo;
    private int symbolInfo;

    /**
     * ライン情報を取得する
     * @return ライン情報
     */
    public int getLineInfo() {
        return lineInfo;
    }

    /**
     * ループ情報を取得する
     * @return ループ情報
     */
    public int getLoopInfo() {
        return loopInfo;
    }

    /**
     * コールグラフ情報を取得する
     * @return コールグラフ情報
     */
    public int getCallGraphInfo() {
        return callGraphInfo;
    }

    /**
     * MPI関数経過時間情報を取得する
     * @return MPI関数経過時間情報
     */
    public int getMpiFuncElapsTimeInfo() {
        return mpiFuncElapsTimeInfo;
    }

    /**
     * 通信情報を取得する
     * @return 通信情報
     */
    public int getComInfo() {
        return comInfo;
    }

    /**
     * シンボル情報を取得する
     * @return シンボル情報
     */
    public int getSymbolInfo() {
        return symbolInfo;
    }

    /**
     * ライン情報を設定する
     * @param lineInfo
     *            設定するライン情報
     */
    public void setLineInfo(int lineInfo) {
        this.lineInfo = lineInfo;
    }

    /**
     * ループ情報を設定する
     * @param loopInfo
     *            設定するループ情報
     */
    public void setLoopInfo(int loopInfo) {
        this.loopInfo = loopInfo;
    }

    /**
     * コールグラフ情報を設定する
     * @param callGraphInfo
     *            設定するコールグラフ情報
     */
    public void setCallGraphInfo(int callGraphInfo) {
        this.callGraphInfo = callGraphInfo;
    }

    /**
     * MPI関数経過時間情報を設定する
     * @param mpiFuncElapsTimeInfo
     *            設定するMPI関数経過時間情報
     */
    public void setMpiFuncElapsTimeInfo(int mpiFuncElapsTimeInfo) {
        this.mpiFuncElapsTimeInfo = mpiFuncElapsTimeInfo;
    }

    /**
     * 通信情報を設定する
     * @param comInfo
     *            設定する通信情報
     */
    public void setComInfo(int comInfo) {
        this.comInfo = comInfo;
    }

    /**
     * シンボル情報を設定する
     * @param symbolInfo
     *            設定するシンボル情報
     */
    public void setSymbolInfo(int symbolInfo) {
        this.symbolInfo = symbolInfo;
    }

}
