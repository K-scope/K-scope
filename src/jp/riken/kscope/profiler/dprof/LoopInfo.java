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
 * DPROF:コストループ情報クラス
 * @author RIKEN
 */
public class LoopInfo {
    private float sampNum;
    private float barrierSyncWaitNum;
    private float mpiLibCostNum;
    private int lineLoopStart;
    private int lineLoopEnd;
    private int nestLevel;
    private short loopType;
    private short parallelInfo;
    private int symbolIndex;
    private int fileIndex;

    /**
     * サンプリング回数を取得する
     * @return サンプリング回数
     */
    public float getSampNum() {
        return sampNum;
    }

    /**
     * バリア同期待ち数を取得する
     * @return バリア同期待ち数
     */
    public float getBarrierSyncWaitNum() {
        return barrierSyncWaitNum;
    }

    /**
     * MPIライブラリコスト数を取得する
     * @return MPIライブラリコスト数
     */
    public float getMpiLibCostNum() {
        return mpiLibCostNum;
    }

    /**
     * ループ開始行を取得する
     * @return ループ開始行
     */
    public int getLineLoopStart() {
        return lineLoopStart;
    }

    /**
     * ループ終了行を取得する
     * @return ループ終了行
     */
    public int getLineLoopEnd() {
        return lineLoopEnd;
    }

    /**
     * ネストレベルを取得する
     * @return ネストレベル
     */
    public int getNestLevel() {
        return nestLevel;
    }

    /**
     * ループ種別を取得する
     * @return ループ種別
     */
    public short getLoopType() {
        return loopType;
    }

    /**
     * 並列化情報を取得する
     * @return 並列化情報
     */
    public short getParallelInfo() {
        return parallelInfo;
    }

    /**
     * シンボルのindexを取得する
     * @return シンボルのindex
     */
    public int getSymbolIndex() {
        return symbolIndex;
    }

    /**
     * ファイルのindexを取得する
     * @return ファイルのindex
     */
    public int getFileIndex() {
        return fileIndex;
    }

    /**
     * サンプリング回数を設定する
     * @param sampNum
     *            設定するサンプリング回数
     */
    public void setSampNum(float sampNum) {
        this.sampNum = sampNum;
    }

    /**
     * バリア同期待ち数を設定する
     * @param barrierSyncWaitNum
     *            設定するバリア同期待ち数
     */
    public void setBarrierSyncWaitNum(float barrierSyncWaitNum) {
        this.barrierSyncWaitNum = barrierSyncWaitNum;
    }

    /**
     * MPIライブラリコスト数を設定する
     * @param mpiLibCostNum
     *            設定するMPIライブラリコスト数
     */
    public void setMpiLibCostNum(float mpiLibCostNum) {
        this.mpiLibCostNum = mpiLibCostNum;
    }

    /**
     * ループ開始行を設定する
     * @param lineLoopStart
     *            設定するループ開始行
     */
    public void setLineLoopStart(int lineLoopStart) {
        this.lineLoopStart = lineLoopStart;
    }

    /**
     * ループ終了行を設定する
     * @param lineLoopEnd
     *            設定するループ終了行
     */
    public void setLineLoopEnd(int lineLoopEnd) {
        this.lineLoopEnd = lineLoopEnd;
    }

    /**
     * ネストレベルを設定する
     * @param nestLevel
     *            設定するネストレベル
     */
    public void setNestLevel(int nestLevel) {
        this.nestLevel = nestLevel;
    }

    /**
     * ループ種別を設定する
     * @param loopType
     *            設定するループ種別
     */
    public void setLoopType(short loopType) {
        this.loopType = loopType;
    }

    /**
     * 並列化情報を設定する
     * @param parallelInfo
     *            設定する並列化情報
     */
    public void setParallelInfo(short parallelInfo) {
        this.parallelInfo = parallelInfo;
    }

    /**
     * シンボルのindexを設定する
     * @param symbolIndex
     *            設定するシンボルのindex
     */
    public void setSymbolIndex(int symbolIndex) {
        this.symbolIndex = symbolIndex;
    }

    /**
     * ファイルのindexを設定する
     * @param fileIndex
     *            設定するファイルのindex
     */
    public void setFileIndex(int fileIndex) {
        this.fileIndex = fileIndex;
    }
}
