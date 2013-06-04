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
 * シンボル情報を保持する
 *
 * @author riken
 *
 */
public class SymbolInfo {
    private float sampNum;
    private float barrierSyncWaitNum;
    private float mpiLibCostNum;
    private int lineSymbolStart;
    private int lineSymbolEnd;
    private int fileIndex;
    private String symbolName;

    /**
     * MPIライブラリコスト数を取得する
     * @return MPIライブラリコスト数
     */
    public float getMpiLibCostNum() {
        return mpiLibCostNum;
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
     * シンボル開始行を取得する
     * @return シンボル開始行
     */
    public int getLineSymbolStart() {
        return lineSymbolStart;
    }

    /**
     * シンボル終了行を取得する
     * @return シンボル終了行
     */
    public int getLineSymbolEnd() {
        return lineSymbolEnd;
    }

    /**
     * ファイルのindexを取得する
     * @return ファイルのindex
     */
    public int getFileIndex() {
        return fileIndex;
    }

    /**
     * シンボル名を取得する
     * @return シンボル名
     */
    public String getSymbolName() {
        return symbolName;
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
     * シンボル開始行を設定する
     * @param lineSymbolStart
     *            設定するシンボル開始行
     */
    public void setLineSymbolStart(int lineSymbolStart) {
        this.lineSymbolStart = lineSymbolStart;
    }

    /**
     * シンボル終了行を設定する
     * @param lineSymbolEnd
     *            設定するシンボル終了行
     */
    public void setLineSymbolEnd(int lineSymbolEnd) {
        this.lineSymbolEnd = lineSymbolEnd;
    }

    /**
     * ファイルのindexを設定する
     * @param fileIndex
     *            設定するファイルのindex
     */
    public void setFileIndex(int fileIndex) {
        this.fileIndex = fileIndex;
    }

    /**
     * シンボル名を設定する
     * @param symbolName
     *            設定するシンボル名
     */
    public void setSymbolName(String symbolName) {
        this.symbolName = symbolName;
    }

}
