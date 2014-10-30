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
 *
 * スレッド情報を保持する
 *
 * @author RIKEN
 */
public class ThreadInfo {
    private int threadNo;
    private float elapsTime;
    private float userTime;
    private float systemTime;
    private float totalSampNum;
    private float barrierSyncWaitNum;
    private float mpiLibCostNum;
    private float mpiFuncElapsTime;
    private double[] paInfo;

    /**
     * スレッド番号を取得する
     * @return スレッド番号
     */
    public int getThreadNo() {
        return threadNo;
    }
    /**
     * 経過時間を取得する
     * @return 経過時間
     */
    public float getElapsTime() {
        return elapsTime;
    }
    /**
     * ユーザ時間を取得する
     * @return ユーザ時間
     */
    public float getUserTime() {
        return userTime;
    }
    /**
     * システム時間を取得する
     * @return システム時間
     */
    public float getSystemTime() {
        return systemTime;
    }
    /**
     * サンプリング総数を取得する
     * @return サンプリング総数
     */
    public float getTotalSampNum() {
        return totalSampNum;
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
     * MPI関数経過時間を取得する
     * @return MPI関数経過時間
     */
    public float getMpiFuncElapsTime() {
        return mpiFuncElapsTime;
    }
    /**
     * PA情報を取得する
     * @return PA情報
     */
    public double[] getPaInfo() {
        return paInfo;
    }
    /**
     * スレッド番号を設定する
     * @param threadNo 設定するスレッド番号
     */
    public void setThreadNo(int threadNo) {
        this.threadNo = threadNo;
    }
    /**
     * 経過時間を設定する
     * @param elapsTime 設定する経過時間
     */
    public void setElapsTime(float elapsTime) {
        this.elapsTime = elapsTime;
    }
    /**
     * ユーザ時間を設定する
     * @param userTime 設定するユーザ時間
     */
    public void setUserTime(float userTime) {
        this.userTime = userTime;
    }
    /**
     * システム時間を設定する
     * @param systemTime 設定するシステム時間
     */
    public void setSystemTime(float systemTime) {
        this.systemTime = systemTime;
    }
    /**
     * サンプリング総数を設定する
     * @param totalSampNum 設定するサンプリング総数
     */
    public void setTotalSampNum(float totalSampNum) {
        this.totalSampNum = totalSampNum;
    }
    /**
     * バリア同期待ち数を設定する
     * @param barrierWaitSyncNum 設定するバリア同期待ち数
     */
    public void setBarrierWaitSyncNum(float barrierWaitSyncNum) {
        this.barrierSyncWaitNum = barrierWaitSyncNum;
    }
    /**
     * MPIライブラリコスト数を設定する
     * @param mpiLibCostNum 設定するMPIライブラリコスト数
     */
    public void setMpiLibCostNum(float mpiLibCostNum) {
        this.mpiLibCostNum = mpiLibCostNum;
    }
    /**
     * MPI関数経過時間を設定する
     * @param mpiFuncElapsTime 設定するMPI関数経過時間
     */
    public void setMpiFuncElapsTime(float mpiFuncElapsTime) {
        this.mpiFuncElapsTime = mpiFuncElapsTime;
    }
    /**
     * PA情報を設定する
     * @param paInfo 設定するPA情報
     */
    public void setPaInfo(double[] paInfo) {
        this.paInfo = paInfo;
    }
}
