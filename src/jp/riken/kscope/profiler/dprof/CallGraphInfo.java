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

import java.util.ArrayList;

/**
 * コールグラフ情報を保持する
 *
 * @author RIKEN
 */
public class CallGraphInfo {
    private float totalSumSampNum;
    private ArrayList<StackInfo> stackInfo;

    /**
     * 総累積サンプリング数を取得する
     * @return 総累積サンプリング数
     */
    public float getTotalSumSampNum() {
        return totalSumSampNum;
    }

    /**
     * スタック毎のコールグラフ情報のリストを取得する
     * @return スタック毎のコールグラフ情報のリスト
     */
    public ArrayList<StackInfo> getStackInfo() {
        return stackInfo;
    }

    /**
     * 総累積サンプリング数を設定する
     * @param totalSumSampNum
     *            設定する総累積サンプリング数
     */
    public void setTotalSumSampNum(float totalSumSampNum) {
        this.totalSumSampNum = totalSumSampNum;
    }

    /**
     * スタック毎のコールグラフ情報のリストを設定する
     * @param stackInfo
     *            設定するスタック毎のコールグラフ情報のリスト
     */
    public void setStackInfo(ArrayList<StackInfo> stackInfo) {
        this.stackInfo = stackInfo;
    }

}
