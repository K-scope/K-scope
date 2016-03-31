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
 * コールグラフ情報のスタック情報を保持する
 *
 * @author RIKEN
 */
public class StackInfo {
    /** ネストレベル */
    private int nestLevel;
    /** サンプリング数 */
    private float sampNum;
    /** 累積サンプリング数 */
    private float sumSampNum;
    /** シンボル名 */
    private String symbolName;

    /**
     * ネストレベルを取得する
     * @return ネストレベル
     */
    public int getNestLevel() {
        return nestLevel;
    }

    /**
     * サンプリング数を取得する
     * @return サンプリング数
     */
    public float getSampNum() {
        return sampNum;
    }

    /**
     * 累積サンプリング数を取得する
     * @return 累積サンプリング数
     */
    public float getSumSampNum() {
        return sumSampNum;
    }

    /**
     * シンボル名を取得する
     * @return シンボル名
     */
    public String getSymbolName() {
        return symbolName;
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
     * サンプリング数を設定する
     * @param sampNum
     *            設定するサンプリング数
     */
    public void setSampNum(float sampNum) {
        this.sampNum = sampNum;
    }

    /**
     * 累積サンプリング数を設定する
     * @param sumSampNum
     *            設定する累積サンプリング数
     */
    public void setSumSampNum(float sumSampNum) {
        this.sumSampNum = sumSampNum;
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
