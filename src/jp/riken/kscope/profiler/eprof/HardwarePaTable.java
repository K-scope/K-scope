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
package jp.riken.kscope.profiler.eprof;

/**
 * ハードウェアモニタ情報（ＰＡ情報）テーブル
 * @author riken
 */
public class HardwarePaTable {
    /** スレッド番号 */
    private int threadno;
    /** ハードウェアモニタ情報(PA情報)テーブル */
    private double[] paTable;

    /**
     * コンストラクタ
     */
    public HardwarePaTable() {
    }

    /**
     * スレッド番号
     * @return 		スレッド番号
     */
    public int getThreadno() {
        return threadno;
    }

    /**
     * スレッド番号
     * @param no スレッド番号
     */
    public void setThreadno(int no) {
        this.threadno = no;
    }
    /**
     * ハードウェアモニタ情報(PA情報)テーブル
     * @return 		ハードウェアモニタ情報(PA情報)テーブル
     */
    public double[] getPaTable() {
        return paTable;
    }

    /**
     * ハードウェアモニタ情報(PA情報)テーブル
     * @param table ハードウェアモニタ情報(PA情報)テーブル
     */
    public void setPaTable(double[] table) {
        this.paTable = table;
    }


}
