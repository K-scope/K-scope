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
import java.util.List;

/**
 * シンボル情報レコードクラス
 * @author riken
 */
public class SymbolRecord {
    /** シンボル情報：シンボル情報リスト(=スレッド数) */
    private List<SymbolList> records;

    /**
     * コンストラクタ
     */
    public SymbolRecord() {
        records = new ArrayList<SymbolList>();
    }

    /**
     * シンボル情報リストを追加する
     * @param symbol		スレッドリスト
     * @return		true=success
     */
    public boolean addSymbolList(SymbolList symbol) {
        return this.records.add(symbol);
    }

    /**
     * シンボル情報レコードを取得する
     * @return		シンボル情報レコード
     */
    public List<SymbolList> getSymbolRecord() {
        return this.records;
    }

    /**
     * スレッド番号(0〜)のシンボル情報リストを取得する
     * @param threadid		スレッド番号(0〜)
     * @return			シンボル情報リスト
     */
    public SymbolList getSymbolList(int threadid) {
        if (this.records == null) return null;
        if (this.records.size() <= threadid) return null;
        return this.records.get(threadid);
    }

    /**
     * シンボル情報を取得するする
     * @param threadid		スレッド番号(0〜)
     * @param symbolid		シンボルインデックス
     * @return		シンボル情報
     */
    public SymbolInfo getSymbolInfo(int threadid, int symbolid) {
        SymbolList list = getSymbolList(threadid);
        if (list == null) return null;
        return list.getSymbolInfo(symbolid);
    }

    /**
     * シンボル情報リスト数を取得する(=スレッド数)
     * @return		シンボル情報リスト数
     */
    public int getSymbolListCount() {
        if (this.records == null) return 0;
        return this.records.size();
    }


    /**
     * シンボル数を取得する
     * @param threadid		スレッド番号(0〜)
     * @return		シンボル数
     */
    public int getSymbolInfoCount(int threadid) {
        SymbolList list = getSymbolList(threadid);
        if (list == null) return 0;
        return list.getSymbolInfoCount();
    }

}
