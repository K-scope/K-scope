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
 * シンボル情報リストクラス
 * @author RIKEN
 */
public class SymbolList {
    /** シンボル情報リスト */
    private List<SymbolInfo> list;

    /**
     * コンストラクタ
     */
    public SymbolList() {
        list = new ArrayList<SymbolInfo>();
    }

    /**
     * シンボル情報を追加する
     * @param symbol		シンボル情報
     * @return		true=success
     */
    public boolean addSymbolInfo(SymbolInfo symbol) {
        return this.list.add(symbol);
    }

    /**
     * シンボル情報リストを取得する
     * @return		シンボル情報リスト
     */
    public List<SymbolInfo> getSymbolList() {
        return this.list;
    }

    /**
     * シンボル情報を取得する
     * @param symbolid		シンボルインデックス
     * @return		シンボル情報
     */
    public SymbolInfo getSymbolInfo(int symbolid) {
        if (list == null) return null;
        if (list.size() <= symbolid) return null;
        return list.get(symbolid);
    }

    /**
     * シンボル数を取得する
     * @return		シンボル数
     */
    public int getSymbolInfoCount() {
        if (list == null) return 0;
        return list.size();
    }
}
