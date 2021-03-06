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

package jp.riken.kscope.language;

import java.io.Serializable;

/**
 * 変数の配列添え字を表現するクラス。
 * 
 * @author RIKEN
 * 
 */
public class DimensionIndex implements Serializable {
    /** シリアル番号 */
    private static final long serialVersionUID = -4025983357574098164L;
    private Expression start;
    private Expression end;

    /**
     * コンストラクタ
     * 
     * @param st
     *            開始インデックス
     * @param en
     *            終了インデックス
     */
    public DimensionIndex(Expression st, Expression en) {
        this.start = st;
        this.end = en;
    }
    // ++++++++++++++++++++++++++++++++++++++++++++

    /**
     * 添字の開始をセットする。
     * 
     * @param iStart
     *            添字の開始
     */
    public void set_start(Expression iStart) {
        start = iStart;
    }

    /**
     * 添字の終了をセットする。
     * 
     * @param iEnd
     *            添字の終了
     */
    public void set_end(Expression iEnd) {
        end = iEnd;
    }

    /**
     * 添字の開始を返す。
     * 
     * @return 添字の開始
     */
    public Expression get_start() {
        return (start);
    }

    /**
     * 添字の終了を返す。
     * 
     * @return 添字の終了
     */
    public Expression get_end() {
        return (end);
    }
}
