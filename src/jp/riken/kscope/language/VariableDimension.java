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
import java.util.HashSet;
import java.util.Set;

/**
 * 変数宣言の配列添え字を表現するクラス。
 *
 * @author RIKEN
 *
 */
public class VariableDimension implements Serializable {
    /** シリアル番号 */
    private static final long serialVersionUID = 1309109368271186155L;
    private DimensionIndex[] indices = null;
    /** 変数の親ブロック */
    private IBlock parentStatement;

    /**
     * コンストラクタ。
     * @param inds         添字の配列
     */
    public VariableDimension(DimensionIndex[] inds) {
        this.indices = inds;
    }

    /**
     * コピーコンストラクタ。
     * @param dimension         配列添え字クラス
     */
    public VariableDimension(VariableDimension dimension) {
        if (dimension == null) return;
        if (dimension.indices == null) return;
        if (dimension.indices.length <= 0) return;

        this.indices = new DimensionIndex[dimension.indices.length];
        for (int i = 0; i<dimension.indices.length; i++) {
            DimensionIndex idx = dimension.indices[i];
            this.indices[i] = new DimensionIndex(idx);
        }
    }


    // ++++++++++++++++++++++++++++++++++++++++++++

    protected DimensionIndex getIndex(int i) {
        return this.indices[i];
    }

    public DimensionIndex[] getIndex() {
        return this.indices;
    }
    protected void set_index_size(int size) {
        indices = new DimensionIndex[size];
    }

    // ++++++++++++++++++++++++++++++++++++++++++++

    protected void set_index_start(int i, Expression index_start) {
        indices[i].set_start(index_start);
    }

    // ++++++++++++++++++++++++++++++++++++++++++++

    protected void set_index_end(int i, Expression index_end) {
        indices[i].set_end(index_end);
    }

    // ++++++++++++++++++++++++++++++++++++++++++++

    protected int get_index_size() {
        if (this.indices == null) return 0;
        return (indices.length);
    }

    // ++++++++++++++++++++++++++++++++++++++++++++

    protected Expression get_index_start(int i) {
        return (indices[i].get_start());
    }

    // ++++++++++++++++++++++++++++++++++++++++++++

    protected Expression get_index_end(int i) {
        return (indices[i].get_end());
    }

    @Override
    public String toString() {
        if (this.indices == null || this.indices.length <= 0) return null;

        StringBuilder buf = new StringBuilder();
        for (DimensionIndex idx : this.indices) {
            String start = null;
            String end = null;
            if (idx.get_start() != null) start = idx.get_start().toString();
            if (idx.get_end() != null) end = idx.get_end().toString();
            if (start != null && start.trim().isEmpty()) start = null;
            if (end != null && end.trim().isEmpty()) end = null;

            if (buf.length() > 0) buf.append(",");
            if (start != null && end != null) {
                buf.append(start);
                buf.append(":");
                buf.append(end);
            }
            else if (start != null && end == null) {
                buf.append(start);
            }
            else if (start == null && end != null) {
                buf.append(end);
            }
            else if (start == null && end == null) {
                buf.append(":");
            }
        }
        if (buf.length() <= 0) return null;

        return "(" + buf.toString() + ")";
    }


    /**
     * 変数リストを取得する.
     */
    public Set<Variable> getAllVariables() {
        Set<Variable> vars = new HashSet<Variable>();
        if (this.indices == null) return null;
        for (DimensionIndex idx : this.indices) {
            Set<Variable> idx_vars = idx.getAllVariables();
            if (idx_vars != null && idx_vars.size() > 0) {
                vars.addAll(idx_vars);
            }
        }

        if (vars.size() <= 0) return null;

        return vars;
    }

    /**
     * 親ブロックを設定する.
     * @param parent 親ブロック
     */
    public void setParentStatement(IBlock parent) {
        this.parentStatement = parent;
        // 子変数に対して設定する
        if (this.indices != null && indices.length > 0) {
            for (DimensionIndex idx : this.indices) {
                if (idx == null) continue;
                idx.setParentStatement(parent);
            }
        }
    }

    /**
     * インデックス配列をクリアする
     */
    public void clearIndices() {
        if (this.indices == null) return;

        for (DimensionIndex idx : this.indices) {
            idx.set_start(new Expression());
            idx.set_end(new Expression());
        }
    }

    /**
     * 式に含まれる全ての手続呼出のセットを返す。変数および手続呼出の添字も対象とする。 再帰呼び出し。
     * @return 手続呼出のセット。
     */
    public Set<ProcedureUsage> getAllFunctions() {
        if (this.indices == null || this.indices.length <= 0) return null;

        Set<ProcedureUsage> list = new HashSet<ProcedureUsage>();
        for (DimensionIndex index : this.indices) {
            Set<ProcedureUsage> calls = index.getAllFunctions();
            if (calls != null && calls.size() > 0) {
                list.addAll(calls);
            }
        }

        if (list.size() <= 0) return null;
        return list;
    }
}
