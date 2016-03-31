/*
 * K-scope
 * Copyright 2012-2015 RIKEN, Japan
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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
*
* 繰り返し処理を表現したクラス.<br>
* FortranにおけるDOループに該当するもの.
*
* @author RIKEN
*
*/
public class Repetition extends Block {
    /** シリアル番号 */
    private static final long serialVersionUID = -2221953302518033528L;
    /** DO文:index変数 */
    private Variable iterator;
    /** DO文:初期値 */
    private Expression initIterator;
    /** DO文:最大値 */
    private Expression endCondition;
    /** DO文:ステップインターバル */
    private Expression step;

    /**
     * コンストラクタ.
     */
    Repetition() {
        super();
    }

    /**
     * コンストラクタ.
     *
     * @param mama 親ブロック
     */

    Repetition(Block mama) {
        super(mama);
    }

    /**
     * コンストラクタ.
     *
     * @param itrtr ループ制御変数
     * @param initItrtr 始値
     * @param endCndtn 終値
     * @param stp 刻み幅
     */
    public Repetition(Variable itrtr,
            Expression initItrtr, Expression endCndtn,
            Expression stp) {
        this();
        this.iterator = itrtr;
        this.initIterator = initItrtr;
        this.endCondition = endCndtn;
        this.step = stp;
    }
    /**
     * ブロックタイプの取得。
     *
     * @return BlockType.REPETITION
     */
    public BlockType getBlockType() {
        return BlockType.REPETITION;
    }

    /**
     * ループ制御変数の設定.
     *
     * @param itrtr ループ制御変数
     */
    protected void setIterator(Variable itrtr) {
        iterator = itrtr;
    }
    /**
     * ループ制御変数の取得.
     *
     * @return ループ制御変数
     */
    public Variable getIterator() {
        return iterator;
    }

    /**
     * 始値の設定.
     *
     * @param initItrtr 始値
     */
    protected void setInitIterator(Expression initItrtr) {
        initIterator = initItrtr;
    }
    /**
     * 始値の取得.
     *
     * @return 始値
     */
    public Expression getInitIterator() {
        return initIterator;
    }

    /**
     * 終値の設定.
     *
     * @param endCndtn 終値
     */
    protected void setEndCondition(Expression endCndtn) {
        endCondition = endCndtn;
    }
    /**
     * 終値の取得.
     *
     * @return 終値
     */
    public Expression getEndCondition() {
        return endCondition;
    }

    /**
     * 刻み幅の設定.
     *
     * @param stp 刻み幅
     */
    protected void setStep(Expression stp) {
        step = stp;
    }
    /**
     * 刻み幅の取得.
     *
     * @return 刻み幅
     */
    public Expression getStep() {
        return step;
    }

    /**
     * メンバー変数の設定.
     *
     * @param itrtr
     *           ループ制御変数
     * @param initItrtr
     *           始値
     * @param endCndtn
     *           終値
     * @param stp
     *           刻み幅
     */
    protected void setProperty(Variable itrtr,
            Expression initItrtr, Expression endCndtn,
            Expression stp) {
        iterator = itrtr;
        initIterator = initItrtr;
        endCondition = endCndtn;
        step = stp;
        // 親DO文をセットする
        if (iterator != null) {
            iterator.setParentStatement(this);
        }
        if (initIterator != null) {
            initIterator.setParentStatement(this);
        }
        if (endCondition != null) {
            endCondition.setParentStatement(this);
        }
        if (step != null) {
            step.setParentStatement(this);
        }
    }

    /**
     * 式の変数リストを取得する.
     * 子ブロックの変数リストも取得する。
     * @return        式の変数リスト
     */
    @Override
    public Set<Variable> getAllVariables() {
        Set<Variable> list = new HashSet<Variable>();
        Set<Variable> vars = super.getAllVariables();
        if (vars != null && vars.size() > 0) {
            list.addAll(vars);
        }
        vars = this.getBlockVariables();
        if (vars != null && vars.size() > 0) {
            list.addAll(vars);
        }

        if (list.size() <= 0) return null;
        return list;
    }

    /**
     * 式の変数リストを取得する.
     * ブロックのみの変数リストを取得する。
     * @return        式の変数リスト
     */
    @Override
    public Set<Variable> getBlockVariables() {

        Set<Variable> list = new HashSet<Variable>();

        if (this.iterator != null) {
            list.add(this.iterator);
            Set<Variable> vars = this.iterator.getAllVariables();
            if (vars != null && vars.size() > 0) {
                list.addAll(vars);
            }
        }
        if (this.initIterator != null) {
            Set<Variable> vars = this.initIterator.getAllVariables();
            if (vars != null && vars.size() > 0) {
                list.addAll(vars);
            }
        }
        if (this.endCondition != null) {
            Set<Variable> vars = this.endCondition.getAllVariables();
            if (vars != null && vars.size() > 0) {
                list.addAll(vars);
            }
        }
        if (this.step != null) {
            Set<Variable> vars = this.step.getAllVariables();
            if (vars != null && vars.size() > 0) {
                list.addAll(vars);
            }
        }

        if (list.size() <= 0) return null;
        return list;
    }

    /**
     * 終了:END文を持つブロックは、終了:END文を返す
     * @return        終了:END文
     */
    @Override
    public String toEndString() {
        String buf = "end do";
        String label = "";
        if (this.get_start().get_label() != null) {
            label = this.get_start().get_label();
        }
        if (label != null
            && !label.isEmpty()
            && !Statement.NO_LABEL.equalsIgnoreCase(label)) {
            buf += " " + label;
        }
        return buf;
    }


    /**
     * 手続呼出しのリストを返す。
     * @return 手続呼出しのリスト
     */
    @Override
    public List<ProcedureUsage> getCalls() {
        List<ProcedureUsage> list = super.getCalls();
        if (list == null) {
            list = new ArrayList<ProcedureUsage>();
        }

        if (this.iterator != null) {
            Set<ProcedureUsage> calls = this.iterator.getAllFunctions();
            if (calls != null && calls.size() > 0) {
                list.addAll(calls);
            }
        }
        if (this.initIterator != null) {
            Set<ProcedureUsage> calls = this.initIterator.getAllFunctions();
            if (calls != null && calls.size() > 0) {
                list.addAll(calls);
            }
        }
        if (this.endCondition != null) {
            Set<ProcedureUsage> calls = this.endCondition.getAllFunctions();
            if (calls != null && calls.size() > 0) {
                list.addAll(calls);
            }
        }
        if (this.step != null) {
            Set<ProcedureUsage> calls = this.step.getAllFunctions();
            if (calls != null && calls.size() > 0) {
                list.addAll(calls);
            }
        }

        if (list == null || list.size() <= 0) return null;
        return list;
    }

}
