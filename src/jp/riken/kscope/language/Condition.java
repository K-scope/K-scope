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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
* IF･･･THEN･･･ELSE文など複数の条件式で構成される. <br/>
* 判断ブロックにおける各条件式ブロックをあらわす.
*
* @author RIKEN
*
*/
public class Condition extends Block {
    /** シリアル番号 */
    private static final long serialVersionUID = 7720056035437803500L;
    /**
     * 分岐の条件式。
     */
    private Expression expression;

    /**
     * コンストラクタ。
     *
     * @param mama
     *            親ブロック
     * @param exprssn
     *            条件式
     */
    Condition(Block mama, Expression exprssn) {
        super(mama);
        this.expression = exprssn;
        // 親IF文を設定する
        if (this.expression != null) {
            this.expression.setParentStatement(this);
        }
    }

    /**
     * 条件式の文字列表現を返す。
     *
     * @return 条件式の文字列表現
     */
    public String conditionToString() {
        return getExpression().toString();
    }

    /**
     * 条件式の取得.
     *
     * @return 条件式
     */
    public Expression getExpression() {
        return expression;
    }
    /**
     * ブロックタイプの取得。
     *
     * @return BlockType.CONDITION
     */
    public BlockType getBlockType() {
        return BlockType.CONDITION;
    }

    /**
     * 同一ブロックであるかチェックする.
     * @param block        ブロック
     * @return        true=一致
     */
    @Override
    public boolean equalsBlocks(Block block) {
        if (block == null) return false;
        if (!(block instanceof Condition)) return false;
        if (!super.equalsBlocks(block)) return false;

        if (this.expression != null && ((Condition)block).expression != null) {
            if (!this.expression.equalsExpression(((Condition)block).expression)) {
                return false;
            }
        }
        else if (this.expression != null || ((Condition)block).expression != null) {
            return false;
        }

        return true;
    }

    /**
     * プロシージャ（関数）からブロックまでの階層文字列表記を取得する
     * 階層文字列表記 : [main()]-[if (...)]-[if (...)]
     * CompoundBlock（空文）は省略する.
     * @return      階層文字列表記
     */
    @Override
    public String toStringProcedureScope() {
        return this.toStringScope(false);
    }

    /**
     * モジュールからブロックまでの階層文字列表記を取得する
     * 階層文字列表記 : [main()]-[if (...)]-[if (...)]
     * CompoundBlock（空文）は省略する.
     * @return      階層文字列表記
     */
    @Override
    public String toStringModuleScope() {
        return this.toStringScope(true);
    }

    /**
     * ブロックの階層文字列表記を取得する
     * 階層文字列表記 : [main()]-[if (...)]-[if (...)]
     * CompoundBlock（空文）は省略する.
     * @param   module     true=Moduleまでの階層文字列表記とする
     * @return      階層文字列表記
     */
    @Override
    public String toStringScope(boolean module) {
        String statement = "";
        if (this.getMotherBlock() != null) {
            String buf = null;
            if (module) buf = this.getMotherBlock().toStringModuleScope();
            else buf = this.getMotherBlock().toStringProcedureScope();
            if (buf != null && !buf.isEmpty()) {
                statement = buf;
            }
        }
        return statement;
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
    public Set<Variable> getBlockVariables() {

        Set<Variable> list = new HashSet<Variable>();
        if (this.expression != null) {
            Set<Variable> vars = this.expression.getAllVariables();
            if (vars != null && vars.size() > 0) {
                list.addAll(vars);
            }
        }

        if (list.size() <= 0) return null;
        return list;
    }

    /**
     * 手続呼出しのリストを返す。
     * @return 手続呼出しのリスト
     */
    @Override
    public List<ProcedureUsage> getCalls() {
        List<ProcedureUsage> list = new ArrayList<ProcedureUsage>();
        List<ProcedureUsage> calls = super.getCalls();
        if (calls != null && calls.size() > 0) {
            list.addAll(calls);
        }
        if (this.expression != null) {
            Set<ProcedureUsage> exp_calls = this.expression.getAllFunctions();
            if (exp_calls != null && exp_calls.size() > 0) {
                list.addAll(exp_calls);
            }
        }
        return list;
    }
}
