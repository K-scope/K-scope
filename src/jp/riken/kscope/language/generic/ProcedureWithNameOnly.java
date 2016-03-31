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

package jp.riken.kscope.language.generic;

import java.io.Serializable;
import java.util.List;

import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.Procedure;

/**
 * 関数名のみを持つ、総称関数クラス.<br>
 * Fortranのmodule procedureに対応。<br>
 *
 * @author RIKEN
 *
 */
public class ProcedureWithNameOnly
 implements Serializable,
        jp.riken.kscope.language.generic.IProcedureItem {
	/** シリアル番号 */
	private static final long serialVersionUID = 682980844988371714L;
    private String name;
    private Procedure definition;

    /**
     * コンストラクタ。
     *
     * @param nm
     *         関数名
     */
    public ProcedureWithNameOnly(String nm) {
        this.name = nm;
    }

    @Override
    public String toString() {
        return "module procedure : " + this.name;
    }
    /**
     * 候補対象となる関数が自分の情報と適合しているかどうかを<br>
     * 調べるメソッド。候補対象の関数名 ＆ 候補対象関数の<br>
     * 引数リストと実引数リストが、適合していれば、trueを返す。<br>
     *
     * @param target
     *            候補対象関数
     * @param actualArguments
     *            実引数リスト
     *
     * @return true:  適合している
     *         false: 適合していない
     */
    @Override
    public boolean matches(Procedure target, List<Expression> actualArguments) {
        if (target == null || actualArguments == null) { return false; }
        if (!(target.get_name().equalsIgnoreCase(this.name))) { return false; }
        if (!target.matches(actualArguments)) { return false; }
        return true;
    }
    /**
     * 対象となる実引数が自分の情報と適合しているかどうかを<br>
     * 調べるメソッド。<br>
     * 引数リストと実引数リストが、適合していれば、trueを返す。<br>
     *
     * @param actualArguments
     *            実引数リスト
     *
     * @return true:  適合している
     *         false: 適合していない
     */
    @Override
    public boolean matches(List<Expression> actualArguments) {
        if (this.definition == null || actualArguments == null) { return false; }
        if (!(this.definition.get_name().equalsIgnoreCase(this.name))) {return false; }
        if (!this.definition.matches(actualArguments)) {return false; }
        return true;
    }

    /**
     * 関数名の取得。
     *
     * @return 関数名
     */
    public String getName() {
        return this.name;
    }

    /**
     * 対応する手続宣言をセットする。
     * @param proc
     */
    public void setDeclaration(Procedure proc) {
        this.definition = proc;
    }
    /**
     * 対応する手続宣言を返す。
     * @return 手続宣言。無ければnullを返す。
     */
    public Procedure getDeclaration() {
        return this.definition;
    }
}
