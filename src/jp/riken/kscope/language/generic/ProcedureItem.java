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
import jp.riken.kscope.language.IVariableType;
import jp.riken.kscope.language.fortran.VariableType;
import jp.riken.kscope.utils.StringUtils;

/**
 * 総称関数に対応したクラス。
 *
 * @author RIKEN
 *
 */
public class ProcedureItem
 implements Serializable,
        jp.riken.kscope.language.generic.IProcedureItem {
    /** シリアル番号 */
    private static final long serialVersionUID = -3790802001452994733L;
    private String name;
    private IVariableType type;
    private Arguments arguments;
    private String result;

    /**
     * コンストラクタ。
     *
     * @param nm
     *         関数名
     * @param typ
     *         関数の型
     */
    public ProcedureItem(String nm, IVariableType typ) {
        this(nm, typ, new Arguments());
    }

    /**
     * コンストラクタ。
     *
     * @param nm
     *         関数名
     * @param typ
     *         関数の型
     * @param argmnts
     *         仮引数リスト
     */
    public ProcedureItem(String nm, IVariableType typ,
            Arguments argmnts) {
        this.name = nm;
        this.type = typ;
        if (argmnts == null) {
            this.arguments = new Arguments();
        } else {
            this.arguments = argmnts;
        }
    }

    /**
     * コピーコンストラクタ。
     * @param  dest    コピー元
     */
    public ProcedureItem(ProcedureItem dest) {
        this.name = dest.name;
        if (dest.type != null) {
            this.type = new VariableType((VariableType)type);
        }
        if (dest.arguments != null) {
            this.arguments = new Arguments(dest.arguments);
        }
        this.result = dest.result;

    }


    @Override
    public String toString() {
        return "interface : " + this.name;
    }
    /**
     * 候補対象となる関数が自分の情報と適合しているかどうかを<br>
     * 調べるメソッド。候補対象関数の名前と、実引数リストが<br>
     * 適合していれば、trueを返す。
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
    public boolean matches(jp.riken.kscope.language.Procedure target,
                           List<Expression> actualArguments) {
        if (target == null || actualArguments == null) { return false; }
        if (target.get_name() != this.name) { return false; }
        if (!this.arguments.isSameArguments(actualArguments)) { return false; }
        return true;
    }

    /**
     * 対象となる実引数リストが自分の情報と適合しているかどうかを<br>
     * 調べるメソッド。実引数リストが適合していれば、trueを返す。
     * @param actualArguments
     *            実引数リスト
     *
     * @return true:  適合している
     *         false: 適合していない
     */
    @Override
    public boolean matches(List<Expression> actualArguments) {
        if (this.arguments == null || actualArguments == null) { return false; }
        if (!this.arguments.isSameArguments(actualArguments)) { return false; }
        return true;
    }

    /**
     * 仮引数を追加する。
     *
     * @param value
     *          追加する仮引数
     */
    public void addArgument(Argument value) {
        this.arguments.add(value);
    }

    /**
     * 関数名の取得。
     *
     * @return 関数名
     */
    @Override
    public String getName() {
        return this.name;
    }

    /**
     * 関数の型の取得。
     *
     * @return 関数の型
     */
    public IVariableType getType() {
        return this.type;
    }

    /**
     * 仮引数リストの取得。
     *
     * @return 仮引数リスト
     */
    public Arguments getArguments() {
        return this.arguments;
    }

    /**
     * 仮引数リストの設定。
     *
     * @param values
     *           仮引数リスト
     */
    public void setArguments(Arguments values) {
        this.arguments = values;
    }

    /**
     * 変数のデータ型をセットする。
     * @param tp
     */
    public void setVariableType(IVariableType tp) {
        this.type = tp;
    }

    /**
     * データ型を返す。
     *
     * @return データ型
     */
    public IVariableType getVariableType() {
        return this.type;
    }
    /**
     * 関数の場合の結果となる変数名を返す。
     * @return 変数名
     */
    public String getResult() {
        return result;
}

    /**
     * 関数の場合の結果となる変数名をセットする。
     * @param res 結果の変数名
     */
    public void setResult(String res) {
        this.result = res;
    }


    /**
     * インターフェイス文を出力する.
     * @return        インターフェイス文
     */
    public String toStructure(int indent) {
        boolean is_function = false;
        if (this.type != null) {
            VariableType var_type = (VariableType)this.type;
            VariableType.PrimitiveDataType premit = var_type.getPrimitiveDataType();
            if (!(premit == VariableType.PrimitiveDataType.UNKOWN
                || premit == VariableType.PrimitiveDataType.VOID)) {
                is_function = true;
            }
        }
        else if (this.result != null && !this.result.isEmpty()) {
            is_function = true;
        }
        StringBuilder buf = new StringBuilder();
        if (is_function) {
            if (this.type != null) {
                buf.append(this.type.toString());
                buf.append(" ");
            }
            buf.append("function ");
        }
        else {
            buf.append("subroutine ");
        }
        buf.append(this.name);

        StringBuilder arg_buf = new StringBuilder();
        StringBuilder def_buf = new StringBuilder();
        if (this.arguments != null) {
            for (Argument arg : this.arguments) {
                if (arg_buf.length() > 0) arg_buf.append(",");
                if (def_buf.length() > 0) def_buf.append("\n");
                arg_buf.append(arg.getName());
                def_buf.append(arg.toDefinition());
            }
        }
        buf.append("(");
        buf.append(arg_buf);
        buf.append(")");
        if (this.result != null) {
            buf.append(" result(");
            buf.append(this.result);
            buf.append(")");
        }
        buf.append("\n");
        // 引数宣言文
        String defs = StringUtils.correctIndent(def_buf.toString(), indent);
        buf.append(defs);

        if (is_function) {
            buf.append("end function ");
        }
        else {
            buf.append("end subroutine ");
        }
        buf.append(this.name);
        buf.append("\n");

        return buf.toString();
    }
}
