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

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import jp.riken.kscope.language.Block;
import jp.riken.kscope.language.BlockType;
import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.utils.StringUtils;

/**
 * 総称関数群に対応したクラス.<br>
 * Fortranのinterface文に対応。<br>
 *
 * @author RIKEN
 *
 */
public class Procedures extends Block {
    /** シリアル番号 */
    private static final long serialVersionUID = 1784597524248458672L;
    /** 総称手続の名前 */
    private String name;
    /** 関数名の総称定義 */
    // modify by @hira at 2013/02/01 挿入順を維持するLinkedHashSetに変更
    // private Set<IProcedureItem> procedures = new HashSet<IProcedureItem>();
    private Set<IProcedureItem> procedures = new LinkedHashSet<IProcedureItem>();


    /**
     * コンストラクタ。<br>
     */
    public Procedures() {
        super();
    }
    /**
     * コンストラクタ。<br>
     *
     * @param nm
     *         名前
     */
    public Procedures(String nm) {
        super();
        this.name = nm;
    }

    /**
     * コピーコンストラクタ.
     */
    public Procedures(Procedures dest) {
        super.clone(dest);
        this.name = dest.name;
        if (dest.procedures != null) {
            for (IProcedureItem item : dest.procedures) {
                ProcedureItem dest_proc = (ProcedureItem)item;
                ProcedureItem this_proc = new ProcedureItem(dest_proc);
                this.add(this_proc);
            }
        }
        return;
    }
    /**
     * ブロックタイプの取得。
     *
     * @return BlockType.PROCEDURES
     */
    public BlockType getBlockType() {
        return BlockType.PROCEDURES;
    }

    public String toString() {
        String nm = "";
        if (this.name != null) {
            nm = " " + this.name;
        }
        return "interface" + nm;
    }
    /**
     * 実引数リストを元に、候補対象となる実関数群から実関数を探しだす。<br>
     *
     * @param actualProcedures
     *            候補対象となる実関数群
     * @param actualArguments
     *            実引数リスト
     *
     * @return 見つかった実関数。見つからなかった場合はnullを返す。
     */
    public Procedure findActualProcedureFrom(Set<Procedure> actualProcedures,
                                             List<Expression> actualArguments) {
        if (actualProcedures == null || actualArguments == null) {
            return null;
        }

        Procedure result = null;

        for (IProcedureItem item : procedures) {
            for (Procedure target : actualProcedures) {
                if (item.matches(target, actualArguments)) {
                    result = target;
                    break;
                }
            }
            if (result != null) { break; }
        }

        return result;
    }

    /**
     * 名前の取得。<br>
     *
     * @return 名前
     */
    public String getName() {
        return this.name;
    }

    /**
     * 総称関数群の取得。<br>
     *
     * @return 総称関数群.ない場合は空のセットを返す。
     */
    public Set<IProcedureItem> getProcedures() {
        return this.procedures;
    }

    /**
     * 総称関数の追加。<br>
     *
     * @param procedure
     *            追加する総称関数
     */
    public void add(IProcedureItem procedure) {
        if (procedure == null) { return; }
        this.procedures.add(procedure);
    }
    /**
     * 与えられた実引数リストを基に、対応する手続き仕様の名前を返す。
     * @param arguments 実引数リスト
     * @return 手続き引用仕様の名前。無ければ総称名を返す。
     */
    public String getActualCallName(List<Expression> arguments) {
        if (arguments == null) {return this.name; }

        for (IProcedureItem item : this.procedures) {
                if (item.matches(arguments)) {
                    return item.getName();
                }
        }
        return this.name;
    }



    /**
     * インターフェイス文を出力する.
     * @return        インターフェイス文
     */
    public String toStructure(int indent) {
        StringBuilder buf = new StringBuilder();
        String interface_name = "interface";
        // 総称手続の名前
        if (this.name != null && !this.name.isEmpty()) {
            interface_name += " " + this.name;
        }

        buf.append(interface_name);
        buf.append("\n");
        if (this.procedures != null) {
            for (IProcedureItem item : this.procedures) {
                if (item == null) continue;
                ProcedureItem proc = (ProcedureItem)item;
                String proc_buf = proc.toStructure(indent);
                proc_buf = StringUtils.correctIndent(proc_buf, indent);
                buf.append(proc_buf);
            }
        }
        buf.append("end " + interface_name);
        buf.append("\n");

        return buf.toString();
    }
}
