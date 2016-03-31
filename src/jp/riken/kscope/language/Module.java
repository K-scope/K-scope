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
import java.util.List;

/**
 * モジュールプログラム単位を表現するクラス。
 *
 * @author RIKEN
 *
 */
public class Module extends ProgramUnit {
    /** シリアル番号 */
    private static final long serialVersionUID = -9149612813518825146L;

    /**
     * コンストラクタ。
     *
     * @param m_name
     *            モジュール名
     */
    public Module(String m_name) {
        super("module", m_name);
    }

    /**
     * ブロックタイプの取得。
     *
     * @return BlockType.MODULE
     */
    @Override
    public BlockType getBlockType() {
        return BlockType.MODULE;
    }

    @Override
    protected String toStringBase() {
        return ("Module " + this.get_name());
    }

    /**
     * モジュールに属するプログラム単位の配列を返す。
     *
     * @return プログラム単位の配列。無ければ空の配列を返す。
     */
    public Procedure[] get_procedures() {
        Procedure[] subs = new Procedure[super.get_num_of_child()];
        for (int i = 0; i < subs.length; i++) {
            subs[i] = get_children()[i];
        }
        return subs;
    }

    protected Procedure get_procedure(String sub_name) {
        return ((Procedure) super.get_child(sub_name));
    }

    /**
     * IDを取得する。
     *
     * @return ID
     */
    @Override
    public String getID() {
        return this.toStringBase();
    }

    /**
     * プロシージャ（関数）からブロックまでの階層文字列表記を取得する
     * 階層文字列表記 : [main()]-[if (...)]-[if (...)]
     * CompoundBlock（空文）は省略する.
     * @return      階層文字列表記
     */
    @Override
    public String toStringProcedureScope() {
        // モジュールは表示しない
        return null;
    }

    /**
     * モジュールからブロックまでの階層文字列表記を取得する
     * 階層文字列表記 : [main()]-[if (...)]-[if (...)]
     * CompoundBlock（空文）は省略する.
     * @return      階層文字列表記
     */
    @Override
    public String toStringModuleScope() {
        // モジュール表記を返す
        String statement = this.toString();
        statement = "[" + statement + "]";
        return statement;
    }

    /**
     * 終了:END文を持つブロックは、終了:END文を返す
     * @return        終了:END文
     */
    @Override
    public String toEndString() {
        if (this.get_name() == null) return null;
        if (this.get_name().equals("NO_MODULE")) return null;
        String buf = "end module " + this.get_name();
        return buf;
    }

    /**
     * 手続呼出しのリストを返す。
     * @return 手続呼出しのリスト
     */
    @Override
    public List<ProcedureUsage> getCalls() {
        List<ProcedureUsage> list = new ArrayList<ProcedureUsage>();
        Procedure[] procs = get_procedures();
        if (procs == null) return null;
        for (Procedure proc : procs) {
            List<ProcedureUsage> calls = proc.getCalls();
            if (calls != null && calls.size() > 0) {
                list.addAll(calls);
            }
        }

        if (list.size() <= 0) return null;

        return list;
    }

    /**
     * NO_MODULEであるかチェックする.
     * @return        true = NO_MODULE
     */
    public boolean isNoModule() {
        String name = this.get_name();
        if (Program.NO_MODULE.equalsIgnoreCase(name)) {
            return true;
        }
        return false;
    }
}
