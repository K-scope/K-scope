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
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import jp.riken.kscope.information.InformationBlocks;

/**
 * 異なるプログラム単位で同じ記憶領域を共有する変数を表現するクラス。FortranにおけるCommon文を表す。
 *
 * @author RIKEN
 *
 */
public class Common extends jp.riken.kscope.language.Block {
    /** シリアル番号 */
    private static final long serialVersionUID = -1820258032815507624L;
    /** 共通ブロックの名前 */
    private String name = "NO_NAME";
    /** スカラの変数、配列、記録、構造体の並び */
    private List<Variable> variables = new ArrayList<Variable>();


    /**
     * コンストラクタ。
     */
    public Common() {
        super();
    }

    /**
     * コピーコンストラクタ。
     * @param    dest         コピー元Commonクラス
     */
    public Common(Common dest) {
        super.clone(dest);
        if (dest == null) return;
        this.name = dest.name;
        if (dest.variables != null && dest.variables.size() > 0) {
            this.variables = new ArrayList<Variable>();
            for (Variable var : dest.variables) {
                this.variables.add(new Variable(var));
            }
        }

        return;
    }


    @Override
    public String toString() {
        StringBuilder st = new StringBuilder("common /" + this.name + "/");
        for (Variable var: this.variables) {
            st.append(" " + var.getName() + ",");
        }
        st.deleteCharAt(st.length() - 1);
        return st.toString();
    }
    /**
     * ブロックタイプの取得。
     *
     * @return BlockType.COMMON
     */
    public BlockType getBlockType() {
        return BlockType.COMMON;
    }

    /**
     * COMMON名をセットする。
     *
     * @param nm
     *            COMMON名
     */
    public void setName(String nm) {
        this.name = nm;
    }

    /**
     * COMMON名を返す。
     *
     * @return COMMON名
     */
    public String getName() {
        return this.name;
    }

    /**
     * 変数のリストをセットする
     *
     * @param vars
     *            変数のリスト
     */
    public void setVariables(List<Variable> vars) {
        this.variables = vars;
        if (this.variables != null) {
            for (Variable var : vars) {
                var.setParentStatement(this);
            }
        }
    }

    /**
     * 変数のリストを取得する。
     *
     * @return variables 変数のリスト.無ければ空のリストを返す。
     */
    public List<Variable> getVariables() {
        return variables;
    }

    /**
     * COMMONリストに指定した変数名が含まれていれば真を返す。
     * @param nm 変数名
     * @return 含まれていれば真
     */
    public boolean contains(String nm) {
        if (nm == null) return false;
        for (Variable var:this.variables) {
            if (var.getName() == null) continue;
            if (var.getName().equalsIgnoreCase(nm)) {
                return true;
            }
        }
        return false;
    }

    /**
     * 付加情報ブロックコレクションを生成する。
     *
     * @return 付加情報ブロックコレクション
     */
    @Override
    public InformationBlocks createInformationBlocks() {
        InformationBlocks result = new InformationBlocks();
        result.addAll(super.createInformationBlocks());
        if (this.variables != null) {
            for (Variable variable : this.variables) {
                result.addAll(variable.createInformationBlocks());
            }
        }
        return result;
    }

    /**
     * idにマッチした情報ブロックを検索する。
     * @param id
     *          ID
     * @return 見つかった情報ブロック。見つからなかった場合はnullが返ります。
     */
    @Override
    public IInformation findInformationBlockBy(String id) {
        IInformation result = super.findInformationBlockBy(id);

        if (result == null && this.getID().equals(id)) {
            result = this;
        }

        if (result == null && this.variables != null) {
            for (Variable variable : this.variables) {
                result = variable.findInformationBlockBy(id);
                if (result != null) { break; }
            }
        }

        return result;
    }

    /**
     * 同一ブロックであるかチェックする.
     * @param block        ブロック
     * @return        true=一致
     */
    @Override
    public boolean equalsBlocks(Block block) {
        if (block == null) return false;
        if (!(block instanceof Common)) return false;
        if (!super.equalsBlocks(block)) return false;

        if (this.variables == ((Common)block).getVariables()) {
            return true;
        }
        else if (this.variables == null) {
            return false;
        }
        else if (this.variables.size() == ((Common)block).getVariables().size()) {
            for (int i=0; i<this.variables.size(); i++) {
                Variable thisVar = this.variables.get(i);
                Variable destVar = ((Common)block).getVariables().get(i);
                if (thisVar == destVar) {
                    continue;
                }
                else if (thisVar == null) {
                    return false;
                }
                else if (!thisVar.equalsVariable(destVar)) {
                    return false;
                }
            }
            return true;
        }

        return false;

    }

    /**
     * 同一ブロックを検索する
     * @param block            IInformationブロック
     * @return        同一ブロック
     */
    @Override
    public IInformation[] searchInformationBlocks(IInformation block) {
        List<IInformation> list = new ArrayList<IInformation>();
        {
            IInformation[] infos = super.searchInformationBlocks(block);
            if (infos != null) {
                list.addAll(Arrays.asList(infos));
            }
        }
        if (this.variables != null) {
            for (Variable variable : this.variables) {
                IInformation[] infos = variable.searchInformationBlocks(block);
                if (infos != null) {
                    list.addAll(Arrays.asList(infos));
                }
            }
        }
        if (list.size() <= 0) {
            return null;
        }

        return list.toArray(new IInformation[0]);
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
        if (this.variables != null && this.variables.size() > 0) {
            list.addAll(this.variables);
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
        if (this.variables == null) return null;

        List<ProcedureUsage> list = new ArrayList<ProcedureUsage>();
        for (Variable var : this.variables) {
            Set<ProcedureUsage> calls = var.getAllFunctions();
            if (calls == null || calls.size() <= 0) continue;
            list.addAll(calls);
        }
        if (list.size() <= 0) return null;
        return list;
    }
}
