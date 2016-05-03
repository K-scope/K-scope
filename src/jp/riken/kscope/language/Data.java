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
 * 変数に初期値をセットするクラス。FortranにおけるData文を表す。
 *
 * @author RIKEN
 *
 */
public class Data extends jp.riken.kscope.language.Block {
    /** シリアル番号 */
    private static final long serialVersionUID = -3729371399836405032L;
    private List<Variable> variables = new ArrayList<Variable>();
    private List<Expression> values = new ArrayList<Expression>();

    /**
     * コンストラクタ。
     */
    public Data() {
        super();
    }

    /**
     * コンストラクタ。
     *
     * @param parent    親ブロック
     */
    public Data(Block parent) {
        super(parent);
    }

    /**
     * コピーコンストラクタ。
     *
     * @param dest    コピーデータ
     */
    public Data(Data dest) {
        super.clone(dest);
        if (dest.variables != null) {
            for (Variable var : dest.variables) {
                this.variables.add(new Variable(var));
            }
        }
        if (dest.values != null) {
            for (Expression exp : dest.values) {
                this.values.add(new Expression(exp));
            }
        }
    }

    /**
     * ブロックタイプの取得。
     *
     * @return BlockType.DATA
     */
    public BlockType getBlockType() {
        return BlockType.DATA;
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
            for (Variable var : this.variables) {
                var.setParentStatement(this);
            }
        }
    }

    /**
     * 変数のリストを取得する。
     *
     * @return variables 変数のリスト
     */
    public List<Variable> getVariables() {
        return variables;
    }


    /**
     * 値のリストをセットする
     *
     * @param vals
     *            値のリスト
     */
    public void setValues(List<Expression> vals) {
        this.values = vals;
        if (this.values != null) {
            for (Expression exp : this.values) {
                exp.setParentStatement(this);
                Set<Variable> vars = exp.getAllVariables();
                if (vars == null || vars.size() <= 0) continue;
                for (Variable var : vars) {
                    var.setParentStatement(this);
                }
            }
        }
    }

    /**
     * 値のリストを取得する。
     *
     * @return values 値のリスト
     */
    public List<Expression> getValues() {
        return values;
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
        if (this.values != null) {
            for (Expression value : this.values) {
                result.addAll(value.createInformationBlocks());
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
        if (result == null && this.values != null) {
            for (Expression value : this.values) {
                result = value.findInformationBlockBy(id);
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
         if (!(block instanceof Data)) return false;
         if (!super.equalsBlocks(block)) return false;

        if (this.variables != null && ((Data)block).variables != null) {
            if (this.variables.size() == ((Data)block).variables.size()) {
                for (int i=0; i<this.variables.size(); i++) {
                    Variable thisVar = this.variables.get(i);
                    Variable destVar = ((Data)block).variables.get(i);
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
            }
        }
        else if (this.variables != null || ((Data)block).variables != null) {
            return false;
        }

        if (this.values != null && ((Data)block).values != null) {
            if (this.values.size() == ((Data)block).values.size()) {
                for (int i=0; i<this.values.size(); i++) {
                    Expression thisExp = this.values.get(i);
                    Expression destExp = ((Data)block).values.get(i);
                    if (thisExp == destExp) {
                        continue;
                    }
                    else if (thisExp == null) {
                        return false;
                    }
                    else if (!thisExp.equalsExpression(destExp)) {
                        return false;
                    }
                }
            }
        }
        else if (this.values != null || ((Data)block).values != null) {
            return false;
        }

        return true;
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
        if (this.values != null) {
            for (Expression value : this.values) {
                IInformation[] infos = value.searchInformationBlocks(block);
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
            for (Variable var: this.variables) {
                Set<Variable> vars = var.getAllVariables();
                if (vars != null && vars.size() > 0) {
                    list.addAll(vars);
                }
            }
        }
        if (this.values != null) {
            for (Expression exp : this.values) {
                Set<Variable> vars = exp.getAllVariables();
                if (vars != null && vars.size() > 0) {
                    list.addAll(vars);
                }
            }
        }

        if (list.size() <= 0) return null;
        return list;
    }

    /**
     * Dataクラスのコピーの作成を行う
     * @param    dest    コピー元Data
     */
    public Data clone()  {

        Data dest = new Data();
        dest.clone(this);

        if (this.variables != null && this.variables.size() > 0) {
            dest.variables = new ArrayList<Variable>();
            for (Variable var : this.variables) {
                dest.variables.add(new Variable(var));
            }
        }
        if (this.values != null && this.values.size() > 0) {
            dest.values = new ArrayList<Expression>();
            for (Expression exp : this.values) {
                dest.values.add(new Expression(exp));
            }
        }
        return dest;
    }

    /**
     * 手続呼出しのリストを返す。
     * @return 手続呼出しのリスト
     */
    @Override
    public List<ProcedureUsage> getCalls() {
        List<ProcedureUsage> list = new ArrayList<ProcedureUsage>();
        if (this.variables != null) {
            for (Variable var : this.variables) {
                Set<ProcedureUsage> calls = var.getAllFunctions();
                if (calls == null || calls.size() <= 0) continue;
                list.addAll(calls);
            }
        }
        if (this.values != null) {
            for (Expression exp : this.values) {
                Set<ProcedureUsage> calls = exp.getAllFunctions();
                if (calls == null || calls.size() <= 0) continue;
                list.addAll(calls);
            }
        }
        if (list.size() <= 0) return null;
        return list;
    }


    /**
     * DATA文の文字列表現
     * @return DATA文
     */
    @Override
    public String toString() {

        /***
        StringBuilder buf = new StringBuilder();
        buf.append("data ");

        // var_list
        List<String> vars = new ArrayList<String>();
        for (Variable var : this.variables) {
            if (!vars.contains(var.getName())) {
                vars.add(var.getName());
            }
        }

        StringBuilder var_buf = new StringBuilder();
        for (String name : vars) {
            if (var_buf.length() > 0) var_buf.append(",");
            var_buf.append(name);
        }
        buf.append(var_buf);

        // value_list
        buf.append(" / ");
        StringBuilder value_buf = new StringBuilder();
        for (Expression value : this.values) {
            if (value_buf.length() > 0) value_buf.append(",");
            value_buf.append(value.toString());
        }
        buf.append(value_buf);

        buf.append(" /");
        return buf.toString();
        */
        if (this.getStartCodeLine() == null) {
            throw new NullPointerException("Data::Codeline is null");
        }
        return this.getStartCodeLine().getStatement();
    }
}
