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

import java.io.Serializable;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jp.riken.kscope.language.utils.LanguageUtils;

/**
 * 複文（空文）クラス。
 * 複文（空文）のみブロック内の変数宣言を定義できる。
 *
 * @author RIKEN
 * @version    2015/03/15     C言語用複文（空文）新規作成
 *
 */
public class CompoundBlock extends jp.riken.kscope.language.Block
                        implements jp.riken.kscope.language.IDeclarations {

    /** シリアル番号 */
    private static final long serialVersionUID = 8400596882091353264L;

    /**
     * 変数宣言文リスト.<br/>
     * Map\<変数名, 変数定義\> <br/>
     * C言語用複文（空文）の変数宣言 at 2015/09/01 by @hira
     */
    public Map<String, VariableDefinition> variables = new LinkedHashMap<String, VariableDefinition>();
    /**
     * 本プログラム単位内で参照されている変数名とそのブロックのマップ。 分析機能のためのメンバー変数。
     */
    private Map<String, Set<IBlock>> refVariableNames = new HashMap<String, Set<IBlock>>();
    /**
     * 本プログラム単位内で定義されている変数名とそのブロックのマップ。 分析機能のためのメンバー変数。
     */
    private Map<String, Set<IBlock>> defVariableNames = new HashMap<String, Set<IBlock>>();
    /**
     * 本プログラム単位内で使用されている変数の名前と宣言(他のプログラム単位含む)のマップ。 分析機能のためのメンバー変数。
     */
    private transient HashMap<Variable, VariableDefinition> variableMap = new HashMap<Variable, VariableDefinition>();


    /**
     * コンストラクタ。
     *
     * @param parent
     *          親ブロック
     */
    public CompoundBlock(Block parent) {
        super(parent);
    }

    /**
     * コンストラクタ。
     */
    public CompoundBlock() {
        super();
    }

    /**
     * ブロックタイプの取得。
     *
     * @return BlockType.DO_NOTHING
     */
    public BlockType getBlockType() {
        return BlockType.COMPOUND;
    }

    @Override
    public String toString() {
        return this.toStringBase();
    }

    @Override
    protected String toStringBase() {
        return "{";
    }


    /**
     * 指定した名前の変数宣言を返す。
     * @param var_name 変数名
     * @return 変数宣言。無ければnullを返す。
     */
    @Override
    public VariableDefinition get_variable(String var_name) {
        return variables.get(var_name);
    }

    /**
     * 変数宣言文を設定する。
     * @param varDef            変数宣言文
     */
    @Override
    public void set_variable_def(VariableDefinition varDef) {
        this.put_variable(varDef);
    }

    /**
     * 変数宣言文を追加する.
     * @param vr        変数宣言文
     */
    protected void put_variable(VariableDefinition vr) {
        vr.setMother(this);
        this.variables.put(vr.get_name(), vr);
    }

    /**
     * 変数宣言文を取得する。
     *
     * @return 変数宣言文リスト
     */
    @Override
    public VariableDefinition[] get_variables() {
        if (this.variables == null) return null;

        Set<String> keys = this.variables.keySet();

        Collection<VariableDefinition> values = this.variables.values();
        return values.toArray(new VariableDefinition[0]);
    }

    /**
     * 自身に係わる変数宣言のマップを返す。
     * @return 変数宣言のマップ。
     */
    @Override
    public Map<String,VariableDefinition> getVariableDefinitionMap() {
        return this.variables;
    }

    /**
     * プログラム単位内で参照されている変数名とブロックリストのマップを返す。
     *
     * @return 変数名とブロックリストのマップ
     */
    private Map<String, Set<IBlock>> getRefVariableNames() {
        return this.refVariableNames;
    }

    /**
     * プログラム単位内で参照されている変数のブロックリストを返す。
     * @param   var_name    変数名
     * @return 変数参照ブロックリスト
     */
    public Set<IBlock> getRefVariableName(String var_name) {
        if (var_name == null) return null;
        if (this.refVariableNames == null) return null;
        String name = this.transferDeclarationName(var_name);
        return this.refVariableNames.get(name);
    }

    /**
     * プログラム単位内で参照されている変数名と現れるブロックを追加する。
     *
     * @param refVarName
     *            変数名
     * @param blk
     *            ブロック
     */
    @Override
    public void putRefVariableName(String refVarName, IBlock blk) {
        // 2015/09/01 C言語大文字・小文字区別対応
        String name = this.transferDeclarationName(refVarName);

        // 構造体の変数分割を行う  (e.g.) student.no  = {student, student.no}
        String[] var_names = LanguageUtils.splitVariableMembers(name);
        if (var_names == null || var_names.length <= 0) return;

        for (String nm : var_names) {
            if (this.refVariableNames.containsKey(nm)) {
                Set<IBlock> block = this.refVariableNames.get(nm);
                block.add(blk);
            } else {
                Set<IBlock> block = new LinkedHashSet<IBlock>();
                block.add(blk);
                this.refVariableNames.put(nm, block);
            }
        }
    }

    /**
     * プログラム単位内で定義されている変数名の集合を取得する。
     *
     * @return 変数名の集合
     */
    private Map<String, Set<IBlock>> getDefVariableNames() {
        return this.defVariableNames;
    }

    /**
     * プログラム単位内で定義されている変数のブロックリストを取得する。
     * @param   var_name    変数名
     * @return 変数定義ブロックリスト
     */
    @Override
    public Set<IBlock> getDefVariableName(String var_name) {
        if (var_name == null) return null;
        if (this.defVariableNames == null) return null;
        String name = this.transferDeclarationName(var_name);
        return this.defVariableNames.get(name);
    }

    /**
     * プログラム単位内で定義されている変数名と現れるブロックを追加する。
     *
     * @param defVarName
     *            変数名
     * @param blk
     *            ブロック
     */
    @Override
    public void putDefVariableName(String defVarName, IBlock blk) {
        String name = this.transferDeclarationName(defVarName);

        // 構造体の変数分割を行う  (e.g.) student.no  = {student, student.no}
        String[] var_names = LanguageUtils.splitVariableMembers(name);
        if (var_names == null || var_names.length <= 0) return;

        for (String nm : var_names) {
            if (this.defVariableNames.containsKey(nm)) {
                Set<IBlock> block = this.defVariableNames.get(nm);
                block.add(blk);
            } else {
                Set<IBlock> block = new LinkedHashSet<IBlock>();
                block.add(blk);
                this.defVariableNames.put(nm, block);
            }
        }
    }

    /**
     * プログラム単位で使用される変数マップを取得する。
     *
     * @return 変数のマップ
     */
    @Override
    public HashMap<Variable, VariableDefinition> getVariableMap() {
        if (this.variableMap == null) {
            this.createVariableMap();
        }
        return this.variableMap;
    }

    /**
     * プログラム単位において指定された名前で使用される変数が存在すればその宣言を返す。
     *
     * @param var           変数
     * @return 変数宣言。無ければnullを返す。
     */
    @Override
    public VariableDefinition getVariableMap(Variable var) {
        if (var == null) return null;
        if (this.variableMap == null) {
            this.createVariableMap();
        }

        // 変数宣言を検索する
        VariableDefinition def = this.variableMap.get(var);
        return def;
    }

    /**
     * プログラム単位において指定された名前で使用される変数が存在すればその宣言を返す。
     *
     * @param nm
     *            変数名
     * @return 変数宣言。無ければnullを返す。
     */
    @Override
    public Variable[] searchVariableFromMap(String nm) {
        if (nm == null || nm.isEmpty()) return null;
        if (this.variableMap == null) {
            this.createVariableMap();
        }

        // 2015/09/01 C言語大文字・小文字区別対応
        nm = this.transferDeclarationName(nm);

        // 変数宣言を検索する
        Variable[] vars = LanguageUtils.searchVariableFromMap(nm, this.variableMap);
        return vars;
    }

    /**
     * プログラム単位において指定された名前で使用される変数が存在すればその宣言を返す。
     *
     * @param nm
     *            変数名
     * @return 変数宣言。無ければnullを返す。
     */
    @Override
    public VariableDefinition[] searchVariableDefinitionFromMap(String nm) {
        if (nm == null || nm.isEmpty()) return null;
        if (this.variableMap == null) {
            this.createVariableMap();
        }

        // 2015/09/01 C言語大文字・小文字区別対応
        nm = this.transferDeclarationName(nm);

        // 変数宣言を検索する
        VariableDefinition[] defs = LanguageUtils.searchVariableDefinitionFromMap(nm, this.variableMap);
        return defs;
    }

    /**
     * 変数宣言リストを作成する.
     */
    @Override
    public void createVariableMap() {
        this.variableMap = new HashMap<Variable, VariableDefinition>();
    }

    /**
     * プログラム単位で使用される変数を追加する。
     * @param var           変数
     */
    @Override
    public void putVariableMap(Variable var) {
        this.putVariableMap(var, null);
    }

    /**
     * プログラム単位で使用される変数名と宣言のマップを追加する。
     *
     * @param var          変数
     * @param varDef       変数宣言
     */
    @Override
    public void putVariableMap(Variable var, VariableDefinition varDef) {
        if (var == null) return;
        if (this.variableMap == null) {
            this.createVariableMap();
        }

        this.variableMap.put(var, varDef);

        if (varDef != null) {
            var.setDefinition(varDef);
        }
    }


    /**
     * プログラム単位内で、ある変数が参照・定義されているブロックのセットを返す。
     *
     * @param name
     *            変数名
     *
     * @return ブロックのセット。無ければ空のセットを返す。
     */
    @Override
    public Set<IBlock> getRefDefBlocks(String name) {
        // 2015/09/01 C言語大文字・小文字区別対応
        String nm = this.transferDeclarationName(name);

        Set<IBlock> blocks = new LinkedHashSet<IBlock>();
        Set<IBlock> refblocks = this.getRefVariableNames().get(nm);
        Set<IBlock> defblocks = this.getDefVariableNames().get(nm);
        if (refblocks == null) {
            if (defblocks == null) {
                return new LinkedHashSet<IBlock>();
            }
            return defblocks;
        } else {
            if (defblocks == null) {
                return refblocks;
            }
            // ref,defともに要素がある場合
            IBlock[] refArray = refblocks.toArray(new IBlock[0]);
            IBlock[] defArray = defblocks.toArray(new IBlock[0]);
            boolean refFlag = true;
            boolean defFlag = true;
            int refIndex = 0;
            int defIndex = 0;
            IBlock currentRef = refArray[0];
            IBlock currentDef = defArray[0];
            int refLine = currentRef.getStartCodeLine().getStartLine();
            int defLine = currentDef.getStartCodeLine().getStartLine();
            while (refFlag || defFlag) {
                if (refLine < defLine) {
                    blocks.add(currentRef);
                    if (refIndex == refArray.length - 1) {
                        refFlag = false;
                        refLine = defArray[defArray.length - 1].getStartCodeLine().getStartLine();
                    } else {
                        refIndex++;
                        currentRef = refArray[refIndex];
                        refLine = currentRef.getStartCodeLine().getStartLine();
                    }
                } else {
                    blocks.add(currentDef);
                    if (defIndex == defArray.length - 1) {
                        defFlag = false;
                        defLine = 1 + refArray[refArray.length - 1].getStartCodeLine().getStartLine();
                    } else {
                        defIndex++;
                        currentDef = defArray[defIndex];
                        defLine = currentDef.getStartCodeLine().getStartLine();
                    }
                }
            }
        }
        return blocks;
    }


    /**
     * 親ブロックからIDeclarationsブロックを取得する.
     * @return    IDeclarationsブロック
     */
    @Override
    public IDeclarations getScopeDeclarationsBlock() {
        return this;
    }


    /**
     * 指定された変数名と、指定されたブロックを参照としてセットする。
     * @param blk ブロック
     * @param var    変数
     */
    @Override
    public void addVariableToRef(IBlock blk, Variable var) {
        if (blk == null) return;
        if (var == null) return;

        Expression exp = new Expression();
        exp.addVariable(var);
        this.addExpressionToRef(blk, exp);

        return;
    }

    /**
     * 手続呼出を参照としてセットする。
     * @param blk ブロック
     * @param func    手続呼出
     */
    @Override
    public void addFunctionToRef(IBlock blk, ProcedureUsage func) {
        if (blk == null) return;
        if (func == null) return;
        if (blk instanceof Block) {
            func.set_mother((Block) blk);
        }
        func.set_block_start(blk.getStartCodeLine());
        func.set_block_end(blk.getEndCodeLine());
        List<Expression> args = func.getArguments();
        for (Expression arg: args) {
            Set<Variable> funcVars = arg.getAllVariables();
            for (Variable var:funcVars) {
                this.putRefVariableName(var.getName(), func);
            }
        }
        return;
    }

    /**
     * 指定された式に含まれる全ての変数名と、指定されたブロックを参照としてセットする。
     * 指定された式に含まれる全ての変数名と、式に含まれる手続呼出を参照としてセットする。
     * @param blk ブロック
     * @param exp 式
     */
    @Override
    public void addExpressionToRef(IBlock blk, Expression exp) {
        Set<Variable> vars = exp.getAllVariables();
        for (Variable var:vars) {
            this.putRefVariableName(var.getName(), blk);
            this.putVariableMap(var);
        }
        Set<ProcedureUsage> rightFunc = exp.getAllFunctions();
        for (ProcedureUsage pu: rightFunc) {
            if (blk instanceof Block) {
                pu.set_mother((Block) blk);
            }
            pu.set_block_start(blk.getStartCodeLine());
            pu.set_block_end(blk.getEndCodeLine());
            List<Expression> args = pu.getArguments();
            for (Expression arg: args) {
                Set<Variable> funcVars = arg.getAllVariables();
                for (Variable var:funcVars) {
                    this.putRefVariableName(var.getName(), pu);
                }
            }
        }
    }

    /**
     * 変数に変数定義をセットする
     */
   @Override
   public void setVariableDefinitions() {
       Set<Variable> vars = getAllVariables();
       if (vars == null || vars.size() <= 0) return;
       for (Variable var : vars) {
           String name = var.getName();
           VariableDefinition def = this.getVariableMap(var);
           if (def != null) {
               var.setDefinition(def);
           }
       }
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
        // CompoundBlock（空文）は表記しない
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
     * ブロックのみの変数リストを取得する。
     * @return        式の変数リスト
     */
    @Override
    public Set<Variable> getBlockVariables() {
        return null;
    }


    /**
     * プログラム単位において指定された名前で使用される変数が存在すればその宣言を返す。
     *
     * @param nm
     *            変数名
     * @return 変数宣言。無ければnullを返す。
     */
    @Override
    public VariableDefinition[] searchVariablesDefinition(String nm) {
        if (nm == null || nm.isEmpty()) return null;
        if (this.variables == null) {
            this.createVariableMap();
        }

        // 2015/09/01 C言語大文字・小文字区別対応
        nm = this.transferDeclarationName(nm);

        // 変数宣言を検索する
        VariableDefinition[] defs = LanguageUtils.searchVariablesDefinition(nm, this.variables);
        return defs;
    }
}
