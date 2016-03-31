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
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.information.InformationBlock;
import jp.riken.kscope.information.InformationBlocks;
import jp.riken.kscope.information.TextInfo;
import jp.riken.kscope.language.fortran.Type;
import jp.riken.kscope.language.generic.Procedures;

/**
 * プログラムを表現する抽象クラス
 */
public abstract class Program implements Serializable {
    /** シリアル番号 */
    private static final long serialVersionUID = 6765615822590486646L;
    /**
     * モジュールに含まれないProcedureを格納するためのModuleオブジェクトの名前。
     */
    public static final String NO_MODULE = "NO_MODULE";
    /** メインプログラム名 */
    private String mainName;
    /** モジュールリスト */
    private Map<String, Module> modules = new HashMap<String, Module>();
    /** Common宣言しているプログラムリスト */
    private Map<String, List<ProgramUnit>> commonMap;
    /** ブロック指定の付加情報 */
    private InformationBlocks informationBlocks = new InformationBlocks();
    /** データベース挿入カレントプロシージャ */
    private transient ProgramUnit currentUnit;

    /**
     * コンストラクタ。
     */
    public Program() {
        init_module(NO_MODULE);
    }

    /**
     * moduleの集合を取得する。
     * @return modules
     */
    public Map<String, Module> getModules() {
        return modules;
    }

    /**
     * モジュールを設定する。
     * @param   modules  モジュール
     */
    public void setModules(Map<String, Module> modules) {
        this.modules = modules;
    }

    /**
     * main名を返す。
     * @return main名
     */
    public String getMainName() {
        return mainName;
    }

    /**
     * mainブロックを開始する。
     * @param main_name main名
     */
    public void init_main(String main_name) {
        currentUnit = module(NO_MODULE);
        module(NO_MODULE).set_child("program", main_name);
        mainName = new String(main_name);
        currentUnit = currentUnit.get_child(main_name);
        currentUnit.set_mother(module(NO_MODULE));
    }

    /**
     * mainブロックを終了する。
     */
    public void end_main() {
        currentUnit = currentUnit.get_mother();
    }

    /**
     * モジュールブロックを開始する。
     * @param module_name モジュール名
     * @return    生成モジュール
     */
    public Module init_module(String module_name) {
        Module module = new Module(module_name);
        modules.put(module_name, module);
        currentUnit = module;
        return module;
    }

    /**
     * モジュールブロックを終了する。
     */
    public void end_module() {
        currentUnit = module(NO_MODULE);
    }

    /**
     * 指定した名前のモジュールを返す。
     * @param module_name モジュール名
     * @return モジュールクラス
     */
    public Module module(String module_name) {
        return modules.get(module_name);
    }

    /**
     * モジュール名の配列を返す。
     * @return モジュール名の配列
     */
    public String[] get_module_name() {
        return modules.keySet().toArray(new String[modules.size()]);
    }

    /**
     * プロシージャブロックを開始する。
     * @param type_name        プロシージャ種別
     * @param sub_name        プロシージャ名
     */
    protected void init_procedure(String type_name, String sub_name) {
        Procedure sub = new Procedure(type_name, sub_name);
        ProgramUnit mama = currentUnit;
        currentUnit.put_child(sub);
        currentUnit = currentUnit.get_child(sub_name);
        currentUnit.set_mother(mama);
    }

    /**
     * プロシージャブロックを開始する。
     * @param type_name        プロシージャ種別
     * @param sub_name        プロシージャ名
     * @param args            プロシージャ仮引数
     */
    protected void init_procedure(String type_name, String sub_name, String[] args) {
        Procedure sub = new Procedure(type_name, sub_name, args);
        ProgramUnit mama = currentUnit;
        currentUnit.put_child(sub);
        currentUnit = currentUnit.get_child(sub_name);
        currentUnit.set_mother(mama);
    }

    /**
     * プロシージャブロックを開始する。
     * @param type_name        プロシージャ種別
     * @param sub_name        プロシージャ名
     * @param args            プロシージャ仮引数
     */
    protected void init_procedure(String type_name, String sub_name, Variable[] args) {
        Procedure sub = new Procedure(type_name, sub_name, args);
        ProgramUnit mama = currentUnit;
        currentUnit.put_child(sub);
        currentUnit = currentUnit.get_child(sub_name);
        currentUnit.set_mother(mama);
    }

    /**
     * プロシージャブロックを終了する。
     */
    protected void end_procedure() {
        currentUnit = currentUnit.get_mother();
    }

    /**
     * 属性を追加する。
     * @param attribute_name 属性の文字列表現
     */
    public void put_attribute(String attribute_name) {
        currentUnit.put_attribute(attribute_name);
    }

    /**
     * USE文を追加する。
     * @param useline USE文
     */
    public void setUse(UseState useline) {
        currentUnit.addUse(useline);
    }
    /**
     * USE文を追加する。
     * @param useline USE文
     * @param lineInfo code行情報
     * @param label ラベル。無ければStatement.NO_LABELをセットする。
     */
    public void setUse(UseState useline, CodeLine lineInfo, String label) {
        currentUnit.addUse(useline);
        useline.set_block_start(lineInfo);
        useline.set_block_end(lineInfo);
        useline.get_start().set_label(label);
    }

    /**
     * 分岐処理の開始行を設定する。
     *
     * @param lineInfo
     *            コード行情報
     * @param label
     *            行ラベル。ない場合はStatement.NO_LABELをセットする。
     * @param type Selectionの型
     */
    public void startSelection(CodeLine lineInfo, String label,
            jp.riken.kscope.language.Selection.SelectionType type) {
        ((Procedure) currentUnit).start_selection(lineInfo, label, type);
    }

    /**
     * 分岐を表す処理ブロックを開始する。else if文やelse文、case文に対応する。
     *
     * @param cond
     *           条件式
     * @param lineInfo 行情報
     * @param label ラベル。無い場合はStatement.NO_LABELをセットする。
     */
    public void startCondition(Expression cond, CodeLine lineInfo, String label) {
        if (label == null) { label = Statement.NO_LABEL; }
        ((Procedure) currentUnit).start_condition(cond, lineInfo, label);
        Block blk = this.getCurrentBlock();

        ProgramUnit dec_block = blk.getScopeDeclarationsBlock();    // 変数の定義ブロックの検索
        if (cond != null) {
            if (blk.get_mother() instanceof Selection) {
                Selection selec = (Selection) blk.get_mother();
                if (selec.getConditions().size() == 1) {
                    dec_block.addExpressionToRef(selec, cond);
                } else {
                    dec_block.addExpressionToRef(blk, cond);
                }
            }
        }
    }

    /**
     * 分岐を表す処理ブロックを終了する。else if文やelse文、case文に対応する。
     *
     * @param lineInfo 行情報
     * @param label ラベル。無い場合はStatement.NO_LABELをセットする。
     */
    public void endCondition(CodeLine lineInfo, String label) {
        ((Procedure) currentUnit).end_condition(lineInfo, label);
    }

    /**
     * 分岐処理に条件式をセットする（Select case文限定）。Selection以外のブロックにはセットされない。
     * Select case文以外では利用されない。
     * @param exp 条件式
     */
    public void setSelectCaseCondition(Expression exp) {
        IBlock blk = this.getCurrentBlock();
        if (blk != null) {
            if (blk instanceof Selection) {
                Selection sel = (Selection) blk;
                ProgramUnit dec_block = blk.getScopeDeclarationsBlock();    // 変数の定義ブロックの検索
                if (sel.isSelect()) {
                    sel.setCaseCondition(exp);
                    if (dec_block != null) {
                        dec_block.addExpressionToRef(sel, exp);
                    }
                }
            }
        }
    }

    /**
     * 分岐の終了行を設定する。
     *
     * @param lineInfo
     *            コード行情報
     */
    public void end_selection(CodeLine lineInfo) {
        ((Procedure) currentUnit).end_selection(lineInfo, Statement.NO_LABEL);
    }

    /**
     * 分岐文の終了行を設定する。
     *
     * @param lineInfo
     *            コード行情報
     * @param label
     *            行ラベル
     */
    protected void end_selection(CodeLine lineInfo, String label) {
        ((Procedure) currentUnit).end_selection(lineInfo, label);
    }

    /**
     * ユーザー定義処理ブロックを開始する.
     * @param lineInfo        コード行情報
     */
    protected void start_user_defined(CodeLine lineInfo) {
        ((Procedure) currentUnit).start_user_defined(lineInfo);
    }

    /**
     * ユーザー定義処理ブロックを開始する.
     * @param lineInfo        コード行情報
     * @param label            コードラベル
     */
    protected void start_user_defined(CodeLine lineInfo, String label) {
        ((Procedure) currentUnit).start_user_defined(lineInfo, label);
    }

    /**
     * ユーザー定義処理ブロックを終了する.
     * @param lineInfo        コード行情報
     */
    protected void end_user_defined(CodeLine lineInfo) {
        ((Procedure) currentUnit).end_user_defined(lineInfo);
    }

    /**
     * ユーザー定義処理ブロックを終了する.
     * @param lineInfo        コード行情報
     * @param label            コードラベル
     */
    protected void end_user_defined(CodeLine lineInfo, String label) {
        ((Procedure) currentUnit).end_user_defined(lineInfo, label);
    }


    /**
     * 手続きの呼び出しをセットする。
     *
     * @param lineInfo
     *            コード行情報
     * @param label
     *            行ラベル
     * @param subroutineName
     *            CALLサブルーチン名
     * @param arguments
     *            引数リスト
     * @param intrinsic
     *            組込関数フラグ:true=組込関数
     */
    protected void setProcedureUsage(CodeLine lineInfo, String label,
            String subroutineName, List<Expression> arguments, boolean intrinsic) {
        this.start_procedure_usage(lineInfo, label, subroutineName, arguments);
        IBlock block = this.getCurrentBlock();
        if (block instanceof ProcedureUsage && intrinsic == true) {
            ((ProcedureUsage) block).setIntrinsic();
        }
        this.end_procedure_usage(lineInfo, label);

        // 分析機能のために参照をセット
        ProgramUnit dec_block = block.getScopeDeclarationsBlock();    // 変数の定義ブロックの検索
        if (arguments != null && dec_block != null) {
        for (Expression arg: arguments) {
            Set<Variable> vars = arg.getAllVariables();
            for (Variable var: vars) {
                    dec_block.putRefVariableName(var.getName(), block);
                    dec_block.putVariableMap(var);
            }
        }
        }
    }

    /**
     * 手続きの呼び出しの開始をセットする。
     *
     * @param lineInfo
     *            コード行情報
     * @param label
     *            行ラベル
     * @param subroutineName
     *            CALLサブルーチン名
     * @param arguments
     *            引数リスト
     */
    protected void start_procedure_usage(CodeLine lineInfo, String label,
            String subroutineName, List<Expression> arguments) {
        ((Procedure) currentUnit).start_procedure_usage(lineInfo, label,
                subroutineName, arguments);
    }

    /**
     * 手続きの呼び出しの終了を設定する。
     *
     * @param lineInfo
     *            コード行情報
     * @param label
     *            行ラベル
     */
    protected void end_procedure_usage(CodeLine lineInfo, String label) {
        ((Procedure) currentUnit).end_procedure_usage(lineInfo, label);
    }

    /**
     * DO文の開始行を設定する。（ラベル付き）
     *
     * @param lineInfo
     *            コード行情報
     * @param label
     *            行ラベル
     */
    protected void start_repetition(CodeLine lineInfo, String label) {
        ((Procedure) currentUnit).start_repetition(lineInfo, label);
    }

    /**
     * DO文の開始行を設定する。
     *
     * @param lineInfo
     *            コード行情報
     * @param label
     *            行ラベル.ない場合はStatement.NO_LABELをセットする。
     * @param iterator
     *            ループ制御変数.ない場合はnull
     * @param initIterator
     *            始値。ない場合はnull
     * @param endCondition
     *            終値。ない場合はnull
     * @param step
     *            刻み幅。ない場合はnull
     */
    protected void start_repetition(CodeLine lineInfo, String label,
            Variable iterator, Expression initIterator,
            Expression endCondition, Expression step) {

        Procedure aCurrentUnit = (Procedure) this.currentUnit;

        aCurrentUnit.start_repetition(lineInfo, label);
        Repetition aCurrentBlock = (Repetition) aCurrentUnit.getBody().getCurrentBlock();
        aCurrentBlock.setProperty(iterator, initIterator, endCondition, step);

        ProgramUnit dec_block = aCurrentBlock.getScopeDeclarationsBlock();    // 変数の定義ブロックの検索
        // 分析機能のために参照をセット
        if (iterator != null) {
            dec_block.putVariableMap(iterator);
            dec_block.putDefVariableName(iterator.getName(), aCurrentBlock);
        }

        if (initIterator != null) {
            dec_block.addExpressionToRef(aCurrentBlock, initIterator);
        }
        if (endCondition != null) {
            dec_block.addExpressionToRef(aCurrentBlock, endCondition);
        }
        if (step != null) {
            dec_block.addExpressionToRef(aCurrentBlock, step);
        }
    }

    /**
     * DO文の終了行を設定する。
     *
     * @param lineInfo
     *            コード行情報
     */
    protected void end_repetition(CodeLine lineInfo) {
        ((Procedure) currentUnit).end_repetition(lineInfo);
    }

    /**
     * DO文の終了行を設定する。
     *
     * @param lineInfo
     *            コード行情報
     * @param label
     *            行ラベル
     */
    protected void end_repetition(CodeLine lineInfo, String label) {
        ((Procedure) currentUnit).end_repetition(lineInfo, label);
    }

    /**
     * CONTINUE文を設定する。
     *
     * @param lineInfo
     *            コード行情報
     * @param label
     *            行ラベル
     */
    protected void set_continue(CodeLine lineInfo, String label) {
        ((Procedure) currentUnit).set_continue(lineInfo, label);
    }

    /**
     * 変数宣言文を設定する。
     *
     * @param var_def
     *            変数宣言文
     */
    public void set_variable_def(VariableDefinition var_def) {
        if (var_def == null) return;
        ProgramUnit dec_block = this.currentUnit;

        // 変数宣言文の配列サイズ等の変数を登録する at 2015/11/01 by @hira
        if (dec_block != null) {
            dec_block.set_variable_def(var_def);
            Set<Variable> var_list = var_def.getAllVariables();
            if (var_list != null) {
                for (Variable var: var_list) {
                    dec_block.putRefVariableName(var.getName(), var_def);
                    dec_block.putVariableMap(var);
                }
            }
        }
    }

    /**
     * データベースに現在格納しているブロックを取得する。
     *
     * @return 現在ブロック
     */
    public Block get_current_block() {
        if (this.currentUnit == null) {
            return null;
        }
        if (!(this.currentUnit instanceof Procedure)) {
            return null;
        }
        if (((Procedure) currentUnit).getBody() == null) {
            return null;
        }

        return ((Procedure) currentUnit).getBody().getCurrentBlock();
    }

    /**
     * データベースに現在格納しているProgramUnitを取得する。
     *
     * @return 現在ProgramUnit
     */
    public ProgramUnit get_current_unit() {
        return this.currentUnit;
    }

    /**
     * データベースに現在格納しているProgramUnitを設定する。
     *
     * @param    module   現在ProgramUnit
     */
    public void set_current_unit(ProgramUnit unit) {
        this.currentUnit = unit;
    }

    /**
     * カレントユニットのカレントブロックを取得する。
     *
     * @return カレントブロック.カレントユニットがサブルーチンではない場合はnullを返す.
     */
    public Block getCurrentBlock() {
        if (currentUnit instanceof Procedure) {
            return ((Procedure) currentUnit).getCurrentBlock();
        } else {
            return null;
        }
    }

    // Blockクラスを継承するクラスの中で、論理的な1行で記述されるクラスのセットメソッド群

    /**
     * 代入文をセットする。
     *
     * @param left
     *            左辺
     * @param right
     *            右辺
     * @param lineInfo
     *            行情報
     * @param label
     *            ラベル。ない場合はStatement.NO_LABELをセットする。
     */
    public void setSubstitution(Expression left, Expression right,
            CodeLine lineInfo, String label) {
        Block block = this.getCurrentBlock();
        Substitution blk = new Substitution(block);
        if (left.isArrayExpression() || right.isArrayExpression()) {
            blk = new ArrayExpression(block);
        }
        blk.set_block_start(lineInfo);
        blk.get_start().set_label(label);
        blk.set_block_end(lineInfo);
        blk.get_end().set_label(label);
        blk.setLeft(left);
        blk.setRight(right);
        block.add_child(blk);

        // 分析機能のために参照をセット
        // 左辺
        List<Variable> leftVars = left.getVariables();
        Variable leftVar = null;
        ProgramUnit dec_block = blk.getScopeDeclarationsBlock();    // 変数の定義ブロックの検索
        if (leftVars != null && leftVars.size() > 0) {
            // 先頭変数を定義変数とする
            leftVar = leftVars.get(0);
            if (leftVar != null) {
                if (dec_block != null) {
                    dec_block.putDefVariableName(leftVar.getName(), blk);
                    dec_block.putVariableMap(leftVar);
                }
            }
        }
        // 2番目以降を参照変数とする
        Set<Variable> leftRefVars = left.getAllVariables();
        for (Variable var: leftRefVars) {
            if (var != null && var != leftVar) {
                if (dec_block != null) {
                    dec_block.addVariableToRef(blk, var);
                }
            }
        }

        Set<ProcedureUsage> leftFuncs = left.getAllFunctions();
        for (ProcedureUsage pu: leftFuncs) {
            if (dec_block != null) {
                dec_block.addFunctionToRef(blk, pu);
            }
        }

        // 右辺
        if (dec_block != null) {
            dec_block.addExpressionToRef(blk, right);
        }
    }

    /**
     * カレントブロックに子要素を持たないBlockをセットする。
     *
     * @param blk
     *            ブロック
     * @param lineInfo
     *            行情報
     * @param label
     *            ラベル。ない場合はStatement.NO_LABELをセットする。
     */
    private void setBlockToCurrent(Block blk, CodeLine lineInfo, String label) {
        Block block = this.getCurrentBlock();
        block.add_child(blk);
        blk.set_block_start(lineInfo);
        blk.get_start().set_label(label);
        blk.set_block_end(lineInfo);
        blk.get_end().set_label(label);

        ProgramUnit dec_block = blk.getScopeDeclarationsBlock();    // 変数の定義ブロックの検索
        if (dec_block != null) {
            String proc_name = dec_block.get_name();
            Set<Variable> vars = blk.getAllVariables();
            if (vars != null) {
                for (Variable var: vars) {
                    if (var == null) continue;
                    var.setParentStatement(blk);
                    dec_block.addVariableToRef(blk, var);
                }
            }
        }
    }

    /**
     * Exit文をセットする。
     *
     * @param lineInfo
     *            行情報
     * @param label
     *            ラベル。ない場合はStatement.NO_LABELをセットする。
     */
    public void setExit(CodeLine lineInfo, String label) {
        Break blk = new Break(label);
        setBlockToCurrent(blk, lineInfo, label);
    }

    /**
     * Return文をセットする。
     *
     * @param lineInfo
     *            行情報
     * @param label
     *            ラベル。ない場合はStatement.NO_LABELをセットする。
     */
    public void setReturn(CodeLine lineInfo, String label) {
        Return blk = new Return();
        setBlockToCurrent(blk, lineInfo, label);
    }

    /**
     * Cycle文をセットする。
     *
     * @param lineInfo
     *            行情報
     * @param label
     *            ラベル。ない場合はStatement.NO_LABELをセットする。
     */
    public void setCycle(CodeLine lineInfo, String label) {
        Continue blk = new Continue(label);
        setBlockToCurrent(blk, lineInfo, label);
    }

    /**
     * Stop文をセットする。
     *
     * @param arg
     *            引数
     *
     * @param lineInfo
     *            行情報
     * @param label
     *            ラベル。ない場合はStatement.NO_LABELをセットする。
     */
    public void setStop(String arg, CodeLine lineInfo, String label) {
        Termination blk = new Termination();
        blk.setArgument(arg);
        setBlockToCurrent(blk, lineInfo, label);
    }

    /**
     * Continue文をセットする。
     *
     * @param lineInfo
     *            行情報
     * @param label
     *            ラベル。ない場合はStatement.NO_LABELをセットする。
     */
    public void setContinue(CodeLine lineInfo, String label) {
        DoNothing blk = new DoNothing(label);
        setBlockToCurrent(blk, lineInfo, label);
    }

    /**
     * Pause文をセットする。
     *
     * @param arg
     *            引数
     *
     * @param lineInfo
     *            行情報
     * @param label
     *            ラベル。ない場合はStatement.NO_LABELをセットする。
     */
    public void setPause(String arg, CodeLine lineInfo, String label) {
        Pause blk = new Pause();
        blk.setArgument(arg);
        setBlockToCurrent(blk, lineInfo, label);
    }

    /**
     * GoTo文をセットする。
     *
     * @param arg
     *            引数
     *
     * @param lineInfo
     *            行情報
     * @param label
     *            ラベル。ない場合はStatement.NO_LABELをセットする。
     */
    public void setGoTo(String arg, CodeLine lineInfo, String label) {
        GoTo blk = new GoTo();
        blk.setArgument(arg);
        setBlockToCurrent(blk, lineInfo, label);
    }

    /**
     * ディレクティブをセットする。
     *
     * @param arg 引数
     * @param lineInfo 行情報
     * @param label ラベル。ない場合はStatement.NO_LABELをセットする。
     */
    public void setDirective(String arg, CodeLine lineInfo, String label) {
        Directive blk = new Directive();
        blk.setArgument(arg);
        if (this.getCurrentBlock() != null) {
            setBlockToCurrent(blk, lineInfo, label);
        } else {
            ProgramUnit pu = this.get_current_unit();
            pu.addDirective(blk);
        }
    }

    /**
     * Nullify文をセットする。
     *
     * @param var 引数
     * @param lineInfo 行情報
     * @param label ラベル。ない場合はStatement.NO_LABELをセットする。
     */
    public void setNullify(List<Variable> var, CodeLine lineInfo, String label) {
        DynamicNullification blk = new DynamicNullification();
        blk.setTarget(var);
        setBlockToCurrent(blk, lineInfo, label);
    }

    /**
     * Allocate文をセットする。
     *
     * @param trgt
     *            ターゲット
     * @param err
     *            エラー変数式。ない場合はnullをセットする。
     *
     * @param lineInfo
     *            行情報
     * @param label
     *            ラベル。ない場合はStatement.NO_LABELをセットする。
     */
    public void setAllocate(Map<Variable, VariableDimension> trgt,
            Variable err, CodeLine lineInfo, String label) {
        DynamicAllocation blk = new DynamicAllocation();
        blk.setTarget(trgt);
        blk.setError(err);
        setBlockToCurrent(blk, lineInfo, label);
    }

    /**
     * Deallocate文をセットする。
     *
     * @param trgt
     *            ターゲット
     * @param err
     *            エラー変数式。ない場合はnullをセットする。
     *
     * @param lineInfo
     *            行情報
     * @param label
     *            ラベル。ない場合はStatement.NO_LABELをセットする。
     */
    public void setDeallocate(List<Variable> trgt, Variable err,
            CodeLine lineInfo, String label) {
        DynamicDeallocation blk = new DynamicDeallocation();
        blk.setTarget(trgt);
        blk.setError(err);
        setBlockToCurrent(blk, lineInfo, label);

    }

    /**
     * Data文をカレントユニットにセットする。
     *
     * @param blk
     *            Dataクラス
     *
     * @param lineInfo
     *            行情報
     * @param label
     *            ラベル。ない場合はStatement.NO_LABELをセットする。
     */
    public void setData(Data blk,
            CodeLine lineInfo, String label) {
        this.currentUnit.addData(blk);
        blk.set_block_start(lineInfo);
        blk.get_start().set_label(label);
        blk.set_block_end(lineInfo);
        blk.get_end().set_label(label);
    }

    /**
     * Equivalence文をカレントユニットにセットする。
     *
     * @param blk
     *            Equivalenceクラス
     *
     * @param lineInfo
     *            行情報
     * @param label
     *            ラベル。ない場合はStatement.NO_LABELをセットする。
     */
    public void setEquivalence(Equivalence blk, CodeLine lineInfo,
            String label) {
        this.currentUnit.addEquivalence(blk);
        blk.set_block_start(lineInfo);
        blk.get_start().set_label(label);
        blk.set_block_end(lineInfo);
        blk.get_end().set_label(label);
    }

    /**
     * Common文をカレントユニットにセットする。
     *
     * @param blk
     *            Commonクラス
     *
     * @param lineInfo
     *            行情報
     * @param label
     *            ラベル。ない場合はStatement.NO_LABELをセットする。
     */
    public void setCommon(Common blk, CodeLine lineInfo, String label) {
        this.currentUnit.addCommon(blk);
        blk.set_block_start(lineInfo);
        blk.get_start().set_label(label);
        blk.set_block_end(lineInfo);
        blk.get_end().set_label(label);

        if (this.commonMap == null) {
            this.commonMap = new HashMap<String, List<ProgramUnit>>();
        }
        if (commonMap.containsKey(blk.getName())) {
            List<ProgramUnit> prList = commonMap.get(blk.getName());
            prList.add(this.currentUnit);
        } else {
            List<ProgramUnit> prList = new ArrayList<ProgramUnit>();
            prList.add(this.currentUnit);
            this.commonMap.put(blk.getName(), prList);
        }
    }

    /**
     * Interface文をカレントユニットにセットする。
     *
     * @param blk
     *            Interfaceクラス
     * @param lineInfo
     *            行情報
     * @param label
     *            ラベル。ない場合はStatement.NO_LABELをセットする。
     */
    public void setInterface(Procedures blk, CodeLine lineInfo, String label) {
        this.currentUnit.addInterface(blk);
        blk.set_block_start(lineInfo);
        blk.get_start().set_label(label);
        blk.set_block_end(lineInfo);
        blk.get_end().set_label(label);
    }
    // ----------------------------------------
    /**
     * カレントユニットにprivate属性をセットする。
     */
    public void setPrivateToCurrentUnit() {
        if (!(currentUnit instanceof Procedure))
            return;
        ((Procedure) (this.currentUnit)).setPrivate();
    }

    /**
     * カレントユニットにpublic属性をセットする。
     */
    public void setPublicToCurrentUnit() {
        if (!(currentUnit instanceof Procedure))
            return;
        ((Procedure) (this.currentUnit)).setPublic();
    }

    // ----------------------------------------
    /**
     * TYPE宣言を登録する。
     *
     * @param tp TYPE宣言
     */
    public void addTypeDefinition(Type tp) {
        this.currentUnit.addTypeDefinition(tp);
        List<VariableDefinition> vars = tp.getDefinitions();
        if (vars != null) {
            for (VariableDefinition var: vars) {
                var.setMother(this.currentUnit);
            }
        }
    }

    /**
     * interfaceブロックを追加する.
     * @param blk interfaceブロック
     */
    public void addInterface(Procedures blk) {
        currentUnit.addInterface(blk);
    }

    /**
     * 領域指定された全ての情報ブロックの取得。
     *
     * @return 領域指定された情報ブロックコレクション
     */
    public InformationBlocks getInformationBlocks() {
        return this.informationBlocks;
    }
    /**
     * 領域指定された全ての情報ブロックのセット。
     * @param blks 領域指定された情報ブロックの集合
     */
    public void setInformationBlocks(InformationBlocks blks) {
        this.informationBlocks = blks;
    }

    // ----------------------------------------
    // --------------Information---------------
    // ----------------------------------------
    /**
     * 子プログラムを含む全ての情報ブロックの取得。
     *
     * @return 情報ブロックコレクション
     */
    public InformationBlocks getInformationBlocksAll() {
        InformationBlocks result = new InformationBlocks();
        result.addAll(this.informationBlocks);
        if (this.modules != null) {
            for (Module module : this.modules.values()) {
                result.addAll(module.createInformationBlocks());
            }
        }
        return result;
    }


    /**
     * カレントユニットに戻り値の型をセットする。
     * @param tp 戻り値の型
     */
    public void setReturnValueType(IVariableType tp) {
        ProgramUnit pu = this.currentUnit;
        if (pu instanceof Procedure) {
            ((Procedure) pu).setReturnValueType(tp);
        }
    }

    /**
     * カレントユニットに結果の名前をセットする。
     * @param st 結果の名前
     */
    public void setResult(String st) {
        ProgramUnit pu = this.currentUnit;
        if (pu instanceof Procedure) {
            ((Procedure) pu).setResult(st);
        }
    }

    /**
     * カレントユニットにexternal宣言された関数の名前とデータ型を追加する。
     * @param name 関数名
     * @param tp データ型
     */
    public void addExternalFunction(String name, IVariableType tp) {
        this.currentUnit.addExternalFunctionList(name, tp);
    }

    /**
     * COMMONマップを返す。
     * @return COMMONマップ
     */
    public Map<String, List<ProgramUnit>> getCommonMap() {
        return commonMap;
    }

    /**
     * 指定したCommon名を宣言しているプログラム単位のリストを返す。
     * @param nm Common名
     * @return プログラム単位のリスト。無ければ空のリストを返す。
     */
    public List<ProgramUnit> getCommonUnit(String nm) {
        if (this.commonMap == null) {
            return new ArrayList<ProgramUnit>();
        }
        if (this.commonMap.get(nm) == null) {
            return new ArrayList<ProgramUnit>();
        }
        return this.commonMap.get(nm);
    }

    /**
     * 指定したファイルに含まれるプログラム単位のリストを返す。
     *
     * @param file
     *            ファイル
     * @return プログラム単位のリスト。無ければ空のリストを返す。
     */
    public List<ProgramUnit> getProgramUnits(SourceFile file) {
        if (file == null) return null;
        ArrayList<ProgramUnit> units = new ArrayList<ProgramUnit>();

        Collection<Module> mods = this.modules.values();
        for (Module mod : mods) {
            List<IBlock> procs = mod.getChildren();
            for (IBlock proc : procs) {
                if (!(proc instanceof ProgramUnit)) continue;
                SourceFile procFile = proc.getStartCodeLine().getSourceFile();
                if (file.equals(procFile)) {
                    units.add((ProgramUnit)proc);
                }
            }
        }
        return units;
    }
    /**
     * 複数ブロック付加情報のリストから指定したstartとendを持つブロックを探索し、該当の付加情報領域を返す。
     * @param start 開始ブロック
     * @param end 終了ブロック
     * @return 付加情報領域。無ければ新しい付加情報領域を作成し返す。
     */
    public IInformation getInformation(IInformation start, IInformation end) {
        InformationBlock block = this.informationBlocks.findObjectBy(start, end);
        if (block != null) {
            return block;
        }
        // 無ければ新しく作成して追加
        TextInfo newInfo = new TextInfo();
        InformationBlock bk = new InformationBlock(newInfo, start, end);
        this.informationBlocks.add(bk);
        return bk;
    }
    /**
     * 複数ブロック付加情報のリストから指定したinfoNodeを開始に持つブロックを探索し、該当の付加情報領域のリストを返す。
     * 指定したinfoNodeがメインの場合、リストを全て返す。
     * @param infoNode ブロック
     * @return 付加情報領域のリスト。無ければ空のリストを返す。
     */
    public List<InformationBlock> getInformation(IInformation infoNode) {
        if (infoNode instanceof Procedure) {
            Procedure proc = (Procedure) infoNode;
            if (proc.get_name().equalsIgnoreCase(this.mainName)) {
                return this.informationBlocks;
            }
        }
        return this.informationBlocks.getStartWith(infoNode);
    }

    /**
     * main名をセットする
     *
     * @param nm
     *            main名
     */
    public void setMainName(String nm) {
        mainName = nm;
    }

    /**
     * モジュールを追加する
     * @param pu        モジュール
     */
    public void addModule(ProgramUnit pu) {
        if (pu instanceof Module) {
            this.modules.put(pu.get_name(), (Module) pu);
        }
    }


    /**
     * Common文をcommonMapにセットする。
     * @param key            Common文ブロック名
     * @param unit           Common文宣言モジュール、サブルーチン
     */
    public void addCommonMap(String key, ProgramUnit unit) {
        if (this.commonMap == null) {
            this.commonMap = new HashMap<String, List<ProgramUnit>>();
        }
        if (commonMap.containsKey(key)) {
            List<ProgramUnit> prList = commonMap.get(key);
            prList.add(unit);
        } else {
            List<ProgramUnit> prList = new ArrayList<ProgramUnit>();
            prList.add(unit);
            this.commonMap.put(key, prList);
        }
    }

    /**
     * シャローコピーを行う.
     * @param program        コピー元データベース
     */
    public void copyShallow(Program program) {
        this.mainName = program.mainName;
        this.modules = program.modules;
        this.commonMap = program.commonMap;
        this.informationBlocks = program.informationBlocks;
    }

    /**
     * メインプログラムを取得する.
     * @return        メインプログラム
     */
    public Procedure getMainProgram() {
        return getProcedureByName(NO_MODULE, this.mainName);
    }

    /**
     * プロシージャを取得する.<br/>
     * モジュールは、NO_MODULEから検索する.
     * @param procudurename        プロシージャ名
     * @return        プロシージャ
     */
    public Procedure getProcedure(String procudurename) {
        return getProcedureByName(null, procudurename);
    }

    /**
     * プロシージャを取得する.<br/>
     * モジュール名がnullの場合は、NO_MODULEから検索する.
     * @param modulename        モジュール名
     * @param procudurename        プロシージャ名
     * @return        プロシージャ
     */
    public Procedure getProcedureByName(String modulename, String procudurename) {
        if (procudurename == null) return null;
        Module module = null;
        if (modulename == null) {
            module = this.module(NO_MODULE);
        }
        else {
            module = this.module(modulename);
        }
        if (module == null) return null;
        ProgramUnit proc = module.get_child(procudurename);
        if (proc instanceof Procedure) {
            return (Procedure)proc;
        }
        return null;
    }

    /**
     * プロシージャを取得する.<br/>
     * 親モジュールがnullの場合は、NO_MODULEから検索する.
     * @param parent            親モジュール
     * @param procudurename        プロシージャ名
     * @return        プロシージャ
     */
    public Procedure getProcedure(ProgramUnit parent, String procudurename) {
        ProgramUnit parentUnit = parent;
        if (parentUnit == null) {
            parentUnit = this.module(NO_MODULE);
        }
        ProgramUnit proc = parentUnit.get_child(procudurename);
        if (proc instanceof Procedure) {
            return (Procedure)proc;
        }
        return null;
    }


    /**
     * 指定したファイルのModule(ファイル)を返す。
     *
     * @param file            ファイル
     * @return    検索結果Module(ファイル)
     */
    public Module getModule(SourceFile file) {
        if (file == null) return null;
        if (this.modules == null) return null;

        Collection<Module> mods = this.modules.values();
        for (Module mod : mods) {
            if (mod == null) continue;
            if (mod.getStartCodeLine() == null) continue;
            SourceFile modFile = mod.getStartCodeLine().getSourceFile();
            if (file.equals(modFile)) {
                return mod;
            }
        }
        return null;
    }

}
