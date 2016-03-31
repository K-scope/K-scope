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
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.information.InformationBlocks;
import jp.riken.kscope.language.fortran.VariableAttribute.ScopeAttribute;
import jp.riken.kscope.language.fortran.VariableType;
import jp.riken.kscope.language.fortran.VariableType.PrimitiveDataType;

/**
 * 手続きを表現するクラス。
 * Fortranにおけるsubroutine,function,entryに対応する。
 *
 * @author RIKEN
 *
 */
public class Procedure extends ProgramUnit {
    /** シリアル番号 */
    private static final long serialVersionUID = -6409164910117026406L;
    /** 仮引数. */
    private Variable[] arguments;
    /**
     * 本手続きを呼び出しているプログラム単位のリスト。 分析機能のためのメンバー変数。
     * @deprecated    未使用
     */
    @Deprecated
    private ArrayList<Procedure> parents = new ArrayList<Procedure>();
    /**
     * 戻り値のデータ型を表現するメンバ。
     */
    private IVariableType returnValueType = new VariableType(PrimitiveDataType.VOID);
    /** 本手続きを呼び出しているCALL文のリスト */
    private transient Set<ProcedureUsage> calls = new LinkedHashSet<ProcedureUsage>();
    /**
     * 戻り値の名前を保持するメンバ。
     */
    private String result;
    /**
     * 手続きの実行文を表現するメンバー変数。
     */
    private ExecutableBody body = new ExecutableBody(this);
    /** scope属性：private, public */
    private ScopeAttribute scope = ScopeAttribute.NONE;

    /**
     * @return returnValueType
     */
    public IVariableType getReturnValueType() {
        return returnValueType;
    }

    /**
     * データ型をセットする。
     *
     * @param tp
     *            データ型
     */
    public void setReturnValueType(IVariableType tp) {
        this.returnValueType = tp;
    }

    /**
     * 戻り値の名前を返す。
     *
     * @return result 戻り値の名前
     */
    public String getResult() {
        return result;
    }

    /**
     * 戻り値の名前をセットする。
     *
     * @param res
     *            戻り値の名前
     */
    public void setResult(String res) {
        this.result = res;
    }

    /**
     * 自身の処理ブロックを返す。
     * @return 処理ブロック
     */
    public ExecutableBody getBody() {
        return this.body;
    }


    /**
     * Public属性をセットする。
     */
    public void setPublic() {
        scope = ScopeAttribute.PUBLIC;
    }

    /**
     * Private属性をセットする。
     */
    public void setPrivate() {
        scope = ScopeAttribute.PRIVATE;
    }
    /**
     * ブロックタイプの取得。
     *
     * @return BlockType.PROCEDURE
     */
    @Override
    public BlockType getBlockType() {
        return BlockType.PROCEDURE;
    }

    /**
     * サブルーチン・関数文を文字列に変換する.
     */
    @Override
    protected String toStringBase() {
        return (this.get_type() + " " + this.get_name());
    }


    /**
     * サブルーチン・関数文を文字列に変換する.
     */
    public String toStringProcedure() {
        StringBuffer buf = new StringBuffer();
        if (this.has_attribute("recursive")) {
            buf.append("recursive ");
        }
        buf.append(this.toStringBase());
        // 仮引数
        buf.append("(");
        if (this.arguments != null) {
            StringBuffer args_buf = new StringBuffer();
            for (Variable var : this.arguments) {
                if (args_buf.length() > 0) args_buf.append(", ");
                args_buf.append(var.getName());
            }
            buf.append(args_buf);
        }
        buf.append(")");

        if (result != null && !result.isEmpty()) {
            buf.append(" result ");
            buf.append("(" + this.result + ")");
        }
        return buf.toString();
    }


    // ++++++++++++++++++++++++++++++++++++++++++++

    /**
     * コンストラクタ。
     *
     * @param type_name
     *            手続の種類
     * @param sub_name
     *            手続の名前
     */
    public Procedure(String type_name, String sub_name) {
        super(type_name, sub_name);
    }

    /**
     * コンストラクタ。
     *
     * @param type_name
     *            手続の種類
     * @param sub_name
     *            手続の名前
     * @param args
     *            仮引数の配列
     */
    public Procedure(String type_name, String sub_name, String[] args) {
        super(type_name, sub_name);
        this.arguments = new Variable[args.length];
        for (int i = 0; i < arguments.length; i++) {
            this.arguments[i] = new Variable(args[i]);
            this.arguments[i].setParentStatement(this);
        }
    }

    /**
     * コンストラクタ。
     *
     * @param type_name      手続の種類
     * @param sub_name        手続の名前
     * @param args            仮引数の配列
     */
    public Procedure(String type_name, String sub_name, Variable[] args) {
        super(type_name, sub_name);
        if (args != null) {
            this.arguments = args;

            for (Variable var : args) {
                var.setParentStatement(this);
                VariableDefinition def = var.getDefinition();
                if (def != null) {
                    this.put_variable(def);
                }
            }
        }

    }

    /**
     * コピーコンストラクタ。
     * @param dest        コピー元Procedureクラス
     */
    public Procedure(Procedure dest) {
        super(dest);
        if (dest == null) return;
        if (dest.arguments != null && dest.arguments.length > 0) {
            this.arguments = new Variable[dest.arguments.length];
            for (int i=0; i<dest.arguments.length; i++) {
                this.arguments[i] = new Variable(dest.arguments[i]);
            }
        }
        if (dest.returnValueType != null) {
            this.returnValueType = new VariableType((VariableType)dest.returnValueType);
        }
        if (dest.calls != null && dest.calls.size() > 0) {
            this.calls = new LinkedHashSet<ProcedureUsage>();
            for (ProcedureUsage call : dest.calls) {
                this.calls.add(new ProcedureUsage(call, this));
            }
        }
        this.result = dest.result;
        if (dest.body != null) {
            this.body = new ExecutableBody(dest.body);
        }
        this.scope = dest.scope;
    }


    /**
     * 本手続きを呼び出しているプログラム単位のリスト
     * @param parent        本手続きを呼び出しているプログラム
     * @deprecated    未使用
     */
    @Deprecated
    protected void add_parent(Procedure parent) {
        parents.add(parent);
    }

    // ++++++++++++++++++++++++++++++++++++++++++++

    /**
     * 指定された順番にある仮引数を返す。存在しなければnullを返す。
     * @param i 引数の順番
     * @return 仮引数。
     */
    public Variable getArgument(int i) {
        if (arguments.length > i) {
            return arguments[i];
        }
        return null;
    }

    // ++++++++++++++++++++++++++++++++++++++++++++

    /**
     * 仮引数の配列を返す。
     *
     * @return 仮引数の配列。
     */
    public Variable[] get_args() {
        return arguments;
    }

    // ++++++++++++++++++++++++++++++++++++++++++++

    protected ExecutableBody get_body() {
        return (body);
    }

    /**
     * 本手続きを呼び出しているプログラム単位のリストを取得する.
     * @return        本手続きを呼び出しているプログラム単位のリスト
     * @deprecated    未使用
     */
    @Deprecated
    protected ArrayList<Procedure> get_parents() {
        return (parents);
    }

    // ++++++++++++++++++++++++++++++++++++++++++//
    // interface method //
    // ++++++++++++++++++++++++++++++++++++++++++//

    // --------------Variable_def----------------//

    /*
     * protected void new_variable_def(String var_name) { Variable_def_class
     * var_def = new Variable_def_class(var_name) ; this.put_variable(var_def) ;
     * }
     */

    // ----------------Selection-----------------//
    /**
     * 条件式の設定。
     *
     * @param cond
     *           条件式
     */
    protected void start_condition(Expression cond, CodeLine lineInfo,
            String label) {
        body.start_condition(cond,lineInfo,label);
    }
    // ++++++++++++++++++++++++++++++++++++++++++++
    protected void end_condition(CodeLine lineInfo,String label) {
        body.end_condition(lineInfo,label);
    }

    // ---------------User_defined---------------//
    protected void start_user_defined(CodeLine lineInfo) {
        body.start_user_defined(lineInfo);
    }

    // ++++++++++++++++++++++++++++++++++++++++++++
    protected void start_user_defined(CodeLine lineInfo, String label) {
        body.start_user_defined(lineInfo, label);
    }

    // ++++++++++++++++++++++++++++++++++++++++++++
    protected void end_user_defined(CodeLine lineInfo) {
        body.end_user_defined(lineInfo);
    }

    // ++++++++++++++++++++++++++++++++++++++++++++
    protected void end_user_defined(CodeLine lineInfo, String label) {
        body.end_user_defined(lineInfo, label);
    }

    // --------------Procedure_usage-------------//
    // ++++++++++++++++++++++++++++++++++++++++++++

    // ++++++++++++++++++++++++++++++++++++++++++++
    protected void add_procedure_usage(ProcedureUsage sub_call) {
        body.add_procedure_usage(sub_call);
    }

    // ++++++++++++++++++++++++++++++++++++++++++++

    /**
     * 手続呼出しのリストを返す。
     * @return 手続呼出しのリスト
     */
    @Override
    public List<ProcedureUsage> getCalls() {
        return body.getCalls();
    }

    // ++++++++++++++++++++++++++++++++++++++++++//
    // output method //
    // ++++++++++++++++++++++++++++++++++++++++++//

    /**
     * CALL, FUNCTION文の開始行を設定する。
     *
     * @param lineInfo
     *            コード行情報
     */
    protected void start_procedure_usage(CodeLine lineInfo) {
        body.start_procedure_usage(lineInfo);
    }

    /**
     * CALL, FUNCTION文の開始行を設定する。
     *
     * @param lineInfo
     *            コード行情報
     * @param label
     *            行ラベル
     */
    protected void start_procedure_usage(CodeLine lineInfo, String label) {
        body.start_procedure_usage(lineInfo, label);
    }

    /**
     * CALL, FUNCTION文の開始行を設定する。
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
        body.start_procedure_usage(lineInfo, label, subroutineName, arguments);
    }

    /**
     * CALL, FUNCTION文の終了行を設定する。
     *
     * @param lineInfo
     *            コード行情報
     */
    protected void end_procedure_usage(CodeLine lineInfo) {
        body.end_procedure_usage(lineInfo);
    }

    /**
     * CALL, FUNCTION文の終了行を設定する。
     *
     * @param lineInfo
     *            コード行情報
     * @param label
     *            行ラベル
     */
    protected void end_procedure_usage(CodeLine lineInfo, String label) {
        body.end_procedure_usage(lineInfo, label);
    }

    /**
     * DO文の開始行を設定する。
     *
     * @param lineInfo
     *            コード行情報
     */
    protected void start_repetition(CodeLine lineInfo) {
        body.start_repetition(lineInfo);
    }

    /**
     * DO文の開始行を設定する。
     *
     * @param lineInfo
     *            コード行情報
     * @param label
     *            行ラベル
     */
    protected void start_repetition(CodeLine lineInfo, String label) {
        body.start_repetition(lineInfo, label);
    }

    /**
     * DO文の終了行を設定する。
     *
     * @param lineInfo
     *            コード行情報
     */
    protected void end_repetition(CodeLine lineInfo) {
        body.end_repetition(lineInfo);
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
        body.end_repetition(lineInfo, label);
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
        body.set_continue(lineInfo, label);
    }

    /**
     * SELECT文の開始行を設定する。（ラベル付き）
     *
     * @param lineInfo
     *            コード行情報
     * @param label
     *            行ラベル
     */
    protected void start_selection(CodeLine lineInfo, String label,
            jp.riken.kscope.language.Selection.SelectionType type) {
        body.start_selection(lineInfo, label, type);
    }

    /**
     * SELECT文の終了行を設定する。（ラベル付き）
     *
     * @param lineInfo
     *            コード行情報
     * @param label
     *            行ラベル
     */
    protected void end_selection(CodeLine lineInfo, String label) {
        body.end_selection(lineInfo, label);
    }

    /**
     * RETURN文をセットする
     *
     * @param lineInfo
     *            コード行情報
     */
    public void setReturn(CodeLine lineInfo) {
        body.setReturn(lineInfo);
    }
    /**
     * 実引数リストが適合しているかどうか。<br>
     *
     * 多重定義されている関数群の中から対応する関数を探索する際に、<br>
     * 仮引数と実引数の型チェックをする必要がある。<br>
     * 「適合している」とは、この型チェックで、同一の型と判定される 事を意味している。<br>
     *
     * @param actualArguments
     *
     * @return true : 適合している<br>
     *         false: 適合していない
     */
    public boolean matches(List<Expression> actualArguments) {
        if (actualArguments == null || this.arguments == null) {
            return false;
        }
        if (actualArguments.size() > this.arguments.length) {
            return false;
        }

        for (int i = 0; i < this.arguments.length; i++) {
            if (i < actualArguments.size()) {
                VariableDefinition def = this.getVariableMap(this.arguments[i]);
                if (def != null) {
                    if (!def.matches(actualArguments.get(i))) {
                        return false;
                    }
                }
            } else {
                if (!this.getVariableMap(this.arguments[i]).getAttribute()
                        .contains("optional")) {
                    return false;
                }
            }
        }

        return true;
    }

    /**
     * カレントブロックを返す。
     *
     * @return カレントブロック
     */
    public Block getCurrentBlock() {
        return body.getCurrentBlock();
    }

    /**
     * @param blk
     */
    public void setCurrentBlock(Block blk) {
        body.setCurrentBlock(blk);

    }

    /**
     * idにマッチした情報ブロックを検索する。
     * @param id
     *          ID
     * @return 見つかった情報ブロック。見つからなかった場合はnullが返ります。
     */
    @Override
    public IInformation findInformationBlockBy(String id) {
        IInformation infoBlock
          = super.findInformationBlockBy(id);

        if (infoBlock == null) {
            infoBlock = body.findInformationBlockBy(id);
        }

        return infoBlock;
    }

    /**
     * scope属性を取得する。
     * @return        scope属性
     */
    public ScopeAttribute getScope() {
        return this.scope;
    }

    /**
     * 指定した変数名の仮引数が含まれていればその順番を返す。
     * @param dummyArg 変数名
     * @return 仮引数の順番。指定した変数を含まない場合-1を返す。
     */
    public int getNumOfDummyArgument(String dummyArg) {
        Variable[] args = this.arguments;
        if (args != null) {
            for (int i = 0;i < args.length;i++) {
                if (args[i].getName().equalsIgnoreCase(dummyArg)) {
                    return i;
                }
            }
        }
        return -1;
    }

    /**
     * この手続を呼び出しているProcedureUsageクラスのリストを返す。
     * @return ProcedureUsageのリスト。無ければ空のリストを返す。
     */
    public Set<ProcedureUsage> getCallMember() {
        if (this.calls == null) {
            return new LinkedHashSet<ProcedureUsage>();
        }
        return this.calls;
    }

    /**
     * この手続を呼び出しているProcedureUsageクラスを追加する。
     * @param pu ProcedureUsageクラス
     */
    public void addCallMember(ProcedureUsage pu) {
        if (this.calls == null) {
            this.calls = new LinkedHashSet<ProcedureUsage>();
        }
        this.calls.add(pu);
    }

    /**
     * この手続を呼び出しているProcedureUsageクラスリストを設定する。
     * @param list   ProcedureUsageリスト
     */
    public void setCallMember(Set<ProcedureUsage> list) {
        this.calls = list;
    }

    /**
     * 付加情報ブロックコレクションを生成する。
     *
     * @return 付加情報ブロックコレクション
     */
    @Override
    public InformationBlocks createInformationBlocks() {
        InformationBlocks informationBlocks = new InformationBlocks();
        informationBlocks.addAll(super.createInformationBlocks());
        informationBlocks.addAll(this.body.createInformationBlocks());
        return informationBlocks;
    }

    /**
     * 同一Procedureであるかチェックする.
     * 引数についてはチェックしない。
     * @param proc        subroutine, function
     * @return        true=一致
     */
    public boolean equalsBlocks(Procedure proc) {
        if (proc == null) return false;

        if (!super.equalsBlocks(proc)) {
            return false;
        }

        if (this.getBody() == null && proc.getBody() == null) {
            return true;
        }
        else if (this.getBody() == null) {
            return false;
        }

        if (!this.getBody().equalsBlocks(proc.getBody())) {
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

        // 変数宣言文、副プログラム
        {
            IInformation[] infos = super.searchInformationBlocks(block);
            if (infos != null) {
                list.addAll(Arrays.asList(infos));
            }
        }
        if (this.body != null) {
            IInformation[] infos = body.searchInformationBlocks(block);
            if (infos != null) {
                list.addAll(Arrays.asList(infos));
            }
        }
        if (list.size() <= 0) {
            return null;
        }

        return list.toArray(new IInformation[0]);
    }


    /**
     * 同一ブロック階層であるかチェックする.
     * @param proc        チェック対象Procedure
     * @return   true=一致
     */
    @Override
    public boolean equalsLayout(ProgramUnit proc) {
        if (proc == null) return false;
        if (!(proc instanceof Procedure)) return false;

        if (!super.equalsLayout(proc)) {
            return false;
        }

        if (this.getBody() == null && ((Procedure)proc).getBody() == null) {
            return true;
        }
        else if (this.getBody() == null) {
            return false;
        }

        if (!this.getBody().equalsLayout(((Procedure)proc).getBody())) {
            return false;
        }

        return true;
    }

    /**
     * layoutIDにマッチした構造ブロックを検索する。
     * @param id    layoutID
     * @return 見つかった構造ブロック
     */
    @Override
    public IInformation findInformationLayoutID(String id) {
        if (id == null || id.isEmpty()) return null;
        IInformation infoBlock = super.findInformationLayoutID(id);
        if (infoBlock == null) {
            infoBlock = body.findInformationLayoutID(id);
        }

        return infoBlock;
    }

    /**
     * 行番号のブロックを検索する
     * @param line            行番号
     * @return        行番号のブロック
     */
    @Override
    public IBlock[] searchCodeLine(CodeLine line) {
        if (line == null) return null;

        List<IBlock> list = new ArrayList<IBlock>();
        IBlock addblock = null;
        CodeLine thisstart = this.getStartCodeLine();
        CodeLine thisend = this.getEndCodeLine();
        if ( line.isOverlap(thisstart, thisend) ) {
            addblock = this;
        }
        // 変数宣言文、副プログラム
        {
            IBlock[] blocks = super.searchCodeLine(line);
            if (blocks != null) {
                list.addAll(Arrays.asList(blocks));
            }
        }
        if (this.body != null) {
            IBlock[] blocks = body.searchCodeLine(line);
            if (blocks != null) {
                list.addAll(Arrays.asList(blocks));
            }
        }
        if (list.size() <= 0) {
            if (addblock != null) {
                list.add(addblock);
            }
            else {
                return null;
            }
        }

        return list.toArray(new IBlock[0]);
    }

     /**
      * 変数リストを取得する.
      */
     @Override
     public Set<Variable> getAllVariables() {
         Set<Variable> list = new HashSet<Variable>();
        // 引数
        {
            Set<Variable> vars = this.getBlockVariables();
            if (vars != null) {
                list.addAll(vars);
            }
        }
        // 変数宣言文、副プログラム
        {
            Set<Variable> vars = super.getAllVariables();
            if (vars != null) {
                list.addAll(vars);
            }
        }
        if (this.body != null) {
            Set<Variable> vars = this.body.getAllVariables();
            if (vars != null) {
                list.addAll(vars);
            }
        }
        if (list.size() <= 0) return null;
        return list;
     }

     /**
      * program文であるかチェックする.
      * @return    true=program文
      */
     public boolean isProgram() {
         if (this.get_type() == null) return false;
         if (this.get_type().equalsIgnoreCase("program")) return true;
         return false;
     }

     /**
      * subroutine文であるかチェックする.
      * @return    true=subroutine文
      */
     public boolean isSubroutine() {
         if (this.get_type() == null) return false;
         if (this.get_type().equalsIgnoreCase("subroutine")) return true;
         return false;
     }

     /**
      * function文であるかチェックする.
      * @return    true=function文
      */
     public boolean isFunction() {
         if (this.get_type() == null) return false;
         if (this.get_type().equalsIgnoreCase("function")) return true;
         return false;
     }

     /**
      * 変数定義は仮引数の定義であるかチェックする.
      * @param def    変数定義
      * @return            true=仮引数の変数定義
      */
     public boolean isArgumentVariableDefinition(VariableDefinition def) {
         if (def == null) return false;
         if (this.arguments == null) return false;
         for (Variable arg : this.arguments) {
             VariableDefinition arg_def = arg.getDefinition();
             if (arg_def == null) continue;
             if (arg_def == def) return true;
         }
         return false;
     }

     /**
      * 式の変数リストを取得する.
      * ブロックのみの変数リストを取得する。
      * @return        式の変数リスト
      */
     @Override
     public Set<Variable> getBlockVariables() {

         Set<Variable> list = new HashSet<Variable>();
         if (this.arguments != null) {
             for (Variable var : this.arguments) {
                 list.add(var);
                 Set<Variable> vars = var.getAllVariables();
                 if (vars != null && vars.size() > 0) {
                     list.addAll(vars);
                 }
             }
         }

         if (list.size() <= 0) return null;
         return list;
     }

    /**
     * public属性を持つかチェックする
     * @return    true = public属性を持つ
     */
    public boolean hasPublic() {
        ScopeAttribute scope = this.getScope();
        if (scope == ScopeAttribute.PUBLIC) {
            return true;
        }

        // XcodeML/Fバグ対応
        // functtion(関数)の場合はすべてpublicとする
        if (this.isFunction()) {
            return true;
        }

        return false;
    }

}
