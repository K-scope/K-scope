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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.information.InformationBlocks;
import jp.riken.kscope.language.fortran.VariableAttribute.SclassAttribute;
import jp.riken.kscope.language.fortran.VariableAttribute.ScopeAttribute;
import jp.riken.kscope.language.fortran.VariableAttribute;
import jp.riken.kscope.language.fortran.VariableType;
import jp.riken.kscope.language.fortran.VariableType.PrimitiveDataType;

/**
 * 手続きを表現するクラス。
 * Fortranにおけるsubroutine,function,entryに対応する。
 * @author RIKEN
 * @version    2015/03/01   	関数表記(toString)をC言語とFortranにて振分
 *                              	仮引数の変数検索をC言語にて大文字・小文字を区別する様に変更(getNumOfDummyArgument)
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
    /** 属性:scope属性,sclass属性. */
    private IVariableAttribute attribute;

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
        VariableAttribute attr = (VariableAttribute) this.attribute;
        attr.addAttribute("public");
    }

    /**
     * Private属性をセットする。
     */
    public void setPrivate() {
        VariableAttribute attr = (VariableAttribute) this.attribute;
        attr.addAttribute("private");
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
     * 関数の文字列表現を取得する.
     */
    @Override
    protected String toStringBase() {
        if (this.isClang()) {
            // C言語
            return this.toStringClang();
        }
        else {
            // Fortran
            return this.toStringFortran();
        }
    }

    /**
     * C言語関数の文字列表現を取得する.
     */
    private String toStringClang() {
        StringBuffer buf = new StringBuffer();
        IVariableType func_type = this.getReturnValueType();
        if (func_type != null) {
            if (func_type.isVoid()) {
                buf.append("void");
            }
            else {
                buf.append(func_type.toString());
            }
        }
        else {
            buf.append("void");
        }
        buf.append(" ");
        buf.append(this.get_name());

        buf.append("(");
        if (arguments != null && arguments.length > 0) {
            int count = 0;
            for (Variable var : this.arguments) {
                if (count > 0) {
                    buf.append(", ");
                }
                VariableDefinition def = var.getDefinition();
                if (def != null) {
                    buf.append(def.toStringClang());
                }
                else if (var != null) {
                    buf.append(var.toString());
                }
                count++;
            }

        }
        buf.append(")");

        return buf.toString();
    }

    /**
     * Fortran関数の文字列表現を取得する.
     */
    private String toStringFortran() {
        return (this.get_type() + " " + this.get_name());
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
        arguments = new Variable[args.length];
        for (int i = 0; i < arguments.length; i++) {
            arguments[i] = new Variable(args[i]);
        }
    }

    /**
     * コンストラクタ。
     *
     * @param type_name      手続の種類
     * @param sub_name		手続の名前
     * @param args			仮引数の配列
     */
    public Procedure(String type_name, String sub_name, Variable[] args) {
        super(type_name, sub_name);
        arguments = new Variable[args.length];
        arguments = args;
    }

    // ++++++++++++++++++++++++++++++++++++++++++++

    /**
     * 本手続きを呼び出しているプログラム単位のリスト
     * @param parent		本手続きを呼び出しているプログラム
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
     * @return		本手続きを呼び出しているプログラム単位のリスト
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
     * 自身に属する手続呼出しのリストを返す。
     *
     * @return 手続呼出しのリスト。無ければ空のリストを返す。
     */
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
     * Return文を設定する。
     * @param lineInfo           コード行情報
     * @param exp           リターン式
     */
    protected void setReturn(CodeLine lineInfo, Expression exp) {
        body.setReturn(lineInfo, exp);
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
                VariableDefinition def = this.getVariableMap(this.arguments[i]
                        .getName());
                if (def != null) {
                    if (!def.matches(actualArguments.get(i))) {
                        return false;
                    }
                }
            } else {
                if (!this.getVariableMap(this.arguments[i].getName()).getAttribute()
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
     * プログラム単位内で、ある変数が参照・定義されているブロックのセットを返す。
     *
     * @param name
     *            変数名
     *
     * @return ブロックのセット。無ければ空のセットを返す。
     */
    public Set<IBlock> getRefDefBlocks(String name) {
        String nm = name.toLowerCase();
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
     * scope属性を取得する。
     * @return		scope属性
     */
    public ScopeAttribute getScope() {
        if (this.attribute == null) return null;
        VariableAttribute attr = (VariableAttribute) this.attribute;
        return attr.getScope();
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
                if (this.isClang()) {
                    // C言語
                    if (args[i].getName().equals(dummyArg)) {
                        return i;
                    }
                }
                else {
                    // Fortran
                    if (args[i].getName().equalsIgnoreCase(dummyArg)) {
                        return i;
                    }
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
     * @param proc		subroutine, function
     * @return		true=一致
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
     * @param block			IInformationブロック
     * @return		同一ブロック
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
     * @param proc		チェック対象Procedure
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
     * @param line			行番号
     * @return		行番号のブロック
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
        // 変数宣言文、副プログラム
        {
            Set<Variable> vars = super.getAllVariables();
            if (vars != null) {
                list.addAll(vars);
            }
        }
        if (this.body != null) {
            Set<Variable> vars = body.getAllVariables();
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
      * 記憶クラスを設定する.
      * @param sclass		記憶クラス
      */
     public void setSclass(String sclass) {
         VariableAttribute attr = (VariableAttribute) this.attribute;
         attr.addAttribute(sclass);
     }

     /**
      * 記憶クラスを取得する.
      * @return   記憶クラス : sclass
      */
     public SclassAttribute getSclass() {
         VariableAttribute attr = (VariableAttribute) this.attribute;
         return attr.getSclass();

     }

     /**
      * 複文(空文)の開始を行う
      * @param lineInfo    コード行情報
      */
     protected void startCompoundBlock(CodeLine lineInfo) {
         body.startCompoundBlock(lineInfo);
     }


     /**
      * 複文(空文)の終了を行う
      * @param lineInfo    コード行情報
      */
     protected void endCompoundBlock(CodeLine lineInfo) {
         body.endCompoundBlock(lineInfo);
     }
}
