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
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jp.riken.kscope.Application;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.generic.IProcedureItem;
import jp.riken.kscope.language.generic.ProcedureWithNameOnly;
import jp.riken.kscope.language.generic.Procedures;
import jp.riken.kscope.language.utils.LanguageUtils;
import jp.riken.kscope.parser.IAnalyseParser;

/**
 * Fortranプログラムを表現するクラス。 ソースファイルをパースして得られた情報は、全てこのクラス(superクラスも含む)のメソッドを用いて生成する。
 * クラスFortranはProgramUnit、Blockの派生クラスで構成され、
 * オブジェクト生成メソッドは常にcurrentUnit、currentBlockに対して実行される。
 * @version    2015/03/15     探索失敗スタックの追加
 */
public final class Fortran extends Program {
    /** シリアル番号 */
    private static final long serialVersionUID = -5141333793433490902L;
    /** モジュール名を格納するための作業用配列 */
    private String[] moduleName;
    /** フォートランソースファイルリスト */
    private ArrayList<SourceFile> sourceFileList = new ArrayList<SourceFile>();
    /** 宣言探索のための作業用変数 : 探索済みの宣言文 */
    private transient Map<String, Procedure> knownProcedure = new HashMap<String, Procedure>();
    /** 宣言探索のための作業用変数 : 探索失敗の関数*/
    private transient Map<String, ProcedureUsage> unknownProcedureUsage = new HashMap<String, ProcedureUsage>();
    /** キャンセルフラグ */
    private transient boolean cancel = false;


    /**
     *
     * コンストラクタ。
     */
    public Fortran() {
        super();
    }

    /**
     * ソースファイルのリストをセットする。
     *
     * @param files
     *            ソースファイルのリスト
     */
    public void setSourceFileList(ArrayList<SourceFile> files) {
        sourceFileList = files;
    }

    /**
     * ソースファイルのリストを返す.<br/>
     * XMLファイルと対応したソースファイルのみ
     * @return ソースファイルのリスト
     */
    public ArrayList<SourceFile> getSourceFileList() {
        return sourceFileList;
    }


    /**
     * ソースファイルのリストを返す.<br/>
     * プロシージャのソースファイルを含むすべてのソースファイル
     * @return ソースファイルのリスト
     */
    public ArrayList<SourceFile> getProcedureFileList() {
        ArrayList<SourceFile> moduleFileList = getSourceFileList();
        ArrayList<SourceFile> list = new ArrayList<SourceFile>();
        list.addAll(moduleFileList);
        Collection<Module> mods = this.getModules().values();
        for (Module mod : mods) {
            List<IBlock> procs = mod.getChildren();
            if (procs != null) {
                for (IBlock proc : procs) {
                    SourceFile procFile = proc.getStartCodeLine().getSourceFile();
                    if (procFile != null && !list.contains(procFile)) {
                        list.add(procFile);
                    }
                }
            }
        }
        if (list.size() <= 0) return null;
        return list;
    }


    // ++++++++++++++++++++++++++++++++++++++++++//
    // interface method //
    // ++++++++++++++++++++++++++++++++++++++++++//

    // ---------------ProgramUnit----------------//
    /**
     * サブルーチン(Procedure)をcurrentUnitのchildとして生成する。 サブルーチンが引数を持たない場合に用いる。
     * 新たにinitメソッドを実行する前にendSubroutineを呼ばなければならない。
     *
     * @param subName
     *            サブルーチン名。
     */
    public void initSubroutine(String subName) {
        super.init_procedure("subroutine", subName);
    }

    /**
     * サブルーチン(Procedure)をcurrentUnitのchildとして生成する。 サブルーチンが引数を持つ場合に用いる。
     * 新たにinitメソッドを実行する前にendSubroutineを呼ばなければならない。
     *
     * @param subName
     *            サブルーチンの名前
     * @param args
     *            サブルーチンの引数
     */
    public void initSubroutine(String subName, String[] args) {
        super.init_procedure("subroutine", subName, args);
    }

    /**
     * currentUnitを親に変更する。
     */
    public void endSubroutine() {
        super.end_procedure();
    }

    /**
     * 関数宣言を開始する。
     *
     * @param sub_name
     *            関数名
     */
    public void init_function(String sub_name) {
        super.init_procedure("function", sub_name);
    }

    /**
     * 関数宣言を開始する。
     *
     * @param sub_name
     *            関数名
     * @param args
     *            仮引数
     */
    public void init_function(String sub_name, String[] args) {
        super.init_procedure("function", sub_name, args);
    }

    /**
     * 関数宣言を開始する。
     *
     * @param sub_name        関数名
     * @param args            仮引数
     */
    public void init_function(String sub_name, Variable[] args) {
        super.init_procedure("function", sub_name, args);
    }

    /**
     * 関数を終了する。
     */
    public void end_function() {
        super.end_procedure();
    }

    // ++++++++++++++++++++++++++++++++++++++++++//
    // analyse method //
    // ++++++++++++++++++++++++++++++++++++++++++//

    /**
     * プログラム全体に対して解析を実行し、宣言と呼び出しを対応付ける。
     *
     * @param parser
     *            GUI制御用クラス
     */
    public void analyseDB(IAnalyseParser parser) {
        moduleName = get_module_name();
        parser.firePropertyChange("status_message", null, "Analyse calls");
        parser.firePropertyChange("status_sub_message", null, "parsing...");
        parser.firePropertyChange("prograss_maxvalue", null, moduleName.length);
        for (int i = 0; i < moduleName.length; i++) {
            Module current_module = module(moduleName[i]);
            List<IBlock> subs = current_module.getChildren();
            analyseDBInUnit(subs);
            parser.firePropertyChange("prograss_string", null,
                    String.valueOf(i));
            parser.firePropertyChange("prograss_value", null, i);
        }
        parser.firePropertyChange("status_sub_message", null, "done");
        parser.firePropertyChange("prograss_clear", null, null);
    }

    /**
     * プログラム全体に対して解析を実行し、宣言と呼び出しを対応付ける。
     */
    public void analyseDB() {
        Application.status.setMessageStatus("analysys database...");
        moduleName = this.get_module_name();
        for (int i = 0; i < moduleName.length; i++) {
            // キャンセルチェック
            if (isCancel()) break;
            Application.status.setMessageStatus("analysys database..." + moduleName[i]);
            Module current_module = module(moduleName[i]);
            List<IBlock> subs = new ArrayList<IBlock>();
            subs.add(current_module);
            this.analyseDBInUnit(subs);
        }

        // 作業領域をクリアする
        this.clearKnownProcedure();
        this.clearUnknownProcedureUsage();

        Application.status.setMessageStatus("analysys database...done");
    }

    /**
     * 渡された手続きについて、宣言と呼び出しを対応付ける。
     *
     * @param subs
     *            手続きの配列
     */
    private void analyseDBInUnit(List<IBlock> subs) {
        if (this.knownProcedure == null) {
            this.knownProcedure = new HashMap<String, Procedure>();
        }
        for (IBlock block : subs) {
            // キャンセルチェック
            if (isCancel()) break;
            if (!(block instanceof ProgramUnit)) continue;
            if (block instanceof Procedure) {
                Procedure sub = (Procedure)block;
                List<ProcedureUsage> calls = sub.getCalls();
                this.knownProcedure.clear();
                if (calls != null) {
                    for (ProcedureUsage call:calls) {
                        if (call.isIntrinsic()) {
                            // TODO INTRINSIC関数に対する処理。現状では不要だが何らかの扱いも可能だと思われる
                        } else {
                            this.searchCallDeclaration(sub, call);
                        }
                    }
                }
            }

            ProgramUnit prog = (ProgramUnit)block;
            Set<String> defSet = prog.getVariableDefinitionMap().keySet();
            Set<Variable> newSet = new HashSet<Variable>(prog.getVariableMap().keySet());
            // newSet.addAll(defSet);
            for (Variable var : newSet) {
                searchVariableDefinition(prog, var);
            }
            if (prog.getChildren().size() > 0) {
                analyseDBInUnit(prog.getChildren());
            }

            // add at 2013/03/01 by @hira
            // 変数に変数定義をセットする
            Set<Variable> vars = prog.getAllVariables();
            if (vars != null) {
                for (Variable var : vars) {
                    String varname = var.getName();
                    this.searchVariableDefinition(prog, var);
                }
            }

            prog.setVariableDefinitions();

        }
    }

    /**
     * 関数呼び出しの宣言を探索して対応付ける。
     *
     * @param pu
     *            関数呼び出しが属するプログラム単位
     * @param call
     *            関数呼び出し
     */
    private void searchCallDeclaration(ProgramUnit pu, ProcedureUsage call) {
        ProgramUnit me = pu;
        ProgramUnit current = me;
        String callName = call.getCallName().toLowerCase();

        if (callName == null) {
            return;
        }

        // 定義先が既知かチェック
        if (this.knownProcedure.containsKey(callName)) {
            call.setCallDefinition(this.knownProcedure.get(callName));
            return;
        }

        // 親プログラム単位に対象を移しながら探索する
        String changeName = callName;
        while (current != null) {
            // currentに対してInterface文の探索を実行する。
            if (callName.equalsIgnoreCase(call.getCallName())) {
                changeName = this.searchCallDeclarationForInterface(current, call);
            }
            if (!(callName.equalsIgnoreCase(changeName))) {
                callName = changeName;
                current = me; // interface文が見つかったので、新たな名前で手続きを探索し直す
            }
            // currentの内部副プログラムを探す
            if (current.getChildren().size() > 0) {
                List<IBlock> children = current.getChildren();
                for (IBlock block: children) {
                    if (!(block instanceof Procedure)) continue;
                    Procedure child = (Procedure)block;
                    if (child.get_name().equalsIgnoreCase(callName)) {
                        knownProcedure.put(callName, child);
                        call.setCallDefinition(child);
                        return;
                    }
                }
            }

            // currentに対してUse文の探索を実行する。
            changeName = this.searchCallDeclarationForUse(current, call, callName);
            if (changeName != null) {
                if (call.getCallDefinition() != null) { return; }
                if (!(callName.equalsIgnoreCase(changeName))) {
                    callName = changeName;
                    current = me; // interface文が見つかったので、新たな名前で手続きを探索し直す
                    continue;
                }
            }
            // motherにcurrentを移す
            current = current.get_mother();
        }

        // NO_MODULEにあるサブルーチンを探索
        Procedure[] subs = module("NO_MODULE").get_procedures();
        for (int i = 0; i < subs.length; i++) {
            if (subs[i].get_name().equalsIgnoreCase(callName)) {
                knownProcedure.put(callName, subs[i]);
                call.setCallDefinition(subs[i]);
                return;
            }
        }
    }

    /**
     * 指定されたプログラム単位のinterface文を探索する。
     * @param pu 探索対象のプログラム単位
     * @param call 宣言を探索中の手続き呼び出し
     * @return 総称名から変換された固有手続き名。無ければ元の名前を返す。
     */
    private String searchCallDeclarationForInterface(ProgramUnit pu,
            ProcedureUsage call) {
        String callName = call.getCallName();
        List<Procedures> interfaceList = pu.getInterfaceList();
        for (Procedures generic: interfaceList) {
            // 無名interfaceのスキップ
            if (generic.getName() != null) {
                if (generic.getName().equalsIgnoreCase(callName)) {
                    Set<IProcedureItem> items = generic.getProcedures();
                    // module procedure文の宣言対応を探索する
                    Procedure declaration = null;
                    for (IProcedureItem item: items) {
                        if (item instanceof ProcedureWithNameOnly) {
                            // add at 2013/02/01 by @hira
                            ProcedureWithNameOnly modProc = (ProcedureWithNameOnly) item;
                            if (modProc.getDeclaration() == null) {
                                String modProcName = modProc.getName();
                                modProc.setDeclaration(this.searchModuleProcedureDeclaration(modProcName, pu));
                                // 対応した手続の仮引数だけ宣言を探索する
                                Variable[] args = modProc.getDeclaration().get_args();
                                for (int i = 0; i < args.length; i++) {
                                    this.searchVariableDefinition(modProc.getDeclaration(), args[i]);
                                }
                            }
                            // add at 2013/02/01 by @hira
                            if (items.size() == 1) {
                                declaration = modProc.getDeclaration();
                            }
                            else {
                                if (declaration == null) {
                                    declaration = modProc.getDeclaration();
                                }
                            }
                        }
                    }

                    // add at 2013/02/01 by @hira
                    if (declaration != null) {
                        call.setCallDefinition(declaration);
                        return callName;
                    }
                    callName = generic.getActualCallName(call.getArguments());
                }
            }
        }
        return callName;
    }
    /**
     * module procedure文に対応する手続宣言を探索する。
     * @param name module procedureの名前
     * @param pu interface文を持つプログラム単位
     * @return module procedureが指す手続。無ければnullを返す。
     */
    private Procedure searchModuleProcedureDeclaration(String name, ProgramUnit pu) {
        ProcedureUsage call = new ProcedureUsage(name, null);
        this.searchCallDeclaration(pu, call);
        if (call.getCallDefinition() == null) {
            return null;
        } else {
            return call.getCallDefinition();
        }
    }

    /**
     * 指定されたプログラム単位のuse文,interface文に対して手続き宣言を探索する。
     * @param pu 宣言保持候補のプログラム単位
     * @param call 宣言を探索中の手続き呼び出し
     * @param callName 探索している手続きの名前
     * @return 総称名から変換された固有手続き名。無ければ元の名前を返す。
     */
    private String searchCallDeclarationForUse(ProgramUnit pu,
            ProcedureUsage call, String callName) {
        String changeName = callName;
        // callの名前とcallNameが一致している場合はpuのinterface文を探す
        if (call.getCallName().equalsIgnoreCase(callName)) {
            changeName = this.searchCallDeclarationForInterface(pu, call);
            if (changeName != null) {
                if (!(changeName.equalsIgnoreCase(call.getCallName()))) {
                    return changeName; // 探索手続き名が変更されたので処理を中断する
                }
                // add by @hira at 2013/02/01
                if (call.getCallDefinition() != null) {
                    return changeName;
                }
            }
        }

        // Use文を探索する
        for (UseState useEle : pu.getUseList()) {
            changeName = callName;
            if (useEle.hasOnlyMember() || useEle.hasTranslation(callName)) {
                // modify hasOnlyMemberからtranslationReverseに変更 at 2016/03/28 by @hira
                changeName = useEle.translationReverse(callName);
                if (changeName != null) {
                    Module useModule = module(useEle.getModuleName());
                    if (useModule != null) {
                        // 手続きのチェック
                        Procedure[] procs = useModule.get_procedures();
                        if (procs != null) {
                            for (int i = 0; i < procs.length; i++) {
                                if (procs[i].get_name().equalsIgnoreCase(
                                        changeName)) {
                                    call.setCallDefinition(procs[i]);
                                    knownProcedure.put(changeName, procs[i]);
                                    return changeName;
                                }
                            }
                        }
                    }
                }
            }
            else {
                Module useModule = module(useEle.getModuleName());
                if (useModule != null) {
                    // 手続きのチェック
                    Procedure[] procs = useModule.get_procedures();
                    if (procs != null) {
                        for (int i = 0; i < procs.length; i++) {
                            if (procs[i].get_name()
                                    .equalsIgnoreCase(changeName)) {
                                call.setCallDefinition(procs[i]);
                                knownProcedure.put(changeName, procs[i]);
                                return changeName;
                            }
                        }
                    }
                    changeName = searchCallDeclarationForUse(useModule, call, changeName);
                    if (changeName != null) {
                        if (!(changeName.equalsIgnoreCase(call.getCallName()))
                                || (call.getCallDefinition() != null)) {
                            return changeName; // 探索手続き名が変更された、または宣言が見つかったので処理を中断する
                        }
                    }
                }
            }
        }
        return changeName;
    }

    /**
     * 変数の宣言を探索して対応付ける。
     *
     * @param dec_block
     *            変数が属するプログラム単位
     * @param var            変数
     */
    private void searchVariableDefinition(ProgramUnit dec_block, Variable var) {
        if (dec_block == null) return;
        if (var == null) return;
        String var_name = var.getName();
        String proc_name = dec_block.get_name();
        if (var.getParentStatement() == null) return;
        if (var.getParentStatement().getStartCodeLine() == null) return;

        // 仮引数の場合はプロシージャ内のみから検索する
        if (dec_block instanceof Procedure
            && ((Procedure)dec_block).get_args() != null) {
            Variable[] args = ((Procedure)dec_block).get_args();
            for (Variable arg : args) {
                if (arg == var) {
                    VariableDefinition varDef = dec_block.get_variable(var_name);
                    dec_block.putVariableMap(var, varDef);
                    return;
                }
            }
        }

        // String var_name = var.getName();
        //  String proc_name = dec_block.get_name();
        IBlock var_parent = var.getParentStatement();
        CodeLine var_lineinfo = var_parent.getStartCodeLine();
        IBlock current = (IBlock)dec_block;
        // 変数が属するProbramUnit
        IBlock var_defblock =var_parent.getScopeDeclarationsBlock();
        if (LanguageUtils.isParentBlock(current, var_defblock)) {
            // 変数が属するProgramUnitがcurrentの子であるので、子から検索
            current = var_defblock;
        }
        while (current != null) {
            if (current instanceof ProgramUnit) {
                // currentの宣言文を探す
                VariableDefinition varDef = ((ProgramUnit)current).get_variable(var_name);
                if (varDef == null) {
                    // 構造体として検索する
                    VariableDefinition[] varDefs = ((ProgramUnit)current).searchVariablesDefinition(var_name);
                    if (varDefs != null && varDefs.length > 0) {
                        varDef = varDefs[0];
                    }
                }
                if (varDef != null && varDef.getStartCodeLine() != null) {
                    CodeLine def_lineinfo = varDef.getStartCodeLine();
                    // 宣言文が変数行番号より大きいことはない
                    if (def_lineinfo.compareTo(var_lineinfo) <= 0) {
                        dec_block.putVariableMap(var, varDef);
                        return;
                    }
                    else if (var_parent.getBlockType() == BlockType.COMMON) {
                        // COMMON文には行番号がない
                        dec_block.putVariableMap(var, varDef);
                        return;
                    }
                    else if (var_lineinfo.getSourceFile() != null
                            && !var_lineinfo.getSourceFile().equals(def_lineinfo.getSourceFile())) {
                        // ソースファイルが異なる。インクルードファイルの場合は追加する
                        dec_block.putVariableMap(var, varDef);
                        return;
                    }
                }
            }

            // currentのuse先を探索
            if (current instanceof ProgramUnit
                    && dec_block instanceof ProgramUnit) {
                if (((ProgramUnit)current).getUseList() != null) {
                    if (searchVariableDefinitionForUse((ProgramUnit)current, var, (ProgramUnit)dec_block)) {
                        return;
                    }
                }
            }

            current = current.getMotherBlock();
        }
        return;
    }

    /**
     * 変数の宣言を、プログラム単位内のUSE文に対して検索し、見つかれば対応付ける。 USE先にさらにUSE文がある場合は再帰的に探索する。
     *
     * @param pu
     *            プログラム単位
     * @param var     変数
     * @param me      変数が属する手続き
     * @return 真偽値。宣言が見つかれば真を返す。
     */
    private boolean searchVariableDefinitionForUse(
            ProgramUnit pu,
            Variable var,
            ProgramUnit me) {
        if (var == null) return false;
        String var_name = var.getName();
        for (UseState useEle : pu.getUseList()) {
            Module useModule = module(useEle.getModuleName());
            if (useModule != null) {
                // rename対応 at 2016/03/28 by @hira
                if (useEle.hasOnlyMember() || useEle.hasTranslation(var_name)) {
                    String transName = useEle.translationReverse(var_name);
                    if (transName == null) continue;    // add by hira at 20160130
                    VariableDefinition vd = useModule.get_variable(transName);
                    if (vd != null) {
                        me.putVariableMap(var, vd);
                        vd.addReferMember(me);
                        return true;
                    }
                    if (useModule.getUseList() != null) {
                        if (searchVariableDefinitionForUse(useModule, var, me)) {
                            return true;
                        }
                    }
                } else {
                    String nm = useEle.getTranslationName(var_name);
                    if (nm != null) {
                        var_name = nm;
                    }
                    VariableDefinition vd = useModule.get_variable(var_name);
                    if (vd != null) {
                        me.putVariableMap(var, vd);
                        vd.addReferMember(me);
                        return true;
                    }
                    if (useModule.getUseList() != null) {
                        if (searchVariableDefinitionForUse(useModule, var, me)) {
                            return true;
                        }
                    }
                }
            }
        }
        return false;
    }

    // ++++++++++++++++++++++++++++++++++++++++++++

    /**
     * 指定した名前のサブルーチンを検索し返す。
     *
     * @param name
     *            サブルーチン名
     * @return 指定した名前のサブルーチン
     */
    public Procedure search_subroutine(String name) {

        if (name == null) {
            return null;
        }

        for (Module md : this.getModules().values()) {
            Procedure proc = search_sub_rec(md, name);
            if (proc != null) {
                return proc;
            }
        }
        return null;
    }

    /**
     * プログラム、モジュールから副プログラムを取得する.
     * @param proc        モジュール
     * @param name        副プログラム名
     * @return            副プログラム
     */
    private Procedure search_sub_rec(ProgramUnit proc, String name) {
        if (proc.is_my_procedure(name)) {
            return (Procedure) (proc.get_child(name));
        } else {
            if (proc.getChildren().size() > 0) {
                Procedure[] proc_child = proc.get_children();
                for (int i = 0; i < proc_child.length; i++) {
                    // return search_sub_rec(proc_child[i], name) ;
                    Procedure sub_proc = search_sub_rec(proc_child[i], name);
                    if (sub_proc != null) {
                        return sub_proc;
                    }
                }
            }
        }
        return null;
    }

    /**
     * CALL文をセットする。
     *
     * @param lineInfo
     *            コード行情報
     * @param label
     *            行ラベル。ない場合はStatement.NO_LABELを渡す。
     * @param subroutineName
     *            CALLサブルーチン名
     * @param arguments
     *            引数のリスト
     * @param intrinsic
     *            組込み関数である場合trueをセットする。組込み関数でなければfalse。
     */
    public void setCall(CodeLine lineInfo, String label, String subroutineName,
            List<Expression> arguments, boolean intrinsic) {
        super.setProcedureUsage(lineInfo, label, subroutineName, arguments,
                intrinsic);
    }

    /**
     * DO文開始行を設定する
     *
     * @param lineInfo
     *            コード行情報
     * @param label
     *            行ラベル.ない場合はStatement.NO_LABELをセットする
     */
    public void start_loop(CodeLine lineInfo, String label) {
        super.start_repetition(lineInfo, label);
    }

    /**
     * DO文開始行を設定する。
     *
     * @param lineInfo
     *            コード行情報
     * @param label
     *            行ラベル.ない場合はStatement.NO_LABELをセットする。
     * @param iterator
     *            ループ制御変数
     * @param initIterator
     *            始値
     * @param endCondition
     *            終値
     * @param step
     *            刻み幅
     */
    public void start_loop(CodeLine lineInfo, String label, Variable iterator,
            Expression initIterator, Expression endCondition, Expression step) {
        super.start_repetition(lineInfo, label, iterator, initIterator,
                endCondition, step);
    }

    /**
     * DO文終了行を設定する。
     *
     * @param lineInfo
     *            コード行情報
     */
    public void end_loop(CodeLine lineInfo) {
        super.end_repetition(lineInfo);
    }

    /**
     * DO文終了行を設定する。
     *
     * @param lineInfo
     *            コード行情報
     * @param label
     *            行ラベル
     */
    protected void end_loop(CodeLine lineInfo, String label) {
        super.end_repetition(lineInfo, label);
    }

    /**
     * CONTINUE文を設定する。
     *
     * @param lineInfo
     *            コード行情報
     * @param label
     *            行ラベル
     */
    @Override
    public void set_continue(CodeLine lineInfo, String label) {
        super.set_continue(lineInfo, label);
    }

    /**
     * シャローコピーを行う.
     * @param fortran        コピー元データベース
     */
    public void copyShallow(Fortran fortran) {
        this.moduleName = fortran.moduleName;
        this.sourceFileList = fortran.sourceFileList;
        super.copyShallow((Program)fortran);
    }

    /**
     * スレッドの実行がキャンセルであるかチェックする
     * @return    true=キャンセル
     */
    public boolean isCancel() {
        return this.cancel;
    }

    /**
     * キャンセルフラグを設定する。
     * @param flag    キャンセルフラグ
     */
    public void setCancel(boolean flag) {
        this.cancel = flag;
    }

    /**
     * 宣言探索のための作業用変数 : 探索済みの宣言文に関数名が存在するかチェックする.
     * @param call_name        検索関数名
     * @return            true = 存在する.
     */
    private boolean containsKnownProcedure(String call_name) {
        Procedure proc = this.getKnownProcedure(call_name);
        return (proc != null);
    }

    /**
     * 宣言探索のための作業用変数 : 探索済みの宣言文から関数名と同じ関数宣言文を取得する.
     * @param call_name        検索関数名
     * @return            探索済み:関数宣言文
     */
    private Procedure getKnownProcedure(String call_name) {
        if (call_name == null) return null;
        if (this.knownProcedure == null) return null;
        Set<String> keys = this.knownProcedure.keySet();
        for(String key : keys){
            Procedure proc = this.knownProcedure.get(key);
            if (proc == null) continue;
            String proc_name = proc.get_name();
            if (call_name.equalsIgnoreCase(proc_name)) {
                return proc;
            }
        }
        return null;
    }


    /**
     * 宣言探索のための作業用変数 : 探索済みの宣言文をクリアする.
     */
    private void clearKnownProcedure() {
        if (this.knownProcedure == null) {
            this.knownProcedure = new HashMap<String, Procedure>();
        }
        this.knownProcedure.clear();
        return;
    }

    /**
     * 宣言探索のための作業用変数 : 探索済みの宣言文を追加する.
     */
    private void putKnownProcedure(String func_name, Procedure proc) {
        if (this.knownProcedure == null) {
            this.knownProcedure = new HashMap<String, Procedure>();
        }
        if (this.containsKnownProcedure(func_name)) {
            return;
        }
        this.knownProcedure.put(func_name, proc);
        return;
    }


    /**
     * 宣言探索のための作業用変数 : 探索失敗の関数に関数名が存在するかチェックする.
     * @param call_name        検索関数名
     * @return            true = 存在する.
     */
    private boolean containsUnknownProcedureUsage(String call_name) {
        ProcedureUsage proc = this.getUnknownProcedureUsage(call_name);
        return (proc != null);
    }

    /**
     * 宣言探索のための作業用変数 : 探索失敗の関数から関数名と同じ関数を取得する.
     * @param call_name        検索関数名
     * @return            探索失敗:関数文
     */
    private ProcedureUsage getUnknownProcedureUsage(String call_name) {
        if (call_name == null) return null;
        if (this.unknownProcedureUsage == null) return null;
        Set<String> keys = this.unknownProcedureUsage.keySet();
        for(String key : keys){
            ProcedureUsage proc = this.unknownProcedureUsage.get(key);
            if (proc == null) continue;
            String proc_name = proc.getCallName();
            if (call_name.equalsIgnoreCase(proc_name)) {
                return proc;
            }
        }
        return null;
    }


    /**
     * 宣言探索のための作業用変数 : 探索失敗の関数をクリアする.
     */
    private void clearUnknownProcedureUsage() {
        if (this.unknownProcedureUsage == null) {
            this.unknownProcedureUsage = new HashMap<String, ProcedureUsage>();
        }
        this.unknownProcedureUsage.clear();
        return;
    }

    /**
     * 宣言探索のための作業用変数 : 探索失敗の関数を追加する.
     */
    private void putUnknownProcedureUsage(String func_name, ProcedureUsage proc) {
        if (this.unknownProcedureUsage == null) {
            this.unknownProcedureUsage = new HashMap<String, ProcedureUsage>();
        }
        if (this.containsUnknownProcedureUsage(func_name)) {
            return;
        }

        // 追加
        this.unknownProcedureUsage.put(func_name, proc);
        return;
    }

}
