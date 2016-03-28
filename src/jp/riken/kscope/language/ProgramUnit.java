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

import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
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
import jp.riken.kscope.language.utils.LanguageUtils;

/**
 * プログラム単位を表現するクラス。
 *
 * @author RIKEN
 *
 */
public abstract class ProgramUnit implements Serializable, IInformation, IBlock, IDeclarations {
    /** シリアル番号 */
    private static final long serialVersionUID = -4778301667477615867L;
    /** プログラム名、モジュール名、サブルーチン名、関数名 */
    private String name;
    /** プログラム単位の種類:program, module, subroutine, function */
    private String type;
    /** 親プログラム */
    private ProgramUnit mother;
    /** 開始行情報 */
    private Statement start;
    /** 終了行情報 */
    private Statement end;
    /**
     * プログラム単位の属性.<br/>
     * 現状、"recursive"のみ。{<"recursive", "">}
     */
    private Map<String, Object> attributes = new HashMap<String, Object>();
    /** 副プログラム単位.*/
    private Map<String, Procedure> children;
    /** INTERFACE文リスト */
    private List<Procedures> interfaceList;
    /** USE文リスト */
    private List<UseState> useList;
    /** 構造体構造定義リスト */
    private List<Type> typeDefinitions = new ArrayList<Type>();
    /** Equivalence文リスト */
    private List<Equivalence> equivalenceList;
    /** COMMON文リスト */
    private List<Common> commonList;
    /** DATA文リスト */
    private List<Data> dataList;
    /** Directive文リスト */
    private List<Directive> directiveList;
    /** external宣言された関数名とデータ型のマップ */
    private Map<String, IVariableType> externalFunctionList;
    /**
     * 変数宣言文リスト.<br/>
     * Map\<変数名, 変数定義\> <br/>
     * HashMapからLinkedHashMapへ変更：挿入順を保持する為 at 2010/03/03 by @hira
     */
    public Map<String, VariableDefinition> variables = new LinkedHashMap<String, VariableDefinition>(); // defined
                                                                                                        // variables
                                                                                                        // in
                                                                                                        // this
                                                                                                        // program
                                                                                                        // unit
    /** 付加情報 */
    private TextInfo information = null;
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
     * @param tp プログラム単位の種類
     * @param nm 名前
     */
    public ProgramUnit(String tp, String nm) {
        type = tp;
        name = nm;
    }

    /**
     * プログラム単位に属する構造体構造定義のリストを返す。
     *
     * @return 構造体構造定義のリスト。無ければ空のリストを返す。
     */
    public List<Type> getTypeList() {
        return this.typeDefinitions;
    }

    /**
     * 構造体定義を取得する
     * @param name        構造体名
     * @return        構造体定義
     */
    public Type getType(String name) {
        if (this.typeDefinitions == null) return null;
        if (name == null) return null;

        for (Type type : this.typeDefinitions) {
            String typename = type.getName();
            if (this.equalsDeclarationName(name,typename)) {
                return type;
            }
        }
        return null;
    }

    /**
     * Data文を追加する。
     *
     * @param dt Dataクラス
     */
    public void addData(Data dt) {
        if (this.dataList == null) {
            this.dataList = new ArrayList<Data>();
        }
        this.dataList.add(dt);
    }

    /**
     * Directive文を追加する。
     *
     * @param di Directiveクラス
     */
    public void addDirective(Directive di) {
        if (this.directiveList == null) {
            this.directiveList = new ArrayList<Directive>();
        }
        this.directiveList.add(di);
    }

    /**
     * Equivalence文を追加する。
     *
     * @param eq
     *            　Equivalenceクラス
     */
    public void addEquivalence(Equivalence eq) {
        if (this.equivalenceList == null) {
            this.equivalenceList = new ArrayList<Equivalence>();
        }
        this.equivalenceList.add(eq);
    }

    /**
     * Common文を追加する。
     *
     * @param cm
     *            Commonクラス
     */
    public void addCommon(Common cm) {
        if (this.commonList == null) {
            this.commonList = new ArrayList<Common>();
        }
        this.commonList.add(cm);
    }

    /**
     * INTERFACE宣言を追加する。
     *
     * @param blk
     *            interfaceクラス
     */
    public void addInterface(Procedures blk) {
        if (this.interfaceList == null) {
            this.interfaceList = new ArrayList<Procedures>();
        }
        this.interfaceList.add(blk);
    }
    /**
     * INTERFACE文リストを返す。
     * @return INTERFACE文リスト。無ければ空のリストを返す。
     */
    public List<Procedures> getInterfaceList() {
        if (this.interfaceList == null) {
            return new ArrayList<Procedures>();
        }
        return this.interfaceList;
    }

    @Override
    public String toString() {
        // delete by @hira at 2013/03/01
//        String info = "";
//        if (this.getInformation() != null) {
//            if (!(this.getInformation().getContent().equals(""))) {
//                info = "[ ! ] ";
//            }
//        }
        // return (info + this.toStringBase());
        return this.toStringBase();
    }

    /**
     * プログラム名、モジュール名、サブルーチン名、関数名を取得する.
     * @return        プログラム名、モジュール名、サブルーチン名、関数名
     */
    protected String toStringBase() {
        return this.name;
    }

    /**
     * ファイルパス（パスを含むファイル名）の取得。<br>
     *
     * @return ファイルパス
     */
    public String getFilePath() {
        String result;
        try {
            result = this.start.lineInfo.getSourceFile().getPath();
        } catch (Exception e) {
            result = null;
        }
        return result;
    }

    /**
     * 親プログラムを設定する.
     * @param mam        親プログラム
     */
    protected void set_mother(ProgramUnit mam) {
        mother = mam;
    }

    /**
     * プログラムの属性を追加する.
     * @param attribute_name        属性名
     */
    protected void put_attribute(String attribute_name) {
        attributes.put(attribute_name, "");
    }

    /**
     * プログラムの属性を追加する.
     * @param attribute_name        属性名
     * @param attribute_value        属性値
     */
    protected void put_attribute(String attribute_name, Object attribute_value) {
        attributes.put(attribute_name, attribute_value);
    }

    /**
     * 副プログラムを追加する.
     * @param child
     */
    protected void put_child(Procedure child) {
        if (this.children == null) {
            this.children = new LinkedHashMap<String, Procedure>();
        }
        children.put(child.get_name(), child);
    }

    /**
     * 副プログラムを追加する.
     * @param type_name        プログラム単位の種類
     * @param proc_name        プログラム名
     */
    protected void set_child(String type_name, String proc_name) {
        Procedure proc = new Procedure(type_name, proc_name);
        put_child(proc);
    }

    /**
     * 変数宣言文を追加する.
     * @param vr        変数宣言文
     */
    protected void put_variable(VariableDefinition vr) {
        vr.setMother(this);
        variables.put(vr.get_name(), vr);
    }

    /**
     * 変数宣言文を追加する.
     * @param varName        変数名
     */
    protected void new_variable_def(String varName) {
        VariableDefinition varDef = new VariableDefinition(varName);
        this.put_variable(varDef);
    }

    /**
     * 指定した属性を持っているか判定する。
     * @param attribute_name 属性の名前
     * @return 真偽値。属性を持っていれば真
     */
    public boolean has_attribute(String attribute_name) {
        return attributes.containsKey(attribute_name);
    }

    /**
     * 副プログラム名が存在するかチェックする.
     * @param name        副プログラム名
     * @return        true=副プログラムに存在する.
     */
    protected boolean is_my_procedure(String name) {
        if (this.children != null) {
            return children.containsKey(name);
        }
        return false;
    }

    /**
     * モジュール名、サブルーチン名、関数名を返す。
     * @return モジュール名、サブルーチン名、関数名
     */
    public String get_name() {
        return this.name;
    }

    /**
     * プログラム単位の種類を返す。
     * @return プログラム単位の種類
     */
    public String get_type() {
        return type;
    }

    /**
     * 開始statementを返す。
     * @return 開始statement
     */
    public Statement get_start() {
        return (start);
    }

    /**
     * 終了statementを返す。
     * @return 終了statement
     */
    public Statement get_end() {
        return (end);
    }

    /**
     * 親プログラム単位を返す。
     * @return 親プログラム単位。いなければnullを返す
     */
    public ProgramUnit get_mother() {
        return mother;
    }

    /**
     * 副プログラム単位の配列を返す。
     * @return 副プログラム単位の配列。無ければnullを返す。
     */
    public Procedure[] get_children() {
        if (this.children == null) {
            return null;
        }
        return children.values().toArray(new Procedure[0]);
    }
    /**
     * 副プログラム単位を取得する。
     * @return 副プログラム単位のリスト。無ければ空のリストを返す。
     */
    public List<IBlock> getChildren() {
        List<IBlock> list = new ArrayList<IBlock>();
        if (this.children == null) {
            return list;
        }
        if (this.children.values().size() > 0) {
            list.addAll(this.children.values());
        }
        return list;
    }

    /**
     * 副プログラムを取得する.
     * @param name        副プログラム名
     * @return            副プログラム
     */
    protected ProgramUnit get_child(String name) {
        if (this.children == null) {
            return null;
        }
        return children.get(name);
    }

    /**
     * 副プログラム数を取得する.
     * @return        副プログラム数
     */
    protected int get_num_of_child() {
        if (this.children == null) {
            return 0;
        }
        return children.size();
    }

    /**
     * 副プログラム名のリストを取得する.
     * @return        副プログラム名のリスト
     */
    protected String[] get_child_name() {
        if (this.children == null) {
            return null;
        }
        return children.keySet().toArray(new String[children.size()]);
    }

    /**
     * プログラム属性値を取得する.
     * @param attribute_name        属性名
     * @return            属性値
     */
    protected Object get_attribute(String attribute_name) {
        return attributes.get(attribute_name);
    }

    /**
     * 指定した名前の変数宣言を返す。
     * @param var_name 変数名
     * @return 変数宣言。無ければnullを返す。
     */
    @Override
    public VariableDefinition get_variable(String var_name) {
        String name = this.transferDeclarationName(var_name);
        return variables.get(name);
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
     * 開始コード行情報を設定する。
     *
     * @param lineInfo
     *            開始コード行情報
     */
    public void set_start(CodeLine lineInfo) {
        start = new Statement(lineInfo);
    }

    /**
     * 終了コード行情報を設定する。
     *
     * @param lineInfo
     *            終了コード行情報
     */
    public void set_end(CodeLine lineInfo) {
        end = new Statement(lineInfo);
    }

    /**
     * 変数宣言文を設定する。
     *
     * @param varDef
     *            変数宣言文
     */
    public void set_variable_def(VariableDefinition varDef) {
        this.put_variable(varDef);
    }

    /**
     * 変数宣言文を取得する。
     *
     * @return 変数宣言文リスト
     */
    @Override
    public VariableDefinition[] get_variables() {
        if (variables == null)
            return null;

        Set<String> keys = variables.keySet();

        Collection<VariableDefinition> values = variables.values();
        return values.toArray(new VariableDefinition[0]);
    }


    @Override
    public void setInformation(TextInfo info) {
        this.information = info;
    }

    /**
     * 付加情報を取得する
     * @return        付加情報
     */
    @Override
    public TextInfo getInformation() {
        return this.information;
    }

    /**
     * 開始行番号情報を取得する
     * @return        開始行番号情報
     */
    @Override
    public CodeLine getStartCodeLine() {
        if (start == null) return null;
        return start.lineInfo;
    }

    /**
     * 終了行番号情報を取得する
     * @return        終了行番号情報
     */
    @Override
    public CodeLine getEndCodeLine() {
        if (end == null) return null;
        return end.lineInfo;
    }

    /**
     * TYPE宣言を登録する。
     *
     * @param tp
     */
    protected void addTypeDefinition(Type tp) {
        this.typeDefinitions.add(tp);
        tp.setMotherBlock(this);
    }

    /**
     * USE文のリストを返す。
     * @return USE文のリスト。無ければ空のリストを返す。
     */
    public List<UseState> getUseList() {
        if (this.useList == null) {
            return new ArrayList<UseState>();
        }
        return this.useList;
    }

    /**
     * USE文のリストが存在しているかチェックする。
     * @return   true = USE文のリストを持つ。
     */
    public boolean hasUseList() {
        if (this.useList == null) return false;
        if (this.useList.size() <= 0) return false;
        return true;
    }

    /**
     * DATA文リストを返す。
     * @return        DATA文リスト
     */
    public List<Data> getDataList() {
        return dataList;
    }

    /**
     * Directive文リストを返す。
     *
     * @return Directive文リスト
     */
    public List<Directive> getDirectiveList() {
        return directiveList;
    }

    /**
     * EQUIVALENCE文リストを返す。
     * @return        EQUIVALENCE文リスト
     */
    public List<Equivalence> getEquivalenceList() {
        return equivalenceList;
    }

    /**
     * COMMON文リストを返す。
     * @return        COMMON文リスト.無ければ空のリストを返す。
     */
    public List<Common> getCommonList() {
        if (this.commonList == null) {
            return new ArrayList<Common>();
        }
        return commonList;
    }

    /**
     * 指定した名前の変数をメンバーに含むCOMMON名の名前を返す。
     *
     * @param varname
     *            変数名
     * @return COMMON名。無名COMMONブロックならば"NO_NAME"を返す。該当するCOMMON文が無ければnullを返す。
     */
    public String getCommonName(String varname) {
        String st = null;
        List<Common> comList = this.getCommonList();
        for (Common com : comList) {
            if (com.contains(varname)) {
                st = com.getName();
            }
        }
        return st;
    }

    /**
     * USE文を追加する。
     * @param useline USE文
     */
    protected void addUse(UseState useline) {
        if (this.useList == null) {
            this.useList = new ArrayList<UseState>();
        }
        this.useList.add(useline);
    }

    /**
     * 名前空間（モジュール名.ルーチン名）を取得する。
     *
     * @return 名前空間（モジュール名.ルーチン名）
     */
    @Override
    public String getNamespace() {
        String result = "";
        if (this.mother == null) {
                result = this.name;
        } else {
            result = mother.getNamespace().concat(".");
            result = result.concat(this.name);
        }
        return result;
    }

    /**
     * 開始位置を取得する。
     *
     * @return 開始位置
     */
    @Override
    public int getStartPos() {
        return this.getStartCodeLine().getStartLine();
    }
    /**
     * 開始位置を設定する。
     *
     * @param pos
     *         開始位置
     */
    @Override
    public void setStartPos(int pos) {
        this.getStartCodeLine().setLine(pos);
    }

    /*
     * TODO: 暫定対応。
     *       本当はプログラムの終了はprogram.getEndCodeLine.getEndLineで
     *       取得するか、programのEndCodeLineを削除し、StartCodeLineを
     *       CodeLineと名称変更すべき。要検討。
     */

    /**
     * 終了位置を取得する。
     *
     * @return 終了位置
     */
    @Override
    public int getEndPos() {
        return this.getStartCodeLine().getEndLine();
    }

    /**
     * 終了位置を設定する。
     *
     * @param pos
     *         終了位置
     */
    @Override
    public void setEndPos(int pos) {
        this.getStartCodeLine().setEndLine(pos);
    }

    /**
     * idにマッチした情報ブロックを検索する。
     * @param id
     *          ID
     * @return 見つかった情報ブロック。見つからなかった場合はnullが返ります。
     */
    @Override
    public IInformation findInformationBlockBy(String id) {
        IInformation result = null;

        if (this.getID().equals(id)) {
            result = this;
        }

        if (result == null && this.variables != null) {
            Collection<VariableDefinition> definitions = this.variables.values();
            for (VariableDefinition definition : definitions) {
                result = definition.findInformationBlockBy(id);
                if (result != null) { break; }
            }
        }

        if (result == null && this.getChildren() != null) {
            List<IBlock> procedures = this.getChildren();
            for (IBlock block : procedures) {
                if (!(block instanceof IInformation)) continue;
                result = ((IInformation)block).findInformationBlockBy(id);
                if (result != null) { break; }
            }
        }

        return result;
    }

    /**
     * 付加情報をすべて削除する。
     */
    @Override
    public void clearInformation() {
        this.setInformation(null);
        for (IBlock child : this.getChildren()) {
            if (!(child instanceof IInformation)) continue;
            ((IInformation)child).clearInformation();
        }
        for (VariableDefinition variable : this.variables.values()) {
            variable.clearInformation();
        }
    }

    /**
     * 付加情報ブロックコレクションを生成する。
     *
     * @return 付加情報ブロックコレクション
     */
    public InformationBlocks createInformationBlocks() {
        InformationBlocks result = new InformationBlocks();

        // 自身の付加情報を追加する
        if (this.information != null) {
            InformationBlock block
              = new InformationBlock(this.information, this, this);
            result.add(block);
        }
        // 自身が持つ副プログラム単位の付加情報を追加する
        for (IBlock procedure : this.getChildren()) {
            if (!(procedure instanceof IInformation)) continue;
            result.addAll(((IInformation)procedure).createInformationBlocks());
        }
        // 自身が持つ変数宣言の付加情報を追加する
        if (this.variables != null) {
            for (VariableDefinition variable : this.variables.values()) {
                result.addAll(variable.createInformationBlocks());
            }
        }
        return result;
    }

    /**
     * 名前空間（モジュール名＋ルーチン名）より、ProgramUnitを検索する。
     *
     * @param namespace          検索対象名前空間
     * @return 最初に見つかったProgramUnitオブジェクト
     */
    public ProgramUnit findProgramUnitBy(String namespace) {
        ProgramUnit result = null;
        if (this.equalsDeclarationName(this.getNamespace(), namespace)) {
            result = this;
        }
        if (result == null) {
            List<IBlock> blocks = this.getChildren();
            for (IBlock proc : blocks) {
                if (!(proc instanceof ProgramUnit)) continue;
                result = ((ProgramUnit)proc).findProgramUnitBy(namespace);
                if (result != null) { break; }
            }
        }
        return result;
    }

    /**
     * external宣言された関数名とデータ型のマップを取得する。
     * @return 関数名とデータ型のマップ
     */
    public Map<String, IVariableType> getExternalFunctionList() {
        return externalFunctionList;
    }

    /**
     * external宣言された関数名とデータ型のマップ要素を追加する。
     * @param funcName 関数名
     * @param tp データ型
     */
    public void addExternalFunctionList(String funcName, IVariableType tp) {
        if (this.externalFunctionList == null) {
            this.externalFunctionList = new HashMap<String, IVariableType>();
        }
        if (!(this.externalFunctionList.containsKey(funcName))) {
            this.externalFunctionList.put(funcName, tp);
        }
    }
    /**
     * IDを取得する。
     *
     * @return ID
     */
    @Override
    public String getID() {
        String result = "";
        if (this.mother != null) {
            result = this.mother.getID() + "." + this.toStringBase();
        } else {
            result = this.toStringBase();
        }
        return result;
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
        return defVariableNames;
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
        // 2015/09/01 C言語大文字・小文字区別対応
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
     * @param var           変数
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
     * @param var           変数
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
     *
     * @param var           変数
     */
    @Override
    public void putVariableMap(Variable var) {
        this.putVariableMap(var, null);
    }

    /**
     * プログラム単位で使用される変数名と宣言のマップを追加する。
     *
     * @param var           変数
     * @param varDef            変数宣言
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
        addExpressionToRef(blk, exp);

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
     * 親ブロックを取得する
     * @return        親ブロック
     */
    @Override
    public IBlock getMotherBlock() {
        return this.get_mother();
    }


    /**
     * 子ブロックに同一ProgramUnitが存在するかチェックする
     * @param unit        検索ProgramUnit
     * @return            true=同一ProgramUnitが存在する
     */
    public boolean containsChildren(ProgramUnit unit) {
        if (this == unit) return true;
        String thisID = this.getID();
        String unitID = unit.getID();
        if (this.equalsDeclarationName(thisID, unitID)) {
            return true;
        }
        if (this.children == null) return false;
        boolean result = false;
        for (String key : this.children.keySet()) {
            ProgramUnit childrenUnit = this.children.get(key);
            if (childrenUnit == null) continue;
            result = childrenUnit.containsChildren(unit);
            if (result) {
                return result;
            }
        }
        return result;
    }

    /**
     * 同一ProgramUnitであるかチェックする.
     * 副プログラム単位(children)と変数宣言(variables)のみチェックする.
     * 付加情報の差替用であるので、createInformationBlocksにて取得ブロックのみチェックする.
     * @param unit        モジュール、サブルーチン
     * @return        true=一致
     */
    public boolean equalsBlocks(ProgramUnit unit) {
        if (unit == null) return false;

        // モジュール、サブルーチン名
        if (this.name != null) {
            if (!this.equalsDeclarationName(this.name, unit.name)) {
                return false;
            }
        }
        // タイプ
        if (this.type != null) {
            if (!this.equalsDeclarationName(this.type, unit.type)) {
                return false;
            }
        }
        // 副プログラム
        if (this.children != null && unit.children != null) {
            if (this.children.size() != unit.children.size()) {
                return false;
            }
        }
        else if (!(this.children == null && unit.children == null)) {
            return false;
        }
        if (this.children != null && unit.children != null) {
            for (String key : this.children.keySet()) {
                Procedure srcProc = this.children.get(key);
                Procedure destProc = unit.children.get(key);
                if (srcProc == null || destProc == null) {
                    return false;
                }
                if (!srcProc.equalsBlocks(destProc)) {
                    return false;
                }
            }
        }


        // 変数宣言文
        if (this.variables != null && unit.variables != null) {
            if (this.variables.size() != unit.variables.size()) {
                return false;
            }
        }
        else if (!(this.variables == null && unit.variables == null)) {
            return false;
        }
        if (this.variables != null && unit.variables != null) {
            for (String key : this.variables.keySet()) {
                VariableDefinition srcVar = this.variables.get(key);
                VariableDefinition destVar = unit.variables.get(key);
                if (srcVar == null || destVar == null) {
                    return false;
                }
                if (!srcVar.equalsBlocks(destVar)) {
                    return false;
                }
            }
        }

        return true;
    }

    /**
     * 同一ブロックを取得する
     * @param block            IInformationブロック
     * @return        同一ブロック
     */
    public IInformation findBlock(IInformation block) {
        if (block == null) return null;
        String id = block.getID();
        return this.findInformationBlockBy(id);
    }

    /**
     * 子ブロック、変数宣言が存在するかチェックする.
     * @return        true=空モジュール、サブルーチン
     */
    public boolean isEmpty() {
        if (this.children != null && this.children.size() > 0) {
            return false;
        }
        if (this.variables != null && this.variables.size() > 0) {
            return false;
        }
        return true;
    }


    /**
     * 同一付加情報ブロックを検索する
     * @param block            IInformationブロック
     * @return        同一ブロック
     */
    public IInformation[] searchInformationBlocks(IInformation block) {
        if (block == null) return null;

        List<IInformation> list = new ArrayList<IInformation>();
        if (block instanceof ProgramUnit) {
            if (this.equalsBlocks((ProgramUnit)block)) {
                list.add(this);
            }
        }

        // 変数宣言文
        if (this.variables != null) {
            Collection<VariableDefinition> definitions = this.variables.values();
            for (VariableDefinition definition : definitions) {
                IInformation[] infos = definition.searchInformationBlocks(block);
                if (infos != null) {
                    list.addAll(Arrays.asList(infos));
                }
            }
        }

        // サブルーチン
        if (this.getChildren() != null) {
            List<IBlock> child_blocks = this.getChildren();
            for (IBlock child_block : child_blocks) {
                if (!(child_block instanceof IInformation)) continue;
                IInformation[] infos = ((IInformation)child_block).searchInformationBlocks(block);
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
     * 同一ブロック階層であるかチェックする.
     * @param unit        チェック対象programUnit
     * @return   true=一致
     */
    public boolean equalsLayout(ProgramUnit unit) {
        // モジュール、サブルーチン名
        if (this.name != null) {
            if (!this.equalsDeclarationName(this.name, unit.name)) {
                return false;
            }
        }
        // タイプ
        if (this.type != null) {
            if (!this.equalsDeclarationName(this.type, unit.type)) {
                return false;
            }
        }

        // 副プログラム
        if (this.children != null && unit.children != null) {
            if (this.children.size() != unit.children.size()) {
                return false;
            }
        }
        else if (!(this.children == null && unit.children == null)) {
            return false;
        }
        if (this.children != null && unit.children != null) {
            for (String key : this.children.keySet()) {
                Procedure srcProc = this.children.get(key);
                Procedure destProc = unit.children.get(key);
                if (srcProc == null || destProc == null) {
                    return false;
                }
                if (!srcProc.equalsLayout(destProc)) {
                    return false;
                }
            }
        }

        // ブロック階層チェックの場合は変数宣言文はチェックしない。

        return true;
    }

    /**
     * 構造IDを取得する。
     *
     * @return 構造ID
     */
    @Override
    public String getLayoutID() {
        return getID();
    }

    /**
     * layoutIDにマッチした構造ブロックを検索する。
     * @param id    layoutID
     * @return 見つかった構造ブロック
     */
    public IInformation findInformationLayoutID(String id) {
        if (id == null || id.isEmpty()) return null;
        IInformation result = null;
        if (this.equalsDeclarationName(this.getLayoutID(), id)) {
            result = this;
        }

        if (result == null && this.getChildren() != null) {
            List<IBlock> blocks = this.getChildren();
            for (IBlock block : blocks) {
                if (!(block instanceof IInformation)) continue;
                result = ((IInformation)blocks).findInformationLayoutID(id);
                if (result != null) { break; }
            }
        }

        return result;
    }

    /**
     * 行番号のブロックを検索する
     * @param line            行番号
     * @return        行番号のブロック
     */
    public IBlock[] searchCodeLine(CodeLine line) {
        if (line == null) return null;

        List<IBlock> list = new ArrayList<IBlock>();
        CodeLine thisstart = this.getStartCodeLine();
        CodeLine thisend = this.getEndCodeLine();
        if ( line.isOverlap(thisstart, thisend) ) {
            ;  // nothing
        }
        else {
            // このブロックが範囲外であるので、子ブロックも範囲外
            return null;
        }

        // 変数宣言文
        if (this.variables != null) {
            Collection<VariableDefinition> definitions = this.variables.values();
            for (VariableDefinition definition : definitions) {
                IBlock[] blocks = definition.searchCodeLine(line);
                if (blocks != null) {
                    list.addAll(Arrays.asList(blocks));
                }
            }
        }

        // サブルーチン
        if (this.getChildren() != null) {
            List<IBlock> child_blocks = this.getChildren();
            for (IBlock procedure : child_blocks) {
                IBlock[] blocks = procedure.searchCodeLine(line);
                if (blocks != null) {
                    list.addAll(Arrays.asList(blocks));
                }
            }
        }
        if (list.size() <= 0) {
            return null;
        }

        return list.toArray(new IBlock[0]);
    }

     /**
      * 変数リストを取得する.
      */
     @Override
     public Set<Variable> getAllVariables() {
        // サブルーチン
         Set<Variable> list = new HashSet<Variable>();
        if (this.getChildren() != null) {
            List<IBlock> blocks = this.getChildren();
            for (IBlock block : blocks) {
                Set<Variable> vars = block.getAllVariables();
                if (vars != null) {
                    list.addAll(vars);
                }
            }
        }
        if (list.size() <= 0) return null;

        return list;
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
     * ファイルタイプ（C言語、Fortran)を取得する.
     * @return        ファイルタイプ（C言語、Fortran)
     */
    @Override
    public jp.riken.kscope.data.FILE_TYPE getFileType() {
        jp.riken.kscope.data.FILE_TYPE type = jp.riken.kscope.data.FILE_TYPE.UNKNOWN;
        if (this.mother != null) {
            type = this.mother.getFileType();
        }
        if (type != jp.riken.kscope.data.FILE_TYPE.UNKNOWN) {
            return type;
        }

        if (start == null) return type;
        if (start.lineInfo == null) return type;
        if (start.lineInfo.getSourceFile() == null) return type;

        return this.start.lineInfo.getSourceFile().getFileType();
    }


    /**
     * ファイルタイプがC言語であるかチェックする.
     * @return         true = C言語
     */
    public boolean isClang() {
        jp.riken.kscope.data.FILE_TYPE type = this.getFileType();
        if (type == jp.riken.kscope.data.FILE_TYPE.UNKNOWN) return false;
        if (type == jp.riken.kscope.data.FILE_TYPE.XCODEML_XML) return false;
        if (type == jp.riken.kscope.data.FILE_TYPE.CLANG) return true;
        if (type == jp.riken.kscope.data.FILE_TYPE.FILE_AUTO) {
            if (this.getStartCodeLine() == null) return false;
            if (this.getStartCodeLine().getSourceFile() == null) return false;
            if (this.getStartCodeLine().getSourceFile().getFile() == null) return false;
            File file = this.getStartCodeLine().getSourceFile().getFile();
            return jp.riken.kscope.data.FILE_TYPE.isClangFile(file);
        }

        return false;
    }

    /**
     * ファイルタイプがFortranであるかチェックする.
     * @return         true = Fortran
     */
    public boolean isFortran() {
        if (this.isClang()) {
            return false;
        }
        jp.riken.kscope.data.FILE_TYPE type = this.getFileType();
        if (type == jp.riken.kscope.data.FILE_TYPE.UNKNOWN) return false;
        if (type == jp.riken.kscope.data.FILE_TYPE.XCODEML_XML) return false;
        if (type == jp.riken.kscope.data.FILE_TYPE.FORTRANLANG) return true;
        if (type == jp.riken.kscope.data.FILE_TYPE.FILE_AUTO) {
            if (this.getStartCodeLine() == null) return false;
            if (this.getStartCodeLine().getSourceFile() == null) return false;
            if (this.getStartCodeLine().getSourceFile().getFile() == null) return false;
            File file = this.getStartCodeLine().getSourceFile().getFile();
            return jp.riken.kscope.data.FILE_TYPE.isFortranFile(file);
        }

        return true;
    }

    /**
     * 変数・関数の宣言名に変換する.
     * Fortranの場合は、小文字に変換する.
     * C言語の場合は、大文字・小文字を区別する
     * @param name        変数・関数名
     * @return            変換変数・関数名
     */
    protected String transferDeclarationName(String name) {
        if (name == null || name.isEmpty()) return null;
        // Fortranの場合は、小文字に変換する.
        if (this.isFortran()) return name.toLowerCase();
        return name;
    }

    /**
     * 変数・関数の宣言名が一致するか比較する.
     * Fortranの場合は、小文字・大文字関係なく比較する.
     * C言語の場合は、大文字・小文字を区別する
     * @param src         変数・関数名1
     * @param dest        変数・関数名2
     * @return            true=変数・関数名が一致
     */
    protected boolean equalsDeclarationName(String src, String dest) {
        if (src == null) return false;
        if (dest == null) return false;
        // Fortranの場合は、小文字・大文字関係なく比較する
        if (this.isFortran()) {
            return src.equalsIgnoreCase(dest);
        }
        return src.equals(dest);
    }


    /**
     * プログラム名、モジュール名、サブルーチン名、関数名が同一であるかチェックする.
     * ファイルタイプから文字列に一致条件を変更する.
     *     C言語  : 大文字・小文字を区別する.
     *     Fortran : 大文字・小文字を区別しない.
     * @param value        チェックプログラム名、モジュール名、サブルーチン名、関数名
     * @return        true = 一致
     */
    public boolean equalsName(String value) {
        if (value == null) return false;
        if (this.name == null) return false;
        if (this.isClang()) {
            return (value.equals(this.name));
        }
        else {
            return (value.equalsIgnoreCase(this.name));
        }
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
     * 子ブロックのIDeclarationsブロックを検索する.
     * @return    IDeclarationsブロックリスト
     */
    @Override
    public Set<IDeclarations> getDeclarationsBlocks() {
        Set<IDeclarations> list = new LinkedHashSet<IDeclarations>();
        list.add(this);
        if (this.children != null) {
            for (String key : this.children.keySet()) {
                ProgramUnit childrenUnit = this.children.get(key);
                if (childrenUnit == null) continue;
                Set<IDeclarations> children_list = childrenUnit.getDeclarationsBlocks();
                if (children_list != null && children_list.size() > 0) {
                    list.addAll(children_list);
                }
            }
        }
        if (list.size() <= 0) return null;
        return list;
    }

    /**
     * コード行情報を設定する。
     * 開始・修了行情報の行番号を比較して開始・修了行情報を設定する.
     * @param lineInfo           コード行情報
     */
    public void setCodeLine(CodeLine lineInfo) {
        if (this.start == null || this.start.getLineInfo() == null) {
            this.start = new Statement(lineInfo);
        }
        else if (this.start.getLineInfo().compareTo(lineInfo) > 0) {
            this.start = new Statement(lineInfo);
        }
        if (this.end == null || this.end.getLineInfo() == null) {
            this.end = new Statement(lineInfo);
        }
        else if (this.end.getLineInfo().compareTo(lineInfo) < 0) {
            this.end = new Statement(lineInfo);
        }

        return;
    }


    /**
     * C言語include文を追加する。
     * ソースファイル(.c)中に出現する他のファイル（ヘッダーファイル）を登録する.
     * 追加済みinclude文は重複追加しない。
     * @param file_name    ファイル名
     * @param lineInfo       行情報
     */
    public void addIncludeFile(String file_name) {
        CodeLine lineInfo = new CodeLine(new SourceFile(file_name), file_name);
        UseState include = new UseState(file_name);
        include.set_block_start(lineInfo);
        this.addInclude(include);
    }

    /**
     * C言語include文を追加する。
     * ソースファイル(.c)中に出現する他のファイル（ヘッダーファイル）を登録する.
     * 追加済みinclude文は重複追加しない。
     * @param file_name    ファイル名
     * @param lineInfo       行情報
     */
    public void addIncludeFiles(List<UseState> use_list) {
        if (use_list == null) return;
        if (use_list.size() <= 0) return;
        for (UseState include : use_list) {
            this.addInclude(include);
        }
    }

    /**
     * C言語include文を追加する。
     * ソースファイル(.c)中に出現する他のファイル（ヘッダーファイル）を登録する.
     * 追加済みinclude文は重複追加しない。
     * @param include    include文
     */
    public void addInclude(UseState include) {
        if (include == null) return;
        if (this.useList == null) {
            this.useList = new ArrayList<UseState>();
        }
        String include_name = include.getModuleName();
        if (include_name == null || include_name.isEmpty()) return;

        // 自身のモジュールと同じであるかチェックする.
        Module mod = this.getModuleBlock();
        if (mod.equalsName(include_name)) {
            return;
        }

        // 追加済みであるかチェックする.
        UseState exists = this.getInclude(include_name);
        if (exists != null) return;

        // USE文リストに追加する
        this.useList.add(include);

        return;
    }


    /**
     * C言語include文を取得する。
     * @param    include_name     includeヘッダーファイル
     * @return    UseState
     */
    protected UseState getInclude(String include_name) {
        if (this.useList == null) return null;
        if (include_name == null) return null;

        for (UseState use : this.useList) {
            String module_name = use.getModuleName();
            if (include_name.equals(module_name)) {
                return use;
            }
        }
        return null;
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
        String statement = this.toString();
        statement = "[" + statement + "]";
        if (this.getMotherBlock() != null) {
            String buf = null;
            if (module) buf = this.getMotherBlock().toStringModuleScope();
            else buf = this.getMotherBlock().toStringProcedureScope();
            if (buf != null && !buf.isEmpty()) {
                statement = buf + "-" + statement;
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
     * 関数呼出を含む自身の子ブロックのリストを返す。
     * @return 子ブロックのリスト
     */
    public List<IBlock> getBlocks() {
        return this.getChildren();
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

