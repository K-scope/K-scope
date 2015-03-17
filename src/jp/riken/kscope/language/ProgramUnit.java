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
import jp.riken.kscope.information.InformationBlock;
import jp.riken.kscope.information.InformationBlocks;
import jp.riken.kscope.information.TextInfo;
import jp.riken.kscope.language.fortran.Type;
import jp.riken.kscope.language.generic.Procedures;
/**
 * プログラム単位を表現するクラス。
 *
 * @author RIKEN
 *
 */
public abstract class ProgramUnit implements Serializable, IInformation, IBlock {
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
    private transient HashMap<String, VariableDefinition> variableMap = new HashMap<String, VariableDefinition>();

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
     * @param name		構造体名
     * @return		構造体定義
     */
    public Type getType(String name) {
        if (this.typeDefinitions == null) return null;
        if (name == null) return null;

        for (Type type : this.typeDefinitions) {
            String typename = type.getName();
            if (name.equalsIgnoreCase(typename)) {
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
     * @return		プログラム名、モジュール名、サブルーチン名、関数名
     */
    protected String toStringBase() {
        return name;
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
     * @param mam		親プログラム
     */
    protected void set_mother(ProgramUnit mam) {
        mother = mam;
    }

    /**
     * プログラムの属性を追加する.
     * @param attribute_name		属性名
     */
    protected void put_attribute(String attribute_name) {
        attributes.put(attribute_name, "");
    }

    /**
     * プログラムの属性を追加する.
     * @param attribute_name		属性名
     * @param attribute_value		属性値
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
            this.children = new HashMap<String, Procedure>();
        }
        children.put(child.get_name(), child);
    }

    /**
     * 副プログラムを追加する.
     * @param type_name		プログラム単位の種類
     * @param proc_name		プログラム名
     */
    protected void set_child(String type_name, String proc_name) {
        Procedure proc = new Procedure(type_name, proc_name);
        put_child(proc);
    }

    /**
     * 変数宣言文を追加する.
     * @param vr		変数宣言文
     */
    protected void put_variable(VariableDefinition vr) {
        vr.setMother(this);
        variables.put(vr.get_name(), vr);
    }

    /**
     * 変数宣言文を追加する.
     * @param varName		変数名
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
     * @param name		副プログラム名
     * @return		true=副プログラムに存在する.
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
    public Collection<Procedure> getChildren() {
        if (this.children == null) {
            return new ArrayList<Procedure>();
        }
        if (this.children.values().size() > 0) {
            return this.children.values();
        }
        return new ArrayList<Procedure>();
    }

    /**
     * 副プログラムを取得する.
     * @param name		副プログラム名
     * @return			副プログラム
     */
    protected ProgramUnit get_child(String name) {
        if (this.children == null) {
            return null;
        }
        return children.get(name);
    }

    /**
     * 副プログラム数を取得する.
     * @return		副プログラム数
     */
    protected int get_num_of_child() {
        if (this.children == null) {
            return 0;
        }
        return children.size();
    }

    /**
     * 副プログラム名のリストを取得する.
     * @return		副プログラム名のリスト
     */
    protected String[] get_child_name() {
        if (this.children == null) {
            return null;
        }
        return children.keySet().toArray(new String[children.size()]);
    }

    /**
     * プログラム属性値を取得する.
     * @param attribute_name		属性名
     * @return			属性値
     */
    protected Object get_attribute(String attribute_name) {
        return attributes.get(attribute_name);
    }

    /**
     * 指定した名前の変数宣言を返す。
     * @param var_name 変数名
     * @return 変数宣言。無ければnullを返す。
     */
    public VariableDefinition get_variable(String var_name) {
        return variables.get(var_name);
    }

    /**
     * 自身に係わる変数宣言のマップを返す。
     * @return 変数宣言のマップ。
     */
    public Map<String,VariableDefinition> getVariables() {
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
    protected void set_variable_def(VariableDefinition varDef) {
        this.put_variable(varDef);
    }

    /**
     * 変数宣言文を取得する。
     *
     * @return 変数宣言文リスト
     */
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
     * @return		付加情報
     */
    @Override
    public TextInfo getInformation() {
        return this.information;
    }

    /**
     * 開始行番号情報を取得する
     * @return		開始行番号情報
     */
    @Override
    public CodeLine getStartCodeLine() {
        if (start == null) return null;
        return start.lineInfo;
    }

    /**
     * 終了行番号情報を取得する
     * @return		終了行番号情報
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
        if (useList == null) {
            return new ArrayList<UseState>();
        }
        return useList;
    }

    /**
     * DATA文リストを返す。
     * @return		DATA文リスト
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
     * @return		EQUIVALENCE文リスト
     */
    public List<Equivalence> getEquivalenceList() {
        return equivalenceList;
    }

    /**
     * COMMON文リストを返す。
     * @return		COMMON文リスト.無ければ空のリストを返す。
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
        useList.add(useline);
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
            Collection<Procedure> procedures = this.getChildren();
            for (Procedure procedure : procedures) {
                result = procedure.findInformationBlockBy(id);
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
        for (Procedure child : this.getChildren()) {
            child.clearInformation();
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
        for (Procedure procedure : this.getChildren()) {
            result.addAll(procedure.createInformationBlocks());
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
     * @param namespace
     *          検索対象名前空間
     * @return 最初に見つかったProgramUnitオブジェクト
     */
    public ProgramUnit findProgramUnitBy(String namespace) {
        ProgramUnit result = null;
        if (this.getNamespace().equalsIgnoreCase(namespace)) {
            result = this;
        }
        if (result == null) {
            for (Procedure proc : this.getChildren()) {
                result = proc.findProgramUnitBy(namespace);
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
    public Map<String, Set<IBlock>> getRefVariableNames() {
        return refVariableNames;
    }

    /**
     * プログラム単位内で参照されている変数名と現れるブロックを追加する。
     *
     * @param refVarName
     *            変数名
     * @param blk
     *            ブロック
     */
    public void putRefVariableName(String refVarName, IBlock blk) {
        String nm = refVarName.toLowerCase();
        if (this.refVariableNames.containsKey(nm)) {
            Set<IBlock> block = this.refVariableNames.get(nm);
            block.add(blk);
        } else {
            Set<IBlock> block = new LinkedHashSet<IBlock>();
            block.add(blk);
            this.refVariableNames.put(nm, block);
        }
    }

    /**
     * プログラム単位内で定義されている変数名の集合を取得する。
     *
     * @return 変数名の集合
     */
    public Map<String, Set<IBlock>> getDefVariableNames() {
        return defVariableNames;
    }

    /**
     * プログラム単位内で定義されている変数名と現れるブロックを追加する。
     *
     * @param defVarName
     *            変数名
     * @param blk
     *            ブロック
     */
    public void putDefVariableName(String defVarName, IBlock blk) {
        String nm = defVarName.toLowerCase();
        if (this.defVariableNames.containsKey(nm)) {
            Set<IBlock> block = this.defVariableNames.get(nm);
            block.add(blk);
        } else {
            Set<IBlock> block = new LinkedHashSet<IBlock>();
            block.add(blk);
            this.defVariableNames.put(nm, block);
        }
    }
    /**
     * プログラム単位で使用される変数マップを取得する。
     *
     * @return 変数のマップ
     */
    public HashMap<String, VariableDefinition> getVariableMap() {
        if (this.variableMap == null) {
            this.createVariableMap();
        }
        return this.variableMap;
    }

    /**
     * プログラム単位において指定された名前で使用される変数が存在すればその宣言を返す。
     *
     * @param nm
     *            変数名
     * @return 変数宣言。無ければnullを返す。
     */
    public VariableDefinition getVariableMap(String nm) {
        if (nm == null || nm.isEmpty()) return null;
        if (this.variableMap == null) {
            this.createVariableMap();
        }
        VariableDefinition def = null;
        if (this.variableMap.get(nm) == null) {
            return def;
        }
        return this.variableMap.get(nm);
    }

    /**
     * 変数宣言リストを作成する.
     */
    private void createVariableMap() {
        this.variableMap = new HashMap<String, VariableDefinition>();
    }

    /**
     * プログラム単位で使用される変数名を追加する。
     *
     * @param nm
     *            変数名
     */
    public void putVariableMap(String nm) {
        if (this.variableMap == null) {
            this.createVariableMap();
        }
        if (!(this.variableMap.containsKey(nm))) {
            VariableDefinition vardef = null;
            this.variableMap.put(nm, vardef);
        }
    }

    /**
     * プログラム単位で使用される変数名と宣言のマップを追加する。
     *
     * @param varName
     *            変数名
     * @param varDef
     *            変数宣言
     */
    public void putVariableMap(String varName, VariableDefinition varDef) {
        if (this.variableMap == null) {
            this.createVariableMap();
        }
        this.variableMap.put(varName, varDef);
    }
    /**
     * 指定された式に含まれる全ての変数名と、指定されたブロックを参照としてセットする。
     * 指定された式に含まれる全ての変数名と、式に含まれる手続呼出を参照としてセットする。
     * @param blk ブロック
     * @param exp 式
     */
    public void addExpressionToRef(IBlock blk, Expression exp) {
        Set<Variable> vars = exp.getAllVariables();
        for (Variable var:vars) {
            this.putRefVariableName(var.getName(), blk);
            this.putVariableMap(var.getName());
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
     * 親ブロックを取得する
     * @return        親ブロック
     */
    @Override
    public IBlock getMotherBlock() {
        return this.get_mother();
    }


    /**
     * 子ブロックに同一ProgramUnitが存在するかチェックする
     * @param unit		検索ProgramUnit
     * @return			true=同一ProgramUnitが存在する
     */
    public boolean containsChildren(ProgramUnit unit) {
        if (this == unit) return true;
        String thisID = this.getID();
        String unitID = unit.getID();
        if (thisID.equalsIgnoreCase(unitID)) {
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
     * @param unit		モジュール、サブルーチン
     * @return		true=一致
     */
    public boolean equalsBlocks(ProgramUnit unit) {
        if (unit == null) return false;

        // モジュール、サブルーチン名
        if (this.name != null) {
            if (!this.name.equalsIgnoreCase(unit.name)) {
                return false;
            }
        }
        // タイプ
        if (this.type != null) {
            if (!this.type.equalsIgnoreCase(unit.type)) {
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
     * @param block			IInformationブロック
     * @return		同一ブロック
     */
    public IInformation findBlock(IInformation block) {
        if (block == null) return null;
        String id = block.getID();
        return this.findInformationBlockBy(id);
    }

    /**
     * 子ブロック、変数宣言が存在するかチェックする.
     * @return		true=空モジュール、サブルーチン
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
     * 同一ブロックを検索する
     * @param block			IInformationブロック
     * @return		同一ブロック
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
            Collection<Procedure> procedures = this.getChildren();
            for (Procedure procedure : procedures) {
                IInformation[] infos = procedure.searchInformationBlocks(block);
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
     * @param unit		チェック対象programUnit
     * @return   true=一致
     */
    public boolean equalsLayout(ProgramUnit unit) {
        // モジュール、サブルーチン名
        if (this.name != null) {
            if (!this.name.equalsIgnoreCase(unit.name)) {
                return false;
            }
        }
        // タイプ
        if (this.type != null) {
            if (!this.type.equalsIgnoreCase(unit.type)) {
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
        if (this.getLayoutID().equalsIgnoreCase(id)) {
            result = this;
        }

        if (result == null && this.getChildren() != null) {
            Collection<Procedure> procedures = this.getChildren();
            for (Procedure procedure : procedures) {
                result = procedure.findInformationLayoutID(id);
                if (result != null) { break; }
            }
        }

        return result;
    }

    /**
     * 行番号のブロックを検索する
     * @param line			行番号
     * @return		行番号のブロック
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
            Collection<Procedure> procedures = this.getChildren();
            for (Procedure procedure : procedures) {
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
            Collection<Procedure> procedures = this.getChildren();
            for (Procedure procedure : procedures) {
                Set<Variable> vars = procedure.getAllVariables();
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
    public void setVariableDefinitions() {
        Set<Variable> vars = getAllVariables();
        if (vars == null || vars.size() <= 0) return;
        for (Variable var : vars) {
            String name = var.getName();
            VariableDefinition def = getVariableMap(name);
            if (def != null) {
                var.setDefinition(def);
            }
        }
    }

    /**
     * ファイルタイプ（C言語、Fortran)を取得する.
     * @return		ファイルタイプ（C言語、Fortran)
     */
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
     * @return		 true = C言語
     */
    public boolean isClang() {
        jp.riken.kscope.data.FILE_TYPE type = this.getFileType();
        if (type == jp.riken.kscope.data.FILE_TYPE.UNKNOWN) return false;
        if (type == jp.riken.kscope.data.FILE_TYPE.XCODEML_XML) return false;
        if (type == jp.riken.kscope.data.FILE_TYPE.CLANG) return true;

        return false;
    }

    /**
     * ファイルタイプがFortranであるかチェックする.
     * @return		 true = Fortran
     */
    public boolean isFortran() {
        jp.riken.kscope.data.FILE_TYPE type = this.getFileType();
        if (type == jp.riken.kscope.data.FILE_TYPE.UNKNOWN) return false;
        if (type == jp.riken.kscope.data.FILE_TYPE.XCODEML_XML) return false;
        if (type == jp.riken.kscope.data.FILE_TYPE.CLANG) return false;


        return true;
    }

    /**
     * プログラム名、モジュール名、サブルーチン名、関数名が同一であるかチェックする.
     * ファイルタイプから文字列に一致条件を変更する.
     *     C言語  : 大文字・小文字を区別する.
     *     Fortran : 大文字・小文字を区別しない.
     * @param value		チェックプログラム名、モジュール名、サブルーチン名、関数名
     * @return		true = 一致
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
}

