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
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.information.InformationBlock;
import jp.riken.kscope.information.InformationBlocks;
import jp.riken.kscope.information.TextInfo;
import jp.riken.kscope.language.fortran.Structure;
import jp.riken.kscope.language.fortran.VariableAttribute;
import jp.riken.kscope.language.fortran.VariableType;
import jp.riken.kscope.language.utils.LanguageUtils;
import jp.riken.kscope.parser.LineSpliter;
import jp.riken.kscope.utils.StringUtils;
/**
 * 変数・構造体の宣言を表現するクラス。
 * @version    2015/03/15      C言語, FortranにてtoString出力文字列の変更
 */
public class VariableDefinition implements Serializable, IInformation, IBlock {
    /** シリアル番号 */
    private static final long serialVersionUID = 8694203337559301954L;
    /** 変数名. */
    private String name;
    /** データ型. */
    private IVariableType type;
    /** 属性. */
    private IVariableAttribute attribute;
    /** 配列要素. */
    private VariableDimension dimension;
    /** 初期値. */
    private Expression initValue;
    /** 型宣言の開始位置情報. */
    private Statement start;
    /** 型宣言の終了位置情報. */
    private Statement end;
    /** 付加情報. */
    private TextInfo information = null;
    /** USE文によって自身を参照・定義しているプログラム単位の集合. */
    private transient Set<ProgramUnit> referMembers = new HashSet<ProgramUnit>();
    /**
     * 本宣言を保持しているプログラム単位.
     * @version    2015/09/01     親プログラム(ProgramUnit)からブロック(IBlock)に変更する
     *                            ProgramUnit又はCompoundBlock
     *             2015/10/01     構造体メンバの場合、親構造体宣言を設定する
     */
    private IBlock mother;

    /**
     * コンストラクタ。
     *
     * @param line
     *            行情報
     */
    public VariableDefinition(CodeLine line) {
        start = new Statement(line);
        end = new Statement(line);
    }

    /**
     * コンストラクタ
     *
     * VariableDefinition(String nm, VariableType typ, VariableAttribute
     * attrbts)<br>
     * あるいは<br>
     * VariableDefinition(String nm, VariableType typ, VariableAttribute
     * attrbts, VariableDimension dmnsn)<br>
     * を使用するようにしてください。
     *
     * @param varnm
     *            変数名
     */
    public VariableDefinition(String varnm) {
        this.name = varnm;
    }

    /**
     * コンストラクタ.
     *
     * @param nm
     *            変数名
     * @param typ
     *          型
     * @param attrbts
     *          属性
     */
    public VariableDefinition(String nm, IVariableType typ,
            IVariableAttribute attrbts) {
        this.name = nm;
        this.setVariableType(typ);
        this.attribute = attrbts;
    }

    /**
     * コンストラクタ.
     *
     * @param nm
     *          変数名
     * @param typ
     *          型
     * @param attrbts
     *          属性
     * @param dmnsn
     *          配列情報
     */
    public VariableDefinition(String nm, IVariableType typ,
            IVariableAttribute attrbts, VariableDimension dmnsn) {
        this(nm, typ, attrbts);
        this.setDimension(dmnsn);
    }


    /**
     * コピーコンストラクタ.
     * @param  var_def          コピー元変数宣言
     */
    public VariableDefinition(VariableDefinition var_def) {
        this.name = var_def.name;
        this.attribute = var_def.attribute;
        this.setDimension(var_def.dimension);
        this.setInitValue(var_def.initValue);
        this.type = var_def.type;
        if (var_def.start != null) {
            this.start = new Statement(var_def.start);
        }
        if (var_def.end != null) {
            this.end = new Statement(var_def.end);
        }
        this.mother = var_def.mother;

        // 付加情報はコピーしない
        // 参照・定義リストはコピーしない

    }

    /**
     * ブロックタイプの取得。
     *
     * @return BlockType.VARIABLEDEFINITION
     */
    @Override
    public BlockType getBlockType() {
        return BlockType.VARIABLEDEFINITION;
    }

    /**
     * データ型を設定する。
     *
     * @param tp
     *            データ型
     */
    public void setVariableType(IVariableType tp) {
        this.type = tp;

        if (this.type != null) {
            this.type.setParentStatement(this);
            // 親ブロックに参照変数を設定する
            this.putRefVariable();
        }
    }

    /**
     * 属性をセットする。
     *
     * @param att
     *            属性
     */
    public void setVariableAttributes(IVariableAttribute att) {
        attribute = att;
    }

    /**
     * 配列要素の下限、上限を設定する。
     *
     * @param i
     *            次元
     * @param startIndex
     *            下限
     * @param endIndex
     *            上限
     */
    public void setDimensionIndex(int i, Expression startIndex, Expression endIndex) {
        setStartIndex(i, startIndex);
        setEndIndex(i, endIndex);
    }

    /**
     * 配列要素の下限を設定する。
     *
     * @param i
     *            次元
     * @param startIndex
     *            下限
     */
    public void setStartIndex(int i, Expression startIndex) {
        dimension.getIndex(i).set_start(startIndex);
    }

    /**
     * 配列要素の上限を設定する。
     *
     * @param i
     *            次元
     * @param indexEnd
     *            上限
     */
    public void setEndIndex(int i, Expression indexEnd) {
        dimension.getIndex(i).set_end(indexEnd);
    }

    /**
     * 自身を参照しているプログラム単位を追加する.<br>
     *
     * @param proc
     *            手続き
     */
    public void addReferMember(ProgramUnit proc) {
        if (proc == null) {
            return;
        }
        this.getReferMember().add(proc);
    }

    /**
     * 自身を参照しているプログラム単位の集合を返す。
     *
     * @return 手続きの集合。存在しない場合は空の集合を返す。
     */
    public Set<ProgramUnit> getReferMember() {
        if (this.referMembers == null) {
            return new HashSet<ProgramUnit>();
        }
        return this.referMembers;
    }

    /**
     * 変数名を取得する。
     *
     * @return 変数名
     */
    public String get_name() {
        return (name);
    }

    /**
     * データ型を取得する。
     *
     * @return データ型
     */
    public IVariableType getType() {
        return type;
    }

    /**
     * 属性を取得する
     *
     * @return 属性
     */
    public IVariableAttribute getAttribute() {
        return attribute;
    }

    /**
     * 配列要素の次元数を取得する
     *
     * @return 配列要素次元数
     */
    public int get_dimension_size() {
        if (dimension == null || dimension.getIndex() == null)
            return 0;
        return (dimension.getIndex().length);
    }

    /**
     * 配列要素から指定次元の下限を取得する
     *
     * @param i
     *            次元数
     * @return 次元下限
     */
    public Expression get_index_start(int i) {
        return (dimension.getIndex(i).get_start());
    }

    /**
     * 配列要素から指定次元の上限を取得する
     *
     * @param i
     *            次元数
     * @return 次元上限
     */
    public Expression get_index_end(int i) {
        return (dimension.getIndex(i).get_end());
    }

    /**
     * 配列要素を設定する。
     *
     * @param dimension
     *            配列要素
     */
    public void setDimension(VariableDimension dimension) {
        this.dimension = dimension;
        if (this.dimension != null) {
            this.dimension.setParentStatement(this);
            // 親ブロックに参照変数を設定する
            this.putRefVariable();
        }
    }

    /**
     * 初期値を設定する。
     *
     * @param value            初期値
     */
    public void setInitValue(Expression value) {
        this.initValue = value;
        if (this.initValue != null) {
            this.initValue.setParentStatement(this);
            // 親ブロックに参照変数を設定する
            this.putRefVariable();
        }
    }

    /**
     * 初期値を取得する
     *
     * @return 初期値
     */
    public Expression getInitValue() {
        return this.initValue;
    }

    @Override
    public String toString() {
        String info = "";
        // delete by @hira at 2013/03/01
//        if (this.getInformation() != null) {
//            if (!(this.getInformation().getContent().equals(""))) {
//                info = "[ ! ] ";
//            }
//        }
        return (info + this.toStringBase());
    }

    /**
     * 変数宣言の文字列表現を返す。
     *
     * @return 変数宣言の文字列表現
     */
    protected String toStringBase() {
        if (this.isClang()) {
            return this.toStringClang();
        }
        else {
            return this.toStringFortran();
        }

    }

    /**
     * 変数宣言のC言語文字列表現を返す。
     *
     * @return 変数宣言のC言語文字列表現
     */
    public String toStringClang() {
        StringBuilder var = new StringBuilder();

        if (this.mother != null && this.mother instanceof Structure) {
            Structure struct = (Structure)this.mother;
            if (struct.getMotherBlock() != null) {
                var.append(struct.getMotherBlock().toString());
            }
            else {
                var.append(struct.toString());
            }
            var.append("::");
        }

        // 属性
        boolean is_pointer = false;
        boolean is_const_pointer = false;
        boolean is_prioritypointer = false;
        String var_attr = "";
        if (this.attribute != null) {
            is_pointer = this.attribute.contains(VariableAttribute.ATTRIBUTE_POINTER);
            is_const_pointer = this.attribute.contains(VariableAttribute.ATTRIBUTE_CONST_POINTER);
            var_attr = this.attribute.toStringClang();
            // ポインタと配列との優先
            is_prioritypointer = ((VariableAttribute)this.attribute).isPointerArrayPriority();

        }
        if (!StringUtils.isNullOrEmpty(var_attr)) {
            var.append(var_attr);
            var.append(" ");
        }

        if (this.type != null) {
            // データ型
            String type_name = this.type.getName();
            if (this.type.isStruct()) {
                type_name = "struct " + type_name;
            }
            var.append(type_name);
        }

        var.append(" ");

        if (is_prioritypointer) {
            if (is_pointer) var.append("(");
        }
        // ポインタ
        if (is_pointer) {
            int pointer_count = this.attribute.countOf("pointer");
            for (int i=0; i<pointer_count; i++) {
                var.append("*");
            }
        }
        if (is_const_pointer) {
            var.append("*const ");
        }

        // 変数名
        var.append(name);
        if (is_prioritypointer) {
            if (is_pointer) var.append(")");
        }

        if (this.dimension != null) {
            String dims = "";
            for (int i = 0; i < this.dimension.get_index_size(); i++) {
                String start = this.dimension.get_index_start(i).toString();
                String end = this.dimension.get_index_end(i).toString();
                dims += "[";
                // C言語はend配列のみ
                if (end != null) {
                    dims += end;
                }
                dims += "]";
            }
            var.append(dims);
        }

        // 初期値
        if (this.initValue != null) {
            var.append(" ");
            var.append("=");
            var.append(" ");
            var.append(this.initValue.getLine());
        }

        return var.toString();
    }


    /**
     * 変数宣言のFortran文字列表現を返す。
     *
     * @return 変数宣言のFortran文字列表現
     */
    public String toStringFortran() {

        StringBuilder var = new StringBuilder();

        if (this.mother != null && this.mother instanceof Structure) {
            Structure struct = (Structure)this.mother;
            if (struct.getMotherBlock() != null) {
                var.append(struct.getMotherBlock().toString());
            }
            else {
                var.append(struct.toString());
            }
            var.append("::");
        }

        if (type != null) {
            // データ型
            var.append(type.toString());
        }

        if (dimension != null) {
            var.append(",dimension(");
            String dims = "";
            boolean explicitly = false;
            for (int i = 0; i < dimension.get_index_size(); i++) {
                String start = dimension.get_index_start(i).toString();
                String end = dimension.get_index_end(i).toString();
                if (start != null && start.trim().isEmpty()) start = null;
                if (end != null && end.trim().isEmpty()) end = null;

                String dim = "";
                if (start != null) {
                    dim += start;
                }
                dim += ":";
                if (end != null) {
                    dim += end;
                    explicitly = true;
                }
                if (start == null && end != null && end.equals("*")) {
                    dim = "*";
                }
                else if (explicitly && start == null && end == null) {
                    dim = "*";
                }
                else if (explicitly && end == null) {
                    dim += "*";
                }
                dims += dim;
                if (i + 1 < dimension.get_index_size()) {
                    dims += ",";
                }
            }
            var.append(dims);
            var.append(")");
        }

        // 属性
        if (attribute != null) {
          Iterator<String> itr = attribute.getAttributes().iterator();
            while (itr.hasNext()) {
                String attr = itr.next();
                var.append(",");
                var.append(attr);
            }
        }
        var.append(" ");

        // 変数名
        var.append("::");
        var.append(name);

        // 初期値
        if (initValue != null) {
            var.append("=");
            var.append(initValue.getLine());
        }
        return var.toString();
    }

    /**
     * 型が適合しているかどうか。<br>
     *
     * 多重定義されている関数群の中から対応する関数を探索する際に、<br>
     * 仮引数と実引数の型チェックをする必要がある。<br>
     * 「適合している」とは、この型チェックで、同一の型と判定される 事を意味している。
     *
     * @param actualArgument
     *
     * @return true : 適合している<br>
     *         false: 適合していない
     *
     */
    public boolean matches(Expression actualArgument) {
        if (actualArgument == null) { return false; }

        // modify by @hira at 2013/02/01
        if (actualArgument.getType() == null) {
            return false;
        }
        if (this.getType() == null) {
            return false;
        }
        return actualArgument.getType().matches(this.getType());
    }
    /**
     * 付加情報を設定する
     *
     * @param info
     *            付加情報
     */
    @Override
    public void setInformation(TextInfo info) {
        this.information = info;
    }

    /**
     * 付加情報を取得する
     *
     * @return 付加情報
     */
    @Override
    public TextInfo getInformation() {
        return this.information;
    }


    /**
     * 開始行番号情報を取得する
     * @return      開始行番号情報
     */
    @Override
    public CodeLine getStartCodeLine() {
        if (start == null) {
            if (this.mother == null) return null;
            return this.mother.getStartCodeLine();
        }
        return start.lineInfo;
    }
    /**
     * 終了行番号情報を取得する
     * @return      終了行番号情報
     */
    @Override
    public CodeLine getEndCodeLine() {
        if (end == null) {
            if (this.mother == null) return null;
            return this.mother.getStartCodeLine();
        }
        return end.lineInfo;
    }


    /**
     * 行情報を設定する.
     *
     * @param line
     *            行情報
     */
    public void setCodeLine(CodeLine line) {
        start = new Statement(line);
        end = new Statement(line);
    }

    /**
     * データ型を取得する。
     *
     * @return データ型
     */
    public IVariableType getVariableType() {
        return type;
    }

    /**
     * 配列情報を取得する。
     *
     * @return 配列情報
     */
    public VariableDimension getVariableDimension() {
        return this.dimension;
    }

    /**
     * スカラーならば真を返す。
     *
     * @return 真偽値：スカラーならば真
     */
    public boolean isScalar() {
        if (this.dimension == null) {
            return true;
        }
        return false;
    }


    /**
     * 親プログラムをセットする。
     *
     * @param mother 親プログラム
     * @version    2015/09/01     親プログラムからブロックに変更する
     *                            ProgramUnit又はCompoundBlock
     */
    public void setMother(IBlock mother) {
        this.mother = mother;
        // 親ブロックに参照変数を設定する
        this.putRefVariable();
    }

    /**
     * 親プログラム単位を習得する。
     *
     * @return 親プログラム単位
     * @version    2015/09/01     親プログラムからブロックに変更する
     *                            ProgramUnit又はCompoundBlock
     */
    public IBlock getMother() {
        return this.mother;
    }
    /**
     * 名前空間（モジュール名.ルーチン名）を取得する。
     *
     * @return 名前空間（モジュール名.ルーチン名）
     */
    @Override
    public String getNamespace() {
        String result = "";
        if (this.mother != null) {
            if (this.mother instanceof IInformation) {
                result = ((IInformation)mother).getNamespace();
            }
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

        return result;
    }

    /**
     * 付加情報をすべて削除する。
     */
    @Override
    public void clearInformation() {
        this.setInformation(null);
    }

    /**
     * 付加情報コンテナコレクションを生成する。
     *
     * @return 付加情報コンテナコレクション
     */
    public InformationBlocks createInformationBlocks() {
        InformationBlocks result = new InformationBlocks();

        if (this.information != null) {
            InformationBlock cont = new InformationBlock(this.information, this, this);
            result.add(cont);
        }

        return result;
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
            if (this.mother instanceof IInformation) {
                IInformation info = (IInformation)this.mother;
                int offset = this.getStartPos() - info.getStartPos();
                result = info.getID() + "$" + offset + ":" + this.toStringBase();
            }
        } else {
            result = this.toStringBase();
        }
        return result;
    }
    /**
     * 親ブロックを取得する
     * @return        親ブロック
     */
    @Override
    public IBlock getMotherBlock() {
        return this.getMother();
    }

    /**
     * 同一VariableDefinitionであるかチェックする.
     * 変数宣言の文字列表現にて同一かチェックする.
     * @param definition        変数・構造体の宣言
     * @return        true=一致
     */
    public boolean equalsBlocks(VariableDefinition definition) {
         // 変数宣言の文字列表現にて同一かチェックする.
        String thisVar = toStringBase();
        String destVar = definition.toStringBase();
        if (thisVar == null && destVar == null) {
            return true;
        }
        else if (thisVar == null) {
            return false;
        }
        if (this.isFortran()) {
            return thisVar.equalsIgnoreCase(destVar);
        }
        else {
            return thisVar.equals(destVar);
        }
    }


    /**
     * 同一ブロックを検索する
     *
     * @param block    IInformationブロック
     * @return 同一ブロック
     */
    public IInformation[] searchInformationBlocks(IInformation block) {
        List<IInformation> list = new ArrayList<IInformation>();
        if (block instanceof VariableDefinition) {
            if (this.equalsBlocks((VariableDefinition)block)) {
                list.addAll(Arrays.asList(this));
            }
        }
        if (list.size() <= 0) {
            return null;
        }
        return list.toArray(new IInformation[0]);
    }

    /**
     * 構造IDを取得する.
     * 構造IDは不要であるので、nullを返す.
     * @return 構造ID
     */
    @Override
    public String getLayoutID() {
        return null;
    }


    /**
     * 行番号のブロックを検索する
     * @param line            行番号
     * @return        行番号のブロック
     */
    public IBlock[] searchCodeLine(CodeLine line) {
        if (line == null) return null;
        if (line.getSourceFile() == null) return null;
        if (this.getStartCodeLine() == null) return null;
        if (!line.getSourceFile().equals(this.getStartCodeLine().getSourceFile())) return null;

        List<IBlock> list = new ArrayList<IBlock>();
        CodeLine thisstart = this.getStartCodeLine();
        CodeLine thisend = this.getEndCodeLine();
        if ( line.isOverlap(thisstart, thisend) ) {
            list.add(this);
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
        Set<Variable> vars = new HashSet<Variable>();
        if (this.dimension != null) {
            Set<Variable> list = this.dimension.getAllVariables();
            if (list != null && list.size() > 0) {
                vars.addAll(list);
            }
        }

        if (this.initValue != null) {
            Set<Variable> list = this.initValue.getAllVariables();
            if (list != null && list.size() > 0) {
                vars.addAll(list);
            }
        }

        if (this.type != null) {
            Set<Variable> list = this.type.getAllVariables();
            if (list != null && list.size() > 0) {
                vars.addAll(list);
            }
        }
        if (vars.size() <= 0) return null;

        return vars;
    }

    /**
     * VariableDefinitionの親モジュールを取得する
     * @param definition        VariableDefinition
     * @return        モジュール
     */
    public ProgramUnit getParentProgram() {
        IBlock block = this.getMother();
        if (block == null) return null;
        while (true) {
            if (block.getMotherBlock() == null) {
                break;
            }
            block = block.getMotherBlock();
        }

        if (block instanceof ExecutableBody) {
            block = ((ExecutableBody)block).getParent();
        }

        if (!(block instanceof ProgramUnit)) {
            return null;
        }

        return (ProgramUnit)block;
    }

    /**
     * 子要素を返す。
     * @return 子要素。無ければ空のリストを返す
     */
    public List<IBlock> getChildren() {
        return new ArrayList<IBlock>();
    }

    /**
     * ファイルタイプ（C言語、Fortran)を取得する.
     * @return        ファイルタイプ（C言語、Fortran)
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
     * @return         true = C言語
     */
    public boolean isClang() {
        if (this.mother == null) return false;
        return this.mother.isClang();
    }

    /**
     * ファイルタイプがFortranであるかチェックする.
     * @return         true = Fortran
     */
    public boolean isFortran() {
        if (this.mother == null) return false;
        return this.mother.isFortran();
    }

    /**
     * layoutIDにマッチした構造ブロックを検索する。
     * @param id    layoutID
     * @return 見つかった構造ブロック
     */
    @Override
    public IInformation findInformationLayoutID(String id) {
        if (id == null || id.isEmpty()) return null;
        IInformation result = null;
        String layoutId = this.getLayoutID();
        if (layoutId == null) return null;

        if (this.isFortran()) {
            if (layoutId.equalsIgnoreCase(id)) {
                result = this;
            }
        }
        else {
            if (layoutId.equals(id)) {
                result = this;
            }
        }
        return result;
    }

    /**
     * 親ブロックからIDeclarationsブロックを取得する.
     * @return    IDeclarationsブロック
     */
    @Override
    public IDeclarations getScopeDeclarationsBlock() {
        if (this.mother == null) return null;
        return this.mother.getScopeDeclarationsBlock();
    }

    /**
     * 子ブロックのIDeclarationsブロックを検索する.
     * @return    IDeclarationsブロックリスト
     */
    @Override
    public Set<IDeclarations> getDeclarationsBlocks() {
        return null;
    }

    /**
     * Procedureブロックを習得する。
     * @return    Procedureブロック
     */
    @Override
    public Procedure getProcedureBlock() {
        if (this.mother == null) return null;
        return this.mother.getProcedureBlock();
    }

    /**
     * Moduleブロックを習得する。
     * @return    Moduleブロック
     */
    @Override
    public Module getModuleBlock() {
        if (this.mother == null) return null;
        return this.mother.getModuleBlock();
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
     * 変数・関数の宣言名に変換する.
     * Fortranの場合は、小文字に変換する.
     * C言語の場合は、大文字・小文字を区別する
     * @param name        変数・関数名
     * @return            変換変数・関数名
     */
    public String transferDeclarationName(String name) {
        if (name == null || name.isEmpty()) return null;
        // Fortranの場合は、小文字に変換する.
        if (this.isFortran()) return name.toLowerCase();
        return name;
    }

    /**
     * 変数定義の変数であるかチェックする。
     * @param var        変数
     * @return            変数定義の変数
     */
    public boolean validateVariable(Variable var) {
        if (var == null) return false;
        if (var.getParentStatement() == null) return false;

        return false;
    }


    /**
     * 式の変数リストを取得する.
     * ブロックのみの変数リストを取得する。
     * @return        式の変数リスト
     */
    @Override
    public Set<Variable> getBlockVariables() {
        return this.getAllVariables();
    }

    /**
     * 関数呼出を含む自身の子ブロックのリストを返す。
     * @return 子ブロックのリスト
     */
    public List<IBlock> getBlocks() {
        List<IBlock> blk = new ArrayList<IBlock>();
        if (this.initValue == null) return null;
        if (this.initValue.getFuncCalls() == null) return null;
        for (ProcedureUsage pu : this.initValue.getFuncCalls()) {
            blk.add(pu);
        }
        return blk;
    }

    /**
     * 親ブロックに参照変数を設定する
     */
    private void putRefVariable() {
        if (this.mother == null) return;
        IDeclarations dec = this.getScopeDeclarationsBlock();
        if (dec == null) return;

        if (this.initValue != null) {
            dec.addExpressionToRef(this, this.initValue);
        }

        if (this.dimension != null) {
            Set<Variable> var_list = this.dimension.getAllVariables();
            if (var_list != null) {
                for (Variable var : var_list) {
                    dec.putRefVariableName(var.getName(), this);
                }
            }
        }
        if (this.type != null) {
            Set<Variable> var_list = this.type.getAllVariables();
            if (var_list != null) {
                for (Variable var : var_list) {
                    dec.putRefVariableName(var.getName(), this);
                }
            }
        }
    }

    /**
     * 構造体メンバの変数定義を取得する
     * @param mem_names        構造体変数メンバ文字列
     * @return        構造体メンバの変数定義
     */
    public VariableDefinition getStructMember(String mem_name) {
        String[] mem_names = LanguageUtils.splitVariableNames(mem_name);
        return this.getStructMember(mem_names);
    }

    /**
     * 構造体メンバの変数定義を取得する
     * @param mem_names        構造体変数メンバ文字列
     * @return        構造体メンバの変数定義
     */
    public VariableDefinition getStructMember(String[] mem_names) {
        if (mem_names == null || mem_names.length <= 0) return null;

        String name = this.transferDeclarationName(mem_names[0]);
        if (name == null) return null;
        if (!name.equals(this.name)) return null;
        if (mem_names.length == 1) return this;

        // 先頭変数名を削除
        List<String> mem_list = new ArrayList<String>();
        mem_list.addAll(Arrays.asList(mem_names));
        mem_list.remove(0);

        if (this.type == null || !this.type.isStruct()) return null;
        VariableType type = (VariableType)this.type;
        if (type.getStructure() != null) {
            Structure struct = type.getStructure();
            VariableDefinition mem_def = struct.getStructMember(mem_list.toArray(new String[0]));
            return mem_def;
        }
        else if (type.getType() != null) {
            jp.riken.kscope.language.fortran.Type struct = type.getType();
            VariableDefinition mem_def = struct.getStructMember(mem_list.toArray(new String[0]));
            return mem_def;
        }
        return null;
    }
}
