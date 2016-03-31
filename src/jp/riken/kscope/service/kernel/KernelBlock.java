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
package jp.riken.kscope.service.kernel;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.BlockType;
import jp.riken.kscope.language.Condition;
import jp.riken.kscope.language.ExecutableBody;
import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IVariableType;
import jp.riken.kscope.language.Module;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProcedureUsage;
import jp.riken.kscope.language.Program;
import jp.riken.kscope.language.ProgramUnit;
import jp.riken.kscope.language.Selection;
import jp.riken.kscope.language.UseState;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.language.VariableDimension;
import jp.riken.kscope.language.fortran.Structure;
import jp.riken.kscope.language.fortran.VariableAttribute;
import jp.riken.kscope.language.fortran.VariableAttribute.ScopeAttribute;
import jp.riken.kscope.language.fortran.VariableType;
import jp.riken.kscope.language.fortran.VariableType.PrimitiveDataType;
import jp.riken.kscope.language.generic.Procedures;
import jp.riken.kscope.properties.KernelProperties;
import jp.riken.kscope.utils.StringUtils;

/**
 * カーネル抽出の出力ブロッククラス
 * @author RIKEN
 *
 */
public class KernelBlock implements IBlock {

    /** カーネル抽出ブロック */
    private IBlock kernel_block;
    /** カーネルコンテキスト */
    private  KernelContext context;
    /** カーネル抽出ルートブロック */
    private IBlock root_block;

    /**
     * コンストラクタ
     * @param block    カーネル抽出ブロック
     */
    public KernelBlock(IBlock block) {
        this.kernel_block = block;
    }

    /**
     * コンストラクタ
     * @param block    カーネル抽出ブロック
     * @param root    カーネル抽出ルートブロック
     */
    public KernelBlock(IBlock block, IBlock root) {
        this.kernel_block = block;
        this.root_block = root;
    }

    /**
     * コンストラクタ
     * @param block    カーネル抽出ブロック
     * @param root    カーネル抽出ルートブロック
     * @param context    カーネルコンテキスト
     */
    public KernelBlock(IBlock block, IBlock root, KernelContext context) {
        this.kernel_block = block;
        this.root_block = root;
        this.context = context;
    }

    /**
     * カーネル出力コード
     */
    @Override
    public String toString() {
        if (this.kernel_block == null) return "";
        String code = null;
        if (this.kernel_block instanceof Procedure) {
            // サブルーチン・関数の場合仮引数を付加する
            code = ((Procedure)this.kernel_block).toStringProcedure();
        }
        else {
            code = this.kernel_block.toString();
        }

        return code;
    }

    /**
     * カーネル出力コード
     * @param  indent    インデント幅
     */
    public String toString(int indent) {
        if (this.kernel_block == null) return "";
        String code = this.kernel_block.toString();
        String indent_column = StringUtils.repeat(" ", indent);
        code = indent_column + code;

        return code;
    }

    /**
     * カーネル抽出ブロックのインデントを取得する.
     * @return        インデント
     */
    protected String getCurrentIndent() {
        return getCurrentIndent(0);
    }

    /**
     * カーネル抽出ブロックのインデントを取得する.
     * @param      デフォルトインデント幅
     * @return        インデント
     */
    protected String getCurrentIndent(int init_indent) {

        int indent = this.getCurrentIndentColumn(init_indent);
        String indent_column = "";
        if (indent > 0) {
            indent_column = StringUtils.repeat(" ", indent);
        }
        return indent_column;
    }

    /**
     * カーネル抽出ブロックのインデントを取得する.
     * @param      デフォルトインデント幅
     * @return        インデント
     */
    protected int getCurrentIndentColumn(int init_indent) {

        int indent = init_indent;
        if (this.kernel_block != null
            && this.context != null) {
            indent += this.context.getCurrentIndentColumn(this.root_block, this.kernel_block);
        }
        if (indent <= 0) return indent;

        return indent;
    }


    /**
     * 開始行番号情報を取得する。
     * @return 開始行番号情報
     */
    @Override
    public CodeLine getStartCodeLine() {
        if (this.kernel_block == null) return null;
        return this.kernel_block.getStartCodeLine();
    }

    /**
     * 終了行番号情報を取得する。
     * @return        終了行番号情報
     */
    @Override
    public CodeLine getEndCodeLine() {
        if (this.kernel_block == null) return null;
        return this.kernel_block.getEndCodeLine();
    }

    /**
     * ブロックタイプを返す。
     * @return ブロックタイプ
     */
    @Override
    public BlockType getBlockType() {
        if (this.kernel_block == null) return null;
        return this.kernel_block.getBlockType();
    }

    /**
     * 親ブロックを習得する。
     * @return 親ブロック
     */
    @Override
    public IBlock getMotherBlock() {
        if (this.kernel_block == null) return null;
        return this.kernel_block.getMotherBlock();
    }

    /**
     * 変数リストを取得する.
     * 子ブロックの変数リストも取得する。
     * @return        変数リスト
     */
    @Override
    public Set<Variable> getAllVariables() {
        if (this.kernel_block == null) return null;
        return this.kernel_block.getAllVariables();
    }

    /**
     * 変数リストを取得する.
     * ブロックのみの変数リストを取得する。
     * @return        変数リスト
     */
    @Override
    public Set<Variable> getBlockVariables() {
        if (this.kernel_block == null) return null;
        return this.kernel_block.getBlockVariables();
    }

    /**
     * 子要素を返す.
     * @return 子要素
     */
    @Override
    public List<IBlock> getChildren() {
        if (this.kernel_block == null) return null;
        if (this.kernel_block.getChildren() == null) return null;
        List<IBlock> list = new ArrayList<IBlock>();
        for (IBlock block : this.kernel_block.getChildren() ) {
            list.add(new KernelBlock(block, this.root_block, this.context));
        }
        if (list.size() <= 0) return null;
        return list;
    }


    /**
     * 行番号のブロックを検索する
     * @param line            行番号
     * @return        行番号のブロック
     */
    @Override
    public IBlock[] searchCodeLine(CodeLine line) {
        if (this.kernel_block == null) return null;
        return this.kernel_block.searchCodeLine(line);
    }

    /**
     * 親ブロックからIDeclarationsブロックを取得する.
     * @return    IDeclarationsブロック
     */
    @Override
    public ProgramUnit getScopeDeclarationsBlock() {
        if (this.kernel_block == null) return null;
        return this.kernel_block.getScopeDeclarationsBlock();
    }

    /**
     * プロシージャ（関数）からブロックまでの階層文字列表記を取得する
     * @return      階層文字列表記
     */
    @Override
    public String toStringProcedureScope() {
        if (this.kernel_block == null) return "";
        return this.kernel_block.toStringProcedureScope();
    }

    /**
     * モジュールからブロックまでの階層文字列表記を取得する
     * @return      階層文字列表記
     */
    @Override
    public String toStringModuleScope() {
        if (this.kernel_block == null) return "";
        return this.kernel_block.toStringModuleScope();
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
        if (this.kernel_block == null) return "";
        return this.kernel_block.toStringScope(module);
    }

    /**
     * 関数呼出を含む自身の子ブロックのリストを返す。
     * @return 子ブロックのリスト
     */
    @Override
    public List<IBlock> getBlocks() {
        if (this.kernel_block == null) return null;
        return this.kernel_block.getBlocks();
    }

    /**
     * カーネル抽出ブロックを取得する
     * @return カーネル抽出ブロック
     */
    public IBlock getKernelBlock() {
        return this.kernel_block;
    }

    /**
     * カーネル抽出ブロックを設定する
     * @param kernel_block カーネル抽出ブロック
     */
    public void setKernelBlock(IBlock kernel_block) {
        this.kernel_block = kernel_block;
    }

    /**
     * 終了:END文を持つブロックは、終了:END文を返す
     * @return        終了:END文
     */
    @Override
    public String toEndString() {
        if (this.kernel_block == null) return "";
        return this.kernel_block.toEndString();
    }

    /**
     * モジュール名を取得する
     * @return    モジュール名
     */
    public String getName() {
        if (this.kernel_block instanceof ProgramUnit) {
            return ((ProgramUnit)this.kernel_block).get_name();
        }
        if (this.kernel_block instanceof VariableDefinition) {
            return ((VariableDefinition)this.kernel_block).get_name();
        }
        if (this.kernel_block instanceof jp.riken.kscope.language.fortran.Type) {
            return ((jp.riken.kscope.language.fortran.Type)this.kernel_block).getName();
        }
        if (this.kernel_block instanceof jp.riken.kscope.language.fortran.Structure) {
            return ((jp.riken.kscope.language.fortran.Structure)this.kernel_block).getName();
        }
        return null;
    }

    /**
     * タイプ名を取得する
     * @return    モジュール名
     */
    public String getType() {
        if (this.kernel_block instanceof ProgramUnit) {
            return ((ProgramUnit)this.kernel_block).get_type();
        }
        if (this.kernel_block instanceof VariableDefinition) {
            return ((VariableDefinition)this.kernel_block).getVariableType().getName();
        }
        if (this.kernel_block instanceof jp.riken.kscope.language.fortran.Type) {
            return ((jp.riken.kscope.language.fortran.Type)this.kernel_block).getName();
        }
        if (this.kernel_block instanceof jp.riken.kscope.language.fortran.Structure) {
            return ((jp.riken.kscope.language.fortran.Structure)this.kernel_block).getName();
        }
        return null;
    }

    /**
     * 変数定義文の初期設定式を出力する.
     * @return        初期設定式
     */
    public String toStringInitialize() {

        StringBuffer buf = new StringBuffer();
        VariableDefinition var_def = null;
        String var_name = null;
        if (this.kernel_block instanceof VariableDefinition) {
            var_def = (VariableDefinition)this.kernel_block;
            var_name = var_def.get_name();
        }
        else if (this.kernel_block instanceof Variable) {
            Variable var = (Variable)this.kernel_block;
            var_def = var.getDefinition();
            var_name = var.toString();
        }
        if (var_name == null || var_name.isEmpty()) return "";
        if (var_def == null) return "";

        // parameter属性に初期値設定不可
        VariableAttribute attr = (VariableAttribute)var_def.getAttribute();
        if (attr != null) {
            if (attr.hasParameter()) return "";
        }

        // 初期設定式はコメントアウトとする.
        buf.append("!");
        buf.append(var_name);

        buf.append(" = ");
        IVariableType var_type = var_def.getVariableType();
        if (var_type.isRealType()) {
            PrimitiveDataType primitive = ((jp.riken.kscope.language.fortran.VariableType)var_type).getPrimitiveDataType();
            if (primitive == PrimitiveDataType.COMPLEX || primitive == PrimitiveDataType.DOUBLE_COMPLEX) {
                buf.append("(0.0,0.0)");
            }
            else {
                buf.append("0.0");
            }
        }
        if (var_type.isIntegerType()) {
            buf.append("0");
        }

        return buf.toString();
    }

    /**
     * 構造体定義文、インターフェイス文を出力する.
     * @return        構造体定義文、インターフェイス文
     */
    public String toStructure() {

        int indent = KernelProperties.DEFAULT_INDELT_COLUMN;
        if (this.context != null) {
            indent = this.context.getIndentColumn();
        }
        int current_indent = this.getCurrentIndentColumn(0);
        String buf = null;
        if (this.kernel_block instanceof VariableDefinition) {
            VariableDefinition def = (VariableDefinition)this.kernel_block;
            if (!def.isStruct()) return "";

            VariableType def_type = (VariableType) def.getType();
            if (def_type.getType() != null) {
                jp.riken.kscope.language.fortran.Type type = def_type.getType();
                buf = type.toStringStruct(indent);
            }
            if (def_type.getStructure() != null) {
                jp.riken.kscope.language.fortran.Structure type = def_type.getStructure();
                buf = type.toStringStruct(indent);
            }
        }
        else if (this.kernel_block instanceof jp.riken.kscope.language.fortran.Type) {
            jp.riken.kscope.language.fortran.Type type = (jp.riken.kscope.language.fortran.Type)this.kernel_block;
            buf = type.toStringStruct(indent);
        }
        else if (this.kernel_block instanceof jp.riken.kscope.language.fortran.Structure) {
            jp.riken.kscope.language.fortran.Structure type = (jp.riken.kscope.language.fortran.Structure)this.kernel_block;
            buf = type.toStringStruct(indent);
        }
        else if (this.kernel_block instanceof Procedures) {
            Procedures inter = (Procedures)this.kernel_block;
            buf = inter.toStructure(indent);
        }

        if (buf == null) return "";

        buf = StringUtils.correctIndent(buf, current_indent);

        // 先頭のインデントは不要
        buf = buf.trim();

        return buf;
    }

    /**
     * 構造体メンバの場合、親構造体定義を取得する。
     * @return   親構造体定義
     */
    public VariableDefinition getStructDefinition() {
        if (this.kernel_block == null) return null;
        if (!(this.kernel_block instanceof VariableDefinition)) return null;
        VariableDefinition def = ((VariableDefinition)this.kernel_block).getStructDefinition();
        return def;
    }

    /**
     * カーネル抽出ブロックが同じであるかチェックする。
     *
     */
    public boolean equals(KernelBlock obj) {
        IBlock this_block = this.getKernelBlock();
        IBlock dest_block = obj.getKernelBlock();
        if (this.getClass() != obj.getClass()) return false;
        if (this_block == dest_block) return true;

        if (this_block instanceof VariableDefinition
            && dest_block instanceof VariableDefinition) {
            VariableDefinition this_def = (VariableDefinition)this_block;
            VariableDefinition dest_def = (VariableDefinition)dest_block;
            if (this_def.getMother() != dest_def.getMother()) return false;
            if (this_def.get_name().equalsIgnoreCase(dest_def.get_name())) {
                return true;
            }
        }

        if (this_block instanceof jp.riken.kscope.language.fortran.Type
            && dest_block instanceof jp.riken.kscope.language.fortran.Type) {
            jp.riken.kscope.language.fortran.Type this_type = (jp.riken.kscope.language.fortran.Type)this_block;
            jp.riken.kscope.language.fortran.Type dest_type = (jp.riken.kscope.language.fortran.Type)dest_block;
            if (this_type.getName().equalsIgnoreCase(dest_type.getName())) {
                return true;
            }
        }
        if (this_block instanceof jp.riken.kscope.language.fortran.Structure
                && dest_block instanceof jp.riken.kscope.language.fortran.Structure) {
            jp.riken.kscope.language.fortran.Structure this_type = (jp.riken.kscope.language.fortran.Structure)this_block;
            jp.riken.kscope.language.fortran.Structure dest_type = (jp.riken.kscope.language.fortran.Structure)dest_block;
            if (this_type.getName().equalsIgnoreCase(dest_type.getName())) {
                return true;
            }
        }

        return false;
    }

    /**
     * 構造体メンバ定義であるかチェックする。
     * @return        true=構造体メンバ定義
     */
    public boolean isStructMember() {
        IBlock this_block = this.getKernelBlock();
        if (this_block == null) return false;
        if (!(this_block instanceof VariableDefinition)) {
            return false;
        }
        VariableDefinition this_def = (VariableDefinition)this_block;

        return this_def.isStructMember();
    }

    /**
     * 構造体定義であるかチェックする.
     * @return        true=構造体定義
     */
    public boolean isStruct() {
        IBlock this_block = this.getKernelBlock();
        if (this_block == null) return false;
        if (!(this_block instanceof VariableDefinition)) {
            return false;
        }
        VariableDefinition this_def = (VariableDefinition)this_block;

        return this_def.isStruct();
    }

    /**
     * 配列変数定義であるかチェックする.
     * @return        true=配列変数定義
     */
    public boolean isArray() {
        IBlock this_block = this.getKernelBlock();
        if (this_block == null) return false;
        if (!(this_block instanceof VariableDefinition)) {
            return false;
        }
        VariableDefinition this_def = (VariableDefinition)this_block;

        return this_def.isArray();
    }


    /**
     * 変数定義文であるかチェックする.
     * @return        true=変数定義文
     */
    public boolean isVariableDefinition() {
        IBlock this_block = this.getKernelBlock();
        if (this_block == null) return false;

        return (this_block instanceof VariableDefinition);
    }

    /**
     * USE文のリストを返す。
     * @return USE文のリスト。無ければ空のリストを返す。
     */
    public List<UseState> getUseList() {
        IBlock this_block = this.getKernelBlock();
        if (this_block == null) return null;
        if (!(this_block instanceof ProgramUnit)) {
            return null;
        }

        return ((ProgramUnit)this_block).getUseList();
    }

    /**
     * 変数定義文の変数名を出力する.
     * @return        変数名
     */
    public String toStringName() {

        String buf = null;
        if (this.kernel_block instanceof VariableDefinition) {
            VariableDefinition def = (VariableDefinition)this.kernel_block;
            buf = def.get_name();
        }
        else if (this.kernel_block instanceof jp.riken.kscope.language.fortran.Type) {
            jp.riken.kscope.language.fortran.Type type = (jp.riken.kscope.language.fortran.Type)this.kernel_block;
            buf = type.getName();
        }
        else if (this.kernel_block instanceof jp.riken.kscope.language.fortran.Structure) {
            jp.riken.kscope.language.fortran.Structure type = (jp.riken.kscope.language.fortran.Structure)this.kernel_block;
            buf = type.getName();
        }
        else if (this.kernel_block instanceof Variable) {
            Variable var = (Variable)this.kernel_block;
            buf = var.getName();
        }
        else if (this.kernel_block instanceof Module) {
            Module var = (Module)this.kernel_block;
            buf = var.get_name();
        }
        else {
            buf = this.kernel_block.toString();
        }

        if (buf == null) return "";

        return buf;
    }

    /**
     * 変数定義文の仮引数定義文を出力する.
     * @param  inout      intent属性値
     * @return        変数名
     */
    public String toStringArgDef(String inout) {
        if (inout != null && !inout.isEmpty()) {
            inout = inout.toLowerCase();
        }
        String buf = null;
        if (this.kernel_block instanceof VariableDefinition) {
            VariableDefinition def = (VariableDefinition)this.kernel_block;
            // コピー作成
            VariableDefinition arg_def = new VariableDefinition(def);

            // public,private,save属性削除
            this.removeModuleVariableAttributes(arg_def);
            // 配列インデックス削除
            this.removeVariableDimension(arg_def);

            // ALLOCATEABLE属性削除 : intent(in)のみ
            if ("in".equals(inout)) {
                String allocatable = VariableAttribute.ATTRIBUTE_ALLOCATABLE.toLowerCase();
                arg_def.removeVariableAttributes(allocatable);
            }

            String attr = new String();
            // INTENT属性追加
            if (inout != null && !inout.isEmpty()) {
                attr = VariableAttribute.ATTRIBUTE_INTENT.toLowerCase();
                attr = attr + "(" + inout + ")";
                arg_def.addVariableAttributes(attr);
            }
            // 初期値削除
            arg_def.setInitValue(null);
            // character length削除
            if (arg_def.getType() != null) {
                VariableType var_type = (VariableType)arg_def.getType();
                if (var_type.getPrimitiveDataType() == PrimitiveDataType.CHARACTER
                    && var_type.getLen() != null
                    && var_type.getLen().getLine() != null
                    && !var_type.getLen().getLine().isEmpty()) {
                    // len=*とする
                    VariableType new_type = new VariableType(var_type);
                    Expression len_exp = new Expression();
                    len_exp.setLine("*");
                    new_type.setLen(len_exp);
                    arg_def.setVariableType(new_type);
                }
            }
            buf = arg_def.toString();
        }
        else {
            buf = "";
        }

        if (buf == null) return "";

        return buf;
    }

    /**
     * カーネルブロックのソースファイル、出力ファイルを対応付ける。
     * @param file       対応付けファイル
     */
    public void setKernelFile(File file) {
        if (this.kernel_block == null) return;

        if (this.kernel_block instanceof ProgramUnit) {
            ProgramUnit prog = (ProgramUnit)this.kernel_block;
            CodeLine code = null;
            if (file != null) {
                code = new CodeLine(new SourceFile(file), null);
            }
            prog.set_start(code);
        }

        return;
    }

    /**
     * カーネルブロックの対応付けたソースファイル、出力ファイルを取得する。
     * @return        対応付けファイル
     */
    public File getKernelFile() {
        if (this.kernel_block == null) return null;

        if (this.kernel_block instanceof ProgramUnit) {
            ProgramUnit prog = (ProgramUnit)this.kernel_block;
            CodeLine code = prog.getStartCodeLine();
            if (code == null) return null;
            if (code.getSourceFile() == null) return null;
            File file = code.getSourceFile().getFile();
            return file;
        }

        return null;
    }


    /**
     * 変数定義:allocatable属性を持っているかチェックする.
     * @return        true=allocatable属性を持つ
     */
    public boolean hasAllocatable() {
        IBlock this_block = this.getKernelBlock();
        if (this_block == null) return false;
        if (!(this_block instanceof VariableDefinition)) {
            return false;
        }
        VariableDefinition this_def = (VariableDefinition)this_block;
        VariableAttribute attr = (VariableAttribute)this_def.getAttribute();
        if (attr == null) return false;

        return attr.hasAllocatable();
    }

    /**
     * 変数定義:pointer属性を持っているかチェックする.
     * @return        true=pointer属性を持つ
     */
    public boolean hasPointer() {
        IBlock this_block = this.getKernelBlock();
        if (this_block == null) return false;
        if (!(this_block instanceof VariableDefinition)) {
            return false;
        }
        VariableDefinition this_def = (VariableDefinition)this_block;
        VariableAttribute attr = (VariableAttribute)this_def.getAttribute();
        if (attr == null) return false;

        return attr.hasPointer();
    }


    /**
     * カーネル文を出力する.
     * テンプレートにはインデントはない。
     * コードから判断する。
     * @return        カーネル文
     */
    public String toKernels() {
        if (this.kernel_block == null) return "";

        StringBuilder buf = new StringBuilder();

        // カーネル文を出力する。
        String indent_column = this.getCurrentIndent();
        // String indent_column = this.getIndentColumnFromParent(this.kernel_block);
        boolean skip_tostring = false;
        if (this.kernel_block.getBlockType() == BlockType.SELECTION) {
            // IF, WHERE文の場合は出力しない。conditionsで出力する
            skip_tostring = ((Selection)this.kernel_block).isIF() || ((Selection)this.kernel_block).isWHERE();
        }
        boolean add_indent = true;
        if (this.kernel_block == this.root_block) add_indent = false;
        if (this.kernel_block.getBlockType() == BlockType.CONDITION
            && this.root_block.getBlockType() == BlockType.SELECTION
            && this.kernel_block.getMotherBlock() == this.root_block) {
            if (!((Selection)this.root_block).isSelect()) {
                if (((Selection)this.root_block).indexOfChildren((Condition)this.kernel_block) == 0) {
                    add_indent = false;
                }
            }
        }
        if (this.kernel_block.getBlockType() == BlockType.DIRECTIVE) add_indent = false;

        if (!skip_tostring) {
            if (add_indent) buf.append(indent_column);
            buf.append(this.kernel_block.toString());
            buf.append('\n');
        }
        List<IBlock> blocks = this.kernel_block.getChildren();
        if (blocks != null) {
            for (IBlock block : blocks) {
                KernelBlock child_block = new KernelBlock(block, this.root_block, this.context);
                String child_buf = child_block.toKernels();
                if (child_buf != null && !child_buf.isEmpty()) {
                    buf.append(StringUtils.trimRight(child_buf));
                    buf.append('\n');
                }
            }
        }

        String end_line = this.kernel_block.toEndString();
        if (end_line != null && !end_line.isEmpty()) {
            buf.append(indent_column);
            buf.append(end_line);
            buf.append('\n');
        }

        return StringUtils.trimRight(buf.toString());
    }

    /**
     * カーネルブロックのインデントを取得する
     * @return        インデント
     */
    private String getKernelIndentColumn() {
        if (this.context == null) return "";
        int init_indent = this.context.getIndentColumn();
        if (this.root_block == null) return "";
        if (this.root_block instanceof ProgramUnit) {
            if (this.root_block.getMotherBlock() == null) {
                // subroutine
                init_indent = 0;
            }
            else {
                init_indent = init_indent*1;
            }
        }
        else {
            init_indent = init_indent*2;
        }
        return this.getCurrentIndent(init_indent);
    }


    /**
     * 手続呼出しのリストを返す。
     * @return 手続呼出しのリスト
     */
    @Override
    public List<ProcedureUsage> getCalls() {
        if (this.kernel_block == null) return null;
        return this.kernel_block.getCalls();
    }


    /**
     * VariableDefinition文のリストを返す。
     * @return    変数定義のリスト
     */
    public List<KernelBlock> get_variables() {
        IBlock this_block = this.getKernelBlock();
        if (this_block == null) return null;
        if (!(this_block instanceof ProgramUnit)) {
            return null;
        }
        VariableDefinition[] defs = ((ProgramUnit)this_block).get_variables();
        if (defs == null) return null;

        KernelProperties properties = null;
        if (this.context != null && this.context.getProperties() != null) {
            properties = this.context.getProperties();
        }

        boolean is_procedure = (this_block.getBlockType() == BlockType.PROCEDURE);
        List<KernelBlock> list = new ArrayList<KernelBlock>();
        for (VariableDefinition def : defs) {
            if (def == null) continue;
            String name = def.get_name();
            ProgramUnit mother = def.getScopeDeclarationsBlock();

            // SourceFileが存在していること。= プロジェクト内のソースファイルであること。
            if (!def.hasSourceFile()) {
                // 暗黙宣言文であるかチェックする。SourceFile==null && StrSourceFile==nullの場合は暗黙宣言とする
                if (!this.isImplicitBlock(def)) {
                    continue;
                }
                // 除外文であるか
                if (properties != null) {
                    if (properties.isExcludeStatement(def.get_name())) {
                        continue;
                    }
                }
            }
            // include文として定義しているファイルの場合は除外する
            else if (properties.isIncludeFile(def.getStartCodeLine().getSourceFile())) {
                continue;
            }

            if (def.getType() == null || StringUtils.isTrimEmpty(def.getType().toString())) {
                // データ型なしで、親サブルーチン・関数と同じ名前の変数定義は出力対象外とする
                if (name.equalsIgnoreCase(mother.get_name())) {
                    continue;
                }
            }
            VariableDefinition add_def = def;
            if (is_procedure) {
                // サブルーチン・関数の場合、public,private,save属性削除
                add_def = new VariableDefinition(def);
                this.removeModuleVariableAttributes(add_def);
            }
            list.add(new KernelBlock(add_def, this.root_block, this.context));
        }
        return list;
    }

    /**
     * プログラム単位に属する構造体構造定義のリストを返す。
     *
     * @return 構造体構造定義のリスト。無ければ空のリストを返す。
     */
    public List<KernelBlock> getTypeList() {
        IBlock this_block = this.getKernelBlock();
        if (this_block == null) return null;
        if (!(this_block instanceof ProgramUnit)) {
            return null;
        }

        List<jp.riken.kscope.language.fortran.Type> types = ((ProgramUnit)this_block).getTypeList();
        if (types == null) return null;
        List<KernelBlock> list = new ArrayList<KernelBlock>();
        for (jp.riken.kscope.language.fortran.Type type : types) {
            list.add(new KernelBlock(type, type, this.context));
        }
        return list;
    }


    /**
     * ExecutableBodyのブロックリストを取得する
     * @return
     */
    public List<KernelBlock> getBodys() {
        IBlock this_block = this.getKernelBlock();
        if (this_block == null) return null;
        if (!(this_block instanceof Procedure)) {
            return null;
        }

        ExecutableBody body = ((Procedure)this_block).getBody();
        if (body == null) return null;
        if (body.getChildren() == null) return null;

        List<KernelBlock> list = new ArrayList<KernelBlock>();
        for (IBlock block : body.getChildren()) {
            list.add(new KernelBlock(block, block, this.context));
        }
        if (list.size() <= 0) return null;

        return list;
    }


    /**
     * 親ブロックからのブロックのインデント幅を取得する.
     * @param block        ブロック
     * @return            インデント幅
     */
    public String getIndentColumnFromParent(IBlock kernel_block) {
        if (kernel_block == null) return "";

        int count = 0;
        IBlock parent = kernel_block.getMotherBlock();
        while (parent != null) {
            if (!(parent instanceof ExecutableBody)) {
                count++;
            }
            parent = parent.getMotherBlock();
        }
        count *= KernelProperties.DEFAULT_INDELT_COLUMN;

        String indent_column = "";
        if (count > 0) {
            indent_column = StringUtils.repeat(" ", count);
        }
        return indent_column;

    }


    /**
     * 構造体メンバを取得する。
     * @return   構造体メンバ
     */
    public List<KernelBlock> getTypeDefinitions() {
        if (this.kernel_block == null) return null;

        jp.riken.kscope.language.fortran.Type kernel_type = null;

        if (this.kernel_block instanceof jp.riken.kscope.language.fortran.Type) {
            kernel_type = ((jp.riken.kscope.language.fortran.Type)this.kernel_block);
        }
        else if (this.kernel_block instanceof VariableDefinition) {
            VariableType type = (VariableType)((VariableDefinition)this.kernel_block).getType();
            if (type != null && type.getType() != null) {
                kernel_type = type.getType();
            }
        }
        if (kernel_type == null) return null;
        List<VariableDefinition> mems = kernel_type.getDefinitions();

        List<KernelBlock> list = new ArrayList<KernelBlock>();
        for (VariableDefinition mem : mems) {
            KernelBlock kernel = new KernelBlock(mem, this.kernel_block, this.context);
            list.add(kernel);
        }
        if (list.size() <= 0) return null;
        return list;
    }

    /**
     * NO_MODULEではないモジュールであるかチェックする.
     * @return        true = NO_MODULEではないモジュール
     */
    public boolean isModule() {
        if (this.kernel_block == null) return false;
        if (this.getBlockType() != BlockType.MODULE) return false;
        if (Program.NO_MODULE.equalsIgnoreCase(this.getName())) return false;
        return true;
    }

    /**
     * NO_MODULEのモジュールであるかチェックする.
     * @return        true = NO_MODULEのモジュール
     */
    public boolean isNoModule() {
        if (this.kernel_block == null) return false;
        if (this.getBlockType() != BlockType.MODULE) return false;
        if (!Program.NO_MODULE.equalsIgnoreCase(this.getName())) return false;
        return true;
    }


    /**
     * NO_MODULEではないモジュールであるかチェックする.
     * @return        true = NO_MODULEではないモジュール
     */
    public boolean isLocalModule() {
        if (this.kernel_block == null) return false;
        String mod_name = null;
        if (this.getBlockType() == BlockType.MODULE) {
            mod_name = this.getName();
        }
        else if (this.getBlockType() == BlockType.USE) {
            UseState use = (UseState)this.kernel_block;
            mod_name = use.getModuleName();
        }

        if (mod_name != null) {
            if (KernelProperties.MODULE_NAME_LOCAL.equalsIgnoreCase(mod_name)) {
                return true;
            }
        }
        return false;
    }

    /**
     * KernelContextを設定する.
     * @param context        KernelContext
     */
    public void setKernelContext(KernelContext context) {
        this.context = context;
    }

    /**
     * mpif.hがインクルードされているかチェックする.: カーネル抽出用
     * mpi_comm_world変数の宣言文が存在するかチェックする。
     * @return
     */
    public boolean hasIncludeMpi() {
        IBlock this_block = this.getKernelBlock();
        if (this_block == null) return false;
        if (!(this_block instanceof ProgramUnit)) {
            return false;
        }

        return ((ProgramUnit)this_block).hasIncludeMpi();
    }

    /**
     * omp_lib.hがインクルードされているかチェックする.: カーネル抽出用
     * openmp_version変数の宣言文が存在するかチェックする。
     * @return
     */
    public boolean hasIncludeOmp() {
        IBlock this_block = this.getKernelBlock();
        if (this_block == null) return false;
        if (!(this_block instanceof ProgramUnit)) {
            return false;
        }

        return ((ProgramUnit)this_block).hasIncludeOmp();
    }

    /**
     * public,private,save属性を削除する.
     * @param def        変数宣言文
     */
    private void removeModuleVariableAttributes(VariableDefinition def) {
        String attr = new String();
        // public属性削除
        attr = VariableAttribute.ScopeAttribute.PUBLIC.toString().toLowerCase();
        def.removeVariableAttributes(attr);
        // private属性削除
        attr = VariableAttribute.ScopeAttribute.PRIVATE.toString().toLowerCase();
        def.removeVariableAttributes(attr);
        // SAVE属性削除
        attr = VariableAttribute.ATTRIBUTE_SAVE.toLowerCase();
        def.removeVariableAttributes(attr);
        // mother=nullとする：public, private属性を出力しないようにするため
        def.setMother(null);

        return;
    }


    /**
     * public,private,save属性を削除する.
     * @param def        変数宣言文
     */
    private void removeVariableDimension(VariableDefinition def) {
        // 配列パラメータ削除
        VariableDimension dim = def.getVariableDimension();
        if (dim != null) {
            dim.clearIndices();
        }

        return;
    }

    /**
     * public属性を持つかチェックする
     * @return    true = public属性を持つ
     */
    public boolean hasPublic() {
        IBlock this_block = this.getKernelBlock();
        if (this_block == null) return false;
        if (this_block instanceof Procedure) {
            return ((Procedure)this_block).hasPublic();
        }
        else if (this_block instanceof VariableDefinition) {
            return ((VariableDefinition)this_block).hasPublic();
        }
        else if (this_block instanceof Variable) {
            VariableDefinition def = ((Variable)this_block).getDefinition();
            if (def != null) {
                return def.hasPublic();
            }
        }

        return false;
    }


    /**
     * DATA文リストを返す。
     * @return        DATA文リスト
     */
    public List<jp.riken.kscope.language.Data> getDataList() {
        IBlock this_block = this.getKernelBlock();
        if (this_block == null) return null;
        if (!(this_block instanceof ProgramUnit)) return null;

        List<jp.riken.kscope.language.Data> list = ((ProgramUnit)this_block).getDataList();
        if (list == null || list.size() <= 0) return null;
        return list;
    }


    /**
     * プログラム単位に属するインターフェイス文リストを返す。
     * @return KernelBlock(インターフェイス文)リスト
     */
    public List<KernelBlock> getInterfaceList() {
        IBlock this_block = this.getKernelBlock();
        if (this_block == null) return null;
        if (!(this_block instanceof ProgramUnit)) {
            return null;
        }

        List<Procedures> interfaces = ((ProgramUnit)this_block).getInterfaceList();
        if (interfaces == null) return null;
        List<KernelBlock> list = new ArrayList<KernelBlock>();
        for (Procedures inter : interfaces) {
            list.add(new KernelBlock(inter, inter, this.context));
        }
        return list;
    }

    /**
     * COMMON文リストを返す。
     * @return        COMMON文リスト
     */
    public List<jp.riken.kscope.language.Common> getCommonList() {
        IBlock this_block = this.getKernelBlock();
        if (this_block == null) return null;
        if (!(this_block instanceof ProgramUnit)) return null;

        List<jp.riken.kscope.language.Common> list = ((ProgramUnit)this_block).getCommonList();
        if (list == null || list.size() <= 0) return null;

        List<jp.riken.kscope.language.Common> common_list = new ArrayList<jp.riken.kscope.language.Common>();

        KernelProperties properties = null;
        if (this.context != null && this.context.getProperties() != null) {
            properties = this.context.getProperties();
        }
        for (jp.riken.kscope.language.Common common : list) {
                // 除外文であるか
            if (properties != null) {
                if (properties.isExcludeStatement(common.getName())) {
                    continue;
                }
            }
            common_list.add(common);
        }

        return common_list;
    }


    /**
     * EQUIVALENCE文リストを返す。
     * @return        EQUIVALENCE文リスト
     */
    public List<jp.riken.kscope.language.Equivalence> getEquivalenceList() {
        IBlock this_block = this.getKernelBlock();
        if (this_block == null) return null;
        if (!(this_block instanceof ProgramUnit)) return null;

        List<jp.riken.kscope.language.Equivalence> list = ((ProgramUnit)this_block).getEquivalenceList();
        if (list == null || list.size() <= 0) return null;
        return list;
    }


    /**
     * 暗黙の宣言文であるかチェックする.
     * SourceFile==null && StrSourceFile==nullの場合は暗黙宣言とする
     * @param block        宣言文
     * @return        true=暗黙の宣言文
     */
    private boolean isImplicitBlock(IBlock block) {
        CodeLine line = block.getStartCodeLine();
        if (line == null) return false;
        if (line.getSourceFile() == null
            && line.getStrSourceFile() == null) {
            return true;
        }

        return false;
    }


    /**
     * Moduleスコープがprivateであるかチェックする.
     * include mpif.hのmpi_comm_worldにprivate属性があるかチェックする。
     * デフォルトはprivateとする。
     * @return        true = Moduleスコープがprivate
     */
    public boolean isPrivateModule() {
        if (this.kernel_block == null) return true;
        if (this.getBlockType() != BlockType.MODULE) return true;

        Module mod = (Module)this.kernel_block;
        if (!mod.hasIncludeMpi()) return true;
        String mpi_scope = mod.getScopeIncludeMpi();
        if (VariableAttribute.ScopeAttribute.PRIVATE.toString().equalsIgnoreCase(mpi_scope)) {
            return true;
        }
        // mpi_comm_worldを持ち、private属性が無いのでpublicである。

        return false;
    }

    /**
     * 変数定義の次元数を取得する.
     * @return     次元数
     */
    public int get_dimension_size() {
        if (!this.isArray()) return 0;
        IBlock this_block = this.getKernelBlock();
        VariableDefinition this_def = (VariableDefinition)this_block;
        return this_def.get_dimension_size();
    }



    /**
     * 構造体メンバを取得する。 : 構造体の構造体は除く
     * @return   構造体メンバ
     */
    public List<KernelBlock> getBoundMembers() {
        if (this.kernel_block == null) return null;

        jp.riken.kscope.language.fortran.Type kernel_type = null;

        if (this.kernel_block instanceof jp.riken.kscope.language.fortran.Type) {
            kernel_type = ((jp.riken.kscope.language.fortran.Type)this.kernel_block);
        }
        else if (this.kernel_block instanceof VariableDefinition) {
            VariableType type = (VariableType)((VariableDefinition)this.kernel_block).getType();
            if (type != null && type.getType() != null) {
                kernel_type = type.getType();
            }
        }
        if (kernel_type == null) return null;
        List<VariableDefinition> mems = kernel_type.getDefinitions();

        List<KernelBlock> list = new ArrayList<KernelBlock>();
        for (VariableDefinition mem : mems) {
            if (mem.isStruct()) continue;
            KernelBlock kernel = new KernelBlock(mem, this.kernel_block, this.context);
            list.add(kernel);
        }
        if (list.size() <= 0) return null;
        return list;
    }

    /**
     * 変数定義文の変数名を出力する.
     * 構造体メンバの場合は、親構造体からの変数名（配列表記は除く）を出力する。
     * @return        変数名
     */
    public String toStructureName() {

        String buf = null;
        if (this.kernel_block instanceof VariableDefinition) {
            VariableDefinition def = (VariableDefinition)this.kernel_block;
            buf = def.toStructureName();
        }
        else if (this.kernel_block instanceof jp.riken.kscope.language.fortran.Type) {
            jp.riken.kscope.language.fortran.Type type = (jp.riken.kscope.language.fortran.Type)this.kernel_block;
            buf = type.toStructureName();
        }
        else if (this.kernel_block instanceof jp.riken.kscope.language.fortran.Structure) {
            jp.riken.kscope.language.fortran.Structure type = (jp.riken.kscope.language.fortran.Structure)this.kernel_block;
            buf = type.toStructureName();
        }
        else {
            buf = this.toStringName();
        }

        if (buf == null) return "";

        return buf;
    }
}


