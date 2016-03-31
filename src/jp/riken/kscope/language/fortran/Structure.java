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

package jp.riken.kscope.language.fortran;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.BlockType;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.ProcedureUsage;
import jp.riken.kscope.language.ProgramUnit;
import jp.riken.kscope.language.Statement;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.language.utils.LanguageUtils;
import jp.riken.kscope.utils.StringUtils;

/**
 * structure型クラス.
 *
 * @author RIKEN
 *
 */
public class Structure implements IBlock, Serializable {
    /** シリアル番号 */
    private static final long serialVersionUID = 6682888216005792092L;
    /** 変数名 */
    private String name = "";
    /** 構造体メンバ変数 */
    private List<VariableDefinition> definitions = new ArrayList<VariableDefinition>();
    /** コード開始行 */
    private Statement start;
    /** コード終了行 */
    private Statement end;
    /** 親ブロック */
    private IBlock mother;
    /** 構造タイプ:STRUCT,UNION,ENUM */
    private BlockType block_type = BlockType.STRUCT;
    /** スコープ： PUBLIC または PRIVATE */
    private VariableAttribute.ScopeAttribute scope = VariableAttribute.ScopeAttribute.PRIVATE;
    /** SEQUENCE文 : true = SEQUENCE文を持つ */
    private boolean is_sequence = false;

    /**
     * コンストラクタ。
     */
    public Structure() {
    }

    /**
     * コンストラクタ。
     * @param nm        構造体の名前:無名(null,空文字)の場合もある。
     */
    public Structure(String nm) {
        this.name = nm;
    }

    /**
     * コンストラクタ
     * @param value        コピー元Structure
     */
    public Structure(Structure value) {
        if (value == null) return;
        this.name = value.name;
        if (value.start != null) {
            this.start = new Statement(value.start);
        }
        if (value.end != null) {
            this.end = new Statement(value.end);
        }
        this.mother = value.mother;
        this.block_type = value.block_type;
        // this.definitions = value.definitions;
        if (value.definitions != null) {
            for (VariableDefinition def : value.definitions) {
                VariableDefinition mem = new VariableDefinition(def);
                this.add(mem);
            }
        }
        this.scope = value.scope;
        this.is_sequence = value.is_sequence;
    }

    /**
     * 変数定義文の追加。
     *
     * @param definition
     *          変数定義文
     */
    public void add(VariableDefinition definition) {
        if (definition != null) {
            // 構造体メンバに親構造体を設定する
            definition.setMother(this);
            // 構造体メンバ追加
            this.definitions.add(definition);
        }
   }

    /**
     * 指定の型の変数定義文の追加。
     *
     * @param typ
     *          変数の型
     * @param nm
     *          変数名
     */
    public void add(VariableType typ, String nm) {
        if (typ != null && nm != null) {
            VariableDefinition definition = new VariableDefinition(nm, typ, new VariableAttribute());
            // 構造体メンバ追加
            this.add(definition);
        }
    }

    /**
     * type文の追加。
     *
     * @param type
     *          構造体
     * @param variableName
     *          変数名
     */
    public void add(Type type, String variableName) {
        if (type != null && variableName != null) {
            VariableType typeType = new VariableType(type);
            this.add(typeType, variableName);
        }
    }

    /**
     * structure文の追加。
     *
     * @param structure
     *          構造体
     * @param variableName
     *          変数名
     */
    public void add(Structure structure, String variableName) {
        if (structure != null && variableName != null) {
            VariableType structureType = new VariableType(structure);
            this.add(structureType, variableName);
        }
    }

    /**
     * union文の追加。
     *
     * @param union
     *          共用体
     */
    public void add(Union union) {
        if (union != null) {
            VariableType unionType = new VariableType(union);
            this.add(unionType, "");
        }
    }

    /**
     * 構造体名の取得。
     *
     * @return 構造体の名前
     */
    public String getName() {
        return this.name;
    }

    /**
     * 構造体内の変数定義文リストの取得。
     *
     * @return 変数定義文リスト
     */
    public List<VariableDefinition> getDefinitions() {
        return definitions;
    }

    /**
     * 構造体内の変数定義文リストの設定。
     * @param 変数定義文リスト
     */
    public void setDefinitions(List<VariableDefinition> list) {
        this.definitions = list;
    }

    /**
     * 型が適合しているかどうか。<br>
     *
     * 多重定義されている関数群の中から対応する関数を探索する際に、<br>
     * 仮引数と実引数の型チェックをする必要がある。<br>
     * 「適合している」とは、この型チェックで、同一の型と判定される
     * 事を意味している。
     *
     * @param value
     *          型
     *
     * @return true : 適合している<br>
     *         false: 適合していない
     *
     */
    public boolean matches(Structure value) {
        if (value == null) { return false; }
        if (this.name == null) { return false; }
        // 名前のチェックのみ
        return this.name.equalsIgnoreCase(value.getName());
    }

    /**
     * 開始コード行情報を取得する。
     * @return         開始コード行情報
     */
    public Statement getStartStatement() {
        return this.start;
    }


    /**
     * 終了コード行情報を設定する。
     * @param lineInfo         終了コード行情報
     */
    public Statement getEndStatement() {
        return this.end;
    }

    /**
     * 開始コード行情報を設定する。
     * @param lineInfo         開始コード行情報
     */
    public void setStartStatement(CodeLine lineInfo) {
        this.start = new Statement(lineInfo);
    }


    /**
     * 終了コード行情報を設定する。
     * @param lineInfo         終了コード行情報
     */
    public void setEndStatement(CodeLine lineInfo) {
        this.end = new Statement(lineInfo);
    }


    /**
     * 親ブロックを取得する
     * @return        親ブロック
     */
    public void setMotherBlock(IBlock block) {
        this.mother = block;
    }


    /**
     * 親ブロックを取得する
     * @return        親ブロック
     */
    @Override
    public IBlock getMotherBlock() {
        return this.mother;
    }

    @Override
    public CodeLine getStartCodeLine() {
        if (this.getStartStatement() == null) {
            if (this.mother == null) return null;
            return this.mother.getStartCodeLine();
        }
        return this.getStartStatement().getLineInfo();
    }

    @Override
    public CodeLine getEndCodeLine() {
        if (this.getEndStatement() == null) {
            if (this.mother == null) return null;
            return this.mother.getEndCodeLine();
        }
        return this.getEndStatement().getLineInfo();
    }

    /**
     * 開始コード行情報を設定する。
     * @param line    開始コード行情報を設定する。
     */
    public void setStartCodeLine(CodeLine line) {
        this.setStartStatement(line);
    }

    /**
     * 終了コード行情報を設定する。
     * @param line    終了コード行情報を設定する。
     */
    public void setEndCodeLine(CodeLine line) {
        this.setEndStatement(line);
    }

    /**
     * ブロックタイプを返す。
     * @return    構造体タイプ:STRUCT or UNION
     */
    @Override
    public BlockType getBlockType() {
        return this.block_type;
    }

    /**
     * ブロックタイプを設定する.
     * @param   type    構造体タイプ:STRUCT or UNION
     */
    public void setBlockType(BlockType type) {
        this.block_type = type;
    }


     /**
      * 変数リストを取得する.
      */
     @Override
     public Set<Variable> getAllVariables() {
         Set<Variable> list = new HashSet<Variable>();
         if (this.definitions != null) {
             for (VariableDefinition def : this.definitions) {
                VariableType type = (VariableType)def.getType();
                if (type != null && type.getStructure() != null) {
                    Structure structure = type.getStructure();
                    if (this == structure) continue;
                    if (structure.isRecursiveStruct(this)) continue;
                }
                 Set<Variable> vars = def.getAllVariables();
                 if (vars != null && vars.size() <= 0) {
                     list.addAll(vars);
                 }
             }
         }

         if (list.size() <= 0) return null;
         return list;
     }

    /**
     * 子要素を返す。
     * @return 子要素。無ければ空のリストを返す
     */
    @Override
    public List<IBlock> getChildren() {
        List<IBlock> list = new ArrayList<IBlock>();
        if (this.definitions == null) return list;
        for (VariableDefinition var : this.definitions) {
            list.add(var);
        }

        return list;
    }

    /**
     * 行番号のブロックを検索する
     * @param line            行番号
     * @return        行番号のブロック
     */
    @Override
    public IBlock[] searchCodeLine(CodeLine line) {
        if (line == null) return null;

        CodeLine thisstart = this.getStartCodeLine();
        CodeLine thisend = this.getEndCodeLine();
        if ( line.isOverlap(thisstart, thisend) ) {
            IBlock[] blocks = {this};
            return blocks;
        }

        return null;
    }

    /**
     * 親ブロックからIDeclarationsブロックを取得する.
     * @return    IDeclarationsブロック
     */
    @Override
    public ProgramUnit getScopeDeclarationsBlock() {
        if (this.mother == null) return null;
        if (this.isRecursiveStruct(this)) return null;
        return this.mother.getScopeDeclarationsBlock();
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
        return this.getAllVariables();
    }

    /**
     * 関数呼出を含む自身の子ブロックのリストを返す。
     * @return 子ブロックのリスト
     */
    public List<IBlock> getBlocks() {
        List<IBlock> blk = new ArrayList<IBlock>();
        if (this.definitions != null) {
            for (VariableDefinition def : this.definitions) {
                List<IBlock> list = def.getBlocks();
                if (list != null) {
                    blk.addAll(list);
                }
            }
        }
        return blk;
    }

    /**
     * 構造体が再帰構造であるかチェックする
     * @return        true=再帰構造
     */
    public boolean hasRecursiveMember() {

        if (this.getDefinitions() != null) {
            for (VariableDefinition def : this.getDefinitions()) {
                if (!def.isStruct()) continue;
                jp.riken.kscope.language.fortran.Structure type_def = ((VariableType)def.getType()).getStructure();
                if (this == type_def) return true;
                if (this.hasRecursiveMember(this)) return true;
            }
        }

        return false;
    }

    /**
     * 構造体が再帰構造であるかチェックする
     * @return        true=再帰構造
     */
    public boolean hasRecursiveMember(jp.riken.kscope.language.fortran.Structure root) {
        if (root == null) return false;
        if (root == this) return true;

        if (this.getDefinitions() != null) {
            for (VariableDefinition def : this.getDefinitions()) {
                if (!def.isStruct()) continue;
                jp.riken.kscope.language.fortran.Structure type_def = ((VariableType)def.getType()).getStructure();
                if (root == type_def) return true;
            }
        }

        return false;
    }


    /**
     * 構造体が再帰構造であるかチェックする
     * @return        true=再帰構造
     */
    private boolean isRecursiveStruct(IBlock block) {

        IBlock mother_block = block.getMotherBlock();
        if (mother_block == this) return true;
        while (mother_block != this) {
            if (mother_block == this) return true;
            if (mother_block == null) return false;
            mother_block = mother_block.getMotherBlock();
        }
        if (mother_block != null) return true;

        Structure block_struct = null;
        if (block instanceof VariableDefinition) {
            VariableDefinition def = (VariableDefinition)block;
            if (def.getType() != null && def.getType().isStruct()) {
                block_struct = ((VariableType)def.getType()).getStructure();
            }
        }
        else if (block instanceof Structure) {
            block_struct = (Structure)block;
        }

        if (block_struct.getDefinitions() != null) {
            for (VariableDefinition def : this.getDefinitions()) {
                if ((IBlock)def == (IBlock)this) return true;
                if (this.isRecursiveStruct(def)) return true;
            }
        }

        return false;
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
        if (this.definitions == null || this.definitions.size() <= 0) return null;

        String name = mem_names[0].toLowerCase();
        if (name == null) return null;

        for (VariableDefinition mem : this.definitions) {
            VariableDefinition mem_def = mem.getStructMember(mem_names);
            if (mem_def != null) {
                return mem_def;
            }
        }

        return null;
    }

    /**
     * 終了:END文を持つブロックは、終了:END文を返す
     * @return        終了:END文
     */
    @Override
    public String toEndString() {
        if (this.getName() == null) return null;
        String buf = "end structure " + this.getName();
        return buf;
    }

    /**
     * 構造体メンバの場合、親構造体定義を取得する
     * @return        親構造体定義
     */
    public VariableDefinition getStructDefinition() {
        if (this.mother == null) return null;
        if (this.mother instanceof VariableDefinition) {
            return ((VariableDefinition)this.mother).getStructDefinition();
        }

        return null;
    }


    /**
     * 構造体定義文を出力する.
     * @return        構造体定義文
     */
    public String toStringStruct(int indent) {
        StringBuffer buf = new StringBuffer();

        String indent_column = "";
        if (indent > 0) {
            indent_column = StringUtils.repeat(" ", indent);
        }

        buf.append("type");
        if (this.isPublic()) {
            buf.append(",public ::");
        }
        buf.append(" ");
        buf.append(this.name);
        buf.append("\n");
        // SEQUENCE文
        if (this.is_sequence) {
            buf.append(indent_column);
            buf.append("sequence");
            buf.append("\n");
        }
        if (this.definitions != null) {
            for (VariableDefinition mem : this.definitions) {
                buf.append(indent_column);
                buf.append(mem.toString());
                buf.append("\n");
            }
        }
        buf.append("end type " + this.name);
        buf.append("\n");

        return buf.toString();
    }

    /**
     * 手続呼出しのリストを返す。
     * @return 手続呼出しのリスト
     */
    @Override
    public List<ProcedureUsage> getCalls() {
        if (this.definitions == null || this.definitions.size() <= 0) return null;

        List<ProcedureUsage> list = new ArrayList<ProcedureUsage>();
        for (VariableDefinition def : this.definitions) {
            List<ProcedureUsage> calls = def.getCalls();
            if (calls != null && calls.size() > 0) {
                list.addAll(calls);
            }
        }

        if (list.size() <= 0) return null;
        return list;
    }

    /**
     * public属性を設定する
     * @param    is_public        true=public属性を持つ
     */
    public void setPublic(boolean is_public) {
        if (is_public) {
            this.scope = VariableAttribute.ScopeAttribute.PUBLIC;
        }
        else {
            this.scope = VariableAttribute.ScopeAttribute.PRIVATE;
        }
        return;
    }

    /**
     * public属性を持つかチェックする
     * @return        true=public属性を持つ
     */
    public boolean isPublic() {
        if (this.scope == VariableAttribute.ScopeAttribute.PUBLIC) {
            return true;
        }
        return false;
    }

    /**
     * 変数定義文の変数名を出力する.
     * 構造体メンバの場合は、親構造体からの変数名（配列表記は除く）を出力する。
     * @return        変数名
     */
    public String toStructureName() {
        if (this.mother == null) return null;
        if (this.mother instanceof VariableDefinition) {
            return ((VariableDefinition)this.mother).toStructureName();
        }

        return null;
    }

    /**
     * sequence属性を取得する
     * @return       true=is_sequence属性を持つ
     */
    public boolean isSequence() {
        return this.is_sequence;
    }

    /**
     * sequence属性を設定する
     * @param    is_sequence        true=is_sequence属性を持つ
     */
    public void setSequence(boolean is_sequence) {
        this.is_sequence = is_sequence;
        return;
    }
}
