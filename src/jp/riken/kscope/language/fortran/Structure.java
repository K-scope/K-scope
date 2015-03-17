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
import java.util.List;
import java.util.Set;

import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.BlockType;
//import jp.riken.kscope.language.Block;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Statement;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.language.VariableDefinition;

/**
 * structure型クラス.
 * C言語:構造体、共同体、列挙体を表現する.
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
     * 変数定義文の追加。
     *
     * @param definition
     *          変数定義文
     */
    public void add(VariableDefinition definition) {
        if (definition != null) {
            definitions.add(definition);
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
            definitions.add(definition);
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
        if (this.getStartStatement() == null)
            return null;
        return this.getStartStatement().getLineInfo();
    }

    @Override
    public CodeLine getEndCodeLine() {
        if (this.getEndStatement() == null)
            return null;
        return this.getEndStatement().getLineInfo();
    }

    /**
     * 開始コード行情報を設定する。
     * @param line	開始コード行情報を設定する。
     */
    public void setStartCodeLine(CodeLine line) {
        this.setStartStatement(line);
    }

    /**
     * 終了コード行情報を設定する。
     * @param line	終了コード行情報を設定する。
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
         return null;
     }
}
