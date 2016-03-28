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
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.BlockType;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IDeclarations;
import jp.riken.kscope.language.Module;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.language.VariableDefinition;

/**
 * type型クラス。
 *
 * @author RIKEN
 *
 */
public class Type implements IBlock, Serializable {
    /** シリアル番号 */
    private static final long serialVersionUID = 5400440025290085684L;
    /**
     * structure型とほぼ同じなため、structureを包含して使う。
     */
    private Structure core;

    /**
     * コンストラクタ。
     */
    public Type() {
        this.core = new Structure();
    }

    /**
     * コンストラクタ。
     *
     * @param nm
     *         構造体の名前
     */
    public Type(String nm) {
        this.core = new Structure(nm);
    }

    /**
     * コピーコンストラクタ。
     *
     * @param   type
     */
    public Type(Type type) {
        if (type == null) return;
        this.core = type.core;
    }

    @Override
    public String toString() {
        return "type (" + this.getName() + ")";
    }

    /**
     * 変数定義文の追加。
     *
     * @param definition
     *          変数定義文
     */
    public void add(VariableDefinition definition) {
        this.core.add(definition);
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
        this.core.add(typ, nm);
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
        this.core.add(type, variableName);
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
        this.core.add(structure, variableName);
    }

    /**
     * union文の追加。
     *
     * @param union
     *          共用体
     */
    public void add(Union union) {
        this.core.add(union);
    }

    /**
     * 構造体名の取得。
     *
     * @return 構造体の名前
     */
    public String getName() {
        return this.core.getName();
    }

    /**
     * 構造体内の変数定義文リストの取得。
     *
     * @return 変数定義文リスト.無ければnullを返す。
     */
    public List<VariableDefinition> getDefinitions() {
        return this.core.getDefinitions();
    }

    /**
     * 構造体内の変数定義文リストの設定。
     * @param 変数定義文リスト
     */
    public void setDefinitions(List<VariableDefinition> list) {
        this.core.setDefinitions(list);
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
    public boolean matches(Type value) {
        if (value == null) { return false; }
        // 名前のチェックのみ
        return this.getName().equalsIgnoreCase(value.getName());
    }

    @Override
    public CodeLine getStartCodeLine() {
        if (this.core.getStartStatement() == null)
            return null;
        return this.core.getStartStatement().getLineInfo();
    }

    @Override
    public CodeLine getEndCodeLine() {
        if (this.core.getEndStatement() == null)
            return null;
        return this.core.getEndStatement().getLineInfo();
    }

    /**
     * 開始コード行情報を設定する。
     * @param line    開始コード行情報を設定する。
     */
    public void setStartCodeLine(CodeLine line) {
        if (this.core == null) return;
        this.core.setStartStatement(line);
    }

    /**
     * 終了コード行情報を設定する。
     * @param line    終了コード行情報を設定する。
     */
    public void setEndCodeLine(CodeLine line) {
        if (this.core == null) return;
        this.core.setEndStatement(line);
    }

    @Override
    public BlockType getBlockType() {
        return BlockType.TYPE;
    }

    @Override
    public IBlock getMotherBlock() {
        return this.core.getMotherBlock();
    }

    /**
     * 親ブロックを設定する.
     * @param block        親ブロック
     */
    public void setMotherBlock(IBlock block) {
        this.core.setMotherBlock(block);
    }

     /**
      * 変数リストを取得する.
      */
     @Override
     public Set<Variable> getAllVariables() {
         return this.core.getAllVariables();
     }

    /**
     * ファイルタイプ（C言語、Fortran)を取得する.
     * @return        ファイルタイプ（C言語、Fortran)
     */
    @Override
    public jp.riken.kscope.data.FILE_TYPE getFileType() {
        return core.getFileType();
    }

    /**
     * 子要素を返す。
     * @return 子要素。無ければ空のリストを返す
     */
    @Override
    public List<IBlock> getChildren() {
        return this.core.getChildren();
    }

    /**
     * 行番号のブロックを検索する
     * @param line            行番号
     * @return        行番号のブロック
     */
    @Override
    public IBlock[] searchCodeLine(CodeLine line) {
        return this.core.searchCodeLine(line);
    }



    /**
     * ファイルタイプがC言語であるかチェックする.
     * @return         true = C言語
     */
    @Override
    public boolean isClang() {
        return this.core.isClang();
    }

    /**
     * ファイルタイプがFortranであるかチェックする.
     * @return         true = Fortran
     */
    @Override
    public boolean isFortran() {
        return this.core.isFortran();
    }

    /**
     * 親ブロックからIDeclarationsブロックを取得する.
     * @return    IDeclarationsブロック
     */
    @Override
    public IDeclarations getScopeDeclarationsBlock() {
        if (this.core == null) return null;
        return this.core.getScopeDeclarationsBlock();
    }

    /**
     * 子ブロックのIDeclarationsブロックを検索する.
     * @return    IDeclarationsブロックリスト
     */
    @Override
    public Set<IDeclarations> getDeclarationsBlocks() {
        if (this.core == null) return null;
        return this.core.getDeclarationsBlocks();
    }

    /**
     * Procedureブロックを習得する。
     * @return    Procedureブロック
     */
    @Override
    public Procedure getProcedureBlock() {
        if (this.core == null) return null;
        return this.core.getProcedureBlock();
    }

    /**
     * Moduleブロックを習得する。
     * @return    Moduleブロック
     */
    @Override
    public Module getModuleBlock() {
        if (this.core == null) return null;
        return this.core.getModuleBlock();
    }


    /**
     * プロシージャ（関数）からブロックまでの階層文字列表記を取得する
     * 階層文字列表記 : [main()]-[if (...)]-[if (...)]
     * CompoundBlock（空文）は省略する.
     * @return      階層文字列表記
     */
    @Override
    public String toStringProcedureScope() {
        if (this.core == null) return null;
        return this.core.toStringProcedureScope();
    }


    /**
     * モジュールからブロックまでの階層文字列表記を取得する
     * 階層文字列表記 : [main()]-[if (...)]-[if (...)]
     * CompoundBlock（空文）は省略する.
     * @return      階層文字列表記
     */
    @Override
    public String toStringModuleScope() {
        if (this.core == null) return null;
        return this.core.toStringModuleScope();
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
        if (this.core == null) return null;
        return this.core.toStringScope(module);
    }


    /**
     * 式の変数リストを取得する.
     * ブロックのみの変数リストを取得する。
     * @return        式の変数リスト
     */
    @Override
    public Set<Variable> getBlockVariables() {
        return this.core.getBlockVariables();
    }

    /**
     * 関数呼出を含む自身の子ブロックのリストを返す。
     * @return 子ブロックのリスト
     */
    public List<IBlock> getBlocks() {
        return this.core.getBlocks();
    }

    /**
     * 構造体メンバの変数定義を取得する
     * @param mem_names        構造体変数メンバ文字列
     * @return        構造体メンバの変数定義
     */
    public VariableDefinition getStructMember(String mem_name) {
        return this.core.getStructMember(mem_name);
    }

    /**
     * 構造体メンバの変数定義を取得する
     * @param mem_names        構造体変数メンバ文字列
     * @return        構造体メンバの変数定義
     */
    public VariableDefinition getStructMember(String[] mem_names) {
        return this.core.getStructMember(mem_names);
    }
}
