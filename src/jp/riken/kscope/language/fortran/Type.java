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
import java.util.List;
import java.util.Set;

import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.BlockType;
import jp.riken.kscope.language.IBlock;
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
        core = new Structure();
    }

    /**
     * コンストラクタ。
     *
     * @param nm
     *         構造体の名前
     */
    public Type(String nm) {
        core = new Structure(nm);
    }

    @Override
    public String toString() {
        return "type " + this.getName();
    }
    /**
     * 変数定義文の追加。
     *
     * @param definition
     *          変数定義文
     */
    public void add(VariableDefinition definition) {
        core.add(definition);
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
        core.add(typ, nm);
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
        core.add(type, variableName);
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
        core.add(structure, variableName);
    }

    /**
     * union文の追加。
     *
     * @param union
     *          共用体
     */
    public void add(Union union) {
        core.add(union);
    }

    /**
     * 構造体名の取得。
     *
     * @return 構造体の名前
     */
    public String getName() {
        return core.getName();
    }

    /**
     * 構造体内の変数定義文リストの取得。
     *
     * @return 変数定義文リスト.無ければnullを返す。
     */
    public List<VariableDefinition> getDefinitions() {
        return core.getDefinitions();
    }

    /**
     * 構造体内の変数定義文リストの設定。
     * @param 変数定義文リスト
     */
    public void setDefinitions(List<VariableDefinition> list) {
    	core.setDefinitions(list);
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
     * @param line	開始コード行情報を設定する。
     */
    public void setStartCodeLine(CodeLine line) {
        if (this.core == null) return;
        this.core.setStartStatement(line);
    }

    /**
     * 終了コード行情報を設定する。
     * @param line	終了コード行情報を設定する。
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
     * @param block		親ブロック
     */
    public void setMotherBlock(IBlock block) {
        this.core.setMotherBlock(block);
    }

 	/**
 	 * 変数リストを取得する.
 	 */
 	@Override
 	public Set<Variable> getAllVariables() {
 		return null;
 	}
}
