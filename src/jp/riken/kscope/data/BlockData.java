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
package jp.riken.kscope.data;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jp.riken.kscope.information.InformationBlocks;
import jp.riken.kscope.information.TextInfo;
import jp.riken.kscope.language.Block;
import jp.riken.kscope.language.BlockType;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IDeclarations;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.language.Module;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.Statement;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.language.VariableDefinition;

/**
 * Blockのデータクラス
 * @author RIKEN
 */
public class BlockData implements IBlock {

    private Statement start;
    private final IBlock current;

    /**
     * コンストラクタ
     * @param current        元ブロッククラス
     */
    public BlockData(IBlock current) {
        this.current = current;
        if (current != null && current.getStartCodeLine() != null) {
            CodeLine lineinfo = new CodeLine(current.getStartCodeLine());
            this.start = new Statement(lineinfo);
        }
    }


    /**
     * コンストラクタ
     * @param lineinfo        コード行情報
     * @param current        元ブロッククラス
     */
    public BlockData(CodeLine lineinfo, IBlock current) {
        this.start = new Statement(lineinfo);
        this.current = current;
    }

    /**
     * コード行情報を設定する。
     * @param lineinfo          開始コード行情報
     */
    public void setBlockLine(CodeLine lineinfo) {
        this.start = new Statement(lineinfo);
    }

    @Override
    public BlockType getBlockType() {
        return current.getBlockType();
    }

    @Override
    public CodeLine getStartCodeLine() {
        if (this.start == null) return null;
        return this.start.getLineInfo();
    }

    @Override
    public CodeLine getEndCodeLine() {
        return this.getStartCodeLine();
    }

    @Override
    public IBlock getMotherBlock() {
        if (this.current == null) return null;
        return this.current.getMotherBlock();
    }

    @Override
    public Set<Variable> getAllVariables() {
        if (this.current == null) return null;
        return this.current.getAllVariables();
    }

    @Override
    public Set<Variable> getBlockVariables() {
        if (this.current == null) return null;
        return this.current.getBlockVariables();
    }

    @Override
    public FILE_TYPE getFileType() {
        if (this.current == null) return null;
        return this.current.getFileType();
    }

    @Override
    public List<IBlock> getChildren() {
        if (this.current == null) return null;
        List<IBlock> list = new ArrayList<IBlock>();
        if (this.current.getChildren() != null) {
            list.addAll(this.current.getChildren());
        }
        if (this.current instanceof Procedure) {
            if (((Procedure)this.current).getBody() != null
                && ((Procedure)this.current).getBody().getChildren() != null) {
                list.addAll(((Procedure)this.current).getBody().getChildren());
            }
            Map<String, VariableDefinition> variables = ((Procedure)this.current).getVariableDefinitionMap();
            if (variables != null) {
                for (VariableDefinition def : variables.values()) {
                    list.add(def);
                }
            }
        }
        return list;
    }

    @Override
    public IBlock[] searchCodeLine(CodeLine line) {
        if (this.current == null) return null;
        return this.current.searchCodeLine(line);
    }

    @Override
    public boolean isClang() {
        if (this.current == null) return false;
        return this.current.isClang();
    }

    @Override
    public boolean isFortran() {
        if (this.current == null) return false;
        return this.current.isFortran();
    }

    @Override
    public IDeclarations getScopeDeclarationsBlock() {
        if (this.current == null) return null;
        return this.current.getScopeDeclarationsBlock();
    }

    @Override
    public Set<IDeclarations> getDeclarationsBlocks() {
        if (this.current == null) return null;
        return this.current.getDeclarationsBlocks();
    }

    /**
     * Procedureブロックを習得する。
     * @return    Procedureブロック
     */
    @Override
    public Procedure getProcedureBlock() {
        if (this.current == null) return null;
        return this.current.getProcedureBlock();
    }


    /**
     * Moduleブロックを習得する。
     * @return    Moduleブロック
     */
    @Override
    public Module getModuleBlock() {
        if (this.current == null) return null;
        return this.current.getModuleBlock();
    }

    /**
     * ブロックの文字列表現を取得する.
     */
    @Override
    public String toString() {
        if (this.current == null) return null;
        return this.current.toString();
    }


    @Override
    public String toStringProcedureScope() {
        return this.toStringScope(false);
    }

    @Override
    public String toStringModuleScope() {
        return this.toStringScope(true);
    }

    @Override
    public String toStringScope(boolean module) {
        if (this.current == null) return null;
        return this.current.toStringScope(module);
    }

    /**
     * 開始行番号を設定する
     * @param lineno        開始行番号
     */
    public void setStartLineno(int lineno) {
        if (this.start == null) return;
        if (this.start.getLineInfo() == null) return;
        CodeLine lineinfo = this.start.getLineInfo();
        lineinfo.setLine(lineno);
    }

    /**
     * 修了行番号を設定する。
     * @param lineno        修了行番号
     */
    public void setEndLineno(int lineno) {
        if (this.start == null) return;
        if (this.start.getLineInfo() == null) return;
        CodeLine lineinfo = this.start.getLineInfo();
        lineinfo.setEndLine(lineno);
    }


    /**
     * @return current
     */
    public IBlock getCurrentBlock() {
        return current;
    }

    /**
     * 関数呼出を含む自身の子ブロックのリストを返す。
     * @return 子ブロックのリスト
     */
    public List<IBlock> getBlocks() {
        return current.getBlocks();
    }

}
