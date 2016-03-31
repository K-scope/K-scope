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
package jp.riken.kscope.data;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import jp.riken.kscope.language.BlockType;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.ProcedureUsage;
import jp.riken.kscope.language.ProgramUnit;
import jp.riken.kscope.language.Variable;

/**
 * Blockのリストクラス
 * @author RIKEN
 */
public class BlockList implements IBlock {
    /** ブロックリスト */
    private List<IBlock> blocks;

    /**
     * コンストラクタ
     */
    public BlockList() {
        this.blocks = new ArrayList<IBlock>();
    }

    /**
     * コンストラクタ
     * @param  list    ブロックリスト
     */
    public BlockList(IBlock[] list) {
        if (list != null && list.length > 0) {
            this.blocks = new ArrayList<IBlock>();
            this.blocks.addAll(Arrays.asList(list));
        }
    }

    /**
     * ブロックリストを取得する.
     * @return        ブロックリスト
     */
    public List<IBlock> getBlocks() {
        return this.blocks;
    }

    /**
     * ブロックリストを設定する.
     * @param list        ブロックリスト
     */
    public void setBlocks(List<IBlock> list) {
        this.blocks = list;
    }

    /**
     * ブロックリスト数を取得する.
     * @return        ブロックリスト数
     */
    public int getBlockCount() {
        if (this.blocks == null) return 0;
        return this.blocks.size();
    }

    /**
     * ブロックを追加する.
     * @param block        追加ブロック
     */
    public void addBlock(IBlock block) {
        if (block == null) return;
        if (this.blocks == null) {
            this.blocks = new ArrayList<IBlock>();
        }
        if (block instanceof BlockList) {
            this.blocks.addAll(((BlockList)block).getBlocks());
        }
        else {
        this.blocks.add(block);
        }
    }

    /**
     * 開始コード行情報を取得する.
     * ブロックリストの最初のブロックの開始コード行情報を渡す.
     */
    @Override
    public CodeLine getStartCodeLine() {
        if (this.blocks == null || this.blocks.size() <= 0) return null;
        return this.blocks.get(0).getStartCodeLine();
    }

    /**
     * 終了コード行情報を取得する.
     * ブロックリストの最後のブロックの終了コード行情報を渡す.
     */
    @Override
    public CodeLine getEndCodeLine() {
        if (this.blocks == null || this.blocks.size() <= 0) return null;
        int index = this.blocks.size() - 1;
        return this.blocks.get(index).getEndCodeLine();
    }

    /**
     * ブロックリストクラスのブロックタイプはUNKNOWNとする.
     */
    @Override
    public BlockType getBlockType() {
        return BlockType.UNKNOWN;
    }

    @Override
    public IBlock getMotherBlock() {
        return null;
    }

    /**
     * ブロックリストから変数リストを取得する.
     */
    @Override
    public Set<Variable> getAllVariables() {
        if (this.blocks == null || this.blocks.size() <= 0) return null;
        Set<Variable> list = new HashSet<Variable>();
        for (IBlock block : this.blocks) {
            if (block == null) continue;
            Set<Variable> vars = block.getAllVariables();
            if (vars != null) {
                list.addAll(vars);
            }
        }
        if (list.size() <= 0) return null;
        return list;
    }

    /**
     * 式の変数リストを取得する.
     * ブロックのみの変数リストを取得する。
     * @return        式の変数リスト
     */
    @Override
    public Set<Variable> getBlockVariables() {
        if (this.blocks == null || this.blocks.size() <= 0) return null;
        Set<Variable> list = new HashSet<Variable>();
        for (IBlock block : this.blocks) {
            if (block == null) continue;
            Set<Variable> vars = block.getBlockVariables();
            if (vars != null) {
                list.addAll(vars);
            }
        }
        if (list.size() <= 0) return null;
        return list;
    }

    /**
     * ブロックリストの文字列表現を取得する.
     * ブロックリストの開始 - 終了の文字列表現を取得する.
     */
    @Override
    public String toString() {
        if (this.blocks == null || this.blocks.size() <= 0) return null;
        int last = this.blocks.size() - 1;
        String start = this.blocks.get(0).toString();
        String end = this.blocks.get(last).toString();
        if (this.blocks.size() == 1) {
            return start;
        }
        // [start] - [end]
        StringBuffer buf = new StringBuffer();
        buf.append("[");
        buf.append(start);
        buf.append("]");
        buf.append(" - ");
        buf.append("[");
        buf.append(end);
        buf.append("]");
        return buf.toString();
    }

    /**
     * ブロックがリストに登録済みかチェックする.
     * @param block        ブロック
     * @return        true=登録済み
     */
    public boolean contains(IBlock block) {
        if (this.blocks == null || this.blocks.size() <= 0) return false;
        return this.blocks.contains(block);
    }

    /**
     * 子要素を返す。
     *
     * @return 子要素。無ければ空のリストを返す
     */
    @Override
    public List<IBlock> getChildren() {
        List<IBlock> list = new ArrayList<IBlock>();

        for (IBlock block : this.blocks) {
            if (block == null) continue;
            List<IBlock> childs = block.getChildren();
            if (childs != null && childs.size() > 0) {
                list.addAll(childs);
            }
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
        List<IBlock> list = new ArrayList<IBlock>();

        for (IBlock block : this.blocks) {
            if (block == null) continue;
            IBlock[] childs = block.searchCodeLine(line);
            if (childs != null && childs.length > 0) {
                list.addAll(Arrays.asList(childs));
            }
        }
        if (list.size() <= 0) return null;

        return list.toArray(new IBlock[0]);
    }

    /**
     * 親ブロックからIDeclarationsブロックを取得する.
     * @return    IDeclarationsブロック
     */
    @Override
    public ProgramUnit getScopeDeclarationsBlock() {
        if (this.blocks == null) return null;

        for (IBlock block : this.blocks) {
            if (block == null) continue;
            return block.getScopeDeclarationsBlock();
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
        if (this.blocks == null) return null;
        if (this.blocks.size() <= 0) return null;
        if (this.blocks.get(0) == null) return null;

        // 開始ブロックのみの表記とする
        String statement = null;
        if (module) statement = this.blocks.get(0).toStringModuleScope();
        else statement = this.blocks.get(0).toStringProcedureScope();

        return statement;
    }


    /**
     * 終了:END文を持つブロックは、終了:END文を返す
     * @return        終了:END文
     */
    @Override
    public String toEndString() {
        return null;
    }


    /**
     * 手続呼出しのリストを返す。
     * @return 手続呼出しのリスト
     */
    @Override
    public List<ProcedureUsage> getCalls() {

        if (this.blocks == null || this.blocks.size() <= 0) return null;

        List<ProcedureUsage> list = new ArrayList<ProcedureUsage>();
        for (IBlock block : this.blocks) {
            if (block == null) continue;
            List<ProcedureUsage> calls = block.getCalls();
            if (calls != null && calls.size() > 0) {
                list.addAll(calls);
            }
        }
        if (list.size() <= 0) return null;
        return list;
    }
}

