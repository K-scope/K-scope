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

import java.util.ArrayList;
import java.util.List;

import jp.riken.kscope.data.CodeLine;

/**
 * 手続きの実行文領域を表現するクラス。 ソースコードの対象領域と行情報に基づく対応付けは実施しない。
 *
 * @author RIKEN
 *
 */
public class ExecutableBody extends Block {
    /** シリアル番号 */
    private static final long serialVersionUID = 1540246071061765356L;
    /** 親プロシージャ */
    private Procedure parent = null;
    /** データベースの現在カーソルブロック */
    private transient Block currentBlock; // パース時に使用する作業用メンバ変数

    // ++++++++++++++++++++++++++++++++++++++++++++

    /**
     * コンストラクタ。
     *
     * @param prnt
     *           親プログラムユニット
     */
    public ExecutableBody(Procedure prnt) {
        this();
        this.parent = prnt;
    }

    /**
     * コンストラクタ。
     */
    protected ExecutableBody() {
        super();
        this.set_block_start(new CodeLine("block start", 1));
        this.set_block_end(new CodeLine("block end", 1));
        currentBlock = this;
    }
    /**
     * ブロックタイプの取得。
     *
     * @return BlockType.BODY
     */
    @Override
    public BlockType getBlockType() {
        return BlockType.BODY;
    }
    // ++++++++++++++++++++++++++++++++++++++++++++

    protected List<ProcedureUsage> getCalls() {
        List<ProcedureUsage> calls = new ArrayList<ProcedureUsage>();
        this.getCalls(calls);
        return calls;
    }

    // ++++++++++++++++++++++++++++++++++++++++++++

    protected Block[] get_blocks() {
        ArrayList<Block> blocks = new ArrayList<Block>();
        this.get_blocks(blocks);
        Block[] block_array = new Block[blocks.size()];
        for (int i = 0; i < blocks.size(); i++)
            block_array[i] = blocks.get(i);
        return block_array;
    }

    // ++++++++++++++++++++++++++++++++++++++++++++

    /**
     * カレントブロックを返す。
     *
     * @return ブロック
     */
    public Block getCurrentBlock() {
        return this.currentBlock;
}

    /**
     * @param blk
     */
    public void setCurrentBlock(Block blk) {
        currentBlock = blk;
    }

    /**
     * 親プログラムユニットの取得。<br>
     *
     * @return 親プログラムユニット
     */
    public Procedure getParent() {
        return parent;
    }

    @Override
    protected String toStringBase() {
        return super.toStringBase();
    }

    // ------------------Block-------------------//

    protected void set_start_label(String label) {
        currentBlock.get_start().set_label(label);
    }

    // ++++++++++++++++++++++++++++++++++++++++++++
    protected void set_end_label(String label) {
        currentBlock.get_end().set_label(label);
    }

    // ++++++++++++++++++++++++++++++++++++++++++++
    protected void end_block() {
        currentBlock = currentBlock.get_mother();
    }

    // ---------------Selection----------------//
    /**
     * 分岐処理の開始を設定する。
     *
     * @param lineInfo
     *            コード行情報
     * @param label
     *            行ラベル。無い場合はStatement.NO_LABELをセットする。
     * @param type
     *            分岐処理の型
     */
    protected void start_selection(CodeLine lineInfo, String label,
            jp.riken.kscope.language.Selection.SelectionType type) {
        Selection new_block = new Selection(currentBlock, type);
        start_block(lineInfo, label, new_block);
    }

    /**
     * 分岐処理の終了を設定する。
     *
     * @param lineInfo
     *            コード行情報
     * @param label
     *            行ラベル。ない場合はStatement.NO_LABELをセットする。
     */
    protected void end_selection(CodeLine lineInfo, String label) {
        end_block(lineInfo, label);
    }

    /**
     * 分岐処理の追加ブロックの設定。else if文やelse文に相当する。
     *
     * @param cond
     *            条件式
     */
    protected void start_condition(Expression cond, CodeLine lineInfo,
            String label) {
        Condition newCond = new Condition(currentBlock, cond);
        newCond.set_block_start(lineInfo);
        newCond.get_start().set_label(label);
        ((Selection) currentBlock).getConditions().add(newCond);
        currentBlock = newCond;
    }

    protected void end_condition(CodeLine lineInfo, String label) {
        end_block(lineInfo, label);
    }

    // -------------User_defined--------------//

    protected void start_user_defined(CodeLine lineInfo) {
        UserDefined new_block = new UserDefined(currentBlock);
        start_block(lineInfo, new_block);
    }

    // ++++++++++++++++++++++++++++++++++++++++++++
    protected void start_user_defined(CodeLine lineInfo, String label) {
        UserDefined new_block = new UserDefined(currentBlock);
        start_block(lineInfo, label, new_block);
    }

    // ++++++++++++++++++++++++++++++++++++++++++++
    protected void end_user_defined(CodeLine lineInfo) {
        end_block(lineInfo);
    }

    // ++++++++++++++++++++++++++++++++++++++++++++
    protected void end_user_defined(CodeLine lineInfo, String label) {
        end_block(lineInfo, label);
    }

    // ++++++++++++++++++++++++++++++++++++++++++++

    // -----------Procedure_usage------------//

    protected void add_procedure_usage(ProcedureUsage sub_call) {
        currentBlock.add_child(sub_call);
    }

    // ++++++++++++++++++++++++++++++++++++++++++++

    /**
     * CALL文, FUNCTION文の開始行を設定する。
     *
     * @param lineInfo
     *            コード行情報
     */
    protected void start_procedure_usage(CodeLine lineInfo) {
        ProcedureUsage new_block = new ProcedureUsage(currentBlock);
        start_block(lineInfo, new_block);
    }

    /**
     * CALL文, FUNCTION文の開始行を設定する。
     *
     * @param lineInfo
     *            コード行情報
     * @param label
     *            行ラベル
     */
    protected void start_procedure_usage(CodeLine lineInfo, String label) {
        ProcedureUsage new_block = new ProcedureUsage(currentBlock);
        start_block(lineInfo, label, new_block);
    }

    /**
     * CALL文, FUNCTION文の開始行を設定する。
     *
     * @param lineInfo
     *            コード行情報
     * @param label
     *            行ラベル
     * @param subroutineName
     *            CALLサブルーチン名
     * @param arguments
     *            引数リスト
     */
    protected void start_procedure_usage(CodeLine lineInfo, String label,
            String subroutineName, List<Expression> arguments) {
        ProcedureUsage new_block = new ProcedureUsage(currentBlock,
                subroutineName, arguments);
        start_block(lineInfo, label, new_block);
    }

    /**
     * 開始ブロックを設定する。
     *
     * @param lineInfo
     *            開始コード行情報
     * @param blk
     *            開始ブロック
     */
    protected void start_block(CodeLine lineInfo, Block blk) {
        blk.set_block_start(lineInfo);

        if (currentBlock == null)
            return;

        currentBlock.add_child(blk);
        currentBlock = blk;
    }

    /**
     * 開始ブロックを設定する。（ラベル付き）
     *
     * @param lineInfo
     *            開始コード行情報
     * @param label
     *            行ラベル
     * @param blk
     *            開始ブロック
     */
    protected void start_block(CodeLine lineInfo, String label, Block blk) {
        start_block(lineInfo, blk);
        currentBlock.get_start().set_label(label);
    }

    /**
     * CALL文, FUNCTION文の終了行を設定する。
     *
     * @param lineInfo
     *            コード行情報
     */
    protected void end_procedure_usage(CodeLine lineInfo) {
        end_block(lineInfo);
    }

    /**
     * CALL文, FUNCTION文の終了行を設定する。（ラベル付き）
     *
     * @param lineInfo
     *            コード行情報
     * @param label
     *            行ラベル
     */
    protected void end_procedure_usage(CodeLine lineInfo, String label) {
        end_block(lineInfo, label);
    }

    /**
     * 終了コード行情報を設定する。
     *
     * @param lineInfo
     *            終了コード行情報
     */
    protected void end_block(CodeLine lineInfo) {
        if (currentBlock == null)
            return;

        currentBlock.set_block_end(lineInfo);
        currentBlock = currentBlock.get_mother();
    }

    /**
     * 終了ブロックを設定する。（ラベル付き）
     *
     * @param lineInfo
     *            終了コード行情報
     * @param label
     *            行ラベル
     */
    protected void end_block(CodeLine lineInfo, String label) {
        currentBlock.set_block_end(lineInfo);
        currentBlock.get_end().set_label(label);
        currentBlock = currentBlock.get_mother();
    }



    /**
     * DO文の開始行を設定する。
     *
     * @param lineInfo
     *            コード行情報
     */
    protected void start_repetition(CodeLine lineInfo) {
        Repetition new_block = new Repetition(currentBlock);
        start_block(lineInfo, new_block);
    }

    /**
     * DO文の終了行を設定する。
     *
     * @param lineInfo
     *            コード行情報
     */
    protected void end_repetition(CodeLine lineInfo) {
        end_block(lineInfo);
    }

    /**
     * DO文の開始行を設定する。（ラベル付き）
     *
     * @param lineInfo
     *            コード行情報
     * @param label
     *            行ラベル
     */
    protected void start_repetition(CodeLine lineInfo, String label) {
        Repetition new_block = new Repetition(currentBlock);
        start_block(lineInfo, label, new_block);
    }

    /**
     * DO文の終了行を設定する。（ラベル付き）
     *
     * @param lineInfo
     *            コード行情報
     * @param label
     *            行ラベル
     */
    protected void end_repetition(CodeLine lineInfo, String label) {
        end_block(lineInfo, label);
    }

    /**
     * CONTINUE文を設定する。
     *
     * @param lineInfo
     *            コード行情報
     * @param label
     *            行ラベル
     */
    protected void set_continue(CodeLine lineInfo, String label) {
        if (currentBlock.get_start().is_labeled()) {
            if (label.equals(currentBlock.get_start().get_label())) {
                end_repetition(lineInfo, label);
                set_continue(lineInfo, label);
            }
        }
    }

    /**
     * Substitution文の開始行を設定する。
     *
     * @param lineInfo
     *            コード行情報
     */
    protected void startSubstitution(CodeLine lineInfo) {
        Substitution new_block = new Substitution(currentBlock);
        start_block(lineInfo, new_block);
    }

    protected void endSubstitution(CodeLine lineInfo) {
        end_block(lineInfo);
    }

    /**
     * Return文を設定する。
     *
     * @param lineInfo
     *            コード行情報
     */
    protected void setReturn(CodeLine lineInfo) {
        Return new_block = new Return(currentBlock);
        start_block(lineInfo, new_block);
        end_block(lineInfo);
    }

    /**
     * IDを取得する。
     *
     * @return ID
     */
    @Override
    public String getID() {
        String result = "";
        if (this.parent != null) {
        	// modify by @hira at 2013/03/01
            // int offset = this.getStartPos() - this.parent.getStartPos();
            // result = this.parent.getID() + "#" + offset + ":" + this.toStringBase();
        	result = this.parent.getID() + ":" + this.toStringBase();
        } else {
            result = this.toStringBase();
        }
        return result;
    }

    /**
     * 名前空間（モジュール名.ルーチン名）を取得する。
     *
     * @return 名前空間（モジュール名.ルーチン名）
     */
    @Override
    public String getNamespace() {
        String result = "";
        if (this.parent != null) {
            result = this.parent.getNamespace();
        }
        return result;
    }

    /**
     * 構造IDを取得する。
     *
     * @return 構造ID
     */
    @Override
    public String getLayoutID() {
        String result = "";
        if (this.parent != null) {
        	result = this.parent.getLayoutID() + ":";
        }
        return result;
    }

}
