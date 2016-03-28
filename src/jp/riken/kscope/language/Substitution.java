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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.information.InformationBlocks;

/**
 * 代入文を表すクラス。
 *
 * @author RIKEN
 *
 */

public class Substitution extends Block {
    /** シリアル番号 */
    private static final long serialVersionUID = 6743876701008619698L;
    /** 左辺式 */
    private Expression leftVar;
    /** 右辺式 */
    private Expression rightVar;

    /**
     *
     * コンストラクタ。
     */
    public Substitution() {
        super();
    }

    /**
     *
     * コンストラクタ。
     * @param mama 親ブロック
     */
    Substitution(Block mama) {
        super(mama);
    }
    /**
     * ブロックタイプの取得。
     *
     * @return BlockType.SUBSTITUTION
     */
    public BlockType getBlockType() {
        return BlockType.SUBSTITUTION;
    }
    // ++++++++++++++++++++++++++++++++++++++++++++

    /**
     * 右辺をセットする。
     * @param ex 右辺の変数
     */
    public void setRight(Expression ex) {
        this.rightVar = ex;

        // 手続呼出（関数呼出）文の親情報、行情報を設定する
        setProcedureUsages(ex);

        // 親代入文を設定する.
        if (this.rightVar != null) {
            this.rightVar.setParentStatement(this);
        }
    }
    /**
     * 左辺をセットする。
     * @param ex 左辺の変数
     */
    public void setLeft(Expression ex) {
        this.leftVar = ex;

        // 手続呼出（関数呼出）文の親情報、行情報を設定する
        setProcedureUsages(ex);

        // 親代入文を設定する.
        if (this.leftVar != null) {
            this.leftVar.setParentStatement(this);
        }
    }

    /**
     * 手続呼出（関数呼出）文の親情報、行情報を設定する
     * @param ex        式クラス
     */
    private void setProcedureUsages(Expression ex) {

        //TODO 本来はパース時点でセット出来れば無駄な処理をせずに済むので要検討
        CodeLine lineInfo = this.getStartCodeLine();
        String label = this.get_start().get_label();
        List<ProcedureUsage> pus = ex.getFuncCalls();
        for (ProcedureUsage pu:pus) {
            pu.set_mother(this.get_mother());
            pu.setTypeIsFunction();
            pu.set_block_start(lineInfo);
            pu.set_block_end(lineInfo);
            pu.get_start().set_label(label);
            pu.get_end().set_label(label);
        }
    }


    /**
     * 右辺を取得する。
     * @return 右辺の変数
     */
    public Expression getRightValue() {
        return this.rightVar;
    }
    /**
     * 左辺を取得する。
     * @return 左辺の変数
     */
    public Expression getLeftValue() {
        return this.leftVar;
    }

    /**
     * 関数呼出を含むすべての子ブロックを取得する
     */
    @Override
    public List<IBlock> getBlocks() {
        List<IBlock> blk = new ArrayList<IBlock>();
        List<IBlock> list = super.getBlocks();
        if (list != null) blk.addAll(list);
        for (ProcedureUsage pu : this.rightVar.getFuncCalls()) {
            blk.add(pu);
        }
        return blk;
    }

    /**
     * 付加情報ブロックコレクションを生成する。
     *
     * @return 付加情報ブロックコレクション
     */
    @Override
    public InformationBlocks createInformationBlocks() {
        InformationBlocks result = new InformationBlocks();
        result.addAll(super.createInformationBlocks());
        if (this.leftVar != null) {
            result.addAll(this.leftVar.createInformationBlocks());
        }
        if (this.rightVar != null) {
            result.addAll(this.rightVar.createInformationBlocks());
        }
        return result;
    }

    /**
     * idにマッチした情報ブロックを検索する。
     * @param id
     *          ID
     * @return 見つかった情報ブロック。見つからなかった場合はnullが返ります。
     */
    @Override
    public IInformation findInformationBlockBy(String id) {
        IInformation result = super.findInformationBlockBy(id);

        if (result == null && this.getID().equals(id)) {
            result = this;
        }

        if (result == null && this.leftVar != null) {
            result = this.leftVar.findInformationBlockBy(id);
        }
        if (result == null && this.rightVar != null) {
            result = this.rightVar.findInformationBlockBy(id);
        }

        return result;
    }

    /**
     * 式に含まれる全ての手続呼出のセットを返す。変数および手続呼出の添字も対象とする。 再帰呼び出し。
     * @return 手続呼出のセット。
     */
    public Set<ProcedureUsage> getAllFunctions() {
        Set<ProcedureUsage> leftCalls = this.leftVar.getAllFunctions();
        Set<ProcedureUsage> rightCalls = this.rightVar.getAllFunctions();
        Set<ProcedureUsage> calls = new HashSet<ProcedureUsage>();
        if (leftCalls != null && leftCalls.size() > 0) {
            calls.addAll(leftCalls);
        }
        if (rightCalls != null && rightCalls.size() > 0) {
            calls.addAll(rightCalls);
        }
        if (calls.size() <= 0) return null;
        return calls;
    }


    /**
     * 同一ブロックであるかチェックする.
     * @param block        ブロック
     * @return        true=一致
     */
    @Override
    public boolean equalsBlocks(Block block) {
        if (block == null) return false;
        if (!(block instanceof Substitution)) return false;
        if (!super.equalsBlocks(block)) return false;

        if (this.leftVar != null && ((Substitution)block).leftVar != null) {
            if (!this.leftVar.equalsExpression(((Substitution)block).leftVar)) {
                return false;
            }
        }
        else if (this.leftVar != null || ((Substitution)block).leftVar != null) {
            return false;
        }

        if (this.rightVar != null && ((Substitution)block).rightVar != null) {
            if (!this.rightVar.equalsExpression(((Substitution)block).rightVar)) {
                return false;
            }
        }
        else if (this.rightVar != null || ((Substitution)block).rightVar != null) {
            return false;
        }
        return true;
    }


    /**
     * 同一ブロックを検索する
     *
     * @param block    IInformationブロック
     * @return 同一ブロック
     */
    @Override
    public IInformation[] searchInformationBlocks(IInformation block) {
        List<IInformation> list = new ArrayList<IInformation>();
        {
            IInformation[] infos = super.searchInformationBlocks(block);
            if (infos != null) {
                list.addAll(Arrays.asList(infos));
            }
        }
        if (this.leftVar != null) {
            IInformation[] infos = this.leftVar.searchInformationBlocks(block);
            if (infos != null) {
                list.addAll(Arrays.asList(infos));
            }
        }
        if (this.rightVar != null) {
            IInformation[] infos = this.rightVar.searchInformationBlocks(block);
            if (infos != null) {
                list.addAll(Arrays.asList(infos));
            }
        }
        if (list.size() <= 0) {
            return null;
        }
        return list.toArray(new IInformation[0]);
    }

    /**
     * 変数リストを取得する.
     */
    @Override
    public Set<Variable> getAllVariables() {
        Set<Variable> list = new HashSet<Variable>();
        Set<Variable> vars = super.getAllVariables();
        if (vars != null && vars.size() > 0) {
            list.addAll(vars);
        }
        vars = this.getBlockVariables();
        if (vars != null && vars.size() > 0) {
            list.addAll(vars);
        }
        if (list.size() <= 0) return null;
        return list;
    }


    /**
     * 式の変数リストを取得する.
     * ブロックのみの変数リストを取得する。
     * @return        式の変数リスト
     */
    public Set<Variable> getBlockVariables() {
        Set<Variable> list = new HashSet<Variable>();
        if (this.leftVar != null) {
            Set<Variable> vars = this.leftVar.getAllVariables();
            if (vars != null) {
                list.addAll(vars);
            }
        }
        if (this.rightVar != null) {
            Set<Variable> vars = this.rightVar.getAllVariables();
            if (vars != null) {
                list.addAll(vars);
            }
        }
        if (list.size() <= 0) return null;
        return list;
    }
}
