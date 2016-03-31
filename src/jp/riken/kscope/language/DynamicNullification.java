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

import jp.riken.kscope.information.InformationBlocks;

/**
 * 動的にpointer参照を破棄（pointer変数にnullを設定）する処理に対応したクラス。
 *
 * @author RIKEN
 *
 */
public class DynamicNullification extends jp.riken.kscope.language.Block {
    /** シリアル番号 */
    private static final long serialVersionUID = -2195173081670708420L;
    private DynamicDeallocation core;

    /**
     * コンストラクタ。
     *
     * @param parent
     *           親ブロック
     * @param trgt
     *           破棄対象となるpointer変数
     */
    public DynamicNullification(Block parent, List<Variable> trgt) {
        core = new DynamicDeallocation(parent, trgt);
    }

    /**
     * コンストラクタ。
     */
    public DynamicNullification() {
        super();
    }

    /**
     * ターゲットをセットする。
     *
     * @param trgt
     *            ターゲット
     */
    public void setTarget(List<Variable> trgt) {
        core = new DynamicDeallocation(trgt);
    }
    /**
     * ブロックタイプの取得。
     *
     * @return BlockType.DYNAMIC_NULLIFICATION
     */
    @Override
    public BlockType getBlockType() {
        return BlockType.DYNAMIC_NULLIFICATION;
    }

    /**
     * 破棄対象pointer変数の取得。
     *
     * @return 破棄対象pointer変数
     */
    public List<Variable> getTarget() {
        return core.getTarget();
    }

    /**
     * 付加情報ブロックコレクションを生成する。
     *
     * @return 付加情報ブロックコレクション
     */
    @Override
    public InformationBlocks createInformationBlocks() {
        return core.createInformationBlocks();
    }

    /**
     * idにマッチした情報ブロックを検索する。
     * @param id
     *          ID
     * @return 見つかった情報ブロック。見つからなかった場合はnullが返ります。
     */
    @Override
    public IInformation findInformationBlockBy(String id) {
        return core.findInformationBlockBy(id);
    }

    /**
     * 同一ブロックであるかチェックする.
     * @param block        ブロック
     * @return        true=一致
     */
    @Override
    public boolean equalsBlocks(Block block) {
        if (block == null) return false;
        if (!(block instanceof DynamicNullification)) return false;
        if (!super.equalsBlocks(block)) return false;
        if (!core.equalsBlocks(((DynamicNullification)block).core)) {
            return false;
        }
        return true;
    }

    /**
     * 同一ブロックを検索する
     * @param block            IInformationブロック
     * @return        同一ブロック
     */
    @Override
    public IInformation[] searchInformationBlocks(IInformation block) {
        List<IInformation> list = new ArrayList<IInformation>();
        IInformation[] infos = this.core.searchInformationBlocks(block);
        if (infos != null) {
            list.addAll(Arrays.asList(infos));
        }
        if (list.size() <= 0) {
            return null;
        }
        return list.toArray(new IInformation[0]);
    }


    /**
     * 式の変数リストを取得する.
     * 子ブロックの変数リストも取得する。
     * @return        式の変数リスト
     */
    @Override
    public Set<Variable> getAllVariables() {
        Set<Variable> list = new HashSet<Variable>();
        Set<Variable> vars = super.getAllVariables();
        if (vars != null && vars.size() > 0) {
            list.addAll(vars);
        }
        if (this.core != null) {
            vars = this.core.getAllVariables();
            if (vars != null && vars.size() > 0) {
                list.addAll(vars);
            }
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
       if (this.core != null) {
           Set<Variable> vars = this.core.getBlockVariables();
           if (vars != null && vars.size() > 0) {
               list.addAll(vars);
           }
       }
       if (list.size() <= 0) return null;
       return list;
   }

   /**
    * 手続呼出しのリストを返す。
    * @return 手続呼出しのリスト
    */
   @Override
   public List<ProcedureUsage> getCalls() {
       return this.core.getCalls();
   }

}
