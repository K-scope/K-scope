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
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jp.riken.kscope.information.InformationBlocks;

/**
 * 動的にメモリ領域を割り当てる処理に対応したクラス。
 *
 * @author RIKEN
 *
 */
public class DynamicAllocation extends jp.riken.kscope.language.Block {
    /** シリアル番号 */
    private static final long serialVersionUID = -2152445474995339483L;
    private Map<Variable, VariableDimension> targets;
    private Variable error;

    /**
     * コンストラクタ。
     *
     * @param parent
     *           親ブロック
     * @param trgt
     *           割り当て対象となる変数
     */
    public DynamicAllocation(Block parent, Map<Variable, VariableDimension> trgt) {
        super(parent);
        targets = trgt;
    }

    /**
     * コンストラクタ。
     */
    public DynamicAllocation() {
        super();
    }

    /**
     * エラー変数をセット。
     *
     * @param err
     *            エラー変数
     */
    public void setError(Variable err) {
        error = err;
    }

    /**
     * エラー変数を取得する
     *
     * @return error エラー変数
     */
    public Variable getError() {
        return error;
    }

    /**
     * ブロックタイプの取得。
     *
     * @return BlockType.DYNAMIC_ALLOCATION
     */
    public BlockType getBlockType() {
        return BlockType.DYNAMIC_ALLOCATION;
    }

    /**
     * 割り当て対象変数の取得。
     *
     * @return 割り当て対象変数
     */
    public Map<Variable, VariableDimension> getTarget() {
        return targets;
    }

    /**
     * 割り当て対象変数をセットする。
     *
     * @param trgt
     *            ターゲット
     */
    public void setTarget(Map<Variable, VariableDimension> trgt) {
        targets = trgt;
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
        if (this.targets != null) {
            for (Variable variable : this.targets.keySet()) {
                result.addAll(variable.createInformationBlocks());
            }
        }
        if (this.error != null) {
            result.addAll(this.error.createInformationBlocks());
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

        if (result == null && this.targets != null) {
            for (Variable variable : this.targets.keySet()) {
                result = variable.findInformationBlockBy(id);
                if (result != null) { break; }
            }
        }
        if (result == null && this.error != null) {
            result = this.error.findInformationBlockBy(id);
        }
        return result;
    }


     /**
      * 同一ブロックであるかチェックする.
      * @param block		ブロック
 	 * @return		true=一致
      */
     @Override
 	public boolean equalsBlocks(Block block) {
 		if (block == null) return false;
 		if (!(block instanceof DynamicAllocation)) return false;
 		if (!super.equalsBlocks(block)) return false;

		if (this.targets != null && ((DynamicAllocation)block).targets != null) {
			if (this.targets.size() == ((DynamicAllocation)block).targets.size()) {
				Set<Variable> setKeys = this.targets.keySet();
				Iterator<Variable> destIter = ((DynamicAllocation)block).targets.keySet().iterator();
				for (Iterator<Variable> thisIter = setKeys.iterator(); thisIter.hasNext(); ) {
					Variable thisVar = thisIter.next();
					Variable destVar = destIter.next();
					if (!thisVar.equalsVariable(destVar)) {
						return false;
					}
				}
			}
		}
		else if (this.targets != null || ((DynamicAllocation)block).targets != null) {
			return false;
		}

		if (error != null && ((DynamicAllocation)block).error != null) {
			if (!error.equalsVariable(((DynamicAllocation)block).error )) {
				return false;
			}
		}
		else if (error != null || ((DynamicAllocation)block).error != null) {
			return false;
		}
		return true;
     }

     /**
      * 同一ブロックを検索する
      * @param block			IInformationブロック
      * @return		同一ブロック
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

    	 if (this.targets != null) {
    		 for (Variable variable : this.targets.keySet()) {
    			 IInformation[] infos = variable.searchInformationBlocks(block);
    			 if (infos != null) {
    				 list.addAll(Arrays.asList(infos));
    			 }
    		 }
    	 }
    	 if (this.error != null) {
    		 IInformation[] infos = this.error.searchInformationBlocks(block);
    		 if (infos != null) {
    			 list.addAll(Arrays.asList(infos));
    		 }
    	 }
    	 if (list.size() <= 0) {
    		 return null;
    	 }
    	 return list.toArray(new IInformation[0]);
     }
}
