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
import java.util.List;
import java.util.Set;

import jp.riken.kscope.information.InformationBlocks;

/**
 * プログラム単位内で同じ記憶領域を共有する変数を表現するクラス。FortranにおけるEquivalence文を表す。
 *
 * @author RIKEN
 *
 */
public class Equivalence extends jp.riken.kscope.language.Block {
    /** シリアル番号 */
    private static final long serialVersionUID = 2994603726150733660L;
    private List<Variable> variables = new ArrayList<Variable>();

    /**
     * コンストラクタ。
     */
    public Equivalence() {
        super();
    }

    /**
     * ブロックタイプの取得。
     *
     * @return BlockType.EQUIVALENCE
     */
    public BlockType getBlockType() {
        return BlockType.EQUIVALENCE;
    }

    /**
     * 変数のリストをセットする
     *
     * @param vars
     *            変数のリスト
     */
    public void setVariables(List<Variable> vars) {
        this.variables = vars;
    }

    /**
     * 変数のリストを取得する。
     *
     * @return variables 変数のリスト
     */
    public List<Variable> getVariables() {
        return variables;
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
        if (this.variables != null) {
            for (Variable variable : this.variables) {
                result.addAll(variable.createInformationBlocks());
            }
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

        if (result == null && this.variables != null) {
            for (Variable variable : this.variables) {
                result = variable.findInformationBlockBy(id);
                if (result != null) { break; }
            }
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
        if (!(block instanceof Equivalence)) return false;
        if (!super.equalsBlocks(block)) return false;

        if (this.variables != null && ((Equivalence)block).variables != null) {
            if (this.variables.size() == ((Equivalence)block).variables.size()) {
                for (int i=0; i<this.variables.size(); i++) {
                    Variable thisVar = this.variables.get(i);
                    Variable destVar = ((Equivalence)block).variables.get(i);
                    if (thisVar == destVar) {
                        continue;
                    }
                    else if (thisVar == null) {
                        return false;
                    }
                    else if (!thisVar.equalsVariable(destVar)) {
                        return false;
                    }
                }
            }
        }
        else if (this.variables != null || ((Equivalence)block).variables != null) {
            return false;
        }

        return true;
    }

    /**
     * 同一ブロックを検索する
     *
     * @param block
     *            IInformationブロック
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

        if (this.variables != null) {
            for (Variable variable : this.variables) {
                IInformation[] infos = variable.searchInformationBlocks(block);
                if (infos != null) {
                    list.addAll(Arrays.asList(infos));
                }
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
 		return null;
 	}
}
