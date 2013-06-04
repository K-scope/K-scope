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

import jp.riken.kscope.information.InformationBlocks;

/**
 * 分岐処理を表現するクラス。
 * @author RIKEN
 */
public class Selection extends Block {
	/** シリアル番号 */
	private static final long serialVersionUID = -2834175766015959116L;
    /** 条件式と実行文からなるブロックのリスト。 */
    private List<Condition> conditions = new ArrayList<Condition>();
    /** 分岐の種類を表現する。 */
    private SelectionType type;
    /** SELECT CASE文の条件式を保持する。 */
    private Expression caseCondition = null;


    /**
     * 分岐の種類を表現するクラス。
     *
     * @author RIKEN
     *
     */
    public enum SelectionType {
    	/** select文. */
    	SELECT,
    	/** if文. */
    	IF,
    	/** where文. */
    	WHERE
    }

    /**
     * コンストラクタ.
     */
    Selection() {
        super();
    }

    /**
     * コンストラクタ。
     * @param mama
     *            親ブロック
     */
    Selection(Block mama) {
        super(mama);
    }

    /**
     * コンストラクタ。
     * @param mama
     *            親ブロック
     * @param tp
     *            ブロックの型
     */
    Selection(Block mama, SelectionType tp) {
        super(mama);
        type = tp;
    }

    /**
     * ブロックタイプの取得。
     * @return BlockType.SELECTION
     */
    public BlockType getBlockType() {
        return BlockType.SELECTION;
    }
    /**
     * 条件ブロックの数を取得する。
     * @return 条件ブロックの数
     */
    protected int getNumOfConditions() {
        return conditions.size();
    }

    /**
     * 条件式リストを取得する。
     * @return		条件式リスト
     */
    protected List<Condition> get_conditions() {
        return (conditions);
    }

    /**
     * 条件式を取得する。
     * @param i    条件式インデックス
     * @return		条件式
     */
    protected Condition getCondition(int i) {
        return conditions.get(i);
    }

    /**
     * 条件ブロックのリストを返す。
     *
     * @return 条件ブロックのリスト
     */
    public List<Condition> getConditions() {
        return this.conditions;
    }

    /**
     * 分岐処理の種類をセットする。
     *
     * @param typ
     */
    protected void setType(SelectionType typ) {
        type = typ;
    }

    /**
     * Select case文ならば真を返す。
     *
     * @return boolean
     */
    public boolean isSelect() {
        if (type == SelectionType.SELECT) {
            return true;
        }
        return false;
    }

    /**
     * IF文ならば真を返す。
     *
     * @return boolean
     */
    public boolean isIF() {
        if (type == SelectionType.IF) {
            return true;
        }
        return false;
    }

    /**
     * WHERE文ならば真を返す。
     *
     * @return boolean
     */
    public boolean isWHERE() {
        if (type == SelectionType.WHERE) {
            return true;
        }
        return false;
    }

    /**
     * case文の条件式を返す。
     * @return 条件式。無ければnullを返す。
     */
    public Expression getCaseCondition() {
        return caseCondition;
    }

    /**
     * case文の条件式をセットする。
     * @param caseCond 条件式
     */
    public void setCaseCondition(Expression caseCond) {
        this.caseCondition = caseCond;
        // 親IF,SELECT文を設定する
        if (this.caseCondition != null) {
        	this.caseCondition.setParentStatement(this);
        }
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
        if (this.conditions != null) {
            for (Condition condition : this.conditions) {
                result.addAll(condition.createInformationBlocks());
            }
        }
        if (this.caseCondition != null) {
            result.addAll(this.caseCondition.createInformationBlocks());
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

        if (result == null && this.conditions != null) {
            for (Condition condition : this.conditions) {
                result = condition.findInformationBlockBy(id);
                if (result != null) { break; }
            }
        }
        if (result == null && this.caseCondition != null) {
            result = this.caseCondition.findInformationBlockBy(id);
        }

        return result;
    }

    /**
     * 自身の子要素であるConditionクラスのリストを返す。
     * @return Conditionのリスト。無ければからのリストを返す。
     */
    @Override
    public ArrayList<Block> getChildren() {
        ArrayList<Block> children = new ArrayList<Block>();
        children.addAll(this.conditions);
        return children;
    }


    /**
     * 同一ブロックであるかチェックする.
     * @param block		ブロック
	 * @return		true=一致
     */
    @Override
	public boolean equalsBlocks(Block block) {
		if (block == null) return false;
		if (!(block instanceof Selection)) return false;
		if (!super.equalsBlocks(block)) return false;

		if (this.type != ((Selection)block).type) {
			return false;
		}
		if (this.conditions != null && ((Selection)block).conditions != null) {
			if (this.conditions.size() == ((Selection)block).conditions.size()) {
				for (int i=0; i<this.conditions.size(); i++) {
					Condition thisCond = this.conditions.get(i);
					Condition destCond = ((Selection)block).conditions.get(i);
					if (thisCond == destCond) {
						continue;
					}
					else if (thisCond == null) {
						return false;
					}
					else if (!thisCond.equalsBlocks(destCond)) {
						return false;
					}
				}
			}
		}
		else if (this.conditions != null || ((Selection)block).conditions != null) {
			return false;
		}

		if (this.caseCondition != null && ((Selection)block).caseCondition != null) {
			if (!this.caseCondition.equalsExpression(((Selection)block).caseCondition)) {
				return false;
			}
		}
		else if (this.caseCondition != null || ((Selection)block).caseCondition != null) {
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
        if (this.caseCondition != null) {
            IInformation[] infos = this.caseCondition.searchInformationBlocks(block);
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
	 * 同一ブロック階層であるかチェックする.
	 * @param block		チェック対象Block
	 * @return   true=一致
	 */
	public boolean equalsLayout(Block block) {
		if (block == null) return false;
		if (!(block instanceof Selection)) return false;
		if (!super.equalsLayout(block)) return false;

		if (this.type != ((Selection)block).type) {
			return false;
		}
		if (this.conditions != null && ((Selection)block).conditions != null) {
			if (this.conditions.size() == ((Selection)block).conditions.size()) {
				for (int i=0; i<this.conditions.size(); i++) {
					Condition thisCond = this.conditions.get(i);
					Condition destCond = ((Selection)block).conditions.get(i);
					if (thisCond == destCond) {
						continue;
					}
					else if (thisCond == null) {
						return false;
					}
					else if (!thisCond.equalsLayout(destCond)) {
						return false;
					}
				}
			}
		}
		else if (this.conditions != null || ((Selection)block).conditions != null) {
			return false;
		}

		return true;
    }

    /**
     * 子ブロックのインデックスを返す.
     * 存在しない場合は、-1を返す。
     * @param block		ブロック
	 * @return			インデックス
     */
    @Override
    protected int indexOfChildren(Block block) {
    	if (block instanceof Condition) {
    		return this.conditions.indexOf(block);
    	}
    	else {
    		return super.indexOfChildren(block);
    	}
	}

    /**
     * 子ブロックのインデックスを返す.
     * DO, SELECT, IF文の出現回数とする
     * 存在しない場合は、-1を返す。
     * @param block		ブロック
	 * @return			インデックス
     */
    @Override
    protected int indexOfLayout(Block block) {
    	return indexOfChildren(block);
	}
}
