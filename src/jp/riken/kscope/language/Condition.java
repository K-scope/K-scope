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


/**
* IF･･･THEN･･･ELSE文など複数の条件式で構成される. <br/>
* 判断ブロックにおける各条件式ブロックをあらわす.
*
* @author RIKEN
*
*/
public class Condition extends Block {
    /** シリアル番号 */
    private static final long serialVersionUID = 7720056035437803500L;
    /**
     * 分岐の条件式。
     */
    private Expression expression;

    /**
     * コンストラクタ。
     *
     * @param mama
     *            親ブロック
     * @param exprssn
     *            条件式
     */
    Condition(Block mama, Expression exprssn) {
        super(mama);
        this.expression = exprssn;
        // 親IF文を設定する
        if (this.expression != null) {
        	this.expression.setParentStatement(this);
        }
    }

    /**
     * 条件式の文字列表現を返す。
     *
     * @return 条件式の文字列表現
     */
    public String conditionToString() {
        return getExpression().toString();
    }

    /**
     * 条件式の取得.
     *
     * @return 条件式
     */
    public Expression getExpression() {
        return expression;
    }
    /**
     * ブロックタイプの取得。
     *
     * @return BlockType.CONDITION
     */
    public BlockType getBlockType() {
        return BlockType.CONDITION;
    }

    /**
     * 同一ブロックであるかチェックする.
     * @param block		ブロック
	 * @return		true=一致
     */
    @Override
	public boolean equalsBlocks(Block block) {
		if (block == null) return false;
		if (!(block instanceof Condition)) return false;
		if (!super.equalsBlocks(block)) return false;

		if (this.expression != null && ((Condition)block).expression != null) {
			if (!this.expression.equalsExpression(((Condition)block).expression)) {
				return false;
			}
		}
		else if (this.expression != null || ((Condition)block).expression != null) {
			return false;
		}

		return true;
    }
}
