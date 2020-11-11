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
 * IF ... THEN ... Consists of multiple conditional expressions such as ELSE statements. <br>
 * Represents each conditional expression block in the judgment block.
 *
 * @author RIKEN
 */
public class Condition extends Block {
  /** Serial number */
  private static final long serialVersionUID = 7720056035437803500L;
  /** Conditional expression for branching. */
  private Expression expression;

  /**
   * Constructor.
   *
   * @param mama Parent block
   * @param exprssn Conditional expression
   */
  Condition(Block mama, Expression exprssn) {
    super(mama);
    this.expression = exprssn;
    // Set the parent IF statement
    if (this.expression != null) {
      this.expression.setParentStatement(this);
    }
  }

  /**
   * Returns a string representation of the conditional expression.
   *
   * @return String representation of conditional expression
   */
  public String conditionToString() {
    return getExpression().toString();
  }

  /**
   * Get conditional expression.
   *
   * @return conditional expression
   */
  public Expression getExpression() {
    return expression;
  }
  /**
   * Get block type.
   *
   * @return BlockType.CONDITION
   */
  public BlockType getBlockType() {
    return BlockType.CONDITION;
  }

  /**
   * Check if they are the same block.
   *
   * @param block block
   * @return true = match
   */
  @Override
  public boolean equalsBlocks(Block block) {
    if (block == null) return false;
    if (!(block instanceof Condition)) return false;
    if (!super.equalsBlocks(block)) return false;

    if (this.expression != null && ((Condition) block).expression != null) {
      if (!this.expression.equalsExpression(((Condition) block).expression)) {
        return false;
      }
    } else if (this.expression != null || ((Condition) block).expression != null) {
      return false;
    }

    return true;
  }
}
