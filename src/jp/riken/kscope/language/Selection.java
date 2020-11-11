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
 * A class that represents branch processing.
 *
 * @author RIKEN
 */
public class Selection extends Block {
  /** Serial number */
  private static final long serialVersionUID = -2834175766015959116L;
  /** A list of blocks consisting of conditional expressions and executable statements. */
  private List<Condition> conditions = new ArrayList<Condition>();
  /** Represents the type of branch. */
  private SelectionType type;
  /** Holds the conditional expression of the SELECT CASE statement. */
  private Expression caseCondition = null;

  /**
   * A class that represents the type of branch.
   *
   * @author RIKEN
   */
  public enum SelectionType {
    /** select statement. */
    SELECT,
    /** if statement. */
    IF,
    /** where statement. */
    WHERE
  }

  /** Constructor. */
  Selection() {
    super();
  }

  /**
   * Constructor.
   *
   * @param mama Parent block
   */
  Selection(Block mama) {
    super(mama);
  }

  /**
   * Constructor.
   *
   * @param mama Parent block
   * @param tp Block type
   */
  Selection(Block mama, SelectionType tp) {
    super(mama);
    type = tp;
  }

  /**
   * Get block type.
   *
   * @return BlockType.SELECTION
   */
  public BlockType getBlockType() {
    return BlockType.SELECTION;
  }
  /**
   * Get the number of condition blocks.
   *
   * @return Number of conditional blocks
   */
  protected int getNumOfConditions() {
    return conditions.size();
  }

  /**
   * Get the conditional expression list.
   *
   * @return Conditional expression list
   */
  protected List<Condition> get_conditions() {
    return (conditions);
  }

  /**
   * Get the conditional expression.
   *
   * @param i Conditional expression index
   * @return conditional expression
   */
  protected Condition getCondition(int i) {
    return conditions.get(i);
  }

  /**
   * Returns a list of conditional blocks.
   *
   * @return List of conditional blocks
   */
  public List<Condition> getConditions() {
    return this.conditions;
  }

  /**
   * Set the type of branch processing.
   *
   * @param typ
   */
  protected void setType(SelectionType typ) {
    type = typ;
  }

  /**
   * Returns true if it is a Select case statement.
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
   * Returns true if it is an IF statement.
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
   * Returns true if it is a WHERE statement.
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
   * Returns the conditional expression of the case statement.
   *
   * @return Conditional expression. If not, it returns null.
   */
  public Expression getCaseCondition() {
    return caseCondition;
  }

  /**
   * Set the conditional expression of the case statement.
   *
   * @param caseCond Conditional expression
   */
  public void setCaseCondition(Expression caseCond) {
    this.caseCondition = caseCond;
    // Set the parent IF and SELECT statements
    if (this.caseCondition != null) {
      this.caseCondition.setParentStatement(this);
    }
  }
  /**
   * Generate an additional information block collection.
   *
   * @return Additional information block collection
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
   * Search for information blocks that match id.
   *
   * @param id ID
   * @return The information block found. If not found, null is returned.
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
        if (result != null) {
          break;
        }
      }
    }
    if (result == null && this.caseCondition != null) {
      result = this.caseCondition.findInformationBlockBy(id);
    }

    return result;
  }

  /**
   * Returns a list of its own child elements, the Condition class. A list of @return conditions. If
   * not, returns the list from.
   */
  @Override
  public ArrayList<Block> getChildren() {
    ArrayList<Block> children = new ArrayList<Block>();
    children.addAll(this.conditions);
    return children;
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
    if (!(block instanceof Selection)) return false;
    if (!super.equalsBlocks(block)) return false;

    if (this.type != ((Selection) block).type) {
      return false;
    }
    if (this.conditions != null && ((Selection) block).conditions != null) {
      if (this.conditions.size() == ((Selection) block).conditions.size()) {
        for (int i = 0; i < this.conditions.size(); i++) {
          Condition thisCond = this.conditions.get(i);
          Condition destCond = ((Selection) block).conditions.get(i);
          if (thisCond == destCond) {
            continue;
          } else if (thisCond == null) {
            return false;
          } else if (!thisCond.equalsBlocks(destCond)) {
            return false;
          }
        }
      }
    } else if (this.conditions != null || ((Selection) block).conditions != null) {
      return false;
    }

    if (this.caseCondition != null && ((Selection) block).caseCondition != null) {
      if (!this.caseCondition.equalsExpression(((Selection) block).caseCondition)) {
        return false;
      }
    } else if (this.caseCondition != null || ((Selection) block).caseCondition != null) {
      return false;
    }

    return true;
  }

  /**
   * Search for the same block
   *
   * @param block IInformation block
   * @return Same block
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
   * Check if they are in the same block hierarchy.
   *
   * @param block Check target Block
   * @return true = match
   */
  public boolean equalsLayout(Block block) {
    if (block == null) return false;
    if (!(block instanceof Selection)) return false;
    if (!super.equalsLayout(block)) return false;

    if (this.type != ((Selection) block).type) {
      return false;
    }
    if (this.conditions != null && ((Selection) block).conditions != null) {
      if (this.conditions.size() == ((Selection) block).conditions.size()) {
        for (int i = 0; i < this.conditions.size(); i++) {
          Condition thisCond = this.conditions.get(i);
          Condition destCond = ((Selection) block).conditions.get(i);
          if (thisCond == destCond) {
            continue;
          } else if (thisCond == null) {
            return false;
          } else if (!thisCond.equalsLayout(destCond)) {
            return false;
          }
        }
      }
    } else if (this.conditions != null || ((Selection) block).conditions != null) {
      return false;
    }

    return true;
  }

  /**
   * Returns the index of the child block. Returns -1 if it does not exist.
   *
   * @param block block
   * @return index
   */
  @Override
  protected int indexOfChildren(Block block) {
    if (block instanceof Condition) {
      return this.conditions.indexOf(block);
    } else {
      return super.indexOfChildren(block);
    }
  }

  /**
   * Returns the index of the child block. The number of occurrences of DO, SELECT, and IF
   * statements Returns -1 if it does not exist.
   *
   * @param block block
   * @return index
   */
  @Override
  protected int indexOfLayout(Block block) {
    return indexOfChildren(block);
  }
}
