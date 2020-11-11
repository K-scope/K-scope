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
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.information.InformationBlocks;

/**
 * A class that represents an assignment statement.
 *
 * @author RIKEN
 */
public class Substitution extends Block {
  /** Serial number */
  private static final long serialVersionUID = 6743876701008619698L;
  /** Left side type */
  private Variable leftVar;
  /** Right-hand side expression */
  private Expression rightVar;

  /** Constructor. */
  public Substitution() {
    super();
  }

  /**
   * Constructor.
   *
   * @param mama Parent block
   */
  Substitution(Block mama) {
    super(mama);
  }
  /**
   * Get block type.
   *
   * @return BlockType.SUBSTITUTION
   */
  public BlockType getBlockType() {
    return BlockType.SUBSTITUTION;
  }
  // ++++++++++++++++++++++++++++++++++++++++++++

  /**
   * Set the right side.
   *
   * @param ex Variable on the right side
   */
  public void setRight(Expression ex) {
    rightVar = ex;
    // TODO Originally, if it can be set at the time of parsing, unnecessary processing will be
    // avoided, so consideration is required.
    CodeLine lineInfo = this.getStartCodeLine();
    String label = this.get_start().get_label();
    List<ProcedureUsage> pus = ex.getFuncCalls();
    for (ProcedureUsage pu : pus) {
      pu.set_mother(this.get_mother());
      pu.setTypeIsFunction();
      pu.set_block_start(lineInfo);
      pu.set_block_end(lineInfo);
      pu.get_start().set_label(label);
      pu.get_end().set_label(label);
    }
    // Set the parent assignment statement.
    if (rightVar != null) {
      rightVar.setParentStatement(this);
    }
  }
  /**
   * Set the left side.
   *
   * @param var Variable on the left side
   */
  public void setLeft(Variable var) {
    leftVar = var;
    // Set the parent assignment statement.
    if (leftVar != null) {
      leftVar.setParentStatement(this);
    }
  }
  /**
   * Get the right-hand side.
   *
   * @return Variable on the right side
   */
  public Expression getRightValue() {
    return rightVar;
  }
  /**
   * Get the left side.
   *
   * @return Variable on the left side
   */
  public Variable getLeftValue() {
    return leftVar;
  }

  @Override
  public List<Block> getBlocks() {
    List<Block> blk = new ArrayList<Block>();
    for (ProcedureUsage pu : this.rightVar.getFuncCalls()) {
      blk.add(pu);
    }
    return blk;
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
    if (this.leftVar != null) {
      result.addAll(this.leftVar.createInformationBlocks());
    }
    if (this.rightVar != null) {
      result.addAll(this.rightVar.createInformationBlocks());
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

    if (result == null && this.leftVar != null) {
      result = this.leftVar.findInformationBlockBy(id);
    }
    if (result == null && this.rightVar != null) {
      result = this.rightVar.findInformationBlockBy(id);
    }

    return result;
  }

  /**
   * Returns the set of all procedure calls contained in the expression. It also covers variables
   * and procedure call subscripts. Recursive call.
   *
   * @return A set of procedure calls.
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
   * Check if they are the same block.
   *
   * @param block block
   * @return true = match
   */
  @Override
  public boolean equalsBlocks(Block block) {
    if (block == null) return false;
    if (!(block instanceof Substitution)) return false;
    if (!super.equalsBlocks(block)) return false;

    if (this.leftVar != null && ((Substitution) block).leftVar != null) {
      if (!this.leftVar.equalsVariable(((Substitution) block).leftVar)) {
        return false;
      }
    } else if (this.leftVar != null || ((Substitution) block).leftVar != null) {
      return false;
    }

    if (this.rightVar != null && ((Substitution) block).rightVar != null) {
      if (!this.rightVar.equalsExpression(((Substitution) block).rightVar)) {
        return false;
      }
    } else if (this.rightVar != null || ((Substitution) block).rightVar != null) {
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

  /** Get the variable list. */
  @Override
  public Set<Variable> getAllVariables() {
    Set<Variable> list = new HashSet<Variable>();
    if (this.leftVar != null) {
      list.add(this.leftVar);
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
    if (list.size() <= 0) {
      return null;
    }
    if (list.size() <= 0) return null;
    return list;
  }
}
