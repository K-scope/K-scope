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
package jp.riken.kscope.service;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import jp.riken.kscope.data.OperationCount;
import jp.riken.kscope.language.Block;
import jp.riken.kscope.language.BlockType;
import jp.riken.kscope.language.Condition;
import jp.riken.kscope.language.ExecutableBody;
import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProcedureUsage;
import jp.riken.kscope.language.ProgramUnit;
import jp.riken.kscope.language.Selection;
import jp.riken.kscope.language.Substitution;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.language.utils.OperationCounterUtils;
import jp.riken.kscope.model.OperandTableModel;
import jp.riken.kscope.properties.OperationProperties;

/**
 * Analysis: Perform calculation count
 *
 * @author RIKEN
 */
public class AnalysisOperandService extends AnalysisBaseService {

  /** Arithmetic count table model */
  private OperandTableModel modelOperand;

  /** Built-in function operation count property */
  private OperationProperties propertiesOperand;

  /**
   * Constructor
   *
   * @param fortran Fortran database
   */
  public AnalysisOperandService(Fortran fortran) {
    super(fortran);
  }

  /**
   * Set the arithmetic count table model
   *
   * @param modelOperand Arithmetic count table model
   */
  public void setModelOperand(OperandTableModel modelOperand) {
    this.modelOperand = modelOperand;
  }

  /**
   * Get the built-in function operation count property
   *
   * @param propertiesOperand Built-in function arithmetic count property
   */
  public void setPropertiesOperand(OperationProperties propertiesOperand) {
    this.propertiesOperand = propertiesOperand;
  }

  /**
   * Get the operation count
   *
   * @param blocks block list
   */
  public void analysisOperand(IBlock[] blocks) {
    if (blocks == null) {
      return;
    }
    Collection<IBlock> blockList = java.util.Arrays.asList(blocks);
    // Get the set of blocks if it is a variable declaration
    if (blocks[0] instanceof VariableDefinition) {
      blockList = this.getBlocks((VariableDefinition) blocks[0]);
    }

    this.modelOperand.setTitle(blocks[0].toString());
    for (IBlock block : blockList) {

      // Get block name
      StringBuilder loopName = new StringBuilder();
      IBlock nameBlock = block;
      BlockType bkType = nameBlock.getBlockType();
      String bkTypeString = bkType.toString();
      if (nameBlock instanceof Condition) {
        nameBlock = ((Condition) block).get_mother();
      }
      if (bkType == BlockType.REPETITION) {
        bkTypeString = "DO";
      } else if (nameBlock instanceof Selection) {
        Selection selec = (Selection) nameBlock;
        if (selec.isIF()) {
          bkTypeString = "IF";
        } else if (selec.isSelect()) {
          bkTypeString = "SELECT";
        } else if (selec.isWHERE()) {
          bkTypeString = "WHERE";
        }
      } else if (bkType == BlockType.PROCEDUREUSAGE) {
        bkTypeString = "CALL";
      }
      loopName.append(bkTypeString);
      if (block.getStartCodeLine() != null) {
        loopName.append(" " + block.getStartCodeLine().getStartLine());
      }
      if (block instanceof Block) {
        String label = ((Block) block).get_start().get_label();
        if (!(label.equalsIgnoreCase("NO_LABEL"))) {
          loopName.append(" " + label);
        }
      }

      // Get block count
      OperationCounterUtils utils = new OperationCounterUtils(this.propertiesOperand);
      utils.countBlock(block);

      // Result set
      OperationCount loop = new OperationCount();
      loop.setName(loopName.toString());
      loop.setF(utils.getOperandFlop());
      loop.setAdd(utils.getAddFlop());
      loop.setSub(utils.getSubFlop());
      loop.setMul(utils.getMulFlop());
      loop.setDiv(utils.getDivFlop());
      loop.setIntrinsic(utils.getIntrinsicFlop());
      this.modelOperand.addOperandBlock(block, loop);
    }
  }

  /**
   * Returns the set of control blocks in which the specified variable declaration is referenced /
   * defined.
   *
   * @param varDef Variable declaration
   * @return A set of control blocks. If not, returns an empty list.
   */
  private Collection<IBlock> getBlocks(VariableDefinition varDef) {
    Set<IBlock> set = new LinkedHashSet<IBlock>();
    String varName = varDef.get_name();
    ProgramUnit pu = varDef.getMother();
    if (!(pu instanceof Procedure)) {
      return set;
    }
    Procedure proc = (Procedure) pu;
    Set<IBlock> blks = proc.getRefDefBlocks(varName);
    for (IBlock block : blks) {
      if (block instanceof Substitution) {
        Substitution sub = (Substitution) block;
        if (!(sub.get_mother() instanceof ExecutableBody)) {
          set.add(sub.get_mother());
        } else {
          // TODO Assignment statements that do not belong to any other block are set individually.
          // It can be very large depending on the program.
          set.add(sub);
        }
      }
    }
    // Processing for internal subprograms
    Collection<Procedure> pus = proc.getChildren();
    for (Procedure pr : pus) {
      if (!(pr.getVariables().containsKey(varName))) {
        blks = pr.getRefDefBlocks(varName);
        for (IBlock block : blks) {
          if (block instanceof Substitution) {
            Substitution sub = (Substitution) block;
            if (!(sub.get_mother() instanceof ExecutableBody)) {
              set.add(sub.get_mother());
            } else {
              // TODO Assignment statements that do not belong to any other block are set
              // individually. It can be very large depending on the program.
              set.add(sub);
            }
          }
        }
      }
    }
    return set;
  }

  /**
   * Count the number of Block operations.
   *
   * @param block Processing block
   * @return Integer array of count information
   */
  @SuppressWarnings("unused")
  private CountResult countBlock(IBlock block) {
    CountResult result = new CountResult();
    this.countChildren(block, result);
    return result;
  }

  /**
   * Count the number of operations for the child elements of the block.
   *
   * @param block block
   * @param result Count result
   */
  private void countChildren(IBlock block, CountResult result) {
    int[] count = result.getCount();
    List<String> leftVar = result.getLeftVar();
    Set<String> rightVar = result.getRightVar();
    List<String> rightfunc = result.getFunc();

    if (block instanceof Substitution) {

      // (2012/5/9) modified by teraim, tomiyama
      // leftVar.add(((Substitution) block).getLeftValue().getVariableString());
      //
      // Determine if getDimensionIndexValue () exists before adding the result on the left side,
      // If it exists, consider it as an array and add it to the result
      if (((Substitution) block).getLeftValue().getDimensionIndexValue() != null) {
        leftVar.add(((Substitution) block).getLeftValue().getVariableString());
      }

      Expression right = ((Substitution) block).getRightValue();
      List<Variable> rightVariables = right.getVariables();
      for (Variable vr : rightVariables) {
        // If it is an array, count it as a load
        if (vr.getDimensionIndexValue() != null) {
          rightVar.add(vr.getVariableString());
        }
      }
      List<ProcedureUsage> rightFuncCalls = right.getFuncCalls();
      for (ProcedureUsage pu : rightFuncCalls) {
        rightfunc.add(pu.getCallName());
      }
      count[0] += right.getAddCount() + right.getSubCount();
      count[1] += right.getMulCount() + right.getDivCount();
      count[2] += right.getPowCount();
      result.setCount(count);

    } else if (block instanceof Block) {
      List<Block> children = ((Block) block).getChildren();
      for (Block bk : children) {
        this.countChildren(bk, result);
      }
    }
  }

  private class CountResult {
    private int[] count = new int[3]; // add,mul,pow
    private List<String> leftVar = new ArrayList<String>();
    private Set<String> rightVar = new HashSet<String>();
    private List<String> rightfunc = new ArrayList<String>();

    private void setCount(int[] count) {
      this.count = count;
    }

    private int[] getCount() {
      return count;
    }

    private List<String> getFunc() {
      return rightfunc;
    }

    private Set<String> getRightVar() {
      return rightVar;
    }

    private List<String> getLeftVar() {
      return leftVar;
    }
  }
}
