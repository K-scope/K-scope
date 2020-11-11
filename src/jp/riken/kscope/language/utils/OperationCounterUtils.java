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

package jp.riken.kscope.language.utils;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import jp.riken.kscope.data.BlockList;
import jp.riken.kscope.data.OperationCount;
import jp.riken.kscope.language.Block;
import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProcedureUsage;
import jp.riken.kscope.language.Substitution;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.properties.OperationProperties;

/**
 * Calculation count utility class
 *
 * @author RIKEN
 */
public class OperationCounterUtils {

  /** Built-in function operation count property */
  private OperationProperties propertiesOperation;
  /** Calculation count result */
  private OperandCountResult result;

  /** Constructor */
  public OperationCounterUtils() {}

  /**
   * Constructor
   *
   * @param properties Built-in function arithmetic count property
   */
  public OperationCounterUtils(OperationProperties properties) {
    this.propertiesOperation = properties;
  }

  /**
   * Get the left-hand variable list.
   *
   * @return Left side variable list
   */
  public List<Variable> getLeftVariables() {
    if (this.result == null) return null;
    return this.result.getLeftVar();
  }

  /**
   * Get the right-hand variable list.
   *
   * @return Right-hand variable list
   */
  public List<Variable> getRightVariables() {
    if (this.result == null) return null;
    return this.result.getRightVar();
  }

  /**
   * Get the number of left-hand variable lists.
   *
   * @return Number of left-side variable lists
   */
  public int getLeftVariableCount() {
    if (this.result == null) return 0;
    if (this.result.getLeftVar() == null) return 0;
    return this.result.getLeftVar().size();
  }

  /**
   * Get the number of right-hand side variable lists.
   *
   * @return Number of variable lists on the right side
   */
  public int getRightVariableCount() {
    if (this.result == null) return 0;
    if (this.result.getRightVar() == null) return 0;
    return this.result.getRightVar().size();
  }

  /**
   * Get the number of addition operations.
   *
   * @return Number of addition operations
   */
  public int getAddCount() {
    if (this.result == null) return 0;
    return this.result.getCountAdd();
  }

  /**
   * Get the number of subtraction operations.
   *
   * @return Number of production reduction operations
   */
  public int getSubCount() {
    if (this.result == null) return 0;
    return this.result.getCountSub();
  }

  /**
   * Get the number of multiplication operations.
   *
   * @return Number of multiplication operations
   */
  public int getMulCount() {
    if (this.result == null) return 0;
    return this.result.getCountMul();
  }

  /**
   * Get the number of division operations.
   *
   * @return Number of division operations
   */
  public int getDivCount() {
    if (this.result == null) return 0;
    return this.result.getCountDiv();
  }

  /**
   * Get the number of arithmetic operations to be performed. Arithmetic * pow (addition +
   * multiplication).
   *
   * @return Number of arithmetic operations to be performed
   */
  public int getPowCount() {
    if (this.result == null) return 0;
    int count = this.result.getCountPow();
    if (this.propertiesOperation == null) return count;
    OperationCount pow = propertiesOperation.getOperationProperty("pow");
    int flop = 1;
    if (pow != null) {
      flop = pow.getAdd() + pow.getMul();
    }
    return count * flop;
  }

  /**
   * Get the number of built-in function operations. Built-in function (addition + multiplication).
   *
   * @return Function operation number
   */
  public int getFunctionCount() {
    if (this.result == null) return 0;
    if (this.result.getFunctions() == null) return 0;
    int count = this.result.getFunctions().size();
    if (count <= 0) return 0;
    if (this.propertiesOperation == null) return count;
    int flop = 0;
    for (ProcedureUsage func : this.result.getFunctions()) {
      String name = func.getCallName();
      OperationCount opc = propertiesOperation.getOperationProperty(name);
      if (opc != null) {
        flop += opc.getAdd() + opc.getMul();
      }
    }
    return flop;
  }

  /**
   * Get power + number of function operations.
   *
   * @return Exponentiation + number of function operations
   */
  public int getIntrinsicCount() {
    return this.getPowCount() + getFunctionCount();
  }

  /**
   * Get the addition operation FLOP.
   *
   * @return Addition operation FLOP
   */
  public int getAddFlop() {
    int count = this.getAddCount();
    if (this.propertiesOperation == null) return count;
    int flop = this.propertiesOperation.getFlopAdd();
    return count * flop;
  }

  /**
   * Get the subtraction operation FLOP.
   *
   * @return Production reduction calculation FLOP
   */
  public int getSubFlop() {
    int count = this.getSubCount();
    if (this.propertiesOperation == null) return count;
    int flop = this.propertiesOperation.getFlopSub();
    return count * flop;
  }

  /**
   * Get multiplication operation FLOP.
   *
   * @return Multiplication operation FLOP
   */
  public int getMulFlop() {
    int count = this.getMulCount();
    if (this.propertiesOperation == null) return count;
    int flop = this.propertiesOperation.getFlopMul();
    return count * flop;
  }

  /**
   * Get the division operation FLOP.
   *
   * @return Division operation FLOP
   */
  public int getDivFlop() {
    int count = this.getDivCount();
    if (this.propertiesOperation == null) return count;
    int flop = this.propertiesOperation.getFlopDiv();
    return count * flop;
  }

  /**
   * Get the power arithmetic FLOP.
   *
   * @return Calculator FLOP
   */
  public int getPowFlop() {
    if (this.result == null) return 0;
    int count = this.result.getCountPow();
    if (this.propertiesOperation == null) return count;
    OperationCount pow = propertiesOperation.getOperationProperty("pow");
    int flop = 1;
    if (pow != null) {
      int add = this.propertiesOperation.getFlopAdd();
      int mul = this.propertiesOperation.getFlopMul();
      flop = pow.getAdd() * add + pow.getMul() * mul;
    }
    return count * flop;
  }

  /**
   * Get the function list FLOP.
   *
   * @return function list FLOP
   */
  public int getFunctionFlop() {
    int count = this.getFunctionCount();
    if (count <= 0) return 0;
    if (this.propertiesOperation == null) return count;
    int flop = 0;
    int add = this.propertiesOperation.getFlopAdd();
    int mul = this.propertiesOperation.getFlopMul();
    for (ProcedureUsage func : this.result.getFunctions()) {
      String name = func.getCallName();
      OperationCount opc = propertiesOperation.getOperationProperty(name);
      if (opc != null) {
        flop += opc.getAdd() * add + opc.getMul() * mul;
      }
    }
    return flop;
  }

  /**
   * Get power + function operation FLOP.
   *
   * @return Exponentiation + function operation FLOP
   */
  public int getIntrinsicFlop() {
    return this.getPowFlop() + getFunctionFlop();
  }

  /**
   * Get the sum of all arithmetic FLOPs
   *
   * @return operation FLOP total
   */
  public int getOperandFlop() {
    return getAddFlop() + getSubFlop() + getMulFlop() + getDivFlop() + getIntrinsicFlop();
  }

  /**
   * Count the number of operations in Block.
   *
   * @param block Processing block
   * @return Count result
   */
  public OperandCountResult countBlock(IBlock block) {
    if (block == null) return null;
    this.result = this.countChildren(block);

    return result;
  }

  /**
   * Count the number of operations for the child elements of the block.
   *
   * @param block block
   * @return Count result
   */
  private OperandCountResult countChildren(IBlock block) {
    if (block == null) return null;
    OperandCountResult result = new OperandCountResult();
    if (block instanceof Substitution) {
      Variable leftVar = ((Substitution) block).getLeftValue();
      if (leftVar != null) {
        if (leftVar.isMemoryAccess()) {
          result.addLeftVariable(leftVar);
        }
      }

      Expression right = ((Substitution) block).getRightValue();
      Set<Variable> rightVariables = right.getAllVariables();
      for (Variable vr : rightVariables) {
        // If it is an array, count it as a load
        if (vr.isMemoryAccess()) {
          result.addRightVariable(vr);
        }
      }
      List<ProcedureUsage> rightFuncCalls = right.getFuncCalls();
      for (ProcedureUsage pu : rightFuncCalls) {
        result.addRightFunction(pu);
      }
      result.incrementAdd(right.getOperandAddCount());
      result.incrementSub(right.getOperandSubCount());
      result.incrementMul(right.getOperandMulCount());
      result.incrementDiv(right.getOperandDivCount());
      result.incrementPow(right.getOperandPowCount());
    } else if (block instanceof Block) {
      List<Block> children = ((Block) block).getChildren();
      for (Block bk : children) {
        OperandCountResult childrenResult = this.countChildren(bk);
        if (childrenResult != null) {
          result.addCount(childrenResult);
        }
      }
    } else if (block instanceof Procedure) {
      Procedure proc = (Procedure) block;
      OperandCountResult childrenResult = this.countChildren(proc.getBody());
      if (childrenResult != null) {
        result.addCount(childrenResult);
      }
    } else if (block instanceof BlockList) {
      List<IBlock> list = ((BlockList) block).getBlocks();
      if (list != null) {
        for (IBlock item : list) {
          OperandCountResult itemResult = this.countChildren(item);
          if (itemResult != null) {
            result.addCount(itemResult);
          }
        }
      }
    }

    return result;
  }

  /**
   * Operation count result class
   *
   * @author RIKEN
   */
  public class OperandCountResult {
    /** Calculation count: Addition */
    private int countAdd;
    /** Calculation count: Subtraction */
    private int countSub;
    /** Calculation count: Multiplication */
    private int countMul;
    /** Calculation count: Division */
    private int countDiv;
    /** Calculation count: Power calculation */
    private int countPow;
    /** Left-side variable representation string list */
    private List<Variable> leftVars;
    /** Right-hand side variable representation string list */
    private List<Variable> rightVars;
    /** Function call list */
    private List<ProcedureUsage> rightFunctions;

    /** Constructor */
    public OperandCountResult() {
      this.countAdd = 0;
      this.countSub = 0;
      this.countMul = 0;
      this.countDiv = 0;
      this.countPow = 0;
      this.leftVars = new ArrayList<Variable>();
      this.rightVars = new ArrayList<Variable>();
      this.rightFunctions = new ArrayList<ProcedureUsage>();
    }

    /**
     * Add operation count result
     *
     * @param result Operation count result
     */
    public void addCount(OperandCountResult result) {
      if (result == null) return;
      this.countAdd += result.countAdd;
      this.countSub += result.countSub;
      this.countMul += result.countMul;
      this.countDiv += result.countDiv;
      this.countPow += result.countPow;
      if (result.leftVars != null) {
        for (Variable var : result.leftVars) {
          this.addLeftVariable(var);
        }
      }
      if (result.rightVars != null) {
        for (Variable var : result.rightVars) {
          this.addRightVariable(var);
        }
      }
      if (result.rightFunctions != null) {
        for (ProcedureUsage call : result.rightFunctions) {
          this.addRightFunction(call);
        }
      }
    }

    /**
     * Get the number of addition operations.
     *
     * @return Number of addition operations
     */
    public int getCountAdd() {
      return this.countAdd;
    }

    /**
     * Get the number of subtraction operations.
     *
     * @return Number of subtraction operations
     */
    public int getCountSub() {
      return this.countSub;
    }

    /**
     * Get the number of multiplication operations.
     *
     * @return Number of multiplication operations
     */
    public int getCountMul() {
      return this.countMul;
    }

    /**
     * Get the number of division operations.
     *
     * @return Number of division operations
     */
    public int getCountDiv() {
      return this.countDiv;
    }

    /**
     * Get the number of power operations.
     *
     * @return Exponentiation number
     */
    public int getCountPow() {
      return this.countPow;
    }

    /**
     * Add the number of addition operations.
     *
     * @param count Number of addition operations
     * @return Number of addition operations
     */
    public int incrementAdd(int count) {
      this.countAdd += count;
      return this.countAdd;
    }

    /**
     * Add the number of subtraction operations.
     *
     * @param count Number of addition operations
     * @return Number of subtraction operations
     */
    public int incrementSub(int count) {
      this.countSub += count;
      return this.countSub;
    }

    /**
     * Add the number of multiplication operations.
     *
     * @param count Number of addition operations
     * @return Number of multiplication operations
     */
    public int incrementMul(int count) {
      this.countMul += count;
      return this.countMul;
    }

    /**
     * Add the number of division operations.
     *
     * @param count Number of addition operations
     * @return Number of division operations
     */
    public int incrementDiv(int count) {
      this.countDiv += count;
      return this.countDiv;
    }

    /**
     * Add the number of power operations.
     *
     * @param count Number of addition operations
     * @return Exponentiation number
     */
    public int incrementPow(int count) {
      this.countPow += count;
      return this.countPow;
    }

    /**
     * Calculation count: Set addition.
     *
     * @param count Operation count: Add
     */
    public void setCountAdd(int count) {
      this.countAdd = count;
    }
    /**
     * Operation count: Set subtraction.
     *
     * @param count Operation count: Subtraction
     */
    public void setCountSub(int count) {
      this.countSub = count;
    }

    /**
     * Operation count: Set multiplication.
     *
     * @param count Operation count: Multiplication
     */
    public void setCountMul(int count) {
      this.countMul = count;
    }

    /**
     * Calculation count: Set division.
     *
     * @param count Operation count: Division
     */
    public void setCountDiv(int count) {
      this.countDiv = count;
    }

    /**
     * Operation count: Set the power calculation.
     *
     * @param count Operation count: Power calculation
     */
    public void setCountPow(int count) {
      this.countPow = count;
    }

    /**
     * Get the function call list.
     *
     * @return Function call list
     */
    public List<ProcedureUsage> getFunctions() {
      return this.rightFunctions;
    }

    /**
     * Get the right-hand variable list.
     *
     * @return Right-hand variable list
     */
    public List<Variable> getRightVar() {
      return this.rightVars;
    }

    /**
     * Get the left-hand variable representation string list.
     *
     * @return Left-hand side variable representation string list
     */
    public List<Variable> getLeftVar() {
      return this.leftVars;
    }

    /**
     * Add to the left-hand variable list.
     *
     * @param var Left-hand side variable
     */
    public void addLeftVariable(Variable var) {
      if (var == null) return;
      if (containsVariable(this.leftVars, var)) {
        return;
      }
      this.leftVars.add(var);
    }

    /**
     * Add to the right-hand variable list.
     *
     * @param var Right-hand side variable
     */
    public void addRightVariable(Variable var) {
      if (var == null) return;
      if (containsVariable(this.rightVars, var)) {
        return;
      }
      this.rightVars.add(var);
    }

    /**
     * Add to the right-hand side function call list.
     *
     * @param call Function call
     */
    public void addRightFunction(ProcedureUsage call) {
      if (call == null) return;
      this.rightFunctions.add(call);
    }

    /**
     * Check if the same variable exists from the variable list.
     *
     * @param list Variable list
     * @param var Search variable
     * @return true = Same variable exists
     */
    private boolean containsVariable(List<Variable> list, Variable var) {
      if (list == null || list.size() <= 0) return false;
      if (var == null) return false;
      List<Variable> results = OperationCounterUtils.this.getVariables(list, var);
      if (results != null && results.size() > 0) return true;

      return false;
    }
  }

  /**
   * Get the same variable list as the variable from the variable list
   *
   * @param listVar Variable list
   * @param value variable
   * @return true = variable list
   */
  public List<Variable> getVariables(List<Variable> listVar, Variable value) {
    if (listVar == null || listVar.size() <= 0) return null;
    if (value == null) return null;
    if (value.getVariableString() == null) return null;
    List<Variable> listEquals = new ArrayList<Variable>();
    for (Variable var : listVar) {
      if (var == null) continue;
      // The definition is the same
      if (value.getDefinition() != var.getDefinition()) continue;
      if (value.equalsVariable(var)) {
        listEquals.add(var);
      }
    }
    if (listEquals.size() <= 0) {
      return null;
    }
    return listEquals;
  }

  /**
   * Set the built-in function operation count property.
   *
   * @param properties Built-in function arithmetic count property
   */
  public void setPropertiesOperation(OperationProperties properties) {
    this.propertiesOperation = properties;
  }
}
