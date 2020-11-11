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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import jp.riken.kscope.information.InformationBlocks;

/**
 * A class for expressing expressions.
 *
 * @author RIKEN
 */
public class Expression implements Serializable {
  /** Serial number */
  private static final long serialVersionUID = -9199930499046575735L;
  /** String representation of the expression */
  private String line;
  /** Data type */
  private IVariableType type;
  /** Variable list */
  private List<Variable> variables;
  /** Call function list */
  private List<ProcedureUsage> funcCalls;
  /** Addition count */
  private int addCount = 0;
  /** Subtraction count */
  private int subCount = 0;
  /** Multiplication count */
  private int mulCount = 0;
  /** Division count */
  private int divCount = 0;
  /** Power count */
  private int powCount = 0;
  /** Parent block of variable: Substitution, syntax to which the variable belongs */
  private IBlock parentStatement;

  /** Constructor. */
  public Expression() {
    variables = new ArrayList<Variable>();
    funcCalls = new ArrayList<ProcedureUsage>();
  }
  /**
   * Constructor.
   *
   * @param val Target expression
   */
  public Expression(String val) {
    line = val;
    variables = new ArrayList<Variable>();
    funcCalls = new ArrayList<ProcedureUsage>();
  }

  /**
   * Set the data type of the result of the expression.
   *
   * @param tp Result type
   */
  public void setVariableType(IVariableType tp) {
    type = tp;
  }
  /**
   * Returns the data type of the result of the expression.
   *
   * @return data type
   */
  public IVariableType getType() {
    return type;
  }

  /**
   * Set the list of variables that appear in the expression.
   *
   * @param vars List of variables
   */
  public void setVariables(List<Variable> vars) {
    this.variables = vars;
  }
  /**
   * Add variables that appear in expressions.
   *
   * @param var variable
   */
  public void addVariable(Variable var) {
    variables.add(var);
  }

  /**
   * Returns a list of variables that appear in the expression.
   *
   * @return A list of variables. If not, returns an empty list.
   */
  public List<Variable> getVariables() {
    return variables;
  }

  /**
   * Returns a set of all variables contained in an expression. Variable subscripts and procedure
   * call subscripts are also included. Recursive call.
   *
   * @return A set of variables. If not, it returns an empty set.
   */
  public Set<Variable> getAllVariables() {
    Set<Variable> vars = new HashSet<Variable>(this.variables);
    for (Variable var : this.variables) {
      vars.addAll(var.getAllVariables());
    }
    List<ProcedureUsage> calls = this.funcCalls;
    if (calls != null) {
      for (ProcedureUsage call : calls) {
        List<Expression> args = call.getArguments();
        for (Expression arg : args) {
          vars.addAll(arg.getAllVariables());
        }
      }
    }
    return vars;
  }
  /**
   * Returns the set of all procedure calls contained in the expression. It also covers variables
   * and procedure call subscripts. Recursive call.
   *
   * @return A set of procedure calls. If not, it returns an empty set.
   */
  public Set<ProcedureUsage> getAllFunctions() {
    Set<ProcedureUsage> pus = new HashSet<ProcedureUsage>();
    for (Variable var : this.variables) {
      Set<ProcedureUsage> list = var.getAllFunctions();
      if (list != null && list.size() > 0) {
        pus.addAll(list);
      }
    }
    pus.addAll(this.funcCalls);
    for (ProcedureUsage call : this.funcCalls) {
      List<Expression> args = call.getArguments();
      for (Expression arg : args) {
        pus.addAll(arg.getAllFunctions());
      }
    }
    return pus;
  }

  /**
   * Set the list of function calls that appear in the expression.
   *
   * @param fCalls List of function calls
   */
  public void setFuncCalls(List<ProcedureUsage> fCalls) {
    this.funcCalls = fCalls;
  }
  /**
   * Add a function call that appears in the expression.
   *
   * @param call Function call
   */
  public void addFuncCall(ProcedureUsage call) {
    funcCalls.add(call);
  }

  /**
   * Returns a list of function calls contained on the right-hand side.
   *
   * @return A list of function calls. If not, returns an empty list.
   */
  public List<ProcedureUsage> getFuncCalls() {
    return funcCalls;
  }

  /**
   * Returns the expression of interest.
   *
   * @return Target expression
   */
  @Override
  public String toString() {
    return line;
  }
  /**
   * Returns a string representation of the expression. String representation of @return expression
   */
  public String getLine() {
    return line;
  }
  /**
   * Set the string representation of the expression.
   *
   * @param ln String representation of an expression
   */
  public void setLine(String ln) {
    this.line = ln;
  }

  /** Addition count. */
  public void incrementAdd() {
    addCount++;
  }
  /** Subtraction count. */
  public void incrementSub() {
    subCount++;
  }
  /** Multiply count. */
  public void incrementMul() {
    mulCount++;
  }
  /** Division count. */
  public void incrementDiv() {
    divCount++;
  }
  /** Cumulative count. */
  public void incrementPow() {
    powCount++;
  }

  /**
   * Returns the addition count.
   *
   * @return Addition count
   */
  public int getAddCount() {
    return addCount;
  }
  /**
   * Returns the subtraction count.
   *
   * @return Subtraction count
   */
  public int getSubCount() {
    return subCount;
  }
  /**
   * Returns the multiplication count.
   *
   * @return multiplication count
   */
  public int getMulCount() {
    return mulCount;
  }
  /**
   * Returns the division count.
   *
   * @return division count
   */
  public int getDivCount() {
    return divCount;
  }
  /**
   * Returns the cumulative count.
   *
   * @return Cumulative count
   */
  public int getPowCount() {
    return powCount;
  }

  /**
   * Set the addition count.
   *
   * @param add Addition count
   */
  public void setAddCount(int add) {
    this.addCount = add;
  }
  /**
   * Set the subtraction count.
   *
   * @param sub Addition count
   */
  public void setSubCount(int sub) {
    this.subCount = sub;
  }
  /**
   * Set the multiplication count.
   *
   * @param mul multiplication count
   */
  public void setMulCount(int mul) {
    this.mulCount = mul;
  }
  /**
   * Set the division count.
   *
   * @param div division count
   */
  public void setDivCount(int div) {
    this.divCount = div;
  }
  /**
   * Set the cumulative count.
   *
   * @param pow Cumulative count
   */
  public void setPowCount(int pow) {
    this.powCount = pow;
  }

  /**
   * Recursively determines whether a variable is included in the expression, including an internal
   * function call, and returns a boolean value.
   *
   * @param name Variable name True if @return variable is included. Other than that, it is false.
   */
  public boolean hasVariable(String name) {
    List<Variable> vars = this.getVariables();
    for (Variable var : vars) {
      if (var.getName().equalsIgnoreCase(name)) {
        return true;
      }
    }
    List<ProcedureUsage> funcs = this.getFuncCalls();
    for (ProcedureUsage func : funcs) {
      List<Expression> funcArgs = func.getArguments();
      for (Expression funcArg : funcArgs) {
        if (funcArg.hasVariable(name)) {
          return true;
        }
      }
    }
    return false;
  }

  /**
   * Generate an additional information block collection.
   *
   * @return Additional information block collection
   */
  public InformationBlocks createInformationBlocks() {
    InformationBlocks result = new InformationBlocks();
    if (this.funcCalls != null) {
      for (ProcedureUsage pu : this.funcCalls) {
        result.addAll(pu.createInformationBlocks());
      }
    }
    return result;
  }
  /**
   * Search for information blocks that match id.
   *
   * @param id ID
   * @return The information block found. If not found, null is returned.
   */
  public IInformation findInformationBlockBy(String id) {
    IInformation result = null;
    if (this.funcCalls != null) {
      for (ProcedureUsage pu : this.funcCalls) {
        result = pu.findInformationBlockBy(id);
        if (result != null) {
          break;
        }
      }
    }
    return result;
  }
  /**
   * Determine if it is an array expression.
   *
   * @return Boolean value. True if it is an array formula
   */
  public boolean isArrayExpression() {
    List<Variable> vars = this.getVariables();
    for (Variable var : vars) {
      if (var.isArrayExpression()) {
        return true;
      }
    }
    return false;
  }

  /**
   * Check if they are the same expression. Check if the string representation of the expression is
   * the same.
   *
   * @param destExp expression
   * @return true = same
   */
  public boolean equalsExpression(Expression destExp) {
    if (destExp == null) return false;
    String thisLine = this.getLine();
    String destLine = destExp.getLine();
    if (thisLine == destLine) {
      return true;
    } else if (thisLine == null) {
      return false;
    } else if (!thisLine.equalsIgnoreCase(destLine)) {
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
  public IInformation[] searchInformationBlocks(IInformation block) {

    List<IInformation> list = new ArrayList<IInformation>();
    if (this.funcCalls != null) {
      for (ProcedureUsage call : this.funcCalls) {
        IInformation[] infos = call.searchInformationBlocks(block);
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
   * Get the parent block. The parent block is the assignment statement (Substitution) and syntax to
   * which the variable belongs.
   *
   * @return Parent block
   */
  public IBlock getParentStatement() {
    return parentStatement;
  }

  /**
   * Set the parent block. The parent block is the assignment statement (Substitution) and syntax to
   * which the variable belongs.
   *
   * @param parent parent block
   */
  public void setParentStatement(IBlock parent) {
    this.parentStatement = parent;
    // Set for child variables
    if (this.variables != null) {
      for (Variable var : this.variables) {
        if (var == null) continue;
        var.setParentStatement(parent);
      }
    }
    if (this.funcCalls != null) {
      for (ProcedureUsage call : this.funcCalls) {
        if (call == null) continue;
        List<Expression> args = call.getArguments();
        for (Expression arg : args) {
          arg.setParentStatement(parent);
        }
      }
    }
  }

  /**
   * Returns the addition count. Also counts child variables, subscripts, and function arguments.
   *
   * @return Addition count
   */
  public int getOperandAddCount() {
    int count = this.addCount;
    if (this.variables != null) {
      for (Variable var : this.variables) {
        count += var.getOperandAddCount();
      }
    }
    List<ProcedureUsage> calls = this.funcCalls;
    if (calls != null) {
      for (ProcedureUsage call : calls) {
        List<Expression> args = call.getArguments();
        for (Expression arg : args) {
          count += arg.getOperandAddCount();
        }
      }
    }
    return count;
  }
  /**
   * Returns the subtraction count. Also counts child variables, subscripts, and function arguments.
   *
   * @return Subtraction count
   */
  public int getOperandSubCount() {
    int count = this.subCount;
    if (this.variables != null) {
      for (Variable var : this.variables) {
        count += var.getOperandSubCount();
      }
    }
    List<ProcedureUsage> calls = this.funcCalls;
    if (calls != null) {
      for (ProcedureUsage call : calls) {
        List<Expression> args = call.getArguments();
        for (Expression arg : args) {
          count += arg.getOperandSubCount();
        }
      }
    }
    return count;
  }
  /**
   * Returns the multiplication count. Also counts child variables, subscripts, and function
   * arguments.
   *
   * @return multiplication count
   */
  public int getOperandMulCount() {
    int count = this.mulCount;
    if (this.variables != null) {
      for (Variable var : this.variables) {
        count += var.getOperandMulCount();
      }
    }
    List<ProcedureUsage> calls = this.funcCalls;
    if (calls != null) {
      for (ProcedureUsage call : calls) {
        List<Expression> args = call.getArguments();
        for (Expression arg : args) {
          count += arg.getOperandMulCount();
        }
      }
    }
    return count;
  }

  /**
   * Returns the division count. Also counts child variables, subscripts, and function arguments.
   *
   * @return division count
   */
  public int getOperandDivCount() {
    int count = this.divCount;
    if (this.variables != null) {
      for (Variable var : this.variables) {
        count += var.getOperandDivCount();
      }
    }
    List<ProcedureUsage> calls = this.funcCalls;
    if (calls != null) {
      for (ProcedureUsage call : calls) {
        List<Expression> args = call.getArguments();
        for (Expression arg : args) {
          count += arg.getOperandDivCount();
        }
      }
    }
    return count;
  }

  /**
   * Returns the cumulative count. Also counts child variables, subscripts, and function arguments.
   *
   * @return Cumulative count
   */
  public int getOperandPowCount() {
    int count = this.powCount;
    if (this.variables != null) {
      for (Variable var : this.variables) {
        count += var.getOperandPowCount();
      }
    }
    List<ProcedureUsage> calls = this.funcCalls;
    if (calls != null) {
      for (ProcedureUsage call : calls) {
        List<Expression> args = call.getArguments();
        for (Expression arg : args) {
          count += arg.getOperandPowCount();
        }
      }
    }
    return count;
  }
}
