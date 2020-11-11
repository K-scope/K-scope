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
import jp.riken.kscope.common.ACCESSMEMORY_TYPE;
import jp.riken.kscope.information.InformationBlocks;

/**
 * A class that represents variables that appear in executable statements.
 *
 * @author RIKEN
 */
public class Variable implements Serializable {
  /** Serial number */
  private static final long serialVersionUID = 7677119337098266464L;
  /** Variable name */
  private String name;
  /** Variable value */
  private String value;
  /** Array subscript */
  private List<Expression> indexValues;
  /** Variable definition */
  private VariableDefinition def;
  /** Access memory */
  private ACCESSMEMORY_TYPE memoryType;
  /** Parent block of variable: Substitution, syntax to which the variable belongs */
  private IBlock parentStatement;
  /** Temporary setting access destination memory */
  private transient ACCESSMEMORY_TYPE temporaryMemoryType;

  /**
   * Constructor.
   *
   * @param varName Variable name
   */
  public Variable(String varName) {
    name = varName;
    this.memoryType = null;
  }

  // ++++++++++++++++++++++++++++++++++++++++++++
  @Override
  public String toString() {
    return (name);
  }

  // ++++++++++++++++++++++++++++++++++++++++++++

  /**
   * A set of variable names.
   *
   * @param nm Variable name
   */
  public void setName(String nm) {
    name = nm;
  }

  // ++++++++++++++++++++++++++++++++++++++++++++

  /**
   * A set of variable definitions.
   *
   * @param varDef Variable definition
   */
  public void setDefinition(VariableDefinition varDef) {
    def = varDef;
  }

  // ++++++++++++++++++++++++++++++++++++++++++++

  /**
   * A set of values.
   *
   * @param val Variable value
   */
  public void setValue(String val) {
    value = val;
  }

  // ++++++++++++++++++++++++++++++++++++++++++++
  /**
   * A set of array subscript values.
   *
   * @param val List of subscripts
   */
  public void setDimensionIndexValue(List<Expression> val) {
    indexValues = val;
  }
  // ++++++++++++++++++++++++++++++++++++++++++++

  /**
   * Get variable name.
   *
   * @return Variable name
   */
  public String getName() {
    return (name);
  }

  // ++++++++++++++++++++++++++++++++++++++++++++

  /**
   * Get the value.
   *
   * @return value
   */
  public String getValue() {
    return (value);
  }

  // ++++++++++++++++++++++++++++++++++++++++++++

  /**
   * Get variable definition.
   *
   * @return variable definition
   */
  public VariableDefinition getDefinition() {
    return (def);
  }

  /**
   * Get array subscript value
   *
   * @return Array subscript
   */
  public List<Expression> getDimensionIndexValue() {
    return this.indexValues;
  }

  /**
   * Reproduces and returns the display of variables in the source code.
   *
   * @return Display variables
   */
  public String getVariableString() {
    StringBuilder st = new StringBuilder();
    st.append(name);
    if (this.indexValues != null) {
      st.append("(");
      for (Expression ex : indexValues) {
        st.append(ex.getLine());
        st.append(",");
      }
      st.replace(st.length() - 1, st.length(), ")");
      // System.out.println("variable name is " + name + "," + "indexValues are exist.");
      // System.out.println("VariableString is " + st.toString());
    } else {
      // System.out.println("variable name is " + name + "," + "indexValues are null.");
    }
    return st.toString();
  }
  /**
   * Generate an additional information block collection.
   *
   * @return Additional information block collection
   */
  public InformationBlocks createInformationBlocks() {
    InformationBlocks result = new InformationBlocks();
    if (this.indexValues != null) {
      for (Expression expression : this.indexValues) {
        result.addAll(expression.createInformationBlocks());
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
    if (this.indexValues != null) {
      for (Expression expression : this.indexValues) {
        result = expression.findInformationBlockBy(id);
        if (result != null) {
          break;
        }
      }
    }
    return result;
  }

  /**
   * Returns a set that includes all the variables contained in its own subscript.
   *
   * @return Set of variables
   */
  public Set<Variable> getAllVariables() {
    Set<Variable> set = new HashSet<Variable>();
    List<Expression> exps = this.indexValues;
    if (exps != null) {
      for (Expression exp : exps) {
        set.addAll(exp.getAllVariables());
      }
    }
    return set;
  }

  /**
   * Determine if it is an array expression.
   *
   * @return Boolean value. True if it is an array formula
   */
  public boolean isArrayExpression() {
    if (this.indexValues == null) {
      return false;
    }
    for (Expression ex : this.indexValues) {
      if (ex.toString().contains(":")) {
        return true;
      }
    }
    return false;
  }

  /**
   * Returns the set of all procedure calls contained in the expression. It also covers variables
   * and procedure call subscripts. Recursive call.
   *
   * @return A set of procedure calls.
   */
  public Set<ProcedureUsage> getAllFunctions() {
    if (this.indexValues == null) return null;
    if (this.indexValues.size() <= 0) return null;
    Set<ProcedureUsage> pus = new HashSet<ProcedureUsage>();
    for (Expression exp : this.indexValues) {
      Set<ProcedureUsage> list = exp.getAllFunctions();
      if (list != null && list.size() > 0) {
        pus.addAll(exp.getAllFunctions());
      }
    }
    if (pus.size() <= 0) return null;
    return pus;
  }

  /**
   * Check if they are the same variable.
   *
   * @param destVar variable class
   * @return true = same
   */
  public boolean equalsVariable(Variable destVar) {
    if (destVar == null) return false;
    if (this.name != null) {
      if (!this.name.equalsIgnoreCase(destVar.name)) {
        return false;
      }
    } else if (destVar.name != null) {
      return false;
    }
    if (this.value != null) {
      if (!this.value.equalsIgnoreCase(destVar.value)) {
        return false;
      }
    } else if (destVar.value != null) {
      return false;
    }

    if (this.indexValues != null && destVar.indexValues != null) {
      if (this.indexValues.size() == destVar.indexValues.size()) {
        for (int i = 0; i < this.indexValues.size(); i++) {
          Expression thisExp = this.indexValues.get(i);
          Expression destExp = destVar.indexValues.get(i);
          if (thisExp == destExp) {
            continue;
          } else if (thisExp == null) {
            return false;
          } else if (!thisExp.equalsExpression(destExp)) {
            return false;
          }
        }
      }
    } else if (this.indexValues != null || destVar.indexValues != null) {
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
    if (block == null) return null;

    List<IInformation> list = new ArrayList<IInformation>();
    if (this.indexValues != null) {
      for (Expression expression : this.indexValues) {
        IInformation[] infos = expression.searchInformationBlocks(block);
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
   * Check if it is a memory access target variable. Real variables: scaler and array Integer
   * variable: array
   *
   * @return true = Memory access target
   */
  public boolean isMemoryAccess() {
    // Must be an array
    if (this.def == null) return false;
    if (this.def.getVariableType() == null) return false;
    if (this.def.getVariableType().isRealType()) {
      return true;
    }
    if (this.def.getVariableType().isIntegerType()) {
      if (this.def.get_dimension_size() > 0) {
        return true;
      }
    }
    return false;
  }

  /**
   * Get access memory.
   *
   * @return Access memory
   */
  public ACCESSMEMORY_TYPE getMemoryType() {
    return memoryType;
  }

  /**
   * Set the access destination memory.
   *
   * @param memory Access memory
   */
  public void setMemoryType(ACCESSMEMORY_TYPE memory) {
    this.memoryType = memory;
  }

  /** Clear the access destination memory setting. */
  public void clearMemoryType() {
    this.memoryType = null;
    this.temporaryMemoryType = null;
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
    // Make settings for array subscripts
    if (this.indexValues != null) {
      for (Expression exp : this.indexValues) {
        if (exp == null) continue;
        exp.setParentStatement(parent);
      }
    }
  }

  /**
   * Returns the addition count of variable subscripts. Also counts child variables, subscripts, and
   * function arguments.
   *
   * @return Addition count
   */
  public int getOperandAddCount() {
    int count = 0;
    List<Expression> exps = this.indexValues;
    if (exps != null) {
      for (Expression exp : exps) {
        count = exp.getOperandAddCount();
      }
    }
    return count;
  }

  /**
   * Returns the subtraction count of variable subscripts. Also counts child variables, subscripts,
   * and function arguments.
   *
   * @return Subtraction count
   */
  public int getOperandSubCount() {
    int count = 0;
    List<Expression> exps = this.indexValues;
    if (exps != null) {
      for (Expression exp : exps) {
        count = exp.getOperandSubCount();
      }
    }
    return count;
  }

  /**
   * Returns the multiplication count of variable subscripts. Also counts child variables,
   * subscripts, and function arguments.
   *
   * @return multiplication count
   */
  public int getOperandMulCount() {
    int count = 0;
    List<Expression> exps = this.indexValues;
    if (exps != null) {
      for (Expression exp : exps) {
        count = exp.getOperandMulCount();
      }
    }
    return count;
  }

  /**
   * Returns the division count of a variable index expression. Also counts child variables,
   * subscripts, and function arguments.
   *
   * @return division count
   */
  public int getOperandDivCount() {
    int count = 0;
    List<Expression> exps = this.indexValues;
    if (exps != null) {
      for (Expression exp : exps) {
        count = exp.getOperandDivCount();
      }
    }
    return count;
  }
  /**
   * Returns the cumulative count of variable subscript expressions. Also counts child variables,
   * subscripts, and function arguments.
   *
   * @return Cumulative count
   */
  public int getOperandPowCount() {
    int count = 0;
    List<Expression> exps = this.indexValues;
    if (exps != null) {
      for (Expression exp : exps) {
        count = exp.getOperandPowCount();
      }
    }
    return count;
  }

  /**
   * Get temporary access destination memory
   *
   * @return Temporary access destination memory
   */
  public ACCESSMEMORY_TYPE getTemporaryMemoryType() {
    return temporaryMemoryType;
  }

  /**
   * Temporary setting Set access destination memory
   *
   * @param memory Temporary access destination memory
   */
  public void setTemporaryMemoryType(ACCESSMEMORY_TYPE memory) {
    this.temporaryMemoryType = memory;
  }
}
