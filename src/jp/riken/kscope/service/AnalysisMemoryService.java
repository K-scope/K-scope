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
import java.util.Arrays;
import java.util.List;
import jp.riken.kscope.common.ACCESSMEMORY_TYPE;
import jp.riken.kscope.data.RequiredBF;
import jp.riken.kscope.data.RequiredBFResult;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Substitution;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.language.fortran.VariableType;
import jp.riken.kscope.language.utils.LanguageVisitor;
import jp.riken.kscope.language.utils.OperationCounterUtils;
import jp.riken.kscope.language.utils.VariableMemoryEntry;
import jp.riken.kscope.model.RequiredBFModel;
import jp.riken.kscope.properties.OperationProperties;
import jp.riken.kscope.properties.RequiredBFProperties;
import jp.riken.kscope.properties.RequiredBFProperties.MEM_THROUGHPUT_CALC_MODE;
import jp.riken.kscope.properties.VariableMemoryProperties;
import jp.riken.kscope.utils.StringUtils;

/**
 * Calculate memory performance
 *
 * @author RIKEN
 */
public class AnalysisMemoryService extends AnalysisBaseService {

  /** Request Byte / FLOP configuration property */
  private RequiredBFProperties properitiesRequiredBF;
  /** Built-in function operation count property */
  private OperationProperties propertiesOperation;
  /** Request Byte / FLOP table model */
  private RequiredBFModel modelRequiredBF;
  /** Request Byte / FLOP calculation result */
  private List<RequiredBFResult> requiedBFResults;
  /** Selection block */
  private List<IBlock> blocks;
  /** Variable access destination memory setting property */
  private VariableMemoryProperties propertiesVariableMemory;

  /** Constructor */
  public AnalysisMemoryService() {
    super();
  }

  /**
   * Get the request Byte / FLOP configuration property.
   *
   * @return Request Byte / FLOP configuration property
   */
  public RequiredBFProperties getProperitiesRequiredBF() {
    return properitiesRequiredBF;
  }

  /**
   * Set the request Byte / FLOP setting property.
   *
   * @param properities Request Byte / FLOP configuration properties
   */
  public void setProperitiesRequiredBF(RequiredBFProperties properities) {
    this.properitiesRequiredBF = properities;
  }

  /**
   * Get the built-in function operation count property.
   *
   * @return Built-in function operation count property
   */
  public OperationProperties getPropertiesOperand() {
    return propertiesOperation;
  }

  /**
   * Set the built-in function operation count property.
   *
   * @param properities Built-in function arithmetic count property
   */
  public void setPropertiesOperand(OperationProperties properities) {
    this.propertiesOperation = properities;
  }

  /**
   * Get the selected block.
   *
   * @return selection block
   */
  public IBlock[] getBlocks() {
    return blocks.toArray(new IBlock[0]);
  }

  /**
   * Set the selection block.
   *
   * @param blocks selection blocks
   */
  public void setBlocks(IBlock[] blocks) {
    if (this.blocks == null) {
      this.blocks = new ArrayList<IBlock>();
    }
    this.blocks.addAll(Arrays.asList(blocks));
  }

  /**
   * Get the request Byte / FLOP table model.
   *
   * @return Request Byte / FLOP table model
   */
  public RequiredBFModel getModelRequiredBF() {
    return modelRequiredBF;
  }

  /**
   * Set the request Byte / FLOP table model.
   *
   * @param model Request Byte / FLOP table model
   */
  public void setModelRequiredBF(RequiredBFModel model) {
    this.modelRequiredBF = model;
  }

  /**
   * Get the request Byte / FLOP calculation result.
   *
   * @return Request Byte / FLOP calculation result
   */
  public List<RequiredBFResult> getReqiedBFResults() {
    return requiedBFResults;
  }

  /**
   * Set the request Byte / FLOP calculation result.
   *
   * @param reqiedBFResults Request Byte / FLOP calculation result
   */
  public void setReqiedBFResults(List<RequiredBFResult> reqiedBFResults) {
    this.requiedBFResults = reqiedBFResults;
  }

  /**
   * Calculate the request Byte / FLOP.
   *
   * @param block Calculation block
   * @return Request Byte / FLOP calculation result
   */
  public RequiredBFResult calcRequiredBF(IBlock block) {
    if (block == null) return null;

    // Count the number of operations, Load, Store.
    OperationCounterUtils utils = new OperationCounterUtils();
    utils.setPropertiesOperation(this.propertiesOperation);
    utils.countBlock(block);

    RequiredBFResult result = new RequiredBFResult();
    // Calculation block
    result.setBlock(block);
    // Right-hand variable
    List<Variable> rights = utils.getRightVariables();
    // Left side variable
    List<Variable> lefts = utils.getLeftVariables();
    // Load, Store
    setStoreLoad(result, lefts, rights);
    // Number of operations
    setOperation(
        result,
        utils.getAddFlop(),
        utils.getSubFlop(),
        utils.getMulFlop(),
        utils.getDivFlop(),
        utils.getPowFlop(),
        utils.getFunctionFlop());
    // Request Byte / FLOP, Request FLOP / Byte
    result.calcRequiredBF();
    // Memory throughput
    setMemThroughput(result, lefts, rights);
    // Effective Byte / FLOP, Effective FLOP / Byte
    result.calcRequiredBF(this.properitiesRequiredBF.getFlopPerformance());
    // Peak performance
    result.calcPeakPerformance();
    // Access memory count
    setAccessCount(result, lefts, rights);
    // BF calculation unit
    result.setBFCalcType(this.properitiesRequiredBF.getBFCalcType());

    return result;
  }

  /**
   * Set the number of operations.
   *
   * @param result Calculation result
   * @param add Addition (+)
   * @param sub Subtraction (-)
   * @param mul multiplication (*)
   * @param div division (/)
   * @param pow to the power
   * @param function Addition (+) + multiplication (*) of built-in functions
   */
  private void setOperation(
      RequiredBFResult result, int add, int sub, int mul, int div, int pow, int function) {
    if (result == null) return;

    // Number of operations (FLOP) = add (F) + mul (F) + intrinsic (F)
    int op = add + sub + mul + div + pow + function;
    result.setOperation(op);
    // Number of additions (+) to variables of floating point data type
    result.setAddCount(add);
    // Number of subtractions (-) for variables of floating point data type
    result.setSubCount(sub);
    // Number of multiplications (*) on variables of floating point data type
    result.setMulCount(mul);
    // Number of divisions (/) on variables of floating point data type
    result.setDivCount(div);
    // Exponentiation of variables of floating point data type, addition of built-in functions (+) +
    // multiplication (*)
    result.setIntrinsicCount(pow + function);

    return;
  }

  /**
   * Set Store and Load in the calculation result.
   *
   * @param result Calculation result
   * @param lefts Left side variable list
   * @param rights Right-hand variable list
   */
  private void setStoreLoad(RequiredBFResult result, List<Variable> lefts, List<Variable> rights) {
    if (result == null) return;
    // List of all variables
    List<Variable> all = getMargeVariable(lefts, rights);
    // Number of variable bytes
    int loadByte = getVariableMemoryByte(all);
    int leftByte = getVariableMemoryByte(lefts);
    // Load calculation = right + left
    result.setLoad(loadByte);
    // Store variable
    result.setStore(leftByte);
  }

  /**
   * Merge the left-hand variable list and the right-hand variable list. Exclude the same variable.
   *
   * @param lefts Left side variable list
   * @param rights Right-hand variable list
   * @return Merge variable list
   */
  private List<Variable> getMargeVariable(List<Variable> lefts, List<Variable> rights) {
    List<Variable> all = new ArrayList<Variable>();
    OperationCounterUtils utils = new OperationCounterUtils();
    if (lefts != null) {
      for (Variable var : lefts) {
        if (utils.getVariables(all, var) == null) {
          all.add(var);
        }
      }
    }
    if (rights != null) {
      for (Variable var : rights) {
        if (utils.getVariables(all, var) == null) {
          all.add(var);
        }
      }
    }
    if (all.size() <= 0) return null;
    return all;
  }

  /**
   * Set memory throughput.
   *
   * @param result Calculation result
   * @param lefts Left side variable list
   * @param rights Right-hand variable list
   */
  private void setMemThroughput(
      RequiredBFResult result, List<Variable> lefts, List<Variable> rights) {
    if (result == null) return;

    // Judgment with or without Store
    // Judge from the left side variable and the throughput mode of the request Byte / FLOP setting
    // dialog.
    boolean isstore = isStore(lefts);

    // Throughput (GB / s)
    float throughput = 0.0F;
    if (isstore) {
      throughput = this.properitiesRequiredBF.getMemThroughputStore();
    } else {
      throughput = this.properitiesRequiredBF.getMemThroughputNostore();
    }
    result.setThroughput(throughput);

    // Memory throughput calculation mode
    result.setMemThroughputCalcMode(this.properitiesRequiredBF.getMemThroughputCalcMode());
    // Calculation source throughput value (GB / s) (with or without store)
    RequiredBF memory = this.properitiesRequiredBF.getRequiredBF(ACCESSMEMORY_TYPE.MEMORY);
    RequiredBF l1 = this.properitiesRequiredBF.getRequiredBF(ACCESSMEMORY_TYPE.L1_CACHE);
    RequiredBF l2 = this.properitiesRequiredBF.getRequiredBF(ACCESSMEMORY_TYPE.L2_CACHE);
    RequiredBF register = this.properitiesRequiredBF.getRequiredBF(ACCESSMEMORY_TYPE.REGISTER);
    RequiredBF custom = this.properitiesRequiredBF.getRequiredBF(ACCESSMEMORY_TYPE.CUSTOM);
    if (isstore) {
      // Calculation source throughput value (GB / s) (with store)
      result.setMemoryMBW(memory.getMemThroughputStore());
      result.setL1MBW(l1.getMemThroughputStore());
      result.setL2MBW(l2.getMemThroughputStore());
      result.setRegisterMBW(register.getMemThroughputStore());
      result.setCustomMBW(custom.getMemThroughputStore());
    } else {
      // Source throughput value (GB / s) (no store)
      result.setMemoryMBW(memory.getMemThroughputNostore());
      result.setL1MBW(l1.getMemThroughputNostore());
      result.setL2MBW(l2.getMemThroughputNostore());
      result.setRegisterMBW(register.getMemThroughputNostore());
      result.setCustomMBW(custom.getMemThroughputNostore());
    }
    // Coefficient
    result.setMemoryCoef(memory.getCoef());
    result.setL1Coef(l1.getCoef());
    result.setL2Coef(l2.getCoef());
    result.setRegisterCoef(register.getCoef());
    result.setCustomCoef(custom.getCoef());
  }

  /**
   * Set the memory access destination memory.
   *
   * @param result Calculation result
   * @param lefts Left side variable list
   * @param rights Right-hand variable list
   */
  private void setAccessCount(
      RequiredBFResult result, List<Variable> lefts, List<Variable> rights) {
    if (result == null) return;
    int memory = 0;
    int l1 = 0;
    int l2 = 0;
    int register = 0;
    int custom = 0;
    if (lefts != null && lefts.size() > 0) {
      for (Variable var : lefts) {
        ACCESSMEMORY_TYPE type = getMemoryType(var);
        if (type == null) {
          type = ACCESSMEMORY_TYPE.getDefaultType(var);
        }
        if (type == ACCESSMEMORY_TYPE.MEMORY) memory++;
        else if (type == ACCESSMEMORY_TYPE.L1_CACHE) l1++;
        else if (type == ACCESSMEMORY_TYPE.L2_CACHE) l2++;
        else if (type == ACCESSMEMORY_TYPE.REGISTER) register++;
        else if (type == ACCESSMEMORY_TYPE.CUSTOM) custom++;
      }
    }
    if (rights != null && rights.size() > 0) {
      for (Variable var : rights) {
        ACCESSMEMORY_TYPE type = getMemoryType(var);
        if (type == null) {
          type = ACCESSMEMORY_TYPE.getDefaultType(var);
        }
        if (type == ACCESSMEMORY_TYPE.MEMORY) memory++;
        else if (type == ACCESSMEMORY_TYPE.L1_CACHE) l1++;
        else if (type == ACCESSMEMORY_TYPE.L2_CACHE) l2++;
        else if (type == ACCESSMEMORY_TYPE.REGISTER) register++;
        else if (type == ACCESSMEMORY_TYPE.CUSTOM) custom++;
      }
    }
    result.setMemoryCount(memory);
    result.setL1Count(l1);
    result.setL2Count(l2);
    result.setRegisterCount(register);
    result.setCustomCount(custom);
  }

  /**
   * Judge whether there is a store for throughput calculation.
   *
   * @param lefts Left side variable list
   * @return true = with store
   */
  private boolean isStore(List<Variable> lefts) {
    // Judgment with or without Store
    boolean isstore = false;
    if (this.properitiesRequiredBF.getMemThroughputCalcMode() == MEM_THROUGHPUT_CALC_MODE.NOSTORE) {
      isstore = false;
    } else if (this.properitiesRequiredBF.getMemThroughputCalcMode()
        == MEM_THROUGHPUT_CALC_MODE.STORE) {
      isstore = true;
    } else if (this.properitiesRequiredBF.getMemThroughputCalcMode()
        == MEM_THROUGHPUT_CALC_MODE.AUTO) {
      int leftbyte = getVariableMemoryByte(lefts);
      if (leftbyte > 0) {
        isstore = true;
      }
    }
    return isstore;
  }

  /**
   * Get the number of bytes in a variable.
   *
   * @param var variable
   * @return Number of bytes
   */
  private int getVariableByte(Variable var) {
    if (var == null) return 0;
    if (var.getDefinition() == null) return 0;
    if (var.getDefinition().getType() == null) return 0;
    if (!(var.getDefinition().getType() instanceof VariableType)) return 0;
    VariableType type = (VariableType) var.getDefinition().getType();
    if (type == null) return 0;

    // kind attribute
    int kind = 0;
    if (type.getKind() != null) {
      if (StringUtils.isNumeric(type.getKind().toString())) {
        kind = Integer.parseInt(type.getKind().toString());
      }
    }
    // Find the number of bytes in the data type.
    int byteValue = 0;
    if (type.getPrimitiveDataType() == VariableType.PrimitiveDataType.BYTE) {
      byteValue = 1;
    } else if (type.getPrimitiveDataType() == VariableType.PrimitiveDataType.INTEGER) {
      byteValue = 4;
      if (kind > 0) {
        byteValue = kind;
      } else if (this.properitiesRequiredBF.getDefaultSizeInteger() > 0) {
        byteValue = this.properitiesRequiredBF.getDefaultSizeInteger();
      }
    } else if (type.getPrimitiveDataType() == VariableType.PrimitiveDataType.REAL) {
      byteValue = 4;
      if (kind > 0) {
        byteValue = kind;
      } else if (this.properitiesRequiredBF.getDefaultSizeReal() > 0) {
        byteValue = this.properitiesRequiredBF.getDefaultSizeReal();
      }
    } else if (type.getPrimitiveDataType() == VariableType.PrimitiveDataType.DOUBLE_PRECISION) {
      byteValue = 8;
    } else if (type.getPrimitiveDataType() == VariableType.PrimitiveDataType.COMPLEX) {
      byteValue = 8;
    } else if (type.getPrimitiveDataType() == VariableType.PrimitiveDataType.DOUBLE_COMPLEX) {
      byteValue = 16;
    }

    return byteValue;
  }

  /**
   * Check if the variable is on the left side.
   *
   * @param var variable
   * @return Number of bytes
   */
  @SuppressWarnings("unused")
  private boolean isRightVariable(Variable var) {
    if (var == null) return false;
    if (var.getParentStatement() == null) return false;
    IBlock parent = var.getParentStatement();
    if (!(parent instanceof Substitution)) return false;
    Substitution sub = (Substitution) parent;
    if (sub.getLeftValue() == var) {
      return false;
    }
    // Variable on the right side because it is not on the left side
    return true;
  }

  /**
   * Check if the variable is on the left side.
   *
   * @param var variable
   * @return Number of bytes
   */
  @SuppressWarnings("unused")
  private boolean isLeftVariable(Variable var) {
    if (var == null) return false;
    if (var.getParentStatement() == null) return false;
    IBlock parent = var.getParentStatement();
    if (!(parent instanceof Substitution)) return false;
    Substitution sub = (Substitution) parent;
    if (sub.getLeftValue() != var) {
      return false;
    }
    return true;
  }

  /**
   * Get the number of variables for request B / F.
   *
   * @param vars Variable list
   * @return Request B / F Number of variables
   */
  @SuppressWarnings("unused")
  private int getVariableMemoryCount(List<Variable> vars) {
    if (vars == null) return 0;
    if (vars.size() <= 0) return 0;
    if (this.properitiesRequiredBF == null) return vars.size();
    int count = 0;
    for (Variable var : vars) {
      ACCESSMEMORY_TYPE type = getMemoryType(var);
      if (type == null) {
        type = ACCESSMEMORY_TYPE.getDefaultType(var);
      }
      if (type == null) continue;
      RequiredBF mem = this.properitiesRequiredBF.getRequiredBF(type);
      if (mem == null) continue;
      if (mem.isRequiredBF()) {
        count++;
      }
    }
    return count;
  }

  /**
   * Get the variable Byte for the request B / F.
   *
   * @param vars Variable list
   * @return Request B / F Target variable Byte
   */
  private int getVariableMemoryByte(List<Variable> vars) {
    if (vars == null) return 0;
    if (vars.size() <= 0) return 0;
    int varbyte = 0;
    for (Variable var : vars) {
      if (this.properitiesRequiredBF != null) {
        ACCESSMEMORY_TYPE type = getMemoryType(var);
        if (type == null) {
          // Get the access destination memory from the configured variable
          type = getAccessMemoryType(var);
          if (type == null) {
            // Get the default access memory
            type = ACCESSMEMORY_TYPE.getDefaultType(var);
          }
        }
        if (type == null) continue;
        RequiredBF mem = this.properitiesRequiredBF.getRequiredBF(type);
        if (mem == null) continue;
        if (mem.isRequiredBF()) {
          varbyte += getVariableByte(var);
        }
      } else {
        varbyte += getVariableByte(var);
      }
    }
    return varbyte;
  }

  /**
   * Get the memory type to access the variable. Priority is given to the temporary setting access
   * destination memory from the source view application access destination memory and temporary
   * setting access destination memory.
   *
   * @param var variable
   * @return Access memory
   */
  private ACCESSMEMORY_TYPE getMemoryType(Variable var) {
    if (var == null) return null;
    ACCESSMEMORY_TYPE type = var.getMemoryType();
    ACCESSMEMORY_TYPE temp = var.getTemporaryMemoryType();
    if (temp == null) return type;
    return temp;
  }

  /**
   * Add the calculation result to the analysis view.
   *
   * @param results Calculation result list
   */
  public void setAnalysisPanel(RequiredBFResult[] results) {
    if (this.modelRequiredBF == null) return;
    if (this.properitiesRequiredBF != null) {
      // Set the calculation unit
      this.modelRequiredBF.setUnitType(this.properitiesRequiredBF.getBFCalcType());
    }
    this.modelRequiredBF.addRequiredByteFlopResults(results);
  }

  /**
   * Get the variable access destination memory setting property.
   *
   * @return variable access destination memory setting property
   */
  public VariableMemoryProperties getPropertiesVariableMemory() {
    return propertiesVariableMemory;
  }

  /**
   * Set the variable access destination memory setting property.
   *
   * @param properties Variable access destination memory setting property
   */
  public void setPropertiesVariableMemory(VariableMemoryProperties properties) {
    this.propertiesVariableMemory = properties;
  }

  /**
   * Set a variable in the variable access destination memory setting property. Get additional
   * variables from the database.
   *
   * @param language Fortran database
   */
  public void createVariableMemoryProperties(Fortran language) {

    VariableMemoryEntry entry = new VariableMemoryEntry(language);
    LanguageVisitor visitor = new LanguageVisitor(entry);
    visitor.entry();
    // Get the set variables of the access destination memory
    Variable[] vars = entry.getListVariable();
    if (vars == null || vars.length <= 0) return;
    for (Variable var : vars) {
      this.propertiesVariableMemory.addVariable(var);
    }
    propertiesVariableMemory.firePropertyChange();
  }

  /**
   * Get the access destination memory from the set variable. Get variables with the same subscript
   * and the same definition. If there are multiple configured variables and the set access
   * destination memory is different, null is returned.
   *
   * @param var Search variable
   * @return Access memory
   */
  private ACCESSMEMORY_TYPE getAccessMemoryType(Variable var) {
    // Get the access destination memory from the configured variable
    if (this.propertiesVariableMemory == null) return null;
    List<Variable> vars = this.propertiesVariableMemory.getEqualsVariableDefinition(var);
    if (vars == null || vars.size() <= 0) return null;

    // Returns null if the configuration access destination memory is different
    ACCESSMEMORY_TYPE type = null;
    for (Variable item : vars) {
      if (type == null) {
        type = item.getMemoryType();
      }
      if (type != item.getMemoryType()) {
        return null;
      }
    }

    return type;
  }
}
