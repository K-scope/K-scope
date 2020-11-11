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
package jp.riken.kscope.properties;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import jp.riken.kscope.common.ACCESSMEMORY_TYPE;
import jp.riken.kscope.data.Keyword;
import jp.riken.kscope.data.RequiredBF;
import jp.riken.kscope.data.VariableMemory;
import jp.riken.kscope.language.Variable;

/**
 * Source code variable access destination memory setting class
 *
 * @author RIKEN
 */
public class VariableMemoryProperties extends PropertiesBase {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Variable (highlight) setting list */
  private List<VariableMemory> listVariable = new ArrayList<VariableMemory>();
  /** Request Byte / FLOP configuration property */
  RequiredBFProperties requiredBFProperties;

  /**
   * Constructor
   *
   * @param properties Request Byte / FLOP configuration properties
   */
  public VariableMemoryProperties(RequiredBFProperties properties) {
    this.requiredBFProperties = properties;
  }

  /** Notify property change event. */
  @Override
  public void firePropertyChange() {
    this.changes.firePropertyChange(this.getClass().getName(), null, this);
  }

  /**
   * Get the variable (highlight) setting list.
   *
   * @return Variable highlight setting list
   */
  public List<VariableMemory> getListVariableMemory() {
    return this.listVariable;
  }

  /**
   * Get the number of variables (highlights).
   *
   * @return Number of variables (highlights)
   */
  public int getVariableCount() {
    if (listVariable == null || listVariable.size() <= 0) {
      return 0;
    }
    return listVariable.size();
  }

  /**
   * Get variables (highlights).
   *
   * @param index index
   * @return variable (highlight)
   */
  public Keyword getVariableMemory(int index) {
    if (listVariable == null || listVariable.size() <= 0) {
      return null;
    }
    if (listVariable.size() <= index) {
      return null;
    }

    return listVariable.get(index);
  }

  /**
   * Set variables (highlights).
   *
   * @param index index
   * @param variable Variable (highlight)
   */
  public void setVariableMemory(int index, VariableMemory variable) {
    if (listVariable == null || listVariable.size() <= 0) {
      return;
    }
    if (listVariable.size() <= index) {
      return;
    }
    listVariable.set(index, variable);
  }

  /**
   * Add variables (highlights).
   *
   * @param variable Variable (highlight)
   */
  public void addVariableMemory(VariableMemory variable) {
    if (listVariable == null) {
      listVariable = new ArrayList<VariableMemory>();
    }
    listVariable.add(variable);
  }

  /**
   * Delete variables (highlights).
   *
   * @param variable Variable (highlight)
   */
  public void removeVariableMemory(VariableMemory variable) {
    if (listVariable == null) return;
    listVariable.remove(variable);
  }

  /**
   * Delete variables (highlights).
   *
   * @param index index
   */
  public void removeVariableMemory(int index) {
    if (listVariable == null) return;
    listVariable.remove(index);
  }

  /**
   * Delete variables (highlights).
   *
   * @param variable Variable (highlight)
   */
  public void removeVariable(Variable variable) {
    if (listVariable == null) return;
    Iterator<VariableMemory> iter = this.listVariable.iterator();
    while (iter.hasNext()) {
      VariableMemory varmem = (VariableMemory) iter.next();
      Variable var = varmem.getVariable();
      if (var == variable) {
        iter.remove();
      }
    }
  }

  /** Clear the variable (highlight) list. */
  public void clearVariableMemory() {
    listVariable = new ArrayList<VariableMemory>();
  }

  /**
   * Get variable (highlight) settings.
   *
   * @param variable variable
   */
  public VariableMemory getVariableMemory(Variable variable) {
    if (variable == null) return null;
    if (this.listVariable == null || this.listVariable.size() <= 0) return null;
    for (VariableMemory varmem : this.listVariable) {
      Variable var = varmem.getVariable();
      if (var == variable) {
        return varmem;
      }
    }
    return null;
  }

  /**
   * Check if variable (highlight) settings have been added.
   *
   * @param variable variable
   */
  public boolean containsVariableMemory(Variable variable) {
    if (variable == null) return false;
    VariableMemory varmem = getVariableMemory(variable);
    return (varmem != null);
  }

  /**
   * Add variables.
   *
   * @param variable variable
   */
  public void addVariable(Variable variable) {
    if (variable == null) return;
    if (containsVariableMemory(variable)) {
      VariableMemory varmem = getVariableMemory(variable);
      RequiredBF mem = getMemoryband(variable);
      varmem.setRequiredBF(mem);
    } else {
      // Generate variable memory data
      VariableMemory varmem = createVariableMemory(variable);
      addVariableMemory(varmem);
    }
  }

  /**
   * Create a variable memory data object.
   *
   * @param variable variable
   * @return variable memory data object
   */
  private VariableMemory createVariableMemory(Variable variable) {
    // Generate variable memory data
    RequiredBF mem = getMemoryband(variable);
    VariableMemory varmem = new VariableMemory(variable, mem);

    return varmem;
  }

  /**
   * Get access memory.
   *
   * @param variable variable
   * @return Access memory
   */
  private RequiredBF getMemoryband(Variable variable) {
    // Generate variable memory data
    ACCESSMEMORY_TYPE memorytype = variable.getMemoryType();
    if (memorytype == null) {
      memorytype = ACCESSMEMORY_TYPE.getDefaultType();
      if (variable.getDefinition() != null) {
        memorytype = ACCESSMEMORY_TYPE.getDefaultType(variable.getDefinition());
      }
    }
    RequiredBF mem = requiredBFProperties.getRequiredBF(memorytype);
    return mem;
  }

  /**
   * Get variables with the same definition from the configured list. Get a list of variables with
   * the same definition and the same subscript.
   *
   * @param variable Search variable
   * @return Same definition variable list
   */
  public List<Variable> getEqualsVariableDefinition(Variable variable) {
    if (variable == null) return null;
    if (variable.getDefinition() == null) return null;
    if (this.listVariable == null || this.listVariable.size() <= 0) return null;
    List<Variable> list = new ArrayList<Variable>();
    for (VariableMemory varmem : this.listVariable) {
      Variable var = varmem.getVariable();
      if (variable.getDefinition() != var.getDefinition()) continue;
      if (variable.equalsVariable(var)) {
        list.add(var);
      }
    }
    if (list.size() <= 0) return null;
    return list;
  }

  /**
   * Get the variable list from the configured list.
   *
   * @return Variable list
   */
  public List<Variable> getListVariable() {
    if (this.listVariable == null || this.listVariable.size() <= 0) return null;
    List<Variable> list = new ArrayList<Variable>();
    for (VariableMemory varmem : this.listVariable) {
      Variable var = varmem.getVariable();
      list.add(var);
    }
    if (list.size() <= 0) return null;
    return list;
  }
}
