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

package jp.riken.kscope.language.fortran;

import java.io.Serializable;
import java.util.List;
import java.util.Set;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.BlockType;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.language.VariableDefinition;

/**
 * type Type class.
 *
 * @author RIKEN
 */
public class Type implements IBlock, Serializable {
  /** Serial number */
  private static final long serialVersionUID = 5400440025290085684L;
  /** Since it is almost the same as the structure type, it is used by including the structure. */
  private Structure core;

  /** Constructor. */
  public Type() {
    core = new Structure();
  }

  /**
   * Constructor.
   *
   * @param nm Structure name
   */
  public Type(String nm) {
    core = new Structure(nm);
  }

  @Override
  public String toString() {
    return "type " + this.getName();
  }
  /**
   * Addition of variable definition statement.
   *
   * @param definition Variable definition statement
   */
  public void add(VariableDefinition definition) {
    core.add(definition);
  }

  /**
   * Add variable definition statement of the specified type.
   *
   * @param typ Variable type
   * @param nm Variable name
   */
  public void add(VariableType typ, String nm) {
    core.add(typ, nm);
  }

  /**
   * Added type statement.
   *
   * @param type Structure
   * @param variableName Variable name
   */
  public void add(Type type, String variableName) {
    core.add(type, variableName);
  }

  /**
   * Addition of structure statement.
   *
   * @param structure Structure
   * @param variableName Variable name
   */
  public void add(Structure structure, String variableName) {
    core.add(structure, variableName);
  }

  /**
   * Added union statement.
   *
   * @param union Union type
   */
  public void add(Union union) {
    core.add(union);
  }

  /**
   * Get the structure name.
   *
   * @return The name of the structure
   */
  public String getName() {
    return core.getName();
  }

  /**
   * Get a list of variable definition statements in the structure.
   *
   * @return Variable definition statement list. If not, null is returned.
   */
  public List<VariableDefinition> getDefinitions() {
    return core.getDefinitions();
  }

  /**
   * Setting the variable definition statement list in the structure.
   *
   * @param Variable definition statement list
   */
  public void setDefinitions(List<VariableDefinition> list) {
    core.setDefinitions(list);
  }

  /**
   * Whether the type is compatible. <br>
   * When searching for the corresponding function from the overloaded function group, <br>
   * It is necessary to check the type of formal and actual arguments. <br>
   * "Matching" is judged to be the same type by this type check Means a thing.
   *
   * @param value Type
   * @return true: Conforms <br>
   *     false: Not compatible
   */
  public boolean matches(Type value) {
    if (value == null) {
      return false;
    }
    // Name check only
    return this.getName().equalsIgnoreCase(value.getName());
  }

  @Override
  public CodeLine getStartCodeLine() {
    if (this.core.getStartStatement() == null) return null;
    return this.core.getStartStatement().getLineInfo();
  }

  @Override
  public CodeLine getEndCodeLine() {
    if (this.core.getEndStatement() == null) return null;
    return this.core.getEndStatement().getLineInfo();
  }

  /**
   * Set the start code line information.
   *
   * @param line Set the start code line information.
   */
  public void setStartCodeLine(CodeLine line) {
    if (this.core == null) return;
    this.core.setStartStatement(line);
  }

  /**
   * Set the exit code line information.
   *
   * @param line Set the exit code line information.
   */
  public void setEndCodeLine(CodeLine line) {
    if (this.core == null) return;
    this.core.setEndStatement(line);
  }

  @Override
  public BlockType getBlockType() {
    return BlockType.TYPE;
  }

  @Override
  public IBlock getMotherBlock() {
    return this.core.getMotherBlock();
  }

  /**
   * Set the parent block.
   *
   * @param block Parent block
   */
  public void setMotherBlock(IBlock block) {
    this.core.setMotherBlock(block);
  }

  /** Get the variable list. */
  @Override
  public Set<Variable> getAllVariables() {
    return null;
  }
}
