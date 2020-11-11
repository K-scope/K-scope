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
import java.util.ArrayList;
import java.util.List;
import jp.riken.kscope.data.CodeLine;
// import jp.riken.kscope.language.Block;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Statement;
import jp.riken.kscope.language.VariableDefinition;

/**
 * structure type class.
 *
 * @author RIKEN
 */
public class Structure implements Serializable {
  /** Serial number */
  private static final long serialVersionUID = 6682888216005792092L;
  /** Variable name */
  private String name = "";
  /** Structure member variables */
  private List<VariableDefinition> definitions = new ArrayList<VariableDefinition>();
  /** Code start line */
  private Statement start;
  /** End line of code */
  private Statement end;
  /** Parent block */
  private IBlock mother;

  /** Constructor. */
  public Structure() {}

  /**
   * Constructor.
   *
   * @param nm Structure name
   */
  public Structure(String nm) {
    name = nm;
  }

  /**
   * Addition of variable definition statement.
   *
   * @param definition Variable definition statement
   */
  public void add(VariableDefinition definition) {
    if (definition != null) {
      definitions.add(definition);
    }
  }

  /**
   * Add variable definition statement of the specified type.
   *
   * @param typ Variable type
   * @param nm Variable name
   */
  public void add(VariableType typ, String nm) {
    if (typ != null && nm != null) {
      VariableDefinition definition = new VariableDefinition(nm, typ, new VariableAttribute());
      definitions.add(definition);
    }
  }

  /**
   * Added type statement.
   *
   * @param type Structure
   * @param variableName Variable name
   */
  public void add(Type type, String variableName) {
    if (type != null && variableName != null) {
      VariableType typeType = new VariableType(type);
      this.add(typeType, variableName);
    }
  }

  /**
   * Addition of structure statement.
   *
   * @param structure Structure
   * @param variableName Variable name
   */
  public void add(Structure structure, String variableName) {
    if (structure != null && variableName != null) {
      VariableType structureType = new VariableType(structure);
      this.add(structureType, variableName);
    }
  }

  /**
   * Added union statement.
   *
   * @param union Union type
   */
  public void add(Union union) {
    if (union != null) {
      VariableType unionType = new VariableType(union);
      this.add(unionType, "");
    }
  }

  /**
   * Get the structure name.
   *
   * @return The name of the structure
   */
  public String getName() {
    return name;
  }

  /**
   * Get a list of variable definition statements in the structure.
   *
   * @return Variable definition statement list
   */
  public List<VariableDefinition> getDefinitions() {
    return definitions;
  }

  /**
   * Setting the variable definition statement list in the structure.
   *
   * @param Variable definition statement list
   */
  public void setDefinitions(List<VariableDefinition> list) {
    this.definitions = list;
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
  public boolean matches(Structure value) {
    if (value == null) {
      return false;
    }
    // Name check only
    return this.name.equalsIgnoreCase(value.getName());
  }

  /**
   * Get the start code line information.
   *
   * @return Start code line information
   */
  public Statement getStartStatement() {
    return this.start;
  }

  /**
   * Set the exit code line information.
   *
   * @param lineInfo Exit code line information
   */
  public Statement getEndStatement() {
    return this.end;
  }

  /**
   * Set the start code line information.
   *
   * @param lineInfo Start code line information
   */
  public void setStartStatement(CodeLine lineInfo) {
    this.start = new Statement(lineInfo);
  }

  /**
   * Set the exit code line information.
   *
   * @param lineInfo Exit code line information
   */
  public void setEndStatement(CodeLine lineInfo) {
    this.end = new Statement(lineInfo);
  }

  /**
   * Get the parent block
   *
   * @return Parent block
   */
  public void setMotherBlock(IBlock block) {
    this.mother = block;
  }

  /**
   * Get the parent block
   *
   * @return Parent block
   */
  public IBlock getMotherBlock() {
    return this.mother;
  }
}
