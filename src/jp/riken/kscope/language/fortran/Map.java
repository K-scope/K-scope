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
import jp.riken.kscope.language.VariableDefinition;

/**
 * Map type class.
 *
 * @author RIKEN
 */
public class Map implements Serializable {
  /** Serial number */
  private static final long serialVersionUID = -6468256404696838753L;
  /** Since it is almost the same as the structure type, it is used by including the structure. */
  private Structure core = new Structure();

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
   * Get a list of variable definition statements in the structure.
   *
   * @return Variable definition statement list
   */
  public List<VariableDefinition> getDefinitions() {
    return core.getDefinitions();
  }
}
