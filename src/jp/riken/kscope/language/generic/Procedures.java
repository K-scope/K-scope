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

package jp.riken.kscope.language.generic;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import jp.riken.kscope.language.Block;
import jp.riken.kscope.language.BlockType;
import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.Procedure;

/**
 * Class corresponding to the generic function group. <br>
 * Supports Fortran interface statement. <br>
 *
 * @author RIKEN
 */
public class Procedures extends Block {
  /** Serial number */
  private static final long serialVersionUID = 1784597524248458672L;
  /** Name of generic procedure */
  private String name;
  /** Generic definition of function name */
  // modify by @hira at 2013/02/01 Change to LinkedHashSet that maintains the insertion order
  // private Set<IProcedureItem> procedures = new HashSet<IProcedureItem>();
  private Set<IProcedureItem> procedures = new LinkedHashSet<IProcedureItem>();

  /** Constructor. <br> */
  public Procedures() {
    super();
  }
  /**
   * Constructor. <br>
   *
   * @param nm name
   */
  public Procedures(String nm) {
    super();
    this.name = nm;
  }

  /**
   * Get block type.
   *
   * @return BlockType.PROCEDURES
   */
  public BlockType getBlockType() {
    return BlockType.PROCEDURES;
  }

  public String toString() {
    String nm = "";
    if (this.name != null) {
      nm = " " + this.name;
    }
    return "interface" + nm;
  }
  /**
   * Find the real function from the candidate real function group based on the real argument list.
   * <br>
   *
   * @param actualProcedures Candidate real function group
   * @param actualArguments List of actual arguments
   * @return The real function found. If not found, returns null.
   */
  public Procedure findActualProcedureFrom(
      Set<Procedure> actualProcedures, List<Expression> actualArguments) {
    if (actualProcedures == null || actualArguments == null) {
      return null;
    }

    Procedure result = null;

    for (IProcedureItem item : procedures) {
      for (Procedure target : actualProcedures) {
        if (item.matches(target, actualArguments)) {
          result = target;
          break;
        }
      }
      if (result != null) {
        break;
      }
    }

    return result;
  }

  /**
   * Get the name. <br>
   *
   * @return name
   */
  public String getName() {
    return this.name;
  }

  /**
   * Acquisition of generic functions. <br>
   *
   * @return Generic function group. If not, returns an empty set.
   */
  public Set<IProcedureItem> getProcedures() {
    return this.procedures;
  }

  /**
   * Addition of generic function. <br>
   *
   * @param procedure Generic function to add
   */
  public void add(IProcedureItem procedure) {
    if (procedure == null) {
      return;
    }
    this.procedures.add(procedure);
  }
  /**
   * Returns the name of the corresponding procedural specification based on the given argument
   * list.
   *
   * @param arguments Actual argument list
   * @return The name of the procedure citation specification. If not, the generic name is returned.
   */
  public String getActualCallName(List<Expression> arguments) {
    if (arguments == null) {
      return this.name;
    }

    for (IProcedureItem item : this.procedures) {
      if (item.matches(arguments)) {
        return item.getName();
      }
    }
    return this.name;
  }
}
