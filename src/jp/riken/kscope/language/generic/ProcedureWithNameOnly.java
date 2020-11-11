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

import java.io.Serializable;
import java.util.List;
import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.Procedure;

/**
 * Generic function class with only function names. <br>
 * Supports Fortran module procedure. <br>
 *
 * @author RIKEN
 */
public class ProcedureWithNameOnly
    implements Serializable, jp.riken.kscope.language.generic.IProcedureItem {
  /** Serial number */
  private static final long serialVersionUID = 682980844988371714L;

  private String name;
  private Procedure definition;

  /**
   * Constructor.
   *
   * @param nm Function name
   */
  public ProcedureWithNameOnly(String nm) {
    this.name = nm;
  }

  @Override
  public String toString() {
    return "module procedure : " + this.name;
  }
  /**
   * Whether the candidate function matches your information <br>
   * Method to check. Candidate function name & Candidate function <br>
   * Returns true if the argument list and the actual argument list match. <br>
   *
   * @param target Candidate function
   * @param actualArguments List of actual arguments
   * @return true: Conforms false: Not compatible
   */
  @Override
  public boolean matches(Procedure target, List<Expression> actualArguments) {
    if (target == null || actualArguments == null) {
      return false;
    }
    if (!(target.get_name().equalsIgnoreCase(this.name))) {
      return false;
    }
    if (!target.matches(actualArguments)) {
      return false;
    }
    return true;
  }
  /**
   * Whether the target actual argument matches your information <br>
   * Method to check. <br>
   * Returns true if the argument list and the actual argument list match. <br>
   *
   * @param actualArguments List of actual arguments
   * @return true: Conforms false: Not compatible
   */
  @Override
  public boolean matches(List<Expression> actualArguments) {
    if (this.definition == null || actualArguments == null) {
      return false;
    }
    if (!(this.definition.get_name().equalsIgnoreCase(this.name))) {
      return false;
    }
    if (!this.definition.matches(actualArguments)) {
      return false;
    }
    return true;
  }

  /**
   * Get the function name.
   *
   * @return function name
   */
  public String getName() {
    return this.name;
  }

  /**
   * Set the corresponding procedure declaration.
   *
   * @param proc
   */
  public void setDeclaration(Procedure proc) {
    this.definition = proc;
  }
  /**
   * Returns the corresponding procedure declaration.
   *
   * @return Declaration of procedure. If not, it returns null.
   */
  public Procedure getDeclaration() {
    return this.definition;
  }
}
