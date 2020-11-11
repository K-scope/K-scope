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
import jp.riken.kscope.language.IVariableType;

/**
 * Class corresponding to the generic function.
 *
 * @author RIKEN
 */
public class ProcedureItem
    implements Serializable, jp.riken.kscope.language.generic.IProcedureItem {
  /** Serial number */
  private static final long serialVersionUID = -3790802001452994733L;

  private String name;
  private IVariableType type;
  private Arguments arguments;
  private String result;

  /**
   * Constructor.
   *
   * @param nm Function name
   * @param typ Function type
   */
  public ProcedureItem(String nm, IVariableType typ) {
    this(nm, typ, new Arguments());
  }

  /**
   * Constructor.
   *
   * @param nm Function name
   * @param typ Function type
   * @param argmnts Formal argument list
   */
  public ProcedureItem(String nm, IVariableType typ, Arguments argmnts) {
    this.name = nm;
    this.type = typ;
    if (argmnts == null) {
      this.arguments = new Arguments();
    } else {
      this.arguments = argmnts;
    }
  }

  @Override
  public String toString() {
    return "interface : " + this.name;
  }
  /**
   * Whether the candidate function matches your information <br>
   * Method to check. The name of the candidate function and the list of actual arguments are <br>
   * Returns true if it matches.
   *
   * @param target Candidate function
   * @param actualArguments List of actual arguments
   * @return true: Conforms false: Not compatible
   */
  @Override
  public boolean matches(
      jp.riken.kscope.language.Procedure target, List<Expression> actualArguments) {
    if (target == null || actualArguments == null) {
      return false;
    }
    if (target.get_name() != this.name) {
      return false;
    }
    if (!this.arguments.isSameArguments(actualArguments)) {
      return false;
    }
    return true;
  }

  /**
   * Whether the target argument list matches your information <br>
   * Method to check. Returns true if the actual argument list matches.
   *
   * @param actualArguments List of actual arguments
   * @return true: Conforms false: Not compatible
   */
  @Override
  public boolean matches(List<Expression> actualArguments) {
    if (this.arguments == null || actualArguments == null) {
      return false;
    }
    if (!this.arguments.isSameArguments(actualArguments)) {
      return false;
    }
    return true;
  }

  /**
   * Add a formal argument.
   *
   * @param value Formal parameters to add
   */
  public void addArgument(Argument value) {
    this.arguments.add(value);
  }

  /**
   * Get the function name.
   *
   * @return function name
   */
  @Override
  public String getName() {
    return this.name;
  }

  /**
   * Get function type.
   *
   * @return function type
   */
  public IVariableType getType() {
    return this.type;
  }

  /**
   * Get the formal argument list.
   *
   * @return Formal argument list
   */
  public Arguments getArguments() {
    return this.arguments;
  }

  /**
   * Setting the formal argument list.
   *
   * @param values Formal argument list
   */
  public void setArguments(Arguments values) {
    this.arguments = values;
  }

  /**
   * Set the data type of the variable.
   *
   * @param tp
   */
  public void setVariableType(IVariableType tp) {
    this.type = tp;
  }

  /**
   * Returns the data type.
   *
   * @return data type
   */
  public IVariableType getVariableType() {
    return this.type;
  }
  /**
   * Returns the variable name that results in the case of a function.
   *
   * @return variable name
   */
  public String getResult() {
    return result;
  }

  /**
   * Set the variable name that will be the result for the function.
   *
   * @param res Result variable name
   */
  public void setResult(String res) {
    this.result = res;
  }
}
