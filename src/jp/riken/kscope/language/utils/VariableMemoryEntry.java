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

package jp.riken.kscope.language.utils;

import java.util.ArrayList;
import java.util.List;
import jp.riken.kscope.language.*;
import jp.riken.kscope.language.fortran.*;
import jp.riken.kscope.language.generic.*;

/**
 * Access destination memory setting variable search class. Get the access destination memory
 * setting variable.
 *
 * @author RIKEN
 */
public class VariableMemoryEntry implements ILanguageEntry {
  /** Access memory setting variable */
  private List<Variable> listVariable;
  /** Fortran database */
  private Fortran language;

  /**
   * Constructor
   *
   * @param Fortran database
   */
  public VariableMemoryEntry(Fortran language) {
    this.language = language;
    this.listVariable = new ArrayList<Variable>();
  }

  /** Add the access destination memory setting variable. */
  @Override
  public void entry(Variable entry) {
    if (entry == null) return;
    if (entry.getMemoryType() == null) return;
    // Add the access destination memory setting variable.
    addVariable(entry);
    return;
  }

  @Override
  public void entry(Module entry) {}

  @Override
  public void entry(Procedure entry) {}

  @Override
  public void entry(ProcedureUsage entry) {}

  @Override
  public void entry(VariableDefinition entry) {}

  @Override
  public void entry(VariableType entry) {}

  @Override
  public void entry(Break entry) {}

  @Override
  public void entry(Common entry) {}

  @Override
  public void entry(Condition entry) {}

  @Override
  public void entry(Continue entry) {}

  @Override
  public void entry(Data entry) {}

  @Override
  public void entry(Directive entry) {}

  @Override
  public void entry(DoNothing entry) {}

  @Override
  public void entry(DynamicAllocation entry) {}

  @Override
  public void entry(DynamicDeallocation entry) {}

  @Override
  public void entry(DynamicNullification entry) {}

  @Override
  public void entry(Equivalence entry) {}

  @Override
  public void entry(ExecutableBody entry) {}

  @Override
  public void entry(GoTo entry) {}

  @Override
  public void entry(Pause entry) {}

  @Override
  public void entry(Procedures entry) {}

  @Override
  public void entry(Repetition entry) {}

  @Override
  public void entry(Return entry) {}

  @Override
  public void entry(Selection entry) {}

  @Override
  public void entry(Substitution entry) {}

  @Override
  public void entry(Termination entry) {}

  @Override
  public void entry(UserDefined entry) {}

  @Override
  public void entry(UseState entry) {}

  @Override
  public void entry(ProcedureWithNameOnly entry) {}

  @Override
  public void entry(VariableAttribute entry) {}

  @Override
  public void entry(VariableDimension entry) {}

  @Override
  public void entry(DimensionIndex entry) {}

  @Override
  public void entry(Expression entry) {}

  @Override
  public void entry(ProcedureItem entry) {}

  @Override
  public void entry(Type entry) {}

  @Override
  public void entry(Structure entry) {}

  @Override
  public void entry(Union entry) {}

  @Override
  public List<Object> getListVisit() {
    return null;
  }

  @Override
  public void setListVisit(List<Object> list) {}

  @Override
  public Fortran getLanguage() {
    return this.language;
  }

  @Override
  public void setLanguage(Fortran language) {
    this.language = language;
  }

  /**
   * Get the access destination memory setting variable.
   *
   * @return Access destination memory setting variable list
   */
  public Variable[] getListVariable() {
    if (this.listVariable == null || this.listVariable.size() <= 0) {
      return null;
    }
    return listVariable.toArray(new Variable[0]);
  }

  /**
   * Add the access destination memory setting variable.
   *
   * @param var Access destination memory setting variable
   */
  private void addVariable(Variable var) {
    if (this.listVariable.contains(var)) return;
    this.listVariable.add(var);
  }
}
