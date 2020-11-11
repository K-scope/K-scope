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

import java.util.List;
import jp.riken.kscope.language.*;
import jp.riken.kscope.language.fortran.*;
import jp.riken.kscope.language.generic.*;

/**
 * Entry interface class. Search the database.
 *
 * @author RIKEN
 */
public interface ILanguageEntry {

  /**
   * Get the search history list.
   *
   * @return Search history list
   */
  public List<Object> getListVisit();

  /**
   * Set the search history list
   *
   * @param list Search history list
   */
  public void setListVisit(List<Object> list);

  /**
   * Get the Fortran database
   *
   * @return Fortran database
   */
  public Fortran getLanguage();

  /**
   * Set up the Fortran database.
   *
   * @param language Fortran database
   */
  public void setLanguage(Fortran language);

  /**
   * Explore modules.
   *
   * @param entry module
   */
  public void entry(Module entry);

  /**
   * Search for subroutines and functions.
   *
   * @param entry Subroutines, functions
   */
  public void entry(Procedure entry);

  /**
   * Search for variables
   *
   * @param entry variable
   */
  public void entry(Variable entry);

  /**
   * Search for structures.
   *
   * @param entry structure
   */
  public void entry(Type entry);

  /**
   * Search for structures.
   *
   * @param entry structure
   */
  public void entry(Structure entry);

  /**
   * Search for BREAK statements.
   *
   * @param entry BREAK statement
   */
  public void entry(Break entry);

  /**
   * Search for COMMON statements.
   *
   * @param entry COMMAND statement
   */
  public void entry(Common entry);

  /**
   * Search for conditional expressions
   *
   * @param entry Conditional expression
   */
  public void entry(Condition entry);

  /**
   * Search for CONTINUE statements
   *
   * @param entry CONTINUE statement
   */
  public void entry(Continue entry);

  /**
   * Search for DATA statements.
   *
   * @param entry DATA statement
   */
  public void entry(Data entry);

  /**
   * Search for DIRECTIVE statements.
   *
   * @param entry DIRECTIVE statement
   */
  public void entry(Directive entry);
  /**
   * D Search for a control statement that does nothing.
   *
   * @param entry Control statement that does nothing
   */
  public void entry(DoNothing entry);

  /**
   * Search for ALLOCATE statements.
   *
   * @param entry ALLOCATE statement
   */
  public void entry(DynamicAllocation entry);

  /**
   * Search for DEALLOCATE statements.
   *
   * @param entry DEALLOCATE statement
   */
  public void entry(DynamicDeallocation entry);

  /**
   * Search for NULLIFY statements.
   *
   * @param entry NULLIFY statement
   */
  public void entry(DynamicNullification entry);

  /**
   * Search for EQUIVALENCE statements.
   *
   * @param entry EQUIVALENCE statement
   */
  public void entry(Equivalence entry);

  /**
   * Search the body of subroutines and functions.
   *
   * @param entry Subroutine, function body
   */
  public void entry(ExecutableBody entry);

  /**
   * Search for GOTO statements.
   *
   * @param entry GOTO statement
   */
  public void entry(GoTo entry);

  /**
   * Search for PAUSE statements.
   *
   * @param entry PAUSE statement
   */
  public void entry(Pause entry);

  /**
   * Search for CALL statements and function calls.
   *
   * @param entry CALL statement, function call
   */
  public void entry(ProcedureUsage entry);

  /**
   * Search for generic functions (interface statement).
   *
   * @param entry Generic function group (interface statement)
   */
  public void entry(Procedures entry);

  /**
   * Search for DO, WHILE statements.
   *
   * @param entry DO, WHILE statement
   */
  public void entry(Repetition entry);

  /**
   * Search for RETURN statements
   *
   * @param entry RETURN statement
   */
  public void entry(Return entry);

  /**
   * Search for SELECT statements.
   *
   * @param entry SELECT statement
   */
  public void entry(Selection entry);

  /**
   * Search for assignment statements.
   *
   * @param entry assignment statement
   */
  public void entry(Substitution entry);

  /**
   * Search for a STOP statement.
   *
   * @param entry STOP statement
   */
  public void entry(Termination entry);

  /**
   * Search for user-defined processing blocks.
   *
   * @param entry User-defined processing block
   */
  public void entry(UserDefined entry);

  /**
   * Search for USE statements.
   *
   * @param entry USE statement
   */
  public void entry(UseState entry);

  /**
   * Search for MODULE PROCEDURE statements.
   *
   * @param entry MODULE PROCEDURE statement
   */
  public void entry(ProcedureWithNameOnly entry);

  /**
   * Search for variable / structure declaration statements.
   *
   * @param entry Variable / structure declaration statement
   */
  public void entry(VariableDefinition entry);

  /**
   * Search for variable attributes.
   *
   * @param entry Variable attribute
   */
  public void entry(VariableAttribute entry);

  /**
   * Search for array subscripts in variable declarations.
   *
   * @param entry Array subscript of variable declaration
   */
  public void entry(VariableDimension entry);

  /**
   * Search for array subscripts of variables.
   *
   * @param entry Variable array subscript
   */
  public void entry(DimensionIndex entry);

  /**
   * Search for expressions.
   *
   * @param entry expression
   */
  public void entry(Expression entry);

  /**
   * Search for generic functions.
   *
   * @param entry Generic function
   */
  public void entry(ProcedureItem entry);

  /**
   * Search for variable data types.
   *
   * @param entry Variable data type
   */
  public void entry(VariableType entry);

  /**
   * Search for UNION type data types.
   *
   * @param entry UNION type
   */
  public void entry(Union entry);
}
