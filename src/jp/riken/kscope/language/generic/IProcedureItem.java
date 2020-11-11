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

import java.util.List;
import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.Procedure;

/**
 * Class corresponding to the generic definition of function names.
 *
 * @author RIKEN
 */
public interface IProcedureItem {

  /**
   * Whether the candidate function matches your information <br>
   * Method to check.
   *
   * @param target Candidate function
   * @param actualArguments List of actual arguments
   * @return true: Conforms false: Not compatible
   */
  boolean matches(Procedure target, List<Expression> actualArguments);

  /**
   * Returns the individual name of the procedure citation specification.
   *
   * @return Individual name of procedure citation specification
   */
  String getName();

  /**
   * A method to check whether the target argument list matches your information.
   *
   * @param arguments Actual argument list
   * @return true: Conforms false: Not compatible
   */
  boolean matches(List<Expression> arguments);
}
