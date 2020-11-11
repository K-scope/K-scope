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

package jp.riken.kscope.language;

/**
 * Interface showing data type. <br>
 * VariableType (enum type) corresponding to each language implements and uses this interface.
 *
 * @author RIKEN
 */
public interface IVariableType {

  /**
   * Get the type name.
   *
   * @return type name
   */
  String getName();

  /**
   * Search for VariableType by type name.
   *
   * @param name Model name
   * @return VariableType corresponding to the type name
   */
  IVariableType findTypeBy(String name);

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
  boolean matches(IVariableType value);

  /**
   * Check if it is a real variable.
   *
   * @return true = real number
   */
  boolean isRealType();

  /**
   * Check if it is an integer variable.
   *
   * @return true = integer
   */
  boolean isIntegerType();
}
