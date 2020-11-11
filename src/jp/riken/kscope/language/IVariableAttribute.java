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

import java.util.Set;

/**
 * Interface showing variable attributes (qualifiers). <br>
 * VariableAttribute class corresponding to each language implements and uses this interface.
 *
 * @author RIKEN
 */
public interface IVariableAttribute {

  /**
   * Attribute settings.
   *
   * @param attrbts All attributes to set
   */
  void setAttributes(Set<String> attrbts);

  /**
   * Attribute settings.
   *
   * @param attrbts All attributes to set
   */
  void setAttributes(String[] attrbts);

  /**
   * Get attributes.
   *
   * @return All attributes
   */
  Set<String> getAttributes();

  /**
   * Add attributes.
   *
   * @param attrbt Attributes to add
   */
  void addAttribute(String attrbt);

  /**
   * Whether the target string is included in the attribute. <br>
   * However, the case of the target character string is ignored. <br>
   *
   * @param keyword Target string
   * @return true: The target string is included
   */
  boolean contains(String keyword);

  /**
   * Returns a variable attribute (qualifier) string.
   *
   * @return attribute (modifier) string
   */
  String toString();

  /**
   * Whether the attributes match. <br>
   * When searching for the corresponding function from the overloaded function group, <br>
   * It is necessary to check the attributes of formal and actual arguments. <br>
   * "Matching" is judged to be the same attribute by this attribute check. Means a thing.
   *
   * @param value Attributes
   * @return true: Conforms <br>
   *     false: Not compatible
   */
  boolean matches(IVariableAttribute value);
}
