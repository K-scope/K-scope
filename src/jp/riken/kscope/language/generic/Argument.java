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
import jp.riken.kscope.language.IVariableAttribute;
import jp.riken.kscope.language.IVariableType;

/**
 * Class corresponding to the formal argument of the generic function.
 *
 * @author RIKEN
 */
public class Argument implements Serializable {
  /** Serial number */
  private static final long serialVersionUID = -5912521170672226755L;

  private IVariableType type;
  private IVariableAttribute attribute;
  private String name;

  /**
   * Constructor.
   *
   * @param nm name
   */
  public Argument(String nm) {
    this.name = nm;
  }

  /**
   * Constructor.
   *
   * @param typ Formal argument type
   * @param attrbt Formal argument attributes
   */
  public Argument(IVariableType typ, IVariableAttribute attrbt) {
    type = typ;
    attribute = attrbt;
  }

  /**
   * Set the formal argument type.
   *
   * @param tp Formal argument type
   */
  public void setType(IVariableType tp) {
    this.type = tp;
  }

  /**
   * Set the formal argument attribute.
   *
   * @param att Formal argument attributes
   */
  public void setVariableAttributes(IVariableAttribute att) {
    this.attribute = att;
  }

  /**
   * Get the type of formal argument.
   *
   * @return Formal argument type
   */
  public IVariableType getType() {
    return type;
  }

  /**
   * Get the attribute of the formal argument.
   *
   * @return Formal argument attributes
   */
  public IVariableAttribute getAttribute() {
    return attribute;
  }

  /**
   * Are the argument types and attributes matched?
   *
   * @param typ Target argument type
   * @param attrbt Target attributes
   * @return true: Conforms. <br>
   *     false: Not compatible.
   */
  public boolean matches(IVariableType typ, IVariableAttribute attrbt) {
    if (typ == null || attrbt == null || this.type == null || this.attribute == null) {
      return false;
    }
    return (this.type.matches(typ) && this.attribute.matches(attrbt));
  }

  /**
   * Get the argument name.
   *
   * @return Argument name
   */
  public String getName() {
    return this.name;
  }
}
