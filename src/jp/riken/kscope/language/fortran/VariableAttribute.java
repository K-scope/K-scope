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
import java.util.HashSet;
import java.util.Set;

/**
 * A class that shows the attributes of variables. <br>
 *
 * @author RIKEN
 */
public class VariableAttribute
    implements Serializable, jp.riken.kscope.language.IVariableAttribute {
  /** Serial number */
  private static final long serialVersionUID = 3279154150773399072L;

  /** Variable scope attribute (public / private / [unspecified]). */
  public enum ScopeAttribute {
    /** Not specified. */
    NONE,
    /** public. */
    PUBLIC,
    /** private. */
    PRIVATE,
  }

  /** Variable pointer (or target) attribute (pointer / target / [unspecified]). */
  public enum PointerAttribute {
    /** Not specified. */
    NONE,
    /** pointer. */
    POINTER,
    /** target. */
    TARGET,
  }

  /** Function position attribute consisting of external, intrinsic, and unspecified ([none]). */
  public enum FunctionPositionAttribute {
    /** Not specified. */
    NONE,
    /** External. */
    EXTERNAL,
    /** Interior. */
    INTRINSIC,
  }

  /** intent attribute. */
  public enum IntentAttribute {
    /** Not specified. */
    NONE,
    /** Input. */
    IN,
    /** Output. */
    OUT,
    /** I / O. */
    INOUT,
  }

  /** Information on all attributes. */
  private Set<String> attributes = new HashSet<String>();

  /** Default constructor. */
  public VariableAttribute() {
    // do nothing
  }

  /**
   * Constructor.
   *
   * @param attrbts List of all attributes
   */
  public VariableAttribute(Set<String> attrbts) {
    this.setAttributes(attrbts);
  }

  /**
   * Whether it has a parameter attribute.
   *
   * @return true: with parameter attribute false: Does not have parameter attribute
   */
  public boolean hasParameter() {
    return this.contains("parameter");
  }

  /**
   * Get the scope attribute (public / private / [unspecified]).
   *
   * @return NONE: Not specified PUBLIC: with public attribute PRIVATE: with private attribute
   */
  public ScopeAttribute getScope() {
    ScopeAttribute result = ScopeAttribute.NONE;
    if (this.contains("public")) {
      result = ScopeAttribute.PUBLIC;
    } else if (this.contains("private")) {
      result = ScopeAttribute.PRIVATE;
    }
    return result;
  }

  /**
   * Whether it has an optional attribute.
   *
   * @return true: with optional attribute false: Does not have optional attribute
   */
  public boolean hasOptional() {
    return this.contains("optional");
  }

  /**
   * Get the pointer (or target) attribute of a variable.
   *
   * @return NONE: Not specified POINTER: with pointer attribute TARGET: with target attribute
   */
  public PointerAttribute getPointerOrTarget() {
    PointerAttribute result = PointerAttribute.NONE;
    if (this.contains("pointer")) {
      result = PointerAttribute.POINTER;
    } else if (this.contains("private")) {
      result = PointerAttribute.TARGET;
    }
    return result;
  }

  /**
   * Whether it has a save attribute.
   *
   * @return true: with save attribute false: Does not have save attribute
   */
  public boolean hasSave() {
    return this.contains("save");
  }

  /**
   * Get the position attribute of the function.
   *
   * @return NONE: Not specified EXTERNAL: Has an external attribute INTRINSIC: Has intrinsic
   *     attribute
   */
  public FunctionPositionAttribute getFunctionPosition() {
    FunctionPositionAttribute result = FunctionPositionAttribute.NONE;
    if (this.contains("external")) {
      result = FunctionPositionAttribute.EXTERNAL;
    } else if (this.contains("intrinsic")) {
      result = FunctionPositionAttribute.INTRINSIC;
    }
    return result;
  }

  /**
   * Whether it has an equivalence attribute.
   *
   * @return true: with equivalence attribute false: Does not have equivalence attribute
   */
  public boolean hasEquivalence() {
    return this.contains("equivalence");
  }

  /**
   * Whether it has a common attribute.
   *
   * @return true: with common attribute false: Does not have common attribute
   */
  public boolean hasCommon() {
    return this.contains("common");
  }

  /**
   * Whether it has a dimension attribute.
   *
   * @return true: with dimension attribute false: Does not have dimension attribute
   */
  public boolean hasDimension() {
    return this.contains("dimension");
  }

  /**
   * Get the intent attribute of a variable.
   *
   * @return NONE: Not specified IN: Has intent (in) attribute OUT: Has an intent (out) attribute
   *     INOUT: with intent (inout) attribute
   */
  public IntentAttribute getIntent() {
    IntentAttribute result = IntentAttribute.NONE;
    String intentItem = this.getAttributeBy("intent");
    if (intentItem != "") {
      if (intentItem.toLowerCase().contains("inout")) {
        result = IntentAttribute.INOUT;
      } else if (intentItem.toLowerCase().contains("out")) {
        result = IntentAttribute.OUT;
      } else if (intentItem.toLowerCase().contains("in")) {
        result = IntentAttribute.IN;
      }
    }
    return result;
  }

  /**
   * Whether it has the allocatable attribute.
   *
   * @return true: with allocatable attribute false: Does not have allocatable attribute
   */
  public boolean hasAllocatable() {
    return this.contains("allocatable");
  }

  /**
   * Attribute settings.
   *
   * @param attrbts All attributes to set
   */
  @Override
  public void setAttributes(Set<String> attrbts) {
    if (attrbts != null) {
      attributes = attrbts;
    }
  }

  /**
   * Attribute settings.
   *
   * @param attrbts All attributes to set
   */
  @Override
  public void setAttributes(String[] attrbts) {
    if (attrbts != null) {
      attributes.clear();
      for (String attribute : attrbts) {
        attributes.add(attribute);
      }
    }
  }

  /**
   * Get attributes.
   *
   * @return All attributes
   */
  @Override
  public Set<String> getAttributes() {
    return attributes;
  }

  /**
   * Add attributes.
   *
   * @param attrbt Attributes to add
   */
  @Override
  public void addAttribute(String attrbt) {
    if (attrbt == null || attrbt.isEmpty()) {
      return;
    }
    attributes.add(attrbt);
  }

  /**
   * Returns the attributes of the variable.
   *
   * @return Variable attribute string
   */
  @Override
  public String toString() {
    String result = "";

    for (String attrbt : attributes) {
      result = result + ", " + attrbt;
    }
    result = result.trim();

    /* Remove the leading "," */
    if (result.startsWith(",")) {
      result = result.substring(1);
      result = result.trim();
    }

    return result;
  }

  /**
   * Whether the target string is included in the attribute. <br>
   * However, the case of the target character string is ignored. <br>
   *
   * @param keyword Target string
   * @return true: The target string is included
   */
  @Override
  public boolean contains(String keyword) {
    return (this.getAttributeBy(keyword) != "");
  }

  /**
   * Get attributes that include the target string (ignoring uppercase and lowercase letters).
   *
   * @param keyword Target string
   * @return First attribute found <br>
   *     If not found, returns an empty string.
   */
  public String getAttributeBy(String keyword) {
    String result = "";
    for (String item : attributes) {
      // Compare in all lowercase
      if (item.toLowerCase().contains(keyword.toLowerCase())) {
        result = item;
        break;
      }
    }
    return result;
  }
  /**
   * Get the number of dimensions.
   *
   * @return Number of dimensions
   */
  public int getDimensionNum() {
    int result = 1;
    String dimensionAttr = getAttributeBy("dimension");
    if (!dimensionAttr.equals("")) {
      result = dimensionAttr.split(",").length;
    }
    return result;
  }

  /**
   * Whether the attributes match. <br>
   * When searching for the corresponding function from the overloaded function group, <br>
   * It is necessary to check the attributes of formal and actual arguments. <br>
   * "Matching" is judged to be the same attribute by this attribute check. Means a thing. In the
   * case of fortran, only the number of dimensions is checked.
   *
   * @param value Attributes
   * @return true: Conforms <br>
   *     false: Not compatible
   */
  @Override
  public boolean matches(jp.riken.kscope.language.IVariableAttribute value) {

    if (!(value instanceof VariableAttribute)) {
      return false;
    }
    VariableAttribute target = (VariableAttribute) value;

    return (this.getDimensionNum() == target.getDimensionNum());
  }
}
