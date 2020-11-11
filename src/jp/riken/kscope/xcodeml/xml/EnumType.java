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

package jp.riken.kscope.xcodeml.xml;

/**
 * Data type identifier
 *
 * @author RIKEN
 */
public enum EnumType {
  /** void type */
  VOID("Fvoid", null, false),
  /** integer type */
  INT("Fint", "INTEGER", true),
  /** real type */
  REAL("Freal", "REAL", true),
  /** complex type */
  COMPLEX("Fcomplex", "COMPLEX", true),
  /** logical type */
  LOGICAL("Flogical", "LOGICAL", true),
  /** character type */
  CHARACTER("Fcharacter", "CHARACTER", true),
  /** numeric type */
  NUMERIC("Fnumeric", null, true),
  /** numericAll type */
  NUMERICALL("FnumericAll", null, true),
  /** Unknown */
  DERIVED(null, null, false);

  /** Primitive type */
  private boolean _isPrimitive = false;
  /** XML element name */
  private String _xcodemlName;
  /** Fortran model name */
  private String _fortranName;

  /**
   * Constructor
   *
   * @param xcodemlName XML element name
   * @param fortranName Fortran type name
   * @param isPrimitive true = Primitive type
   */
  private EnumType(String xcodemlName, String fortranName, boolean isPrimitive) {
    _isPrimitive = isPrimitive;
    _xcodemlName = xcodemlName;
    _fortranName = fortranName;
  }

  /**
   * Get if it is a primitive type
   *
   * @return true = primitive type
   */
  public boolean isPrimitive() {
    return _isPrimitive;
  }

  /**
   * Get the XML element name
   *
   * @return XML element name
   */
  public String xcodemlName() {
    return _xcodemlName;
  }

  /**
   * Get a Fortran type name
   *
   * @return Fortran model name
   */
  public String fortranName() {
    return _fortranName;
  }

  /**
   * Get the data type identifier from the XML element name
   *
   * @param xcodemlTypeName XML element name
   * @return data type identifier
   */
  public static EnumType getTypeIdFromXcodemlTypeName(String xcodemlTypeName) {
    if (xcodemlTypeName == null) {
      throw new IllegalArgumentException();
    }

    for (EnumType type : EnumType.values()) {
      String workTypeName = type.xcodemlName();
      if (workTypeName != null) {
        if (xcodemlTypeName.compareToIgnoreCase(type.xcodemlName()) == 0) {
          return type;
        }
      }
    }
    return DERIVED;
  }
}
