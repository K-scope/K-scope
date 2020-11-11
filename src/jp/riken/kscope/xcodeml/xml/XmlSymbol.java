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
 * Symbol list class
 *
 * @author RIKEN
 */
public class XmlSymbol {
  /** Symbol name */
  private String _symbolName;
  /** Type ID */
  private EnumType _typeId;
  /** Derived name */
  private String _derivedName;

  /**
   * Constructor
   *
   * @param symbolName Symbol name
   */
  public XmlSymbol(String symbolName) {
    this(symbolName, EnumType.VOID, null);
  }

  /**
   * Constructor
   *
   * @param symbolName Symbol name
   * @param typeId Type ID
   */
  public XmlSymbol(String symbolName, EnumType typeId) {
    this(symbolName, typeId, null);
  }

  /**
   * Constructor
   *
   * @param symbolName Symbol name
   * @param typeId Type ID
   * @param derivedName Derived name
   */
  public XmlSymbol(String symbolName, EnumType typeId, String derivedName) {
    _symbolName = symbolName;
    _typeId = typeId;
    _derivedName = derivedName;
  }

  /**
   * Get type ID
   *
   * @return Type ID
   */
  public EnumType getTypeId() {
    return _typeId;
  }

  /**
   * Get the symbol name
   *
   * @return Symbol name
   */
  public String getSymbolName() {
    return _symbolName;
  }

  /**
   * Get the derived name
   *
   * @return Derived name
   */
  public String getDerivedName() {
    return _derivedName;
  }
}
