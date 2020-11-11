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
package jp.riken.kscope.common;

import jp.riken.kscope.language.Variable;
import jp.riken.kscope.language.VariableDefinition;

/**
 * Access memory type
 *
 * @author RIKEN
 */
public enum ACCESSMEMORY_TYPE {
  /** Memory */
  MEMORY("Memory", "Mem", "memory"),
  /** L1 Cache */
  L1_CACHE("L1 Cache", "L1", "l1_cache"),
  /** L2 Cache */
  L2_CACHE("L2 Cache", "L2", "l2_cache"),
  /** Register */
  REGISTER("Register", "Reg", "register"),
  /** CUSTOM settings */
  CUSTOM("Custom", "Custom", "custom"),
  /** Default setting */
  DEFAULT("Default", "Default", "default");

  /** Access name */
  private String name;
  /** Short name of access destination */
  private String shortname;
  /** Property file key name */
  private String key;

  /**
   * Constructor
   *
   * @param name Access name
   * @param shortname Access short name
   */
  private ACCESSMEMORY_TYPE(String name, String shortname, String key) {
    this.name = name;
    this.shortname = shortname;
    this.key = key;
  }

  /**
   * Get the access name
   *
   * @return name Access name
   */
  public String getName() {
    return name;
  }

  /**
   * Get the access destination short name
   *
   * @return name Access short name
   */
  public String getShortname() {
    return this.shortname;
  }

  /**
   * Get the property file key name
   *
   * @return name Property file key name
   */
  public String getKey() {
    return this.key;
  }

  /**
   * Get the access destination memory name list
   *
   * @return Access memory name list
   */
  public static String[] getAccessMemoryList() {
    ACCESSMEMORY_TYPE types[] = ACCESSMEMORY_TYPE.values();
    String list[] = new String[types.length];
    for (int i = 0; i < types.length; i++) {
      list[i] = types[i].getName();
    }
    return list;
  }

  /**
   * Get the default access destination memory by variable.
   *
   * @param def variable
   * @return Default access memory
   */
  public static ACCESSMEMORY_TYPE getDefaultType() {
    return MEMORY;
  }

  /**
   * Get the default access destination memory by variable.
   *
   * @param def variable
   * @return Default access memory
   */
  public static ACCESSMEMORY_TYPE getDefaultType(Variable var) {
    if (var == null) return getDefaultType();
    if (var.getDefinition() == null) return getDefaultType();
    return getDefaultType(var.getDefinition());
  }

  /**
   * Get the default access destination memory by variable type.
   *
   * @param def Variable definition
   * @return Default access memory
   */
  public static ACCESSMEMORY_TYPE getDefaultType(VariableDefinition def) {
    if (def == null) return getDefaultType();
    if (def.getVariableType() == null) return getDefaultType();
    // scaler variable is a register
    if (def.get_dimension_size() <= 0) return REGISTER;
    if (def.getVariableType().isIntegerType()) return MEMORY;
    if (def.getVariableType().isRealType()) return MEMORY;

    return getDefaultType();
  }

  /**
   * Get ACCESSMEMORY_TYPE from the string. Judge from enum name, access destination name, access
   * destination short name, property file key name.
   *
   * @param value ACCESSMEMORY_TYPE string
   * @return ACCESSMEMORY_TYPE
   */
  public static ACCESSMEMORY_TYPE getAccessMemoryType(String value) {
    if (value == null) return null;
    ACCESSMEMORY_TYPE type = null;
    try {
      type = ACCESSMEMORY_TYPE.valueOf(value.toUpperCase());
    } catch (Exception ex) {
    }
    if (type != null) {
      return type;
    }
    ACCESSMEMORY_TYPE types[] = ACCESSMEMORY_TYPE.values();
    for (ACCESSMEMORY_TYPE access : types) {
      if (value.equalsIgnoreCase(access.getName())) return access;
      if (value.equalsIgnoreCase(access.getShortname())) return access;
      if (value.equalsIgnoreCase(access.getKey())) return access;
    }
    return null;
  }
}
