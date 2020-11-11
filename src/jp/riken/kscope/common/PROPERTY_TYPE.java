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

/**
 * Property value type
 *
 * @author RIKEN
 */
public enum PROPERTY_TYPE {
  // Property value type
  /** Font settings */
  FONT("font"),
  /** Color setting */
  COLOR("color"),
  /** Integer value setting */
  INTEGER("integer"),
  /** Unknown */
  UNKNOWN("unknown");

  /** Type name */
  private String typename;

  /**
   * Constructor
   *
   * @param tabname Type name
   */
  private PROPERTY_TYPE(String type) {
    this.typename = type;
  }

  /**
   * Get the type name
   *
   * @return type name
   */
  public String getTypename() {
    return this.typename;
  }

  /**
   * Get property value type from type name
   *
   * @param type type name
   * @return Property value type
   */
  public static PROPERTY_TYPE parseType(String type) {
    if (type == null) return PROPERTY_TYPE.UNKNOWN;

    PROPERTY_TYPE types[] = PROPERTY_TYPE.values();
    for (int i = 0; i < types.length; i++) {
      if (types[i].getTypename().equalsIgnoreCase(type)) {
        return types[i];
      }
    }
    return PROPERTY_TYPE.UNKNOWN;
  }
}
