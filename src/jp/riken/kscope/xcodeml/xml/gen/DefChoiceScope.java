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

package jp.riken.kscope.xcodeml.xml.gen;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import javax.xml.bind.annotation.XmlType;

/**
 * Java class for defChoiceScope.
 *
 * <p>The following schema fragment specifies the expected content contained within this class.
 *
 * <p>
 *
 * <pre>
 * &lt;simpleType name="defChoiceScope">
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}token">
 *     &lt;enumeration value="local"/>
 *     &lt;enumeration value="global"/>
 *     &lt;enumeration value="param"/>
 *   &lt;/restriction>
 * &lt;/simpleType>
 * </pre>
 */
@XmlType(name = "defChoiceScope")
@XmlEnum
public enum DefChoiceScope {

  /** local */
  @XmlEnumValue("local")
  LOCAL("local"),
  /** global */
  @XmlEnumValue("global")
  GLOBAL("global"),
  /** param */
  @XmlEnumValue("param")
  PARAM("param");

  /** Identification name */
  private final String value;

  /**
   * Constructor
   *
   * @param v Distinguished name
   */
  DefChoiceScope(String v) {
    value = v;
  }

  /**
   * Get the distinguished name
   *
   * @return Distinguished name
   */
  public String value() {
    return value;
  }

  /**
   * Get the DefChoiceScope class from the distinguished name
   *
   * @param v Distinguished name
   * @return DefChoiceSclass
   */
  public static DefChoiceScope fromValue(String v) {
    for (DefChoiceScope c : DefChoiceScope.values()) {
      if (c.value.equals(v)) {
        return c;
      }
    }
    throw new IllegalArgumentException(v);
  }
}
