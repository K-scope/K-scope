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
 * Java class for defChoiceIntent.
 *
 * <p>The following schema fragment specifies the expected content contained within this class.
 *
 * <p>
 *
 * <pre>
 * &lt;simpleType name="defChoiceIntent">
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}token">
 *     &lt;enumeration value="in"/>
 *     &lt;enumeration value="out"/>
 *     &lt;enumeration value="inout"/>
 *   &lt;/restriction>
 * &lt;/simpleType>
 * </pre>
 */
@XmlType(name = "defChoiceIntent")
@XmlEnum
public enum DefChoiceIntent {

  /** intent(in) */
  @XmlEnumValue("in")
  IN("in"),
  /** intent(out) */
  @XmlEnumValue("out")
  OUT("out"),
  /** intent(inout) */
  @XmlEnumValue("inout")
  INOUT("inout");

  /** intent statement */
  private final String value;

  /**
   * Constructor
   *
   * @param v intent statement
   */
  DefChoiceIntent(String v) {
    value = v;
  }

  /**
   * Get the intent statement
   *
   * @return intent statement
   */
  public String value() {
    return value;
  }

  /**
   * Get the Intent identifier from the intent statement
   *
   * @param v intent statement
   * @return Intent identifier
   */
  public static DefChoiceIntent fromValue(String v) {
    for (DefChoiceIntent c : DefChoiceIntent.values()) {
      if (c.value.equals(v)) {
        return c;
      }
    }
    throw new IllegalArgumentException(v);
  }
}
