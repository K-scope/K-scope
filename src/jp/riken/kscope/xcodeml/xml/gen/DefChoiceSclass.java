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
 * Java class for defChoiceSclass.
 *
 * <p>The following schema fragment specifies the expected content contained within this class.
 *
 * <p>
 *
 * <pre>
 * &lt;simpleType name="defChoiceSclass">
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}token">
 *     &lt;enumeration value="flocal"/>
 *     &lt;enumeration value="fsave"/>
 *     &lt;enumeration value="fcommon"/>
 *     &lt;enumeration value="fparam"/>
 *     &lt;enumeration value="ffunc"/>
 *     &lt;enumeration value="ftype_name"/>
 *     &lt;enumeration value="fcommon_name"/>
 *     &lt;enumeration value="fnamelist_name"/>
 *   &lt;/restriction>
 * &lt;/simpleType>
 * </pre>
 */
@XmlType(name = "defChoiceSclass")
@XmlEnum
public enum DefChoiceSclass {
  /** flocal */
  @XmlEnumValue("flocal")
  FLOCAL("flocal"),
  /** fsave */
  @XmlEnumValue("fsave")
  FSAVE("fsave"),
  /** fcommon */
  @XmlEnumValue("fcommon")
  FCOMMON("fcommon"),
  /** fparam */
  @XmlEnumValue("fparam")
  FPARAM("fparam"),
  /** ffunc */
  @XmlEnumValue("ffunc")
  FFUNC("ffunc"),
  /** ftype_name */
  @XmlEnumValue("ftype_name")
  FTYPE_NAME("ftype_name"),
  /** fcommon_name */
  @XmlEnumValue("fcommon_name")
  FCOMMON_NAME("fcommon_name"),
  /** fnamelist_name */
  @XmlEnumValue("fnamelist_name")
  FNAMELIST_NAME("fnamelist_name");

  /** Identification name */
  private final String value;

  /**
   * Constructor
   *
   * @param v Distinguished name
   */
  DefChoiceSclass(String v) {
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
   * Get the DefChoiceSclass class from the distinguished name
   *
   * @param v Distinguished name
   * @return DefChoiceSclass
   */
  public static DefChoiceSclass fromValue(String v) {
    for (DefChoiceSclass c : DefChoiceSclass.values()) {
      if (c.value.equals(v)) {
        return c;
      }
    }
    throw new IllegalArgumentException(v);
  }
}
