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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import jp.riken.kscope.xcodeml.xml.*;

/**
 * Java class for anonymous complex type.
 *
 * <p>The following schema fragment specifies the expected content contained within this class.
 *
 * <pre>
 * &lt;complexType>
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{}name"/>
 *         &lt;element ref="{}arguments" minOccurs="0"/>
 *       &lt;/sequence>
 *       &lt;attGroup ref="{}defAttrTypeName"/>
 *       &lt;attGroup ref="{}defAttrFunction"/>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(
    name = "",
    propOrder = {"name", "arguments"})
@XmlRootElement(name = "functionCall")
public class FunctionCall implements IXmlNode {

  @XmlElement(required = true)
  protected Name name;

  protected Arguments arguments;
  @XmlAttribute protected String type;

  @XmlAttribute(name = "is_intrinsic")
  protected Boolean isIntrinsic;

  /**
   * Gets the value of the name property.
   *
   * @return possible object is {@link Name }
   */
  public Name getName() {
    return name;
  }

  /**
   * Sets the value of the name property.
   *
   * @param value allowed object is {@link Name }
   */
  public void setName(Name value) {
    this.name = value;
  }

  /**
   * Gets the value of the arguments property.
   *
   * @return possible object is {@link Arguments }
   */
  public Arguments getArguments() {
    return arguments;
  }

  /**
   * Sets the value of the arguments property.
   *
   * @param value allowed object is {@link Arguments }
   */
  public void setArguments(Arguments value) {
    this.arguments = value;
  }

  /**
   * Gets the value of the type property.
   *
   * @return possible object is {@link String }
   */
  public String getType() {
    return type;
  }

  /**
   * Sets the value of the type property.
   *
   * @param value allowed object is {@link String }
   */
  public void setType(String value) {
    this.type = value;
  }

  /**
   * Gets the value of the isIntrinsic property.
   *
   * @return possible object is {@link Boolean }
   */
  public Boolean isIsIntrinsic() {
    return isIntrinsic;
  }

  /**
   * Sets the value of the isIntrinsic property.
   *
   * @param value allowed object is {@link Boolean }
   */
  public void setIsIntrinsic(Boolean value) {
    this.isIntrinsic = value;
  }

  @Override
  public boolean enter(jp.riken.kscope.xcodeml.xml.IXmlVisitor visitor) {
    return (visitor.enter(this));
  }

  @Override
  public void leave(jp.riken.kscope.xcodeml.xml.IXmlVisitor visitor) {
    visitor.leave(this);
  }
}
