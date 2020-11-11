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
 *         &lt;element ref="{}symbols"/>
 *       &lt;/sequence>
 *       &lt;attGroup ref="{}defAttrStructTypeModifier"/>
 *       &lt;attGroup ref="{}defAttrTypeName"/>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(
    name = "",
    propOrder = {"symbols"})
@XmlRootElement(name = "FstructType")
public class FstructType implements IXmlTypeTableChoice, IXmlNode {

  @XmlElement(required = true)
  protected Symbols symbols;

  @XmlAttribute(name = "is_public")
  protected Boolean isPublic;

  @XmlAttribute(name = "is_private")
  protected Boolean isPrivate;

  @XmlAttribute(name = "is_sequence")
  protected Boolean isSequence;

  @XmlAttribute(name = "is_internal_private")
  protected Boolean isInternalPrivate;

  @XmlAttribute protected String type;

  /**
   * Gets the value of the symbols property.
   *
   * @return possible object is {@link Symbols }
   */
  public Symbols getSymbols() {
    return symbols;
  }

  /**
   * Sets the value of the symbols property.
   *
   * @param value allowed object is {@link Symbols }
   */
  public void setSymbols(Symbols value) {
    this.symbols = value;
  }

  /**
   * Gets the value of the isPublic property.
   *
   * @return possible object is {@link Boolean }
   */
  public Boolean isIsPublic() {
    return isPublic;
  }

  /**
   * Sets the value of the isPublic property.
   *
   * @param value allowed object is {@link Boolean }
   */
  public void setIsPublic(Boolean value) {
    this.isPublic = value;
  }

  /**
   * Gets the value of the isPrivate property.
   *
   * @return possible object is {@link Boolean }
   */
  public Boolean isIsPrivate() {
    return isPrivate;
  }

  /**
   * Sets the value of the isPrivate property.
   *
   * @param value allowed object is {@link Boolean }
   */
  public void setIsPrivate(Boolean value) {
    this.isPrivate = value;
  }

  /**
   * Gets the value of the isSequence property.
   *
   * @return possible object is {@link Boolean }
   */
  public Boolean isIsSequence() {
    return isSequence;
  }

  /**
   * Sets the value of the isSequence property.
   *
   * @param value allowed object is {@link Boolean }
   */
  public void setIsSequence(Boolean value) {
    this.isSequence = value;
  }

  /**
   * Gets the value of the isInternalPrivate property.
   *
   * @return possible object is {@link Boolean }
   */
  public Boolean isIsInternalPrivate() {
    return isInternalPrivate;
  }

  /**
   * Sets the value of the isInternalPrivate property.
   *
   * @param value allowed object is {@link Boolean }
   */
  public void setIsInternalPrivate(Boolean value) {
    this.isInternalPrivate = value;
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

  @Override
  public boolean enter(jp.riken.kscope.xcodeml.xml.IXmlVisitor visitor) {
    return (visitor.enter(this));
  }

  @Override
  public void leave(jp.riken.kscope.xcodeml.xml.IXmlVisitor visitor) {
    visitor.leave(this);
  }
}
