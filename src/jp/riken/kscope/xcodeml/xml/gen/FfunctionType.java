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
 *         &lt;element ref="{}params" minOccurs="0"/>
 *       &lt;/sequence>
 *       &lt;attGroup ref="{}defAttrTypeName"/>
 *       &lt;attGroup ref="{}defAttrResultName"/>
 *       &lt;attGroup ref="{}defAttrFunctionTypeModifier"/>
 *       &lt;attGroup ref="{}defAttrReturnTypeName"/>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(
    name = "",
    propOrder = {"params"})
@XmlRootElement(name = "FfunctionType")
public class FfunctionType implements IXmlTypeTableChoice, IXmlNode {

  protected Params params;
  @XmlAttribute protected String type;

  @XmlAttribute(name = "result_name")
  protected String resultName;

  @XmlAttribute(name = "is_recursive")
  protected Boolean isRecursive;

  @XmlAttribute(name = "is_program")
  protected Boolean isProgram;

  @XmlAttribute(name = "is_internal")
  protected Boolean isInternal;

  @XmlAttribute(name = "is_intrinsic")
  protected Boolean isIntrinsic;

  @XmlAttribute(name = "is_external")
  protected Boolean isExternal;

  @XmlAttribute(name = "is_public")
  protected Boolean isPublic;

  @XmlAttribute(name = "is_private")
  protected Boolean isPrivate;

  @XmlAttribute(name = "return_type")
  protected String returnType;

  /**
   * Gets the value of the params property.
   *
   * @return possible object is {@link Params }
   */
  public Params getParams() {
    return params;
  }

  /**
   * Sets the value of the params property.
   *
   * @param value allowed object is {@link Params }
   */
  public void setParams(Params value) {
    this.params = value;
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
   * Gets the value of the resultName property.
   *
   * @return possible object is {@link String }
   */
  public String getResultName() {
    return resultName;
  }

  /**
   * Sets the value of the resultName property.
   *
   * @param value allowed object is {@link String }
   */
  public void setResultName(String value) {
    this.resultName = value;
  }

  /**
   * Gets the value of the isRecursive property.
   *
   * @return possible object is {@link Boolean }
   */
  public Boolean isIsRecursive() {
    return isRecursive;
  }

  /**
   * Sets the value of the isRecursive property.
   *
   * @param value allowed object is {@link Boolean }
   */
  public void setIsRecursive(Boolean value) {
    this.isRecursive = value;
  }

  /**
   * Gets the value of the isProgram property.
   *
   * @return possible object is {@link Boolean }
   */
  public Boolean isIsProgram() {
    return isProgram;
  }

  /**
   * Sets the value of the isProgram property.
   *
   * @param value allowed object is {@link Boolean }
   */
  public void setIsProgram(Boolean value) {
    this.isProgram = value;
  }

  /**
   * Gets the value of the isInternal property.
   *
   * @return possible object is {@link Boolean }
   */
  public Boolean isIsInternal() {
    return isInternal;
  }

  /**
   * Sets the value of the isInternal property.
   *
   * @param value allowed object is {@link Boolean }
   */
  public void setIsInternal(Boolean value) {
    this.isInternal = value;
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

  /**
   * Gets the value of the isExternal property.
   *
   * @return possible object is {@link Boolean }
   */
  public Boolean isIsExternal() {
    return isExternal;
  }

  /**
   * Sets the value of the isExternal property.
   *
   * @param value allowed object is {@link Boolean }
   */
  public void setIsExternal(Boolean value) {
    this.isExternal = value;
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
   * Gets the value of the returnType property.
   *
   * @return possible object is {@link String }
   */
  public String getReturnType() {
    return returnType;
  }

  /**
   * Sets the value of the returnType property.
   *
   * @param value allowed object is {@link String }
   */
  public void setReturnType(String value) {
    this.returnType = value;
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
