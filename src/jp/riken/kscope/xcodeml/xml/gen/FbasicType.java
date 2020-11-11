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

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
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
 *         &lt;element ref="{}kind" minOccurs="0"/>
 *         &lt;choice minOccurs="0">
 *           &lt;element ref="{}len"/>
 *           &lt;group ref="{}defModelArraySubscriptSequence1"/>
 *         &lt;/choice>
 *         &lt;element ref="{}coShape" minOccurs="0"/>
 *       &lt;/sequence>
 *       &lt;attGroup ref="{}defAttrBasicTypeModifier"/>
 *       &lt;attGroup ref="{}defAttrRefTypeName"/>
 *       &lt;attGroup ref="{}defAttrTypeName"/>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(
    name = "",
    propOrder = {"kind", "len", "indexRangeOrArrayIndex", "coShape"})
@XmlRootElement(name = "FbasicType")
public class FbasicType implements IXmlTypeTableChoice, IXmlNode {

  protected Kind kind;
  protected Len len;

  @XmlElements({
    @XmlElement(name = "arrayIndex", type = ArrayIndex.class),
    @XmlElement(name = "indexRange", type = IndexRange.class)
  })
  protected List<IXmlNode> indexRangeOrArrayIndex;

  protected CoShape coShape;

  @XmlAttribute(name = "is_public")
  protected Boolean isPublic;

  @XmlAttribute(name = "is_private")
  protected Boolean isPrivate;

  @XmlAttribute(name = "is_pointer")
  protected Boolean isPointer;

  @XmlAttribute(name = "is_target")
  protected Boolean isTarget;

  @XmlAttribute(name = "is_optional")
  protected Boolean isOptional;

  @XmlAttribute(name = "is_save")
  protected Boolean isSave;

  @XmlAttribute(name = "is_parameter")
  protected Boolean isParameter;

  @XmlAttribute(name = "is_allocatable")
  protected Boolean isAllocatable;

  @XmlAttribute protected DefChoiceIntent intent;
  @XmlAttribute protected String ref;
  @XmlAttribute protected String type;

  /**
   * Gets the value of the kind property.
   *
   * @return possible object is {@link Kind }
   */
  public Kind getKind() {
    return kind;
  }

  /**
   * Sets the value of the kind property.
   *
   * @param value allowed object is {@link Kind }
   */
  public void setKind(Kind value) {
    this.kind = value;
  }

  /**
   * Gets the value of the len property.
   *
   * @return possible object is {@link Len }
   */
  public Len getLen() {
    return len;
  }

  /**
   * Sets the value of the len property.
   *
   * @param value allowed object is {@link Len }
   */
  public void setLen(Len value) {
    this.len = value;
  }

  /**
   * Gets the value of the indexRangeOrArrayIndex property.
   *
   * <p>This accessor method returns a reference to the live list, not a snapshot. Therefore any
   * modification you make to the returned list will be present inside the JAXB object. This is why
   * there is not a <CODE>set</CODE> method for the indexRangeOrArrayIndex property.
   *
   * <p>For example, to add a new item, do as follows:
   *
   * <pre>
   * getIndexRangeOrArrayIndex().add(newItem);
   * </pre>
   *
   * <p>Objects of the following type(s) are allowed in the list {@link ArrayIndex } {@link
   * IndexRange }
   *
   * @return IXmlNode List
   */
  public List<IXmlNode> getIndexRangeOrArrayIndex() {
    if (indexRangeOrArrayIndex == null) {
      indexRangeOrArrayIndex = new ArrayList<IXmlNode>();
    }
    return this.indexRangeOrArrayIndex;
  }

  /**
   * Gets the value of the coShape property.
   *
   * @return possible object is {@link CoShape }
   */
  public CoShape getCoShape() {
    return coShape;
  }

  /**
   * Sets the value of the coShape property.
   *
   * @param value allowed object is {@link CoShape }
   */
  public void setCoShape(CoShape value) {
    this.coShape = value;
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
   * Gets the value of the isPointer property.
   *
   * @return possible object is {@link Boolean }
   */
  public Boolean isIsPointer() {
    return isPointer;
  }

  /**
   * Sets the value of the isPointer property.
   *
   * @param value allowed object is {@link Boolean }
   */
  public void setIsPointer(Boolean value) {
    this.isPointer = value;
  }

  /**
   * Gets the value of the isTarget property.
   *
   * @return possible object is {@link Boolean }
   */
  public Boolean isIsTarget() {
    return isTarget;
  }

  /**
   * Sets the value of the isTarget property.
   *
   * @param value allowed object is {@link Boolean }
   */
  public void setIsTarget(Boolean value) {
    this.isTarget = value;
  }

  /**
   * Gets the value of the isOptional property.
   *
   * @return possible object is {@link Boolean }
   */
  public Boolean isIsOptional() {
    return isOptional;
  }

  /**
   * Sets the value of the isOptional property.
   *
   * @param value allowed object is {@link Boolean }
   */
  public void setIsOptional(Boolean value) {
    this.isOptional = value;
  }

  /**
   * Gets the value of the isSave property.
   *
   * @return possible object is {@link Boolean }
   */
  public Boolean isIsSave() {
    return isSave;
  }

  /**
   * Sets the value of the isSave property.
   *
   * @param value allowed object is {@link Boolean }
   */
  public void setIsSave(Boolean value) {
    this.isSave = value;
  }

  /**
   * Gets the value of the isParameter property.
   *
   * @return possible object is {@link Boolean }
   */
  public Boolean isIsParameter() {
    return isParameter;
  }

  /**
   * Sets the value of the isParameter property.
   *
   * @param value allowed object is {@link Boolean }
   */
  public void setIsParameter(Boolean value) {
    this.isParameter = value;
  }

  /**
   * Gets the value of the isAllocatable property.
   *
   * @return possible object is {@link Boolean }
   */
  public Boolean isIsAllocatable() {
    return isAllocatable;
  }

  /**
   * Sets the value of the isAllocatable property.
   *
   * @param value allowed object is {@link Boolean }
   */
  public void setIsAllocatable(Boolean value) {
    this.isAllocatable = value;
  }

  /**
   * Gets the value of the intent property.
   *
   * @return possible object is {@link DefChoiceIntent }
   */
  public DefChoiceIntent getIntent() {
    return intent;
  }

  /**
   * Sets the value of the intent property.
   *
   * @param value allowed object is {@link DefChoiceIntent }
   */
  public void setIntent(DefChoiceIntent value) {
    this.intent = value;
  }

  /**
   * Gets the value of the ref property.
   *
   * @return possible object is {@link String }
   */
  public String getRef() {
    return ref;
  }

  /**
   * Sets the value of the ref property.
   *
   * @param value allowed object is {@link String }
   */
  public void setRef(String value) {
    this.ref = value;
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
