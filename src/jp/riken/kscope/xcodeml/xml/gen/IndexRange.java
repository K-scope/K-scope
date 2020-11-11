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
 *         &lt;element ref="{}lowerBound" minOccurs="0"/>
 *         &lt;element ref="{}upperBound" minOccurs="0"/>
 *         &lt;element ref="{}step" minOccurs="0"/>
 *       &lt;/sequence>
 *       &lt;attGroup ref="{}defAttrIndexRange"/>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(
    name = "",
    propOrder = {"lowerBound", "upperBound", "step"})
@XmlRootElement(name = "indexRange")
public class IndexRange implements IXmlNode {

  protected LowerBound lowerBound;
  protected UpperBound upperBound;
  protected Step step;

  @XmlAttribute(name = "is_assumed_shape")
  protected Boolean isAssumedShape;

  @XmlAttribute(name = "is_assumed_size")
  protected Boolean isAssumedSize;

  /**
   * Gets the value of the lowerBound property.
   *
   * @return possible object is {@link LowerBound }
   */
  public LowerBound getLowerBound() {
    return lowerBound;
  }

  /**
   * Sets the value of the lowerBound property.
   *
   * @param value allowed object is {@link LowerBound }
   */
  public void setLowerBound(LowerBound value) {
    this.lowerBound = value;
  }

  /**
   * Gets the value of the upperBound property.
   *
   * @return possible object is {@link UpperBound }
   */
  public UpperBound getUpperBound() {
    return upperBound;
  }

  /**
   * Sets the value of the upperBound property.
   *
   * @param value allowed object is {@link UpperBound }
   */
  public void setUpperBound(UpperBound value) {
    this.upperBound = value;
  }

  /**
   * Gets the value of the step property.
   *
   * @return possible object is {@link Step }
   */
  public Step getStep() {
    return step;
  }

  /**
   * Sets the value of the step property.
   *
   * @param value allowed object is {@link Step }
   */
  public void setStep(Step value) {
    this.step = value;
  }

  /**
   * Gets the value of the isAssumedShape property.
   *
   * @return possible object is {@link Boolean }
   */
  public Boolean isIsAssumedShape() {
    return isAssumedShape;
  }

  /**
   * Sets the value of the isAssumedShape property.
   *
   * @param value allowed object is {@link Boolean }
   */
  public void setIsAssumedShape(Boolean value) {
    this.isAssumedShape = value;
  }

  /**
   * Gets the value of the isAssumedSize property.
   *
   * @return possible object is {@link Boolean }
   */
  public Boolean isIsAssumedSize() {
    return isAssumedSize;
  }

  /**
   * Sets the value of the isAssumedSize property.
   *
   * @param value allowed object is {@link Boolean }
   */
  public void setIsAssumedSize(Boolean value) {
    this.isAssumedSize = value;
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
