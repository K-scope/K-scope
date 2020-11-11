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
 *         &lt;choice>
 *           &lt;element ref="{}Var"/>
 *           &lt;element ref="{}FmemberRef"/>
 *         &lt;/choice>
 *         &lt;group ref="{}defModelArraySubscriptSequence0"/>
 *         &lt;element ref="{}coShape"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(
    name = "",
    propOrder = {"var", "fmemberRef", "indexRangeOrArrayIndex", "coShape"})
@XmlRootElement(name = "alloc")
public class Alloc implements IXmlNode {

  @XmlElement(name = "Var")
  protected Var var;

  @XmlElement(name = "FmemberRef")
  protected FmemberRef fmemberRef;

  @XmlElements({
    @XmlElement(name = "indexRange", type = IndexRange.class),
    @XmlElement(name = "arrayIndex", type = ArrayIndex.class)
  })
  protected List<IXmlNode> indexRangeOrArrayIndex;

  @XmlElement(required = true)
  protected CoShape coShape;

  /**
   * Gets the value of the var property.
   *
   * @return possible object is {@link Var }
   */
  public Var getVar() {
    return var;
  }

  /**
   * Sets the value of the var property.
   *
   * @param value allowed object is {@link Var }
   */
  public void setVar(Var value) {
    this.var = value;
  }

  /**
   * Gets the value of the fmemberRef property.
   *
   * @return possible object is {@link FmemberRef }
   */
  public FmemberRef getFmemberRef() {
    return fmemberRef;
  }

  /**
   * Sets the value of the fmemberRef property.
   *
   * @param value allowed object is {@link FmemberRef }
   */
  public void setFmemberRef(FmemberRef value) {
    this.fmemberRef = value;
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
   * <p>Objects of the following type(s) are allowed in the list {@link IndexRange } {@link
   * ArrayIndex }
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

  @Override
  public boolean enter(jp.riken.kscope.xcodeml.xml.IXmlVisitor visitor) {
    return (visitor.enter(this));
  }

  @Override
  public void leave(jp.riken.kscope.xcodeml.xml.IXmlVisitor visitor) {
    visitor.leave(this);
  }
}
