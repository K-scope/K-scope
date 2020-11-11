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
 *         &lt;element ref="{}Var"/>
 *         &lt;element ref="{}indexRange"/>
 *         &lt;element ref="{}value" maxOccurs="unbounded"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(
    name = "",
    propOrder = {"var", "indexRange", "value"})
@XmlRootElement(name = "FdoLoop")
public class FdoLoop implements IXmlNode {

  @XmlElement(name = "Var", required = true)
  protected Var var;

  @XmlElement(required = true)
  protected IndexRange indexRange;

  @XmlElement(required = true)
  protected List<Value> value;

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
   * Gets the value of the indexRange property.
   *
   * @return possible object is {@link IndexRange }
   */
  public IndexRange getIndexRange() {
    return indexRange;
  }

  /**
   * Sets the value of the indexRange property.
   *
   * @param value allowed object is {@link IndexRange }
   */
  public void setIndexRange(IndexRange value) {
    this.indexRange = value;
  }

  /**
   * Gets the value of the value property.
   *
   * <p>This accessor method returns a reference to the live list, not a snapshot. Therefore any
   * modification you make to the returned list will be present inside the JAXB object. This is why
   * there is not a <CODE>set</CODE> method for the value property.
   *
   * <p>For example, to add a new item, do as follows:
   *
   * <pre>
   * getValue().add(newItem);
   * </pre>
   *
   * <p>Objects of the following type(s) are allowed in the list {@link Value }
   *
   * @return Value List
   */
  public List<Value> getValue() {
    if (value == null) {
      value = new ArrayList<Value>();
    }
    return this.value;
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
