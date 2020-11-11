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
 *       &lt;choice maxOccurs="unbounded" minOccurs="0">
 *         &lt;element ref="{}varRef"/>
 *         &lt;element ref="{}FdoLoop"/>
 *       &lt;/choice>
 *       &lt;attGroup ref="{}defAttrGenericName"/>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(
    name = "",
    propOrder = {"varRefOrFdoLoop"})
@XmlRootElement(name = "varList")
public class VarList implements IXmlNode {

  @XmlElements({
    @XmlElement(name = "varRef", type = VarRef.class),
    @XmlElement(name = "FdoLoop", type = FdoLoop.class)
  })
  protected List<IXmlNode> varRefOrFdoLoop;

  @XmlAttribute protected String name;

  /**
   * Gets the value of the varRefOrFdoLoop property.
   *
   * <p>This accessor method returns a reference to the live list, not a snapshot. Therefore any
   * modification you make to the returned list will be present inside the JAXB object. This is why
   * there is not a <CODE>set</CODE> method for the varRefOrFdoLoop property.
   *
   * <p>For example, to add a new item, do as follows:
   *
   * <pre>
   * getVarRefOrFdoLoop().add(newItem);
   * </pre>
   *
   * <p>Objects of the following type(s) are allowed in the list {@link VarRef } {@link FdoLoop }
   *
   * @return IXmlNode List
   */
  public List<IXmlNode> getVarRefOrFdoLoop() {
    if (varRefOrFdoLoop == null) {
      varRefOrFdoLoop = new ArrayList<IXmlNode>();
    }
    return this.varRefOrFdoLoop;
  }

  /**
   * Gets the value of the name property.
   *
   * @return possible object is {@link String }
   */
  public String getName() {
    return name;
  }

  /**
   * Sets the value of the name property.
   *
   * @param value allowed object is {@link String }
   */
  public void setName(String value) {
    this.name = value;
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
