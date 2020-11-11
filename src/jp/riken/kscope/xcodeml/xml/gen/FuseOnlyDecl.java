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
 *         &lt;element ref="{}renamable" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
 *       &lt;attGroup ref="{}defAttrSourceLine"/>
 *       &lt;attGroup ref="{}defAttrGenericName"/>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(
    name = "",
    propOrder = {"renamable"})
@XmlRootElement(name = "FuseOnlyDecl")
public class FuseOnlyDecl implements IDefBaseStatement, IXmlNode {

  protected List<Renamable> renamable;
  @XmlAttribute protected String lineno;
  @XmlAttribute protected String endlineno;
  @XmlAttribute protected String rawlineno;
  @XmlAttribute protected String file;
  @XmlAttribute protected String name;

  /**
   * Gets the value of the renamable property.
   *
   * <p>This accessor method returns a reference to the live list, not a snapshot. Therefore any
   * modification you make to the returned list will be present inside the JAXB object. This is why
   * there is not a <CODE>set</CODE> method for the renamable property.
   *
   * <p>For example, to add a new item, do as follows:
   *
   * <pre>
   * getRenamable().add(newItem);
   * </pre>
   *
   * <p>Objects of the following type(s) are allowed in the list {@link Renamable }
   *
   * @return Renamable List
   */
  public List<Renamable> getRenamable() {
    if (renamable == null) {
      renamable = new ArrayList<Renamable>();
    }
    return this.renamable;
  }

  /**
   * Gets the value of the lineno property.
   *
   * @return possible object is {@link String }
   */
  public String getLineno() {
    return lineno;
  }

  /**
   * Sets the value of the lineno property.
   *
   * @param value allowed object is {@link String }
   */
  public void setLineno(String value) {
    this.lineno = value;
  }

  /**
   * Gets the value of the endlineno property.
   *
   * @return possible object is {@link String }
   */
  public String getEndlineno() {
    return endlineno;
  }

  /**
   * Sets the value of the endlineno property.
   *
   * @param value allowed object is {@link String }
   */
  public void setEndlineno(String value) {
    this.endlineno = value;
  }

  /**
   * Gets the value of the rawlineno property.
   *
   * @return possible object is {@link String }
   */
  public String getRawlineno() {
    return rawlineno;
  }

  /**
   * Sets the value of the rawlineno property.
   *
   * @param value allowed object is {@link String }
   */
  public void setRawlineno(String value) {
    this.rawlineno = value;
  }

  /**
   * Gets the value of the file property.
   *
   * @return possible object is {@link String }
   */
  public String getFile() {
    return file;
  }

  /**
   * Sets the value of the file property.
   *
   * @param value allowed object is {@link String }
   */
  public void setFile(String value) {
    this.file = value;
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
