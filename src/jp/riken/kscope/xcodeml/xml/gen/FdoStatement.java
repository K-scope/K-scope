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
 *         &lt;sequence minOccurs="0">
 *           &lt;element ref="{}Var"/>
 *           &lt;element ref="{}indexRange"/>
 *         &lt;/sequence>
 *         &lt;element ref="{}body" minOccurs="0"/>
 *       &lt;/sequence>
 *       &lt;attGroup ref="{}defAttrSourceLine"/>
 *       &lt;attGroup ref="{}defAttrConstructName"/>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(
    name = "",
    propOrder = {"var", "indexRange", "body"})
@XmlRootElement(name = "FdoStatement")
public class FdoStatement implements IDefBaseStatement, IXmlNode {

  @XmlElement(name = "Var")
  protected Var var;

  protected IndexRange indexRange;
  protected Body body;
  @XmlAttribute protected String lineno;
  @XmlAttribute protected String endlineno;
  @XmlAttribute protected String rawlineno;
  @XmlAttribute protected String file;

  @XmlAttribute(name = "construct_name")
  protected String constructName;

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
   * Gets the value of the body property.
   *
   * @return possible object is {@link Body }
   */
  public Body getBody() {
    return body;
  }

  /**
   * Sets the value of the body property.
   *
   * @param value allowed object is {@link Body }
   */
  public void setBody(Body value) {
    this.body = value;
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
   * Gets the value of the constructName property.
   *
   * @return possible object is {@link String }
   */
  public String getConstructName() {
    return constructName;
  }

  /**
   * Sets the value of the constructName property.
   *
   * @param value allowed object is {@link String }
   */
  public void setConstructName(String value) {
    this.constructName = value;
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
