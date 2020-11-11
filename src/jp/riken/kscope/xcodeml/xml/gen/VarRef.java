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
 *         &lt;group ref="{}defModelLValue"/>
 *       &lt;/sequence>
 *       &lt;attGroup ref="{}defAttrTypeName"/>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(
    name = "",
    propOrder = {"var", "farrayRef", "fcharacterRef", "fmemberRef", "fcoArrayRef"})
@XmlRootElement(name = "varRef")
public class VarRef implements IXmlNode {

  @XmlElement(name = "Var")
  protected Var var;

  @XmlElement(name = "FarrayRef")
  protected FarrayRef farrayRef;

  @XmlElement(name = "FcharacterRef")
  protected FcharacterRef fcharacterRef;

  @XmlElement(name = "FmemberRef")
  protected FmemberRef fmemberRef;

  @XmlElement(name = "FcoArrayRef")
  protected FcoArrayRef fcoArrayRef;

  @XmlAttribute protected String type;

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
   * Gets the value of the farrayRef property.
   *
   * @return possible object is {@link FarrayRef }
   */
  public FarrayRef getFarrayRef() {
    return farrayRef;
  }

  /**
   * Sets the value of the farrayRef property.
   *
   * @param value allowed object is {@link FarrayRef }
   */
  public void setFarrayRef(FarrayRef value) {
    this.farrayRef = value;
  }

  /**
   * Gets the value of the fcharacterRef property.
   *
   * @return possible object is {@link FcharacterRef }
   */
  public FcharacterRef getFcharacterRef() {
    return fcharacterRef;
  }

  /**
   * Sets the value of the fcharacterRef property.
   *
   * @param value allowed object is {@link FcharacterRef }
   */
  public void setFcharacterRef(FcharacterRef value) {
    this.fcharacterRef = value;
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
   * Gets the value of the fcoArrayRef property.
   *
   * @return possible object is {@link FcoArrayRef }
   */
  public FcoArrayRef getFcoArrayRef() {
    return fcoArrayRef;
  }

  /**
   * Sets the value of the fcoArrayRef property.
   *
   * @param value allowed object is {@link FcoArrayRef }
   */
  public void setFcoArrayRef(FcoArrayRef value) {
    this.fcoArrayRef = value;
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
