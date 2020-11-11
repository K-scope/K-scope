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
 *       &lt;attGroup ref="{}defAttrRenamable"/>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "")
@XmlRootElement(name = "renamable")
public class Renamable implements IXmlNode {

  @XmlAttribute(name = "use_name")
  protected String useName;

  @XmlAttribute(name = "local_name")
  protected String localName;

  /**
   * Gets the value of the useName property.
   *
   * @return possible object is {@link String }
   */
  public String getUseName() {
    return useName;
  }

  /**
   * Sets the value of the useName property.
   *
   * @param value allowed object is {@link String }
   */
  public void setUseName(String value) {
    this.useName = value;
  }

  /**
   * Gets the value of the localName property.
   *
   * @return possible object is {@link String }
   */
  public String getLocalName() {
    return localName;
  }

  /**
   * Sets the value of the localName property.
   *
   * @param value allowed object is {@link String }
   */
  public void setLocalName(String value) {
    this.localName = value;
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
