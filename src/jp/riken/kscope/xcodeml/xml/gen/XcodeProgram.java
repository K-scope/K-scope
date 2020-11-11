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
 *         &lt;element ref="{}typeTable"/>
 *         &lt;element ref="{}globalSymbols"/>
 *         &lt;element ref="{}globalDeclarations"/>
 *       &lt;/sequence>
 *       &lt;attGroup ref="{}defAttrProgram"/>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(
    name = "",
    propOrder = {"typeTable", "globalSymbols", "globalDeclarations"})
@XmlRootElement(name = "XcodeProgram")
public class XcodeProgram implements IXmlNode {

  @XmlElement(required = true)
  protected TypeTable typeTable;

  @XmlElement(required = true)
  protected GlobalSymbols globalSymbols;

  @XmlElement(required = true)
  protected GlobalDeclarations globalDeclarations;

  @XmlAttribute(name = "compiler-info")
  protected String compilerInfo;

  @XmlAttribute protected String version;
  @XmlAttribute protected String time;
  @XmlAttribute protected String language;
  @XmlAttribute protected String source;

  /**
   * Gets the value of the typeTable property.
   *
   * @return possible object is {@link TypeTable }
   */
  public TypeTable getTypeTable() {
    return typeTable;
  }

  /**
   * Sets the value of the typeTable property.
   *
   * @param value allowed object is {@link TypeTable }
   */
  public void setTypeTable(TypeTable value) {
    this.typeTable = value;
  }

  /**
   * Gets the value of the globalSymbols property.
   *
   * @return possible object is {@link GlobalSymbols }
   */
  public GlobalSymbols getGlobalSymbols() {
    return globalSymbols;
  }

  /**
   * Sets the value of the globalSymbols property.
   *
   * @param value allowed object is {@link GlobalSymbols }
   */
  public void setGlobalSymbols(GlobalSymbols value) {
    this.globalSymbols = value;
  }

  /**
   * Gets the value of the globalDeclarations property.
   *
   * @return possible object is {@link GlobalDeclarations }
   */
  public GlobalDeclarations getGlobalDeclarations() {
    return globalDeclarations;
  }

  /**
   * Sets the value of the globalDeclarations property.
   *
   * @param value allowed object is {@link GlobalDeclarations }
   */
  public void setGlobalDeclarations(GlobalDeclarations value) {
    this.globalDeclarations = value;
  }

  /**
   * Gets the value of the compilerInfo property.
   *
   * @return possible object is {@link String }
   */
  public String getCompilerInfo() {
    return compilerInfo;
  }

  /**
   * Sets the value of the compilerInfo property.
   *
   * @param value allowed object is {@link String }
   */
  public void setCompilerInfo(String value) {
    this.compilerInfo = value;
  }

  /**
   * Gets the value of the version property.
   *
   * @return possible object is {@link String }
   */
  public String getVersion() {
    return version;
  }

  /**
   * Sets the value of the version property.
   *
   * @param value allowed object is {@link String }
   */
  public void setVersion(String value) {
    this.version = value;
  }

  /**
   * Gets the value of the time property.
   *
   * @return possible object is {@link String }
   */
  public String getTime() {
    return time;
  }

  /**
   * Sets the value of the time property.
   *
   * @param value allowed object is {@link String }
   */
  public void setTime(String value) {
    this.time = value;
  }

  /**
   * Gets the value of the language property.
   *
   * @return possible object is {@link String }
   */
  public String getLanguage() {
    return language;
  }

  /**
   * Sets the value of the language property.
   *
   * @param value allowed object is {@link String }
   */
  public void setLanguage(String value) {
    this.language = value;
  }

  /**
   * Gets the value of the source property.
   *
   * @return possible object is {@link String }
   */
  public String getSource() {
    return source;
  }

  /**
   * Sets the value of the source property.
   *
   * @param value allowed object is {@link String }
   */
  public void setSource(String value) {
    this.source = value;
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
