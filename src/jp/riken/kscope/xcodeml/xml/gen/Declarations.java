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
 *       &lt;choice maxOccurs="unbounded" minOccurs="0">
 *         &lt;element ref="{}varDecl"/>
 *         &lt;element ref="{}externDecl"/>
 *         &lt;element ref="{}FuseDecl"/>
 *         &lt;element ref="{}FuseOnlyDecl"/>
 *         &lt;element ref="{}FinterfaceDecl"/>
 *         &lt;element ref="{}FnamelistDecl"/>
 *         &lt;element ref="{}FequivalenceDecl"/>
 *         &lt;element ref="{}FcommonDecl"/>
 *         &lt;element ref="{}FstructDecl"/>
 *         &lt;element ref="{}FentryDecl"/>
 *         &lt;element ref="{}FdataDecl"/>
 *         &lt;element ref="{}FpragmaStatement"/>
 *       &lt;/choice>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(
    name = "",
    propOrder = {"varDeclOrExternDeclOrFuseDecl"})
@XmlRootElement(name = "declarations")
public class Declarations implements IXmlNode {

  @XmlElements({
    @XmlElement(name = "FnamelistDecl", type = FnamelistDecl.class),
    @XmlElement(name = "FuseOnlyDecl", type = FuseOnlyDecl.class),
    @XmlElement(name = "FinterfaceDecl", type = FinterfaceDecl.class),
    @XmlElement(name = "FcommonDecl", type = FcommonDecl.class),
    @XmlElement(name = "externDecl", type = ExternDecl.class),
    @XmlElement(name = "FdataDecl", type = FdataDecl.class),
    @XmlElement(name = "varDecl", type = VarDecl.class),
    @XmlElement(name = "FuseDecl", type = FuseDecl.class),
    @XmlElement(name = "FequivalenceDecl", type = FequivalenceDecl.class),
    @XmlElement(name = "FpragmaStatement", type = FpragmaStatement.class),
    @XmlElement(name = "FentryDecl", type = FentryDecl.class),
    @XmlElement(name = "FstructDecl", type = FstructDecl.class)
  })
  protected List<IXmlNode> varDeclOrExternDeclOrFuseDecl;

  /**
   * Gets the value of the varDeclOrExternDeclOrFuseDecl property.
   *
   * <p>This accessor method returns a reference to the live list, not a snapshot. Therefore any
   * modification you make to the returned list will be present inside the JAXB object. This is why
   * there is not a <CODE>set</CODE> method for the varDeclOrExternDeclOrFuseDecl property.
   *
   * <p>For example, to add a new item, do as follows:
   *
   * <pre>
   * getVarDeclOrExternDeclOrFuseDecl().add(newItem);
   * </pre>
   *
   * <p>Objects of the following type(s) are allowed in the list {@link FnamelistDecl } {@link
   * FuseOnlyDecl } {@link FinterfaceDecl } {@link FcommonDecl } {@link ExternDecl } {@link
   * FdataDecl } {@link VarDecl } {@link FuseDecl } {@link FequivalenceDecl } {@link
   * FpragmaStatement } {@link FentryDecl } {@link FstructDecl }
   *
   * @return IXmlNode List
   */
  public List<IXmlNode> getVarDeclOrExternDeclOrFuseDecl() {
    if (varDeclOrExternDeclOrFuseDecl == null) {
      varDeclOrExternDeclOrFuseDecl = new ArrayList<IXmlNode>();
    }
    return this.varDeclOrExternDeclOrFuseDecl;
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
