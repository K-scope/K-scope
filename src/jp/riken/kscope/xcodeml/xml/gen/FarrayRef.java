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
 * <p>
 * Java class for anonymous complex type.
 *
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 *
 * <pre>
 * &lt;complexType>
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{}varRef"/>
 *         &lt;choice maxOccurs="unbounded" minOccurs="0">
 *           &lt;element ref="{}indexRange"/>
 *           &lt;element ref="{}arrayIndex"/>
 *           &lt;element ref="{}FarrayConstructor"/>
 *           &lt;element ref="{}FarrayRef"/>
 *         &lt;/choice>
 *       &lt;/sequence>
 *       &lt;attGroup ref="{}defAttrTypeName"/>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 *
 *
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = { "varRef",
        "indexRangeOrArrayIndexOrFarrayConstructor" })
@XmlRootElement(name = "FarrayRef")
public class FarrayRef implements IXmlNode {

    @XmlElement(required = true)
    protected VarRef varRef;
    @XmlElements({
            @XmlElement(name = "FarrayRef", type = FarrayRef.class),
            @XmlElement(name = "indexRange", type = IndexRange.class),
            @XmlElement(name = "FarrayConstructor", type = FarrayConstructor.class),
            @XmlElement(name = "arrayIndex", type = ArrayIndex.class) })
    protected List<IXmlNode> indexRangeOrArrayIndexOrFarrayConstructor;
    @XmlAttribute
    protected String type;

    /**
     * Gets the value of the varRef property.
     *
     * @return possible object is {@link VarRef }
     *
     */
    public VarRef getVarRef() {
        return varRef;
    }

    /**
     * Sets the value of the varRef property.
     *
     * @param value
     *            allowed object is {@link VarRef }
     *
     */
    public void setVarRef(VarRef value) {
        this.varRef = value;
    }

    /**
     * Gets the value of the indexRangeOrArrayIndexOrFarrayConstructor property.
     *
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the indexRangeOrArrayIndexOrFarrayConstructor
     * property.
     *
     * <p>
     * For example, to add a new item, do as follows:
     *
     * <pre>
     * getIndexRangeOrArrayIndexOrFarrayConstructor().add(newItem);
     * </pre>
     *
     *
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link FarrayRef } {@link IndexRange } {@link FarrayConstructor }
     * {@link ArrayIndex }
     *
     * @return      IXmlNode List
     *
     */
    public List<IXmlNode> getIndexRangeOrArrayIndexOrFarrayConstructor() {
        if (indexRangeOrArrayIndexOrFarrayConstructor == null) {
            indexRangeOrArrayIndexOrFarrayConstructor = new ArrayList<IXmlNode>();
        }
        return this.indexRangeOrArrayIndexOrFarrayConstructor;
    }

    /**
     * Gets the value of the type property.
     *
     * @return possible object is {@link String }
     *
     */
    public String getType() {
        return type;
    }

    /**
     * Sets the value of the type property.
     *
     * @param value
     *            allowed object is {@link String }
     *
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
