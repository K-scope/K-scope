//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.4-2 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2015.01.08 at 01:05:45 AM JST 
//


package jp.riken.kscope.xcodeml.clang.xml.gen;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;
import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import jp.riken.kscope.xcodeml.clang.xml.*;



/**
 * <p>Java class for anonymous complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType>
 *   &lt;simpleContent>
 *     &lt;extension base="&lt;http://www.w3.org/2001/XMLSchema>string">
 *       &lt;attGroup ref="{}BaseExpression"/>
 *     &lt;/extension>
 *   &lt;/simpleContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {
    "value"
})
@XmlRootElement(name = "longlongConstant")
public class LonglongConstant implements IXmlNode
{

    @XmlValue
    protected String value;
    @XmlAttribute(name = "type")
    protected String type;
    @XmlAttribute(name = "is_gccSyntax")
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    protected String isGccSyntax;
    @XmlAttribute(name = "is_modified")
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    protected String isModified;

    /**
     * Gets the value of the value property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getValue() {
        return value;
    }

    /**
     * Sets the value of the value property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setValue(String value) {
        this.value = value;
    }

    /**
     * Gets the value of the type property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getType() {
        return type;
    }

    /**
     * Sets the value of the type property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setType(String value) {
        this.type = value;
    }

    /**
     * Gets the value of the isGccSyntax property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getIsGccSyntax() {
        return isGccSyntax;
    }

    /**
     * Sets the value of the isGccSyntax property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setIsGccSyntax(String value) {
        this.isGccSyntax = value;
    }

    /**
     * Gets the value of the isModified property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getIsModified() {
        return isModified;
    }

    /**
     * Sets the value of the isModified property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setIsModified(String value) {
        this.isModified = value;
    }



	@Override
	public boolean enter(jp.riken.kscope.xcodeml.clang.xml.IXmlVisitor visitor) {
        return (visitor.enter(this));
	}

	@Override
	public void leave(jp.riken.kscope.xcodeml.clang.xml.IXmlVisitor visitor) {
        visitor.leave(this);
	}
}
