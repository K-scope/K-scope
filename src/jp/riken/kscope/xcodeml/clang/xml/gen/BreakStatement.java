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
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;attGroup ref="{}BaseStatement"/>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "")
@XmlRootElement(name = "breakStatement")
public class BreakStatement implements jp.riken.kscope.xcodeml.clang.xml.IBaseStatement,  IXmlNode
{

    @XmlAttribute(name = "lineno")
    protected String lineno;
    @XmlAttribute(name = "endlineno")
    protected String endlineno;
    @XmlAttribute(name = "rawlineno")
    protected String rawlineno;
    @XmlAttribute(name = "file")
    protected String file;
    @XmlAttribute(name = "is_gccSyntax")
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    protected String isGccSyntax;
    @XmlAttribute(name = "is_modified")
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    protected String isModified;

    /**
     * Gets the value of the lineno property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLineno() {
        return lineno;
    }

    /**
     * Sets the value of the lineno property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLineno(String value) {
        this.lineno = value;
    }

    /**
     * Gets the value of the endlineno property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getEndlineno() {
        return endlineno;
    }

    /**
     * Sets the value of the endlineno property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setEndlineno(String value) {
        this.endlineno = value;
    }

    /**
     * Gets the value of the rawlineno property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getRawlineno() {
        return rawlineno;
    }

    /**
     * Sets the value of the rawlineno property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setRawlineno(String value) {
        this.rawlineno = value;
    }

    /**
     * Gets the value of the file property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFile() {
        return file;
    }

    /**
     * Sets the value of the file property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFile(String value) {
        this.file = value;
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
