//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.4-2 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2014.12.31 at 01:36:45 AM JST 
//


package jp.riken.kscope.xcodeml.fortran.xml.gen;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import jp.riken.kscope.xcodeml.fortran.xml.IXmlNode;




/**
 * <p>Java class for anonymous complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType>
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;attGroup ref="{}defAttrSourceLine"/>
 *       &lt;attGroup ref="{}defAttrMessage"/>
 *       &lt;attGroup ref="{}defAttrCode"/>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "")
@XmlRootElement(name = "FstopStatement")
public class FstopStatement implements jp.riken.kscope.xcodeml.fortran.xml.IDefBaseStatement,  IXmlNode
{

    @XmlAttribute(name = "lineno")
    protected String lineno;
    @XmlAttribute(name = "endlineno")
    protected String endlineno;
    @XmlAttribute(name = "rawlineno")
    protected String rawlineno;
    @XmlAttribute(name = "file")
    protected String file;
    @XmlAttribute(name = "message")
    protected String message;
    @XmlAttribute(name = "code")
    protected String code;

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
     * Gets the value of the message property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getMessage() {
        return message;
    }

    /**
     * Sets the value of the message property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setMessage(String value) {
        this.message = value;
    }

    /**
     * Gets the value of the code property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCode() {
        return code;
    }

    /**
     * Sets the value of the code property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCode(String value) {
        this.code = value;
    }



	@Override
	public boolean enter(jp.riken.kscope.xcodeml.fortran.xml.IXmlVisitor visitor) {
        return (visitor.enter(this));
	}

	@Override
	public void leave(jp.riken.kscope.xcodeml.fortran.xml.IXmlVisitor visitor) {
        visitor.leave(this);
	}
}
