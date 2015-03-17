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
import javax.xml.bind.annotation.XmlElement;
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
 *       &lt;sequence>
 *         &lt;element ref="{}name"/>
 *         &lt;element ref="{}value" minOccurs="0"/>
 *         &lt;element ref="{}bitField" minOccurs="0"/>
 *         &lt;element ref="{}gccAttributes" minOccurs="0"/>
 *       &lt;/sequence>
 *       &lt;attGroup ref="{}gccExtendable"/>
 *       &lt;attGroup ref="{}is_gccThread"/>
 *       &lt;attribute name="type" type="{http://www.w3.org/2001/XMLSchema}string" />
 *       &lt;attribute name="sclass">
 *         &lt;simpleType>
 *           &lt;restriction base="{http://www.w3.org/2001/XMLSchema}token">
 *             &lt;enumeration value="auto"/>
 *             &lt;enumeration value="param"/>
 *             &lt;enumeration value="extern"/>
 *             &lt;enumeration value="extern_def"/>
 *             &lt;enumeration value="static"/>
 *             &lt;enumeration value="register"/>
 *             &lt;enumeration value="label"/>
 *             &lt;enumeration value="tagname"/>
 *             &lt;enumeration value="moe"/>
 *             &lt;enumeration value="typedef_name"/>
 *             &lt;enumeration value="gccLabel"/>
 *           &lt;/restriction>
 *         &lt;/simpleType>
 *       &lt;/attribute>
 *       &lt;attribute name="bit_field" type="{http://www.w3.org/2001/XMLSchema}string" />
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {
    "name",
    "value",
    "bitField",
    "gccAttributes"
})
@XmlRootElement(name = "id")
public class Id implements IXmlNode
{

    @XmlElement(required = true)
    protected Name name;
    protected Value value;
    protected BitField bitField;
    protected GccAttributes gccAttributes;
    @XmlAttribute(name = "type")
    protected String type;
    @XmlAttribute(name = "sclass")
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    protected String sclass;
    @XmlAttribute(name = "bit_field")
    protected String bitFieldAttribute;
    @XmlAttribute(name = "is_gccExtension")
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    protected String isGccExtension;
    @XmlAttribute(name = "is_gccThread")
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    protected String isGccThread;

    /**
     * Gets the value of the name property.
     * 
     * @return
     *     possible object is
     *     {@link Name }
     *     
     */
    public Name getName() {
        return name;
    }

    /**
     * Sets the value of the name property.
     * 
     * @param value
     *     allowed object is
     *     {@link Name }
     *     
     */
    public void setName(Name value) {
        this.name = value;
    }

    /**
     * Gets the value of the value property.
     * 
     * @return
     *     possible object is
     *     {@link Value }
     *     
     */
    public Value getValue() {
        return value;
    }

    /**
     * Sets the value of the value property.
     * 
     * @param value
     *     allowed object is
     *     {@link Value }
     *     
     */
    public void setValue(Value value) {
        this.value = value;
    }

    /**
     * Gets the value of the bitField property.
     * 
     * @return
     *     possible object is
     *     {@link BitField }
     *     
     */
    public BitField getBitField() {
        return bitField;
    }

    /**
     * Sets the value of the bitField property.
     * 
     * @param value
     *     allowed object is
     *     {@link BitField }
     *     
     */
    public void setBitField(BitField value) {
        this.bitField = value;
    }

    /**
     * Gets the value of the gccAttributes property.
     * 
     * @return
     *     possible object is
     *     {@link GccAttributes }
     *     
     */
    public GccAttributes getGccAttributes() {
        return gccAttributes;
    }

    /**
     * Sets the value of the gccAttributes property.
     * 
     * @param value
     *     allowed object is
     *     {@link GccAttributes }
     *     
     */
    public void setGccAttributes(GccAttributes value) {
        this.gccAttributes = value;
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
     * Gets the value of the sclass property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getSclass() {
        return sclass;
    }

    /**
     * Sets the value of the sclass property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSclass(String value) {
        this.sclass = value;
    }

    /**
     * Gets the value of the bitFieldAttribute property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getBitFieldAttribute() {
        return bitFieldAttribute;
    }

    /**
     * Sets the value of the bitFieldAttribute property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setBitFieldAttribute(String value) {
        this.bitFieldAttribute = value;
    }

    /**
     * Gets the value of the isGccExtension property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getIsGccExtension() {
        return isGccExtension;
    }

    /**
     * Sets the value of the isGccExtension property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setIsGccExtension(String value) {
        this.isGccExtension = value;
    }

    /**
     * Gets the value of the isGccThread property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getIsGccThread() {
        return isGccThread;
    }

    /**
     * Sets the value of the isGccThread property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setIsGccThread(String value) {
        this.isGccThread = value;
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
