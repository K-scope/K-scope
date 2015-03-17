//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.4-2 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2014.12.31 at 01:36:45 AM JST 
//


package jp.riken.kscope.xcodeml.fortran.xml.gen;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlType;

import jp.riken.kscope.xcodeml.fortran.xml.IXmlNode;




/**
 * <p>Java class for namedValueList complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="namedValueList">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{}namedValue" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "namedValueList", propOrder = {
    "namedValue"
})
@XmlSeeAlso({
    FinquireStatement.class,
    FcloseStatement.class,
    FopenStatement.class,
    FbackspaceStatement.class,
    FendFileStatement.class,
    FrewindStatement.class,
    FwriteStatement.class,
    FreadStatement.class
})
public class NamedValueList implements IXmlNode
{

    protected List<NamedValue> namedValue;

    /**
     * Gets the value of the namedValue property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the namedValue property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getNamedValue().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link NamedValue }
     * 
     * 
     */
    public List<NamedValue> getNamedValue() {
        if (namedValue == null) {
            namedValue = new ArrayList<NamedValue>();
        }
        return this.namedValue;
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
