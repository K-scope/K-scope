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
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
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
 *       &lt;choice maxOccurs="unbounded" minOccurs="0">
 *         &lt;element ref="{}FfunctionDefinition"/>
 *         &lt;element ref="{}FmoduleDefinition"/>
 *         &lt;element ref="{}FblockDataDefinition"/>
 *         &lt;element ref="{}text"/>
 *       &lt;/choice>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {
    "ffunctionDefinitionOrFmoduleDefinitionOrFblockDataDefinition"
})
@XmlRootElement(name = "globalDeclarations")
public class GlobalDeclarations
    implements IXmlNode
{

    @XmlElements({
        @XmlElement(name = "FfunctionDefinition", type = FfunctionDefinition.class),
        @XmlElement(name = "FmoduleDefinition", type = FmoduleDefinition.class),
        @XmlElement(name = "FblockDataDefinition", type = FblockDataDefinition.class),
        @XmlElement(name = "text", type = Text.class)
    })
    protected List<IXmlNode> ffunctionDefinitionOrFmoduleDefinitionOrFblockDataDefinition;

    /**
     * Gets the value of the ffunctionDefinitionOrFmoduleDefinitionOrFblockDataDefinition property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the ffunctionDefinitionOrFmoduleDefinitionOrFblockDataDefinition property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getFfunctionDefinitionOrFmoduleDefinitionOrFblockDataDefinition().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link FfunctionDefinition }
     * {@link FmoduleDefinition }
     * {@link FblockDataDefinition }
     * {@link Text }
     * 
     * 
     */
    public List<IXmlNode> getFfunctionDefinitionOrFmoduleDefinitionOrFblockDataDefinition() {
        if (ffunctionDefinitionOrFmoduleDefinitionOrFblockDataDefinition == null) {
            ffunctionDefinitionOrFmoduleDefinitionOrFblockDataDefinition = new ArrayList<IXmlNode>();
        }
        return this.ffunctionDefinitionOrFmoduleDefinitionOrFblockDataDefinition;
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
