//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.4-2 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2014.12.31 at 01:36:45 AM JST 
//


package jp.riken.kscope.xcodeml.fortran.xml.gen;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for defChoiceSclass.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * <p>
 * <pre>
 * &lt;simpleType name="defChoiceSclass">
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}token">
 *     &lt;enumeration value="flocal"/>
 *     &lt;enumeration value="fsave"/>
 *     &lt;enumeration value="fcommon"/>
 *     &lt;enumeration value="fparam"/>
 *     &lt;enumeration value="ffunc"/>
 *     &lt;enumeration value="ftype_name"/>
 *     &lt;enumeration value="fcommon_name"/>
 *     &lt;enumeration value="fnamelist_name"/>
 *   &lt;/restriction>
 * &lt;/simpleType>
 * </pre>
 * 
 */
@XmlType(name = "defChoiceSclass")
@XmlEnum
public enum DefChoiceSclass {

    @XmlEnumValue("flocal")
    FLOCAL("flocal"),
    @XmlEnumValue("fsave")
    FSAVE("fsave"),
    @XmlEnumValue("fcommon")
    FCOMMON("fcommon"),
    @XmlEnumValue("fparam")
    FPARAM("fparam"),
    @XmlEnumValue("ffunc")
    FFUNC("ffunc"),
    @XmlEnumValue("ftype_name")
    FTYPE_NAME("ftype_name"),
    @XmlEnumValue("fcommon_name")
    FCOMMON_NAME("fcommon_name"),
    @XmlEnumValue("fnamelist_name")
    FNAMELIST_NAME("fnamelist_name");
    private final String value;

    DefChoiceSclass(String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    public static DefChoiceSclass fromValue(String v) {
        for (DefChoiceSclass c: DefChoiceSclass.values()) {
            if (c.value.equals(v)) {
                return c;
            }
        }
        throw new IllegalArgumentException(v);
    }

}
