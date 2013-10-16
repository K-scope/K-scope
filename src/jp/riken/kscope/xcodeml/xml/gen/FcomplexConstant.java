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
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
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
 *         &lt;group ref="{}defModelExpr"/>
 *         &lt;group ref="{}defModelExpr"/>
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
@XmlType(name = "", propOrder = { "content" })
@XmlRootElement(name = "FcomplexConstant")
public class FcomplexConstant implements IXmlNode {

    @XmlElementRefs({
            @XmlElementRef(name = "FlogicalConstant", type = FlogicalConstant.class),
            @XmlElementRef(name = "Var", type = Var.class),
            @XmlElementRef(name = "logEQVExpr", type = LogEQVExpr.class),
            @XmlElementRef(name = "varRef", type = VarRef.class),
            @XmlElementRef(name = "logGTExpr", type = LogGTExpr.class),
            @XmlElementRef(name = "FstructConstructor", type = FstructConstructor.class),
            @XmlElementRef(name = "functionCall", type = FunctionCall.class),
            @XmlElementRef(name = "minusExpr", type = MinusExpr.class),
            @XmlElementRef(name = "unaryMinusExpr", type = UnaryMinusExpr.class),
            @XmlElementRef(name = "logLTExpr", type = LogLTExpr.class),
            @XmlElementRef(name = "divExpr", type = DivExpr.class),
            @XmlElementRef(name = "FcharacterRef", type = FcharacterRef.class),
            @XmlElementRef(name = "logGEExpr", type = LogGEExpr.class),
            @XmlElementRef(name = "userUnaryExpr", type = UserUnaryExpr.class),
            @XmlElementRef(name = "logNEQExpr", type = LogNEQExpr.class),
            @XmlElementRef(name = "FintConstant", type = FintConstant.class),
            @XmlElementRef(name = "FpowerExpr", type = FpowerExpr.class),
            @XmlElementRef(name = "FdoLoop", type = FdoLoop.class),
            @XmlElementRef(name = "FarrayConstructor", type = FarrayConstructor.class),
            @XmlElementRef(name = "FcharacterConstant", type = FcharacterConstant.class),
            @XmlElementRef(name = "logNEQVExpr", type = LogNEQVExpr.class),
            @XmlElementRef(name = "FconcatExpr", type = FconcatExpr.class),
            @XmlElementRef(name = "FcomplexConstant", type = FcomplexConstant.class),
            @XmlElementRef(name = "FrealConstant", type = FrealConstant.class),
            @XmlElementRef(name = "logNotExpr", type = LogNotExpr.class),
            @XmlElementRef(name = "logOrExpr", type = LogOrExpr.class),
            @XmlElementRef(name = "FmemberRef", type = FmemberRef.class),
            @XmlElementRef(name = "FcoArrayRef", type = FcoArrayRef.class),
            @XmlElementRef(name = "plusExpr", type = PlusExpr.class),
            @XmlElementRef(name = "userBinaryExpr", type = UserBinaryExpr.class),
            @XmlElementRef(name = "logLEExpr", type = LogLEExpr.class),
            @XmlElementRef(name = "FarrayRef", type = FarrayRef.class),
            @XmlElementRef(name = "logAndExpr", type = LogAndExpr.class),
            @XmlElementRef(name = "mulExpr", type = MulExpr.class),
            @XmlElementRef(name = "logEQExpr", type = LogEQExpr.class) })
    protected List<IXmlNode> content;
    @XmlAttribute
    protected String type;

    /**
     * Gets the rest of the content model.
     *
     * <p>
     * You are getting this "catch-all" property because of the following
     * reason: The field name "FintConstant" is used by two different parts of a
     * schema. See: line 174 of
     * file:/home/hira/hira_works/fortran_analysis_xcodeml
     * /src/code_viewer/xsd_generated/XcodeML_TR.xsd line 174 of
     * file:/home/hira/
     * hira_works/fortran_analysis_xcodeml/src/code_viewer/xsd_generated
     * /XcodeML_TR.xsd
     * <p>
     * To get rid of this property, apply a property customization to one of
     * both of the following declarations to change their names: Gets the value
     * of the content property.
     *
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a
     * <CODE>set</CODE> method for the content property.
     *
     * <p>
     * For example, to add a new item, do as follows:
     *
     * <pre>
     * getContent().add(newItem);
     * </pre>
     *
     *
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link FlogicalConstant } {@link Var } {@link VarRef } {@link LogEQVExpr }
     * {@link LogGTExpr } {@link FstructConstructor } {@link FunctionCall }
     * {@link MinusExpr } {@link UnaryMinusExpr } {@link LogLTExpr }
     * {@link DivExpr } {@link FcharacterRef } {@link LogGEExpr }
     * {@link UserUnaryExpr } {@link FpowerExpr } {@link FintConstant }
     * {@link LogNEQExpr } {@link FdoLoop } {@link FcharacterConstant }
     * {@link FarrayConstructor } {@link LogNEQVExpr } {@link FcomplexConstant }
     * {@link FconcatExpr } {@link LogNotExpr } {@link FrealConstant }
     * {@link LogOrExpr } {@link FmemberRef } {@link FcoArrayRef } {@link PlusExpr }
     * {@link UserBinaryExpr } {@link LogLEExpr } {@link FarrayRef }
     * {@link MulExpr } {@link LogAndExpr } {@link LogEQExpr }
     *
     * @return      IXmlNode List
     *
     */
    public List<IXmlNode> getContent() {
        if (content == null) {
            content = new ArrayList<IXmlNode>();
        }
        return this.content;
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
