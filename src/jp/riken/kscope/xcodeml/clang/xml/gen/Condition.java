//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.4-2 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2015.01.08 at 01:05:45 AM JST 
//


package jp.riken.kscope.xcodeml.clang.xml.gen;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

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
 *       &lt;group ref="{}expressions" minOccurs="0"/>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {
    "intConstant",
    "floatConstant",
    "longlongConstant",
    "stringConstant",
    "moeConstant",
    "funcAddr",
    "pointerRef",
    "var",
    "varAddr",
    "arrayRef",
    "arrayAddr",
    "memberAddr",
    "memberRef",
    "memberArrayRef",
    "memberArrayAddr",
    "assignExpr",
    "plusExpr",
    "minusExpr",
    "mulExpr",
    "divExpr",
    "modExpr",
    "lshiftExpr",
    "rshiftExpr",
    "bitAndExpr",
    "bitOrExpr",
    "bitXorExpr",
    "asgPlusExpr",
    "asgMinusExpr",
    "asgMulExpr",
    "asgDivExpr",
    "asgModExpr",
    "asgLshiftExpr",
    "asgRshiftExpr",
    "asgBitAndExpr",
    "asgBitOrExpr",
    "asgBitXorExpr",
    "logEQExpr",
    "logNEQExpr",
    "logGEExpr",
    "logGTExpr",
    "logLEExpr",
    "logLTExpr",
    "logAndExpr",
    "logOrExpr",
    "unaryMinusExpr",
    "bitNotExpr",
    "logNotExpr",
    "functionCall",
    "commaExpr",
    "postIncrExpr",
    "postDecrExpr",
    "preIncrExpr",
    "preDecrExpr",
    "castExpr",
    "condExpr",
    "sizeOfExpr",
    "addrOfExpr",
    "xmpDescOf",
    "compoundValue",
    "compoundValueAddr",
    "gccAlignOfExpr",
    "gccLabelAddr",
    "gccCompoundExpr",
    "builtinOp",
    "subArrayRef",
    "coArrayRef",
    "coArrayAssignExpr"
})
@XmlRootElement(name = "condition")
public class Condition
    implements jp.riken.kscope.xcodeml.clang.xml.IExpressions,  IXmlNode
{

    protected IntConstant intConstant;
    protected FloatConstant floatConstant;
    protected LonglongConstant longlongConstant;
    protected StringConstant stringConstant;
    protected MoeConstant moeConstant;
    protected FuncAddr funcAddr;
    protected PointerRef pointerRef;
    @XmlElement(name = "Var")
    protected Var var;
    protected VarAddr varAddr;
    protected ArrayRef arrayRef;
    protected ArrayAddr arrayAddr;
    protected MemberAddr memberAddr;
    protected MemberRef memberRef;
    protected MemberArrayRef memberArrayRef;
    protected MemberArrayAddr memberArrayAddr;
    protected AssignExpr assignExpr;
    protected PlusExpr plusExpr;
    protected MinusExpr minusExpr;
    protected MulExpr mulExpr;
    protected DivExpr divExpr;
    protected ModExpr modExpr;
    @XmlElement(name = "LshiftExpr")
    protected LshiftExpr lshiftExpr;
    @XmlElement(name = "RshiftExpr")
    protected RshiftExpr rshiftExpr;
    protected BitAndExpr bitAndExpr;
    protected BitOrExpr bitOrExpr;
    protected BitXorExpr bitXorExpr;
    protected AsgPlusExpr asgPlusExpr;
    protected AsgMinusExpr asgMinusExpr;
    protected AsgMulExpr asgMulExpr;
    protected AsgDivExpr asgDivExpr;
    protected AsgModExpr asgModExpr;
    protected AsgLshiftExpr asgLshiftExpr;
    protected AsgRshiftExpr asgRshiftExpr;
    protected AsgBitAndExpr asgBitAndExpr;
    protected AsgBitOrExpr asgBitOrExpr;
    protected AsgBitXorExpr asgBitXorExpr;
    protected LogEQExpr logEQExpr;
    protected LogNEQExpr logNEQExpr;
    protected LogGEExpr logGEExpr;
    protected LogGTExpr logGTExpr;
    protected LogLEExpr logLEExpr;
    protected LogLTExpr logLTExpr;
    protected LogAndExpr logAndExpr;
    protected LogOrExpr logOrExpr;
    protected UnaryMinusExpr unaryMinusExpr;
    protected BitNotExpr bitNotExpr;
    protected LogNotExpr logNotExpr;
    protected FunctionCall functionCall;
    protected CommaExpr commaExpr;
    protected PostIncrExpr postIncrExpr;
    protected PostDecrExpr postDecrExpr;
    protected PreIncrExpr preIncrExpr;
    protected PreDecrExpr preDecrExpr;
    protected CastExpr castExpr;
    protected CondExpr condExpr;
    protected SizeOfExpr sizeOfExpr;
    protected AddrOfExpr addrOfExpr;
    protected XmpDescOf xmpDescOf;
    protected CompoundValueExpr compoundValue;
    protected CompoundValueAddr compoundValueAddr;
    protected GccAlignOfExpr gccAlignOfExpr;
    protected GccLabelAddr gccLabelAddr;
    protected GccCompoundExpr gccCompoundExpr;
    @XmlElement(name = "builtin_op")
    protected BuiltinOp builtinOp;
    protected SubArrayRef subArrayRef;
    protected CoArrayRef coArrayRef;
    protected CoArrayAssignExpr coArrayAssignExpr;

    /**
     * Gets the value of the intConstant property.
     * 
     * @return
     *     possible object is
     *     {@link IntConstant }
     *     
     */
    public IntConstant getIntConstant() {
        return intConstant;
    }

    /**
     * Sets the value of the intConstant property.
     * 
     * @param value
     *     allowed object is
     *     {@link IntConstant }
     *     
     */
    public void setIntConstant(IntConstant value) {
        this.intConstant = value;
    }

    /**
     * Gets the value of the floatConstant property.
     * 
     * @return
     *     possible object is
     *     {@link FloatConstant }
     *     
     */
    public FloatConstant getFloatConstant() {
        return floatConstant;
    }

    /**
     * Sets the value of the floatConstant property.
     * 
     * @param value
     *     allowed object is
     *     {@link FloatConstant }
     *     
     */
    public void setFloatConstant(FloatConstant value) {
        this.floatConstant = value;
    }

    /**
     * Gets the value of the longlongConstant property.
     * 
     * @return
     *     possible object is
     *     {@link LonglongConstant }
     *     
     */
    public LonglongConstant getLonglongConstant() {
        return longlongConstant;
    }

    /**
     * Sets the value of the longlongConstant property.
     * 
     * @param value
     *     allowed object is
     *     {@link LonglongConstant }
     *     
     */
    public void setLonglongConstant(LonglongConstant value) {
        this.longlongConstant = value;
    }

    /**
     * Gets the value of the stringConstant property.
     * 
     * @return
     *     possible object is
     *     {@link StringConstant }
     *     
     */
    public StringConstant getStringConstant() {
        return stringConstant;
    }

    /**
     * Sets the value of the stringConstant property.
     * 
     * @param value
     *     allowed object is
     *     {@link StringConstant }
     *     
     */
    public void setStringConstant(StringConstant value) {
        this.stringConstant = value;
    }

    /**
     * Gets the value of the moeConstant property.
     * 
     * @return
     *     possible object is
     *     {@link MoeConstant }
     *     
     */
    public MoeConstant getMoeConstant() {
        return moeConstant;
    }

    /**
     * Sets the value of the moeConstant property.
     * 
     * @param value
     *     allowed object is
     *     {@link MoeConstant }
     *     
     */
    public void setMoeConstant(MoeConstant value) {
        this.moeConstant = value;
    }

    /**
     * Gets the value of the funcAddr property.
     * 
     * @return
     *     possible object is
     *     {@link FuncAddr }
     *     
     */
    public FuncAddr getFuncAddr() {
        return funcAddr;
    }

    /**
     * Sets the value of the funcAddr property.
     * 
     * @param value
     *     allowed object is
     *     {@link FuncAddr }
     *     
     */
    public void setFuncAddr(FuncAddr value) {
        this.funcAddr = value;
    }

    /**
     * Gets the value of the pointerRef property.
     * 
     * @return
     *     possible object is
     *     {@link PointerRef }
     *     
     */
    public PointerRef getPointerRef() {
        return pointerRef;
    }

    /**
     * Sets the value of the pointerRef property.
     * 
     * @param value
     *     allowed object is
     *     {@link PointerRef }
     *     
     */
    public void setPointerRef(PointerRef value) {
        this.pointerRef = value;
    }

    /**
     * Gets the value of the var property.
     * 
     * @return
     *     possible object is
     *     {@link Var }
     *     
     */
    public Var getVar() {
        return var;
    }

    /**
     * Sets the value of the var property.
     * 
     * @param value
     *     allowed object is
     *     {@link Var }
     *     
     */
    public void setVar(Var value) {
        this.var = value;
    }

    /**
     * Gets the value of the varAddr property.
     * 
     * @return
     *     possible object is
     *     {@link VarAddr }
     *     
     */
    public VarAddr getVarAddr() {
        return varAddr;
    }

    /**
     * Sets the value of the varAddr property.
     * 
     * @param value
     *     allowed object is
     *     {@link VarAddr }
     *     
     */
    public void setVarAddr(VarAddr value) {
        this.varAddr = value;
    }

    /**
     * Gets the value of the arrayRef property.
     * 
     * @return
     *     possible object is
     *     {@link ArrayRef }
     *     
     */
    public ArrayRef getArrayRef() {
        return arrayRef;
    }

    /**
     * Sets the value of the arrayRef property.
     * 
     * @param value
     *     allowed object is
     *     {@link ArrayRef }
     *     
     */
    public void setArrayRef(ArrayRef value) {
        this.arrayRef = value;
    }

    /**
     * Gets the value of the arrayAddr property.
     * 
     * @return
     *     possible object is
     *     {@link ArrayAddr }
     *     
     */
    public ArrayAddr getArrayAddr() {
        return arrayAddr;
    }

    /**
     * Sets the value of the arrayAddr property.
     * 
     * @param value
     *     allowed object is
     *     {@link ArrayAddr }
     *     
     */
    public void setArrayAddr(ArrayAddr value) {
        this.arrayAddr = value;
    }

    /**
     * Gets the value of the memberAddr property.
     * 
     * @return
     *     possible object is
     *     {@link MemberAddr }
     *     
     */
    public MemberAddr getMemberAddr() {
        return memberAddr;
    }

    /**
     * Sets the value of the memberAddr property.
     * 
     * @param value
     *     allowed object is
     *     {@link MemberAddr }
     *     
     */
    public void setMemberAddr(MemberAddr value) {
        this.memberAddr = value;
    }

    /**
     * Gets the value of the memberRef property.
     * 
     * @return
     *     possible object is
     *     {@link MemberRef }
     *     
     */
    public MemberRef getMemberRef() {
        return memberRef;
    }

    /**
     * Sets the value of the memberRef property.
     * 
     * @param value
     *     allowed object is
     *     {@link MemberRef }
     *     
     */
    public void setMemberRef(MemberRef value) {
        this.memberRef = value;
    }

    /**
     * Gets the value of the memberArrayRef property.
     * 
     * @return
     *     possible object is
     *     {@link MemberArrayRef }
     *     
     */
    public MemberArrayRef getMemberArrayRef() {
        return memberArrayRef;
    }

    /**
     * Sets the value of the memberArrayRef property.
     * 
     * @param value
     *     allowed object is
     *     {@link MemberArrayRef }
     *     
     */
    public void setMemberArrayRef(MemberArrayRef value) {
        this.memberArrayRef = value;
    }

    /**
     * Gets the value of the memberArrayAddr property.
     * 
     * @return
     *     possible object is
     *     {@link MemberArrayAddr }
     *     
     */
    public MemberArrayAddr getMemberArrayAddr() {
        return memberArrayAddr;
    }

    /**
     * Sets the value of the memberArrayAddr property.
     * 
     * @param value
     *     allowed object is
     *     {@link MemberArrayAddr }
     *     
     */
    public void setMemberArrayAddr(MemberArrayAddr value) {
        this.memberArrayAddr = value;
    }

    /**
     * Gets the value of the assignExpr property.
     * 
     * @return
     *     possible object is
     *     {@link AssignExpr }
     *     
     */
    public AssignExpr getAssignExpr() {
        return assignExpr;
    }

    /**
     * Sets the value of the assignExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link AssignExpr }
     *     
     */
    public void setAssignExpr(AssignExpr value) {
        this.assignExpr = value;
    }

    /**
     * Gets the value of the plusExpr property.
     * 
     * @return
     *     possible object is
     *     {@link PlusExpr }
     *     
     */
    public PlusExpr getPlusExpr() {
        return plusExpr;
    }

    /**
     * Sets the value of the plusExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link PlusExpr }
     *     
     */
    public void setPlusExpr(PlusExpr value) {
        this.plusExpr = value;
    }

    /**
     * Gets the value of the minusExpr property.
     * 
     * @return
     *     possible object is
     *     {@link MinusExpr }
     *     
     */
    public MinusExpr getMinusExpr() {
        return minusExpr;
    }

    /**
     * Sets the value of the minusExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link MinusExpr }
     *     
     */
    public void setMinusExpr(MinusExpr value) {
        this.minusExpr = value;
    }

    /**
     * Gets the value of the mulExpr property.
     * 
     * @return
     *     possible object is
     *     {@link MulExpr }
     *     
     */
    public MulExpr getMulExpr() {
        return mulExpr;
    }

    /**
     * Sets the value of the mulExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link MulExpr }
     *     
     */
    public void setMulExpr(MulExpr value) {
        this.mulExpr = value;
    }

    /**
     * Gets the value of the divExpr property.
     * 
     * @return
     *     possible object is
     *     {@link DivExpr }
     *     
     */
    public DivExpr getDivExpr() {
        return divExpr;
    }

    /**
     * Sets the value of the divExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link DivExpr }
     *     
     */
    public void setDivExpr(DivExpr value) {
        this.divExpr = value;
    }

    /**
     * Gets the value of the modExpr property.
     * 
     * @return
     *     possible object is
     *     {@link ModExpr }
     *     
     */
    public ModExpr getModExpr() {
        return modExpr;
    }

    /**
     * Sets the value of the modExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link ModExpr }
     *     
     */
    public void setModExpr(ModExpr value) {
        this.modExpr = value;
    }

    /**
     * Gets the value of the lshiftExpr property.
     * 
     * @return
     *     possible object is
     *     {@link LshiftExpr }
     *     
     */
    public LshiftExpr getLshiftExpr() {
        return lshiftExpr;
    }

    /**
     * Sets the value of the lshiftExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link LshiftExpr }
     *     
     */
    public void setLshiftExpr(LshiftExpr value) {
        this.lshiftExpr = value;
    }

    /**
     * Gets the value of the rshiftExpr property.
     * 
     * @return
     *     possible object is
     *     {@link RshiftExpr }
     *     
     */
    public RshiftExpr getRshiftExpr() {
        return rshiftExpr;
    }

    /**
     * Sets the value of the rshiftExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link RshiftExpr }
     *     
     */
    public void setRshiftExpr(RshiftExpr value) {
        this.rshiftExpr = value;
    }

    /**
     * Gets the value of the bitAndExpr property.
     * 
     * @return
     *     possible object is
     *     {@link BitAndExpr }
     *     
     */
    public BitAndExpr getBitAndExpr() {
        return bitAndExpr;
    }

    /**
     * Sets the value of the bitAndExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link BitAndExpr }
     *     
     */
    public void setBitAndExpr(BitAndExpr value) {
        this.bitAndExpr = value;
    }

    /**
     * Gets the value of the bitOrExpr property.
     * 
     * @return
     *     possible object is
     *     {@link BitOrExpr }
     *     
     */
    public BitOrExpr getBitOrExpr() {
        return bitOrExpr;
    }

    /**
     * Sets the value of the bitOrExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link BitOrExpr }
     *     
     */
    public void setBitOrExpr(BitOrExpr value) {
        this.bitOrExpr = value;
    }

    /**
     * Gets the value of the bitXorExpr property.
     * 
     * @return
     *     possible object is
     *     {@link BitXorExpr }
     *     
     */
    public BitXorExpr getBitXorExpr() {
        return bitXorExpr;
    }

    /**
     * Sets the value of the bitXorExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link BitXorExpr }
     *     
     */
    public void setBitXorExpr(BitXorExpr value) {
        this.bitXorExpr = value;
    }

    /**
     * Gets the value of the asgPlusExpr property.
     * 
     * @return
     *     possible object is
     *     {@link AsgPlusExpr }
     *     
     */
    public AsgPlusExpr getAsgPlusExpr() {
        return asgPlusExpr;
    }

    /**
     * Sets the value of the asgPlusExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link AsgPlusExpr }
     *     
     */
    public void setAsgPlusExpr(AsgPlusExpr value) {
        this.asgPlusExpr = value;
    }

    /**
     * Gets the value of the asgMinusExpr property.
     * 
     * @return
     *     possible object is
     *     {@link AsgMinusExpr }
     *     
     */
    public AsgMinusExpr getAsgMinusExpr() {
        return asgMinusExpr;
    }

    /**
     * Sets the value of the asgMinusExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link AsgMinusExpr }
     *     
     */
    public void setAsgMinusExpr(AsgMinusExpr value) {
        this.asgMinusExpr = value;
    }

    /**
     * Gets the value of the asgMulExpr property.
     * 
     * @return
     *     possible object is
     *     {@link AsgMulExpr }
     *     
     */
    public AsgMulExpr getAsgMulExpr() {
        return asgMulExpr;
    }

    /**
     * Sets the value of the asgMulExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link AsgMulExpr }
     *     
     */
    public void setAsgMulExpr(AsgMulExpr value) {
        this.asgMulExpr = value;
    }

    /**
     * Gets the value of the asgDivExpr property.
     * 
     * @return
     *     possible object is
     *     {@link AsgDivExpr }
     *     
     */
    public AsgDivExpr getAsgDivExpr() {
        return asgDivExpr;
    }

    /**
     * Sets the value of the asgDivExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link AsgDivExpr }
     *     
     */
    public void setAsgDivExpr(AsgDivExpr value) {
        this.asgDivExpr = value;
    }

    /**
     * Gets the value of the asgModExpr property.
     * 
     * @return
     *     possible object is
     *     {@link AsgModExpr }
     *     
     */
    public AsgModExpr getAsgModExpr() {
        return asgModExpr;
    }

    /**
     * Sets the value of the asgModExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link AsgModExpr }
     *     
     */
    public void setAsgModExpr(AsgModExpr value) {
        this.asgModExpr = value;
    }

    /**
     * Gets the value of the asgLshiftExpr property.
     * 
     * @return
     *     possible object is
     *     {@link AsgLshiftExpr }
     *     
     */
    public AsgLshiftExpr getAsgLshiftExpr() {
        return asgLshiftExpr;
    }

    /**
     * Sets the value of the asgLshiftExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link AsgLshiftExpr }
     *     
     */
    public void setAsgLshiftExpr(AsgLshiftExpr value) {
        this.asgLshiftExpr = value;
    }

    /**
     * Gets the value of the asgRshiftExpr property.
     * 
     * @return
     *     possible object is
     *     {@link AsgRshiftExpr }
     *     
     */
    public AsgRshiftExpr getAsgRshiftExpr() {
        return asgRshiftExpr;
    }

    /**
     * Sets the value of the asgRshiftExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link AsgRshiftExpr }
     *     
     */
    public void setAsgRshiftExpr(AsgRshiftExpr value) {
        this.asgRshiftExpr = value;
    }

    /**
     * Gets the value of the asgBitAndExpr property.
     * 
     * @return
     *     possible object is
     *     {@link AsgBitAndExpr }
     *     
     */
    public AsgBitAndExpr getAsgBitAndExpr() {
        return asgBitAndExpr;
    }

    /**
     * Sets the value of the asgBitAndExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link AsgBitAndExpr }
     *     
     */
    public void setAsgBitAndExpr(AsgBitAndExpr value) {
        this.asgBitAndExpr = value;
    }

    /**
     * Gets the value of the asgBitOrExpr property.
     * 
     * @return
     *     possible object is
     *     {@link AsgBitOrExpr }
     *     
     */
    public AsgBitOrExpr getAsgBitOrExpr() {
        return asgBitOrExpr;
    }

    /**
     * Sets the value of the asgBitOrExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link AsgBitOrExpr }
     *     
     */
    public void setAsgBitOrExpr(AsgBitOrExpr value) {
        this.asgBitOrExpr = value;
    }

    /**
     * Gets the value of the asgBitXorExpr property.
     * 
     * @return
     *     possible object is
     *     {@link AsgBitXorExpr }
     *     
     */
    public AsgBitXorExpr getAsgBitXorExpr() {
        return asgBitXorExpr;
    }

    /**
     * Sets the value of the asgBitXorExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link AsgBitXorExpr }
     *     
     */
    public void setAsgBitXorExpr(AsgBitXorExpr value) {
        this.asgBitXorExpr = value;
    }

    /**
     * Gets the value of the logEQExpr property.
     * 
     * @return
     *     possible object is
     *     {@link LogEQExpr }
     *     
     */
    public LogEQExpr getLogEQExpr() {
        return logEQExpr;
    }

    /**
     * Sets the value of the logEQExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link LogEQExpr }
     *     
     */
    public void setLogEQExpr(LogEQExpr value) {
        this.logEQExpr = value;
    }

    /**
     * Gets the value of the logNEQExpr property.
     * 
     * @return
     *     possible object is
     *     {@link LogNEQExpr }
     *     
     */
    public LogNEQExpr getLogNEQExpr() {
        return logNEQExpr;
    }

    /**
     * Sets the value of the logNEQExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link LogNEQExpr }
     *     
     */
    public void setLogNEQExpr(LogNEQExpr value) {
        this.logNEQExpr = value;
    }

    /**
     * Gets the value of the logGEExpr property.
     * 
     * @return
     *     possible object is
     *     {@link LogGEExpr }
     *     
     */
    public LogGEExpr getLogGEExpr() {
        return logGEExpr;
    }

    /**
     * Sets the value of the logGEExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link LogGEExpr }
     *     
     */
    public void setLogGEExpr(LogGEExpr value) {
        this.logGEExpr = value;
    }

    /**
     * Gets the value of the logGTExpr property.
     * 
     * @return
     *     possible object is
     *     {@link LogGTExpr }
     *     
     */
    public LogGTExpr getLogGTExpr() {
        return logGTExpr;
    }

    /**
     * Sets the value of the logGTExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link LogGTExpr }
     *     
     */
    public void setLogGTExpr(LogGTExpr value) {
        this.logGTExpr = value;
    }

    /**
     * Gets the value of the logLEExpr property.
     * 
     * @return
     *     possible object is
     *     {@link LogLEExpr }
     *     
     */
    public LogLEExpr getLogLEExpr() {
        return logLEExpr;
    }

    /**
     * Sets the value of the logLEExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link LogLEExpr }
     *     
     */
    public void setLogLEExpr(LogLEExpr value) {
        this.logLEExpr = value;
    }

    /**
     * Gets the value of the logLTExpr property.
     * 
     * @return
     *     possible object is
     *     {@link LogLTExpr }
     *     
     */
    public LogLTExpr getLogLTExpr() {
        return logLTExpr;
    }

    /**
     * Sets the value of the logLTExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link LogLTExpr }
     *     
     */
    public void setLogLTExpr(LogLTExpr value) {
        this.logLTExpr = value;
    }

    /**
     * Gets the value of the logAndExpr property.
     * 
     * @return
     *     possible object is
     *     {@link LogAndExpr }
     *     
     */
    public LogAndExpr getLogAndExpr() {
        return logAndExpr;
    }

    /**
     * Sets the value of the logAndExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link LogAndExpr }
     *     
     */
    public void setLogAndExpr(LogAndExpr value) {
        this.logAndExpr = value;
    }

    /**
     * Gets the value of the logOrExpr property.
     * 
     * @return
     *     possible object is
     *     {@link LogOrExpr }
     *     
     */
    public LogOrExpr getLogOrExpr() {
        return logOrExpr;
    }

    /**
     * Sets the value of the logOrExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link LogOrExpr }
     *     
     */
    public void setLogOrExpr(LogOrExpr value) {
        this.logOrExpr = value;
    }

    /**
     * Gets the value of the unaryMinusExpr property.
     * 
     * @return
     *     possible object is
     *     {@link UnaryMinusExpr }
     *     
     */
    public UnaryMinusExpr getUnaryMinusExpr() {
        return unaryMinusExpr;
    }

    /**
     * Sets the value of the unaryMinusExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link UnaryMinusExpr }
     *     
     */
    public void setUnaryMinusExpr(UnaryMinusExpr value) {
        this.unaryMinusExpr = value;
    }

    /**
     * Gets the value of the bitNotExpr property.
     * 
     * @return
     *     possible object is
     *     {@link BitNotExpr }
     *     
     */
    public BitNotExpr getBitNotExpr() {
        return bitNotExpr;
    }

    /**
     * Sets the value of the bitNotExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link BitNotExpr }
     *     
     */
    public void setBitNotExpr(BitNotExpr value) {
        this.bitNotExpr = value;
    }

    /**
     * Gets the value of the logNotExpr property.
     * 
     * @return
     *     possible object is
     *     {@link LogNotExpr }
     *     
     */
    public LogNotExpr getLogNotExpr() {
        return logNotExpr;
    }

    /**
     * Sets the value of the logNotExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link LogNotExpr }
     *     
     */
    public void setLogNotExpr(LogNotExpr value) {
        this.logNotExpr = value;
    }

    /**
     * Gets the value of the functionCall property.
     * 
     * @return
     *     possible object is
     *     {@link FunctionCall }
     *     
     */
    public FunctionCall getFunctionCall() {
        return functionCall;
    }

    /**
     * Sets the value of the functionCall property.
     * 
     * @param value
     *     allowed object is
     *     {@link FunctionCall }
     *     
     */
    public void setFunctionCall(FunctionCall value) {
        this.functionCall = value;
    }

    /**
     * Gets the value of the commaExpr property.
     * 
     * @return
     *     possible object is
     *     {@link CommaExpr }
     *     
     */
    public CommaExpr getCommaExpr() {
        return commaExpr;
    }

    /**
     * Sets the value of the commaExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link CommaExpr }
     *     
     */
    public void setCommaExpr(CommaExpr value) {
        this.commaExpr = value;
    }

    /**
     * Gets the value of the postIncrExpr property.
     * 
     * @return
     *     possible object is
     *     {@link PostIncrExpr }
     *     
     */
    public PostIncrExpr getPostIncrExpr() {
        return postIncrExpr;
    }

    /**
     * Sets the value of the postIncrExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link PostIncrExpr }
     *     
     */
    public void setPostIncrExpr(PostIncrExpr value) {
        this.postIncrExpr = value;
    }

    /**
     * Gets the value of the postDecrExpr property.
     * 
     * @return
     *     possible object is
     *     {@link PostDecrExpr }
     *     
     */
    public PostDecrExpr getPostDecrExpr() {
        return postDecrExpr;
    }

    /**
     * Sets the value of the postDecrExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link PostDecrExpr }
     *     
     */
    public void setPostDecrExpr(PostDecrExpr value) {
        this.postDecrExpr = value;
    }

    /**
     * Gets the value of the preIncrExpr property.
     * 
     * @return
     *     possible object is
     *     {@link PreIncrExpr }
     *     
     */
    public PreIncrExpr getPreIncrExpr() {
        return preIncrExpr;
    }

    /**
     * Sets the value of the preIncrExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link PreIncrExpr }
     *     
     */
    public void setPreIncrExpr(PreIncrExpr value) {
        this.preIncrExpr = value;
    }

    /**
     * Gets the value of the preDecrExpr property.
     * 
     * @return
     *     possible object is
     *     {@link PreDecrExpr }
     *     
     */
    public PreDecrExpr getPreDecrExpr() {
        return preDecrExpr;
    }

    /**
     * Sets the value of the preDecrExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link PreDecrExpr }
     *     
     */
    public void setPreDecrExpr(PreDecrExpr value) {
        this.preDecrExpr = value;
    }

    /**
     * Gets the value of the castExpr property.
     * 
     * @return
     *     possible object is
     *     {@link CastExpr }
     *     
     */
    public CastExpr getCastExpr() {
        return castExpr;
    }

    /**
     * Sets the value of the castExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link CastExpr }
     *     
     */
    public void setCastExpr(CastExpr value) {
        this.castExpr = value;
    }

    /**
     * Gets the value of the condExpr property.
     * 
     * @return
     *     possible object is
     *     {@link CondExpr }
     *     
     */
    public CondExpr getCondExpr() {
        return condExpr;
    }

    /**
     * Sets the value of the condExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link CondExpr }
     *     
     */
    public void setCondExpr(CondExpr value) {
        this.condExpr = value;
    }

    /**
     * Gets the value of the sizeOfExpr property.
     * 
     * @return
     *     possible object is
     *     {@link SizeOfExpr }
     *     
     */
    public SizeOfExpr getSizeOfExpr() {
        return sizeOfExpr;
    }

    /**
     * Sets the value of the sizeOfExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link SizeOfExpr }
     *     
     */
    public void setSizeOfExpr(SizeOfExpr value) {
        this.sizeOfExpr = value;
    }

    /**
     * Gets the value of the addrOfExpr property.
     * 
     * @return
     *     possible object is
     *     {@link AddrOfExpr }
     *     
     */
    public AddrOfExpr getAddrOfExpr() {
        return addrOfExpr;
    }

    /**
     * Sets the value of the addrOfExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link AddrOfExpr }
     *     
     */
    public void setAddrOfExpr(AddrOfExpr value) {
        this.addrOfExpr = value;
    }

    /**
     * Gets the value of the xmpDescOf property.
     * 
     * @return
     *     possible object is
     *     {@link XmpDescOf }
     *     
     */
    public XmpDescOf getXmpDescOf() {
        return xmpDescOf;
    }

    /**
     * Sets the value of the xmpDescOf property.
     * 
     * @param value
     *     allowed object is
     *     {@link XmpDescOf }
     *     
     */
    public void setXmpDescOf(XmpDescOf value) {
        this.xmpDescOf = value;
    }

    /**
     * Gets the value of the compoundValue property.
     * 
     * @return
     *     possible object is
     *     {@link CompoundValueExpr }
     *     
     */
    public CompoundValueExpr getCompoundValue() {
        return compoundValue;
    }

    /**
     * Sets the value of the compoundValue property.
     * 
     * @param value
     *     allowed object is
     *     {@link CompoundValueExpr }
     *     
     */
    public void setCompoundValue(CompoundValueExpr value) {
        this.compoundValue = value;
    }

    /**
     * Gets the value of the compoundValueAddr property.
     * 
     * @return
     *     possible object is
     *     {@link CompoundValueAddr }
     *     
     */
    public CompoundValueAddr getCompoundValueAddr() {
        return compoundValueAddr;
    }

    /**
     * Sets the value of the compoundValueAddr property.
     * 
     * @param value
     *     allowed object is
     *     {@link CompoundValueAddr }
     *     
     */
    public void setCompoundValueAddr(CompoundValueAddr value) {
        this.compoundValueAddr = value;
    }

    /**
     * Gets the value of the gccAlignOfExpr property.
     * 
     * @return
     *     possible object is
     *     {@link GccAlignOfExpr }
     *     
     */
    public GccAlignOfExpr getGccAlignOfExpr() {
        return gccAlignOfExpr;
    }

    /**
     * Sets the value of the gccAlignOfExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link GccAlignOfExpr }
     *     
     */
    public void setGccAlignOfExpr(GccAlignOfExpr value) {
        this.gccAlignOfExpr = value;
    }

    /**
     * Gets the value of the gccLabelAddr property.
     * 
     * @return
     *     possible object is
     *     {@link GccLabelAddr }
     *     
     */
    public GccLabelAddr getGccLabelAddr() {
        return gccLabelAddr;
    }

    /**
     * Sets the value of the gccLabelAddr property.
     * 
     * @param value
     *     allowed object is
     *     {@link GccLabelAddr }
     *     
     */
    public void setGccLabelAddr(GccLabelAddr value) {
        this.gccLabelAddr = value;
    }

    /**
     * Gets the value of the gccCompoundExpr property.
     * 
     * @return
     *     possible object is
     *     {@link GccCompoundExpr }
     *     
     */
    public GccCompoundExpr getGccCompoundExpr() {
        return gccCompoundExpr;
    }

    /**
     * Sets the value of the gccCompoundExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link GccCompoundExpr }
     *     
     */
    public void setGccCompoundExpr(GccCompoundExpr value) {
        this.gccCompoundExpr = value;
    }

    /**
     * Gets the value of the builtinOp property.
     * 
     * @return
     *     possible object is
     *     {@link BuiltinOp }
     *     
     */
    public BuiltinOp getBuiltinOp() {
        return builtinOp;
    }

    /**
     * Sets the value of the builtinOp property.
     * 
     * @param value
     *     allowed object is
     *     {@link BuiltinOp }
     *     
     */
    public void setBuiltinOp(BuiltinOp value) {
        this.builtinOp = value;
    }

    /**
     * Gets the value of the subArrayRef property.
     * 
     * @return
     *     possible object is
     *     {@link SubArrayRef }
     *     
     */
    public SubArrayRef getSubArrayRef() {
        return subArrayRef;
    }

    /**
     * Sets the value of the subArrayRef property.
     * 
     * @param value
     *     allowed object is
     *     {@link SubArrayRef }
     *     
     */
    public void setSubArrayRef(SubArrayRef value) {
        this.subArrayRef = value;
    }

    /**
     * Gets the value of the coArrayRef property.
     * 
     * @return
     *     possible object is
     *     {@link CoArrayRef }
     *     
     */
    public CoArrayRef getCoArrayRef() {
        return coArrayRef;
    }

    /**
     * Sets the value of the coArrayRef property.
     * 
     * @param value
     *     allowed object is
     *     {@link CoArrayRef }
     *     
     */
    public void setCoArrayRef(CoArrayRef value) {
        this.coArrayRef = value;
    }

    /**
     * Gets the value of the coArrayAssignExpr property.
     * 
     * @return
     *     possible object is
     *     {@link CoArrayAssignExpr }
     *     
     */
    public CoArrayAssignExpr getCoArrayAssignExpr() {
        return coArrayAssignExpr;
    }

    /**
     * Sets the value of the coArrayAssignExpr property.
     * 
     * @param value
     *     allowed object is
     *     {@link CoArrayAssignExpr }
     *     
     */
    public void setCoArrayAssignExpr(CoArrayAssignExpr value) {
        this.coArrayAssignExpr = value;
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
