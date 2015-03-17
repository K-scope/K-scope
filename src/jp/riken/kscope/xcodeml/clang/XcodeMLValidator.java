package jp.riken.kscope.xcodeml.clang;

import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.xcodeml.EnumError;
import jp.riken.kscope.xcodeml.clang.utils.XmlNodeUtil;
import jp.riken.kscope.xcodeml.clang.xml.IXmlNode;
import jp.riken.kscope.xcodeml.clang.xml.gen.ArrayType;
import jp.riken.kscope.xcodeml.clang.xml.gen.BasicType;
import jp.riken.kscope.xcodeml.clang.xml.gen.CoArrayType;
import jp.riken.kscope.xcodeml.clang.xml.gen.EnumType;
import jp.riken.kscope.xcodeml.clang.xml.gen.ExprStatement;
import jp.riken.kscope.xcodeml.clang.xml.gen.FunctionDecl;
import jp.riken.kscope.xcodeml.clang.xml.gen.FunctionDefinition;
import jp.riken.kscope.xcodeml.clang.xml.gen.FunctionType;
import jp.riken.kscope.xcodeml.clang.xml.gen.Id;
import jp.riken.kscope.xcodeml.clang.xml.gen.Name;
import jp.riken.kscope.xcodeml.clang.xml.gen.PointerType;
import jp.riken.kscope.xcodeml.clang.xml.gen.StatementLabel;
import jp.riken.kscope.xcodeml.clang.xml.gen.StructType;
import jp.riken.kscope.xcodeml.clang.xml.gen.UnionType;
import jp.riken.kscope.xcodeml.clang.xml.gen.VarDecl;

/**
 * XcodeML/C validation utility.
 */
public class XcodeMLValidator {

    /** エラーメッセージ */
    private String errorDescription;

    /**
     * Get error description
     * @return  エラーメッセージ
     */
    public String getErrDesc() {
        return errorDescription;
    }


    /**
     * BasicType属性をチェックする
     * @param obj		BasicType
     * @return		false=属性エラー
     */
    public final boolean validate(BasicType obj) {
        if (obj == null)
            throw new IllegalArgumentException();

        if (StringUtils.isNullOrEmpty(obj.getType())) {
            errorDescription = XmlNodeUtil.formatError(obj,
                    EnumError.XCODEML_NEED_ATTR, "type",
                    XmlNodeUtil.getElementName(obj));
            return false;
        }

        if (StringUtils.isNullOrEmpty(obj.getName())) {
            errorDescription = XmlNodeUtil.formatError(obj,
                    EnumError.XCODEML_NEED_ATTR, "name",
                    XmlNodeUtil.getElementName(obj));
            return false;
        }

        return true;
    }

    /**
     * PointerType属性をチェックする
     * @param obj		PointerType
     * @return		false=属性エラー
     */
    public final boolean validate(PointerType obj) {
        if (obj == null)
            throw new IllegalArgumentException();

        if (StringUtils.isNullOrEmpty(obj.getType())) {
            errorDescription = XmlNodeUtil.formatError(obj,
                    EnumError.XCODEML_NEED_ATTR, "type",
                    XmlNodeUtil.getElementName(obj));
            return false;
        }

        if (StringUtils.isNullOrEmpty(obj.getRef())) {
            errorDescription = XmlNodeUtil.formatError(obj,
                    EnumError.XCODEML_NEED_ATTR, "ref",
                    XmlNodeUtil.getElementName(obj));
            return false;
        }

        return true;
    }

    /**
     * FunctionType属性をチェックする
     * @param obj		FunctionType
     * @return		false=属性エラー
     */
    public final boolean validate(FunctionType obj) {
        if (obj == null)
            throw new IllegalArgumentException();

        if (StringUtils.isNullOrEmpty(obj.getType())) {
            errorDescription = XmlNodeUtil.formatError(obj,
                    EnumError.XCODEML_NEED_ATTR, "type",
                    XmlNodeUtil.getElementName(obj));
            return false;
        }

        if (StringUtils.isNullOrEmpty(obj.getReturnType())) {
            errorDescription = XmlNodeUtil.formatError(obj,
                    EnumError.XCODEML_NEED_ATTR, "return_type",
                    XmlNodeUtil.getElementName(obj));
            return false;
        }

        return true;
    }


    /**
     * ArrayType属性をチェックする
     * @param obj		ArrayType
     * @return		false=属性エラー
     */
    public final boolean validate(ArrayType obj) {
        if (obj == null)
            throw new IllegalArgumentException();

        if (StringUtils.isNullOrEmpty(obj.getType())) {
            errorDescription = XmlNodeUtil.formatError(obj,
                    EnumError.XCODEML_NEED_ATTR, "type",
                    XmlNodeUtil.getElementName(obj));
            return false;
        }

        if (StringUtils.isNullOrEmpty(obj.getElementType())) {
            errorDescription = XmlNodeUtil.formatError(obj,
                    EnumError.XCODEML_NEED_ATTR, "element_type",
                    XmlNodeUtil.getElementName(obj));
            return false;
        }

        return true;
    }

    /**
     * CoArrayType属性をチェックする
     * @param obj		CoArrayType
     * @return		false=属性エラー
     */
    public final boolean validate(CoArrayType obj) {
        if (obj == null)
            throw new IllegalArgumentException();

        if (StringUtils.isNullOrEmpty(obj.getType())) {
            errorDescription = XmlNodeUtil.formatError(obj,
                    EnumError.XCODEML_NEED_ATTR, "type",
                    XmlNodeUtil.getElementName(obj));
            return false;
        }

        if (StringUtils.isNullOrEmpty(obj.getElementType())) {
            errorDescription = XmlNodeUtil.formatError(obj,
                    EnumError.XCODEML_NEED_ATTR, "element_type",
                    XmlNodeUtil.getElementName(obj));
            return false;
        }

        return true;
    }

    /**
     * StructType属性をチェックする
     * @param obj		StructType
     * @return		false=属性エラー
     */
    public final boolean validate(StructType obj) {
        if (obj == null)
            throw new IllegalArgumentException();

        if (StringUtils.isNullOrEmpty(obj.getType())) {
            errorDescription = XmlNodeUtil.formatError(obj,
                    EnumError.XCODEML_NEED_ATTR, "type",
                    XmlNodeUtil.getElementName(obj));
            return false;
        }

        return true;
    }

    /**
     * UnionType属性をチェックする
     * @param obj		UnionType
     * @return		false=属性エラー
     */
    public final boolean validate(UnionType obj) {
        if (obj == null)
            throw new IllegalArgumentException();

        if (StringUtils.isNullOrEmpty(obj.getType())) {
            errorDescription = XmlNodeUtil.formatError(obj,
                    EnumError.XCODEML_NEED_ATTR, "type",
                    XmlNodeUtil.getElementName(obj));
            return false;
        }

        return true;
    }

    /**
     * EnumType属性をチェックする
     * @param obj		EnumType
     * @return		false=属性エラー
     */
    public final boolean validate(EnumType obj) {
        if (obj == null)
            throw new IllegalArgumentException();

        if (StringUtils.isNullOrEmpty(obj.getType())) {
            errorDescription = XmlNodeUtil.formatError(obj,
                    EnumError.XCODEML_NEED_ATTR, "type",
                    XmlNodeUtil.getElementName(obj));
            return false;
        }

        return true;
    }

    /**
     * FunctionDefinition属性をチェックする
     * @param obj		FunctionDefinition
     * @return		false=属性エラー
     */
    public final boolean validate(FunctionDefinition obj) {
        if (obj == null)
            throw new IllegalArgumentException();

        Name name = obj.getName();
        if (!validateName(obj.getName())) {
            errorDescription = XmlNodeUtil.formatError(obj,
                    EnumError.XCODEML_NEED_ELEMENT, "name",
                    XmlNodeUtil.getElementName(obj));
            return false;
        }

        return false;
    }

    /**
     * VarDecl属性をチェックする
     * @param obj		VarDecl
     * @return		false=属性エラー
     */
    public final boolean validate(VarDecl obj) {
        if (obj == null)
            throw new IllegalArgumentException();

        Name name = obj.getName();
        if (!validateName(obj.getName())) {
            errorDescription = XmlNodeUtil.formatError(obj,
                    EnumError.XCODEML_NEED_ELEMENT, "name",
                    XmlNodeUtil.getElementName(obj));
            return false;
        }

        return true;
    }

    /**
     * FunctionDecl属性をチェックする
     * @param obj		FunctionDecl
     * @return		false=属性エラー
     */
    public final boolean validate(FunctionDecl obj) {
        if (obj == null)
            throw new IllegalArgumentException();

        Name name = obj.getName();
        if (!validateName(obj.getName())) {
            errorDescription = XmlNodeUtil.formatError(obj,
                    EnumError.XCODEML_NEED_ELEMENT, "name",
                    XmlNodeUtil.getElementName(obj));
            return false;
        }

        return true;
    }


    /**
     * StatementLabel属性をチェックする
     * @param obj		StatementLabel
     * @return		false=属性エラー
     */
    public final boolean validate(StatementLabel obj) {
        if (obj == null)
            throw new IllegalArgumentException();

        Name name = obj.getName();
        if (!validateName(obj.getName())) {
            errorDescription = XmlNodeUtil.formatError(obj,
                    EnumError.XCODEML_NEED_ELEMENT, "name",
                    XmlNodeUtil.getElementName(obj));
            return false;
        }

        return true;
    }

    /**
     * Id属性をチェックする
     * @param obj		Id
     * @return		false=属性エラー
     */
    public final boolean validate(Id obj) {
        if (obj == null)
            throw new IllegalArgumentException();

        Name name = obj.getName();
        if (!validateName(obj.getName())) {
            errorDescription = XmlNodeUtil.formatError(obj,
                    EnumError.XCODEML_NEED_ELEMENT, "name",
                    XmlNodeUtil.getElementName(obj));
            return false;
        }

        return true;
    }

    /**
     * Name属性をチェックする
     * @param obj		Name
     * @return		false=属性エラー
     */
    public final boolean validate(Name obj) {
        if (obj == null)
            throw new IllegalArgumentException();

        if (!this.validateName(obj)) {
            errorDescription = XmlNodeUtil.formatError(obj,
                    EnumError.XCODEML_NEED_ELEMENT, "name",
                    XmlNodeUtil.getElementName(obj));
            return false;
        }

        return true;
    }

    /**
     * IXmlNode属性をチェックする
     * @param node		IXmlNode
     * @return		false=属性エラー
     */
    public final boolean validate(IXmlNode node) {
        if (node instanceof BasicType)
            return validate((BasicType) node);
        if (node instanceof PointerType)
            return validate((PointerType) node);
        if (node instanceof FunctionType)
            return validate((FunctionType) node);
        if (node instanceof ArrayType)
            return validate((ArrayType) node);
        if (node instanceof CoArrayType)
            return validate((CoArrayType) node);
        if (node instanceof StructType)
            return validate((StructType) node);
        if (node instanceof UnionType)
            return validate((UnionType) node);
        if (node instanceof EnumType)
            return validate((EnumType) node);
        if (node instanceof FunctionDefinition)
            return validate((FunctionDefinition) node);
        if (node instanceof VarDecl)
            return validate((VarDecl) node);
        if (node instanceof FunctionDecl)
            return validate((FunctionDecl) node);
        if (node instanceof StatementLabel)
            return validate((StatementLabel) node);
        if (node instanceof Id)
            return validate((Id) node);
        if (node instanceof Name)
            return validate((Name) node);

        return true;
    }


    /**
     * Name要素をチェックする
     * @param obj		Name
     * @return		false=属性エラー
     */
    private final boolean validateName(Name name) {
        if (name == null) return false;

        if (name != null) {
            if (!StringUtils.isNullOrEmpty(name.getValue())) {
                return true;
            }
        }

        return false;
    }

}
