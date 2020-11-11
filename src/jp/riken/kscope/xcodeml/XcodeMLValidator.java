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

package jp.riken.kscope.xcodeml;

import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.xcodeml.util.XmlNodeUtil;
import jp.riken.kscope.xcodeml.xml.EnumError;
import jp.riken.kscope.xcodeml.xml.IXmlNode;
import jp.riken.kscope.xcodeml.xml.gen.*;

/** XcodeML/F validation utility. */
public class XcodeMLValidator {
  /** Error message */
  private String errorDescription;

  /**
   * Get error description
   *
   * @return error message
   */
  public String getErrDesc() {
    return errorDescription;
  }

  /**
   * Validate attributes of 'FbasicType'
   *
   * @param obj obj is validated aboute attributes.
   * @return false if there are insufficient attributes.
   */
  public final boolean validAttr(FbasicType obj) {
    if (obj == null) throw new IllegalArgumentException();

    if (StringUtils.isNullOrEmpty(obj.getType())) {
      errorDescription =
          XmlNodeUtil.formatError(
              obj, EnumError.XCODEML_NEED_ATTR, "type", XmlNodeUtil.getElementName(obj));
      return false;
    }

    if (StringUtils.isNullOrEmpty(obj.getRef())) {
      errorDescription =
          XmlNodeUtil.formatError(
              obj, EnumError.XCODEML_NEED_ATTR, "ref", XmlNodeUtil.getElementName(obj));
      return false;
    }

    return true;
  }

  /**
   * Check the FfunctionType attribute
   *
   * @param obj FfunctionType
   * @return false = attribute error
   */
  public final boolean validAttr(FfunctionType obj) {
    if (obj == null) throw new IllegalArgumentException();

    if (StringUtils.isNullOrEmpty(obj.getType())) {
      errorDescription =
          XmlNodeUtil.formatError(
              obj, EnumError.XCODEML_NEED_ATTR, "type", XmlNodeUtil.getElementName(obj));
      return false;
    }

    if (StringUtils.isNullOrEmpty(obj.getReturnType())) {
      errorDescription =
          XmlNodeUtil.formatError(
              obj, EnumError.XCODEML_NEED_ATTR, "return_type", XmlNodeUtil.getElementName(obj));
      return false;
    }

    return true;
  }

  /**
   * Check the FstructType attribute
   *
   * @param obj FstructType
   * @return false = attribute error
   */
  public final boolean validAttr(FstructType obj) {
    if (obj == null) throw new IllegalArgumentException();

    if (StringUtils.isNullOrEmpty(obj.getType())) {
      errorDescription =
          XmlNodeUtil.formatError(
              obj, EnumError.XCODEML_NEED_ATTR, "type", XmlNodeUtil.getElementName(obj));
      return false;
    }

    return true;
  }

  /**
   * Check the FmoduleDefinition attribute
   *
   * @param obj FmoduleDefinition
   * @return false = attribute error
   */
  public final boolean validAttr(FmoduleDefinition obj) {
    if (obj == null) throw new IllegalArgumentException();

    if (StringUtils.isNullOrEmpty(obj.getName())) {
      errorDescription =
          XmlNodeUtil.formatError(
              obj, EnumError.XCODEML_NEED_ATTR, "name", XmlNodeUtil.getElementName(obj));
      return false;
    }

    return true;
  }

  /**
   * Check the FblockDataDefinition attribute
   *
   * @param obj FblockDataDefinition
   * @return false = attribute error
   */
  public final boolean validAttr(FblockDataDefinition obj) {
    if (obj == null) throw new IllegalArgumentException();

    if (StringUtils.isNullOrEmpty(obj.getName())) {
      errorDescription =
          XmlNodeUtil.formatError(
              obj, EnumError.XCODEML_NEED_ATTR, "name", XmlNodeUtil.getElementName(obj));
      return false;
    }

    return true;
  }

  /**
   * Check the FuseDecl attribute
   *
   * @param obj FuseDecl
   * @return false = attribute error
   */
  public final boolean validAttr(FuseDecl obj) {
    if (obj == null) throw new IllegalArgumentException();

    if (StringUtils.isNullOrEmpty(obj.getName())) {
      errorDescription =
          XmlNodeUtil.formatError(
              obj, EnumError.XCODEML_NEED_ATTR, "name", XmlNodeUtil.getElementName(obj));
      return false;
    }

    return true;
  }

  /**
   * Check the FuseOnlyDecl attribute
   *
   * @param obj FuseOnlyDecl
   * @return false = attribute error
   */
  public final boolean validAttr(FuseOnlyDecl obj) {
    if (obj == null) throw new IllegalArgumentException();

    if (StringUtils.isNullOrEmpty(obj.getName())) {
      errorDescription =
          XmlNodeUtil.formatError(
              obj, EnumError.XCODEML_NEED_ATTR, "name", XmlNodeUtil.getElementName(obj));
      return false;
    }

    return true;
  }

  /**
   * Check StatementLabel attribute
   *
   * @param obj Statement Label
   * @return false = attribute error
   */
  public final boolean validAttr(StatementLabel obj) {
    if (obj == null) throw new IllegalArgumentException();

    if (StringUtils.isNullOrEmpty(obj.getLabelName())) {
      errorDescription =
          XmlNodeUtil.formatError(
              obj, EnumError.XCODEML_NEED_ATTR, "label_name", XmlNodeUtil.getElementName(obj));
      return false;
    }

    return true;
  }

  /**
   * Check the FprintStatement attribute
   *
   * @param obj FprintStatement
   * @return false = attribute error
   */
  public final boolean validAttr(FprintStatement obj) {
    if (obj == null) throw new IllegalArgumentException();

    if (StringUtils.isNullOrEmpty(obj.getFormat())) {
      errorDescription =
          XmlNodeUtil.formatError(
              obj, EnumError.XCODEML_NEED_ATTR, "format", XmlNodeUtil.getElementName(obj));
      return false;
    }

    return true;
  }

  /**
   * Check the FformatDecl attribute
   *
   * @param obj FformatDecl
   * @return false = attribute error
   */
  public final boolean validAttr(FformatDecl obj) {
    if (obj == null) throw new IllegalArgumentException();

    if (StringUtils.isNullOrEmpty(obj.getFormat())) {
      errorDescription =
          XmlNodeUtil.formatError(
              obj, EnumError.XCODEML_NEED_ATTR, "format", XmlNodeUtil.getElementName(obj));
      return false;
    }

    return true;
  }

  /**
   * Check the FmemberRef attribute
   *
   * @param obj FmemberRef
   * @return false = attribute error
   */
  public final boolean validAttr(FmemberRef obj) {
    if (obj == null) throw new IllegalArgumentException();

    if (StringUtils.isNullOrEmpty(obj.getMember())) {
      errorDescription =
          XmlNodeUtil.formatError(
              obj, EnumError.XCODEML_NEED_ATTR, "member", XmlNodeUtil.getElementName(obj));
      return false;
    }

    return true;
  }

  /**
   * Check the Id attribute
   *
   * @param obj Id
   * @return false = attribute error
   */
  public final boolean validAttr(Id obj) {
    if (obj == null) throw new IllegalArgumentException();

    if (StringUtils.isNullOrEmpty(obj.getSclass().value())) {
      errorDescription =
          XmlNodeUtil.formatError(
              obj, EnumError.XCODEML_NEED_ATTR, "sclass", XmlNodeUtil.getElementName(obj));
      return false;
    }

    if (StringUtils.isNullOrEmpty(obj.getType())) {
      errorDescription =
          XmlNodeUtil.formatError(
              obj, EnumError.XCODEML_NEED_ATTR, "type", XmlNodeUtil.getElementName(obj));
      return false;
    }

    return true;
  }

  /**
   * Check the Name attribute
   *
   * @param obj Name
   * @return false = attribute error
   */
  public final boolean validAttr(Name obj) {
    if (obj == null) throw new IllegalArgumentException();

    if (StringUtils.isNullOrEmpty(obj.getType())) {
      errorDescription =
          XmlNodeUtil.formatError(
              obj, EnumError.XCODEML_NEED_ATTR, "type", XmlNodeUtil.getElementName(obj));
      return false;
    }

    return true;
  }

  /**
   * Check the Rename attribute
   *
   * @param obj Rename
   * @return false = attribute error
   */
  public final boolean validAttr(Rename obj) {
    if (obj == null) throw new IllegalArgumentException();

    if (StringUtils.isNullOrEmpty(obj.getUseName())) {
      errorDescription =
          XmlNodeUtil.formatError(
              obj, EnumError.XCODEML_NEED_ATTR, "use_name", XmlNodeUtil.getElementName(obj));
      return false;
    }

    if (StringUtils.isNullOrEmpty(obj.getLocalName())) {
      errorDescription =
          XmlNodeUtil.formatError(
              obj, EnumError.XCODEML_NEED_ATTR, "local_name", XmlNodeUtil.getElementName(obj));
      return false;
    }

    return true;
  }

  /**
   * Check Renamable attribute
   *
   * @param obj Renamable
   * @return false = attribute error
   */
  public final boolean validAttr(Renamable obj) {
    if (obj == null) throw new IllegalArgumentException();

    if (StringUtils.isNullOrEmpty(obj.getUseName())) {
      errorDescription =
          XmlNodeUtil.formatError(
              obj, EnumError.XCODEML_NEED_ATTR, "use_name", XmlNodeUtil.getElementName(obj));
      return false;
    }

    return true;
  }

  /**
   * Check the NamedValue attribute
   *
   * @param obj NamedValue
   * @return false = attribute error
   */
  public final boolean validAttr(NamedValue obj) {
    if (obj == null) throw new IllegalArgumentException();

    if (StringUtils.isNullOrEmpty(obj.getName())) {
      errorDescription =
          XmlNodeUtil.formatError(
              obj, EnumError.XCODEML_NEED_ATTR, "name", XmlNodeUtil.getElementName(obj));
      return false;
    }

    return true;
  }

  /*
   * NOTE: Required attributes are follows.
   *
   * FbasicType.type FbasicType.ref FfunctionType.type
   * FfunctionType.return_type FstructType.type FmoduleDefinition.name
   * FuseDecl.name FuseOnlyDecl.name StatementLabel.label_name
   * FPrintStatement.format FformatDecl.format FmemberRef.member id.sclass
   * id.type name.type rename.use_name rename.local_name renamable.use_name
   * renamable.local_name namedValue.name (anyStatement).lineno
   * (anyStatement).file
   */

  /**
   * Check the IXmlNode attribute
   *
   * @param node IXmlNode
   * @return false = attribute error
   */
  public final boolean validAttr(IXmlNode node) {
    if (node instanceof FbasicType) return validAttr((FbasicType) node);
    if (node instanceof FfunctionType) return validAttr((FfunctionType) node);
    if (node instanceof FstructType) return validAttr((FstructType) node);
    if (node instanceof FmoduleDefinition) return validAttr((FmoduleDefinition) node);
    if (node instanceof FuseDecl) return validAttr((FuseDecl) node);
    if (node instanceof FuseOnlyDecl) return validAttr((FuseOnlyDecl) node);
    if (node instanceof StatementLabel) return validAttr((StatementLabel) node);
    if (node instanceof FprintStatement) return validAttr((FprintStatement) node);
    if (node instanceof FformatDecl) return validAttr((FformatDecl) node);
    if (node instanceof FmemberRef) return validAttr((FmemberRef) node);

    return true;
  }
}
