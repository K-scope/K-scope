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
package jp.riken.kscope.xcodeml.language;

import java.util.ArrayList;
import java.util.List;
import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.language.fortran.VariableType;
import jp.riken.kscope.language.fortran.VariableType.PrimitiveDataType;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.xcodeml.XcodeMLTypeManager;
import jp.riken.kscope.xcodeml.xml.EnumType;
import jp.riken.kscope.xcodeml.xml.IXmlTypeTableChoice;
import jp.riken.kscope.xcodeml.xml.gen.FbasicType;
import jp.riken.kscope.xcodeml.xml.gen.FfunctionType;
import jp.riken.kscope.xcodeml.xml.gen.FstructDecl;
import jp.riken.kscope.xcodeml.xml.gen.FstructType;
import jp.riken.kscope.xcodeml.xml.gen.Id;
import jp.riken.kscope.xcodeml.xml.gen.Kind;
import jp.riken.kscope.xcodeml.xml.gen.Len;
import jp.riken.kscope.xcodeml.xml.gen.Name;
import jp.riken.kscope.xcodeml.xml.gen.Symbols;
import jp.riken.kscope.xcodeml.xml.gen.Var;

/**
 * VariableType parser class
 *
 * @author RIKEN
 */
public class VariableTypeParser {

  /** typeTable */
  private XcodeMLTypeManager typeManager;

  /** Nested list of structures */
  private List<jp.riken.kscope.language.fortran.Type> stackType;

  /**
   * Constructor
   *
   * @param typeManager typeTable
   */
  public VariableTypeParser(XcodeMLTypeManager typeManager) {
    this.typeManager = typeManager;
  }

  /**
   * Parse and generate DB :: VariableType class from XcodeML :: Name class.
   *
   * @param type_name Variable, structure declaration
   * @return DB :: VariableType class
   */
  public VariableType parseVariableType(String type_name) {
    if (type_name == null || type_name.isEmpty()) return null;

    // Variable name
    Var var = new Var();
    var.setType(type_name);

    return parseVariableType(var);
  }

  /**
   * Parse and generate DB :: VariableType class from XcodeML :: Var class. <br>
   *
   * @param var variable, structure name
   * @return DB :: VariableType class
   */
  public VariableType parseVariableType(Var var) {

    // Data type
    VariableType varType = null;
    String type_name = var.getType();
    EnumType type = EnumType.getTypeIdFromXcodemlTypeName(type_name);
    if (type != null && type.isPrimitive()) {
      varType = parseVarDefEnumType(type);
    } else {
      IXmlTypeTableChoice typeChoice = this.typeManager.findType(type_name);
      if (typeChoice instanceof FbasicType) {
        varType = parseVarDefBasicType((FbasicType) typeChoice);
      } else if (typeChoice instanceof FstructType) {
        varType = parseVarDefStructType((FstructType) typeChoice);
      } else if (typeChoice instanceof FfunctionType) {
        varType = parseVarDefFunctionType((FfunctionType) typeChoice);
      }
    }

    return varType;
  }

  /**
   * Parse and generate DB :: VariableType class from XcodeML :: Name class. <br>
   * Do not get the function data type.
   *
   * @param name variable, structure declaration
   * @return DB :: VariableType class
   */
  public VariableType parseVariableType(Name name) {

    // Data type
    VariableType varType = null;
    String type_name = name.getType();
    EnumType type = EnumType.getTypeIdFromXcodemlTypeName(type_name);
    if (type != null && type.isPrimitive()) {
      varType = parseVarDefEnumType(type);
    } else {
      IXmlTypeTableChoice typeChoice = this.typeManager.findType(name);
      if (typeChoice instanceof FbasicType) {
        varType = parseVarDefBasicType((FbasicType) typeChoice);
      } else if (typeChoice instanceof FstructType) {
        varType = parseVarDefStructType((FstructType) typeChoice);
      } else if (typeChoice instanceof FfunctionType) {
        varType = parseVarDefFunctionType((FfunctionType) typeChoice);
      }
    }

    return varType;
  }

  /**
   * Parse and generate DB :: VariableType class from XcodeML :: visitable class.
   *
   * @param visitable Structure declaration
   * @return DB :: VariableType class
   */
  public VariableType parseVariableType(FstructDecl visitable) {

    // Variable name
    Name name = visitable.getName();

    // Data type
    IXmlTypeTableChoice typeChoice = this.typeManager.findType(name);
    if (!(typeChoice instanceof FstructType)) {
      return null;
    }
    FstructType structType = (FstructType) typeChoice;

    return parseVarDefStructType(structType);
  }

  /**
   * Parse and generate DB :: VariableType class from XcodeML :: Name class. <br>
   * Also get the function data type.
   *
   * @param name Variable, structure declaration, function name
   * @return DB :: VariableType class
   */
  public VariableType parseVariableTypeWithFfunctionType(Name name) {

    // Data type
    VariableType varType = parseVariableType(name);
    if (varType != null) {
      return varType;
    }

    IXmlTypeTableChoice typeChoice = this.typeManager.findType(name);
    if (typeChoice instanceof FfunctionType) {
      varType = parseVarDefFunctionType((FfunctionType) typeChoice);
    }

    return varType;
  }

  /**
   * Parse and generate DB :: VariableType class from type definition element.
   *
   * @param type Defining references to primitive types and other type definition elements
   * @return DB :: VariableType class
   */
  public VariableType parseVarDefEnumType(EnumType type) {
    // Data type
    String type_name = type.fortranName();

    PrimitiveDataType primitive = PrimitiveDataType.findTypeBy(type_name);
    VariableType varType = new VariableType(primitive);

    return varType;
  }

  /**
   * Parse and generate DB :: VariableType class from type definition element.
   *
   * @param basicType Defining references to primitive types and other type definition elements
   * @return DB :: VariableType class
   */
  public VariableType parseVarDefBasicType(FbasicType basicType) {

    // Data type
    VariableType varType = null;
    String refName = basicType.getRef();
    EnumType refTypeId = EnumType.getTypeIdFromXcodemlTypeName(refName);
    if (refTypeId == null || refTypeId.isPrimitive() == false) {
      IXmlTypeTableChoice typeChoice = this.typeManager.findType(refName);
      if (typeChoice instanceof FbasicType) {
        varType = parseVarDefBasicType((FbasicType) typeChoice);
      } else if (typeChoice instanceof FstructType) {
        varType = parseVarDefStructType((FstructType) typeChoice);
      } else if (typeChoice instanceof FfunctionType) {
        varType = parseVarDefFunctionType((FfunctionType) typeChoice);
      }
    } else {
      PrimitiveDataType primitive = PrimitiveDataType.findTypeBy(refTypeId.fortranName());
      varType = new VariableType(primitive);
    }
    if (varType == null) return null;

    // ExprModel parser;
    ExpressionParser exprParser = new ExpressionParser(this.typeManager);

    // Number of data bytes
    Kind kindElem = basicType.getKind();
    if (kindElem != null) {
      // Data type parsing
      exprParser.setParseNode(kindElem);
      Expression expr = exprParser.getExpression();
      if (expr != null) {
        varType.setKind(expr);
      }
    }

    // CHARACTER string length
    Len lenElem = basicType.getLen();
    if (lenElem != null) {
      exprParser.setParseNode(lenElem);
      Expression expr = exprParser.getExpression();
      if (expr != null) {
        varType.setLen(expr);
      } else {
        varType.setLen(new Expression("*"));
      }
    }

    return varType;
  }

  /**
   * Parse and generate DB :: VariableType class from structure elements.
   *
   * @param structType Structure definition
   * @return DB :: VariableType class
   */
  public VariableType parseVarDefStructType(FstructType structType) {

    // Structure name
    // String structTypeName = structType.getType();
    String structName = this.typeManager.getAliasTypeName(structType.getType());

    // modify by @hira at 2013/02/01
    if (structName == null) {
      return null;
    }

    // Check if it is a nested structure
    if (this.containsStackType(structName)) {
      // Returns the created structure because it is already defined.
      return new VariableType(this.getStackType(structName));
    }

    // DB :: Type class generation
    jp.riken.kscope.language.fortran.Type typeDef =
        new jp.riken.kscope.language.fortran.Type(structName);
    this.addStackType(typeDef);

    // Variable declaration parser
    VariableDefinitionParser defParser = new VariableDefinitionParser(this.typeManager);
    defParser.setStackType(this.getStackType());

    // Structure member
    Symbols symbols = structType.getSymbols();
    if (symbols == null) return null;

    List<Id> ids = symbols.getId();
    for (Id id_elem : ids) {
      String id_type = id_elem.getType();
      String id_name = id_elem.getName().getValue();

      // Data type
      EnumType refTypeId = EnumType.getTypeIdFromXcodemlTypeName(id_type);
      if (refTypeId == null || refTypeId.isPrimitive() == false) {
        IXmlTypeTableChoice typeidChoice = this.typeManager.findType(id_type);
        VariableDefinition varMem = null;
        if (typeidChoice instanceof FbasicType) {
          String ref = ((FbasicType) typeidChoice).getRef();
          varMem = defParser.parseVarDefBasicType(id_name, (FbasicType) typeidChoice);
          /*
          if (structTypeName.equals (ref)) {
              // Structure member = Add same structure member
              varMem = new VariableType (typeDef);
          }
          else {
              varMem = defParser.parseVarDefBasicType (id_name, (FbasicType) typeidChoice);
          }
          */
        } else if (typeidChoice instanceof FstructType) {
          String ref = ((FstructType) typeidChoice).getType();
          varMem = defParser.parseVarDefStructType(id_name, (FstructType) typeidChoice);
          /*
          if (structTypeName.equals (ref)) {
              // Structure member = Add same structure member
              varMem = new VariableType (typeDef);
          }
          else {
              varMem = parseVarDefStructType ((FstructType) typeidChoice);
          }
          */
        }
        if (varMem != null) {
          // Add structure member
          typeDef.add(varMem);
        }
      } else {
        PrimitiveDataType primitive = PrimitiveDataType.findTypeBy(refTypeId.fortranName());
        VariableType varType = new VariableType(primitive);
        typeDef.add(varType, id_name);
      }
    }

    // Set the data type of the structure
    VariableType varType = new VariableType(typeDef);

    return varType;
  }

  /**
   * Parse and generate DB :: VariableType class from the function definition element.
   *
   * @param functionType Function definition element
   * @return DB :: VariableType class
   */
  public VariableType parseVarDefFunctionType(FfunctionType functionType) {

    // Data type
    VariableType varType = null;
    String refName = functionType.getReturnType();
    EnumType refTypeId = EnumType.getTypeIdFromXcodemlTypeName(refName);
    if (refTypeId == EnumType.VOID) {
      PrimitiveDataType primitive = PrimitiveDataType.VOID;
      varType = new VariableType(primitive);
    } else if (refTypeId == null || refTypeId.isPrimitive() == false) {
      IXmlTypeTableChoice typeChoice = this.typeManager.findType(refName);
      if (typeChoice instanceof FbasicType) {
        varType = parseVarDefBasicType((FbasicType) typeChoice);
      } else if (typeChoice instanceof FstructType) {
        varType = parseVarDefStructType((FstructType) typeChoice);
      } else if (typeChoice instanceof FfunctionType) {
        varType = parseVarDefFunctionType((FfunctionType) typeChoice);
      }
    } else {
      PrimitiveDataType primitive = PrimitiveDataType.findTypeBy(refTypeId.fortranName());
      varType = new VariableType(primitive);
    }
    return varType;
  }

  /**
   * Get a nested list of structs
   *
   * @return Nested list of structures
   */
  public List<jp.riken.kscope.language.fortran.Type> getStackType() {
    return stackType;
  }

  /**
   * Set a nested list of structs
   *
   * @param stackType Nested list of structures
   */
  public void setStackType(List<jp.riken.kscope.language.fortran.Type> stackType) {
    this.stackType = stackType;
  }

  /**
   * Add to the nested list of structs
   *
   * @param type Additional structure
   */
  private void addStackType(jp.riken.kscope.language.fortran.Type type) {
    if (type == null) return;
    if (this.stackType == null) {
      this.stackType = new ArrayList<jp.riken.kscope.language.fortran.Type>();
    }
    this.stackType.add(type);
  }

  /**
   * Check if the structure has been added to the nested list of structures.
   *
   * @param type Structure name
   * @return true = added
   */
  private boolean containsStackType(String typeName) {
    if (StringUtils.isNullOrEmpty(typeName)) return false;
    if (this.stackType == null) return false;

    if (getStackType(typeName) == null) return false;

    return true;
  }

  /**
   * Get the structure with the structure name from the nested list of structures.
   *
   * @param typeName Structure name
   * @return structure
   */
  private jp.riken.kscope.language.fortran.Type getStackType(String typeName) {
    if (StringUtils.isNullOrEmpty(typeName)) return null;
    if (this.stackType == null) return null;

    for (jp.riken.kscope.language.fortran.Type men : this.stackType) {
      if (typeName.equalsIgnoreCase(men.getName())) {
        return men;
      }
    }

    return null;
  }
}
