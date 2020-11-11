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
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.language.VariableDimension;
import jp.riken.kscope.language.fortran.VariableAttribute;
import jp.riken.kscope.language.fortran.VariableType;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.xcodeml.XcodeMLTypeManager;
import jp.riken.kscope.xcodeml.util.XmlNodeUtil;
import jp.riken.kscope.xcodeml.xml.EnumType;
import jp.riken.kscope.xcodeml.xml.IXmlNode;
import jp.riken.kscope.xcodeml.xml.IXmlTypeTableChoice;
import jp.riken.kscope.xcodeml.xml.gen.FbasicType;
import jp.riken.kscope.xcodeml.xml.gen.FfunctionType;
import jp.riken.kscope.xcodeml.xml.gen.FstructDecl;
import jp.riken.kscope.xcodeml.xml.gen.FstructType;
import jp.riken.kscope.xcodeml.xml.gen.Name;
import jp.riken.kscope.xcodeml.xml.gen.Var;

/**
 * VariableDefinition parser class
 *
 * @author RIKEN
 */
public class VariableDefinitionParser {

  /** PUBLIC attribute */
  private final String ATTRIBUTE_PUBLIC = "public";
  /** PRIVATE attribute */
  private final String ATTRIBUTE_PRIVATE = "private";
  /** EXTERNAL attribute */
  private final String ATTRIBUTE_EXTERNAL = "external";
  /** INTERNAL attribute */
  private final String ATTRIBUTE_INTERNAL = "internal";
  /** INTRINSIC attribute */
  private final String ATTRIBUTE_INTRINSIC = "intrinsic";
  /** POINTER attribute */
  private final String ATTRIBUTE_POINTER = "pointer";
  /** TARGET attribute */
  private final String ATTRIBUTE_TARGET = "target";
  /** OPTIONAL attribute */
  private final String ATTRIBUTE_OPTIONAL = "optional";
  /** SAVE attribute */
  private final String ATTRIBUTE_SAVE = "save";
  /** PARAMETER attribute */
  private final String ATTRIBUTE_PARAMETER = "parameter";
  /** ALLOCATABLE attribute */
  private final String ATTRIBUTE_ALLOCATABLE = "allocatable";
  /** INTENT attribute */
  private final String ATTRIBUTE_INTENT = "intent";
  /** SEQUENCE attribute */
  private final String ATTRIBUTE_SEQUENCE = "sequence";
  /** INTERNAL PRIVATE attribute */
  private final String ATTRIBUTE_INTERNALPRIVATE = "private";
  /** PROGRAM */
  private final String ATTRIBUTE_PROGRAM = "program";
  /** RECURSIVE attribute */
  private final String ATTRIBUTE_RECURSIVE = "recursive";

  /** typeTable */
  private XcodeMLTypeManager typeManager;

  /** Nested list of structures */
  private List<jp.riken.kscope.language.fortran.Type> stackType =
      new ArrayList<jp.riken.kscope.language.fortran.Type>();

  /**
   * Constructor
   *
   * @param typeManager typeTable
   */
  public VariableDefinitionParser(XcodeMLTypeManager typeManager) {
    this.typeManager = typeManager;
  }

  /**
   * Parse and generate DB :: VariableDefinition class from XcodeML :: Name class. <br>
   * The variable name of VariableDefinition is null.
   *
   * @param type_name Variable, structure declaration
   * @return DB :: VariableDefinition class
   */
  public VariableDefinition parseVariableDefinition(String type_name) {

    // Variable name
    Var var = new Var();
    var.setType(type_name);

    return parseVariableDefinition(var);
  }

  /**
   * Parse and generate DB :: VariableDefinition class from XcodeML :: Var class. <br>
   *
   * @param var variable, structure name
   * @return DB :: VariableDefinition class
   */
  public VariableDefinition parseVariableDefinition(Var var) {
    if (var == null) return null;

    // Variable name
    String var_name = var.getValue();

    // Data type
    VariableDefinition varDef = null;
    String type_name = var.getType();
    EnumType type = EnumType.getTypeIdFromXcodemlTypeName(type_name);
    if (type != null && type.isPrimitive()) {
      varDef = parseVarDefEnumType(var_name, type);
    } else {
      IXmlTypeTableChoice typeChoice = this.typeManager.findType(type_name);
      if (typeChoice instanceof FbasicType) {
        varDef = parseVarDefBasicType(var_name, (FbasicType) typeChoice);
      } else if (typeChoice instanceof FstructType) {
        varDef = parseVarDefStructType(var_name, (FstructType) typeChoice);
      } else if (typeChoice instanceof FfunctionType) {
        varDef = parseVarDefFunctionType(var_name, (FfunctionType) typeChoice);
      }
    }

    return varDef;
  }

  /**
   * Parse and generate DB :: VariableDefinition class from XcodeML :: FstructDecl class. <br>
   *
   * @param visitable struct element
   * @return DB :: VariableDefinition class
   */
  public VariableDefinition parseVariableDefinition(FstructDecl visitable) {

    // Variable name
    Name name = visitable.getName();
    VariableDefinition varDef = parseVariableDefinition(name);

    return varDef;
  }

  /**
   * Parse and generate DB :: VariableDefinition class from XcodeML :: Name class. <br>
   * Do not get the function data type.
   *
   * @param name variable, structure declaration
   * @return DB :: VariableDefinition class
   */
  public VariableDefinition parseVariableDefinition(Name name) {

    // Variable name
    String var_name = name.getValue();

    // Data type
    VariableDefinition varDef = null;
    String type_name = name.getType();
    EnumType type = EnumType.getTypeIdFromXcodemlTypeName(type_name);
    if (type != null && type.isPrimitive()) {
      varDef = parseVarDefEnumType(var_name, type);
    } else {
      IXmlTypeTableChoice typeChoice = this.typeManager.findType(name);
      if (typeChoice instanceof FbasicType) {
        varDef = parseVarDefBasicType(var_name, (FbasicType) typeChoice);
      } else if (typeChoice instanceof FstructType) {
        varDef = parseVarDefStructType(var_name, (FstructType) typeChoice);
      }
    }

    return varDef;
  }

  /**
   * Parse and generate DB :: VariableDefinition class from XcodeML :: Name class. <br>
   * Also get the function data type.
   *
   * @param name Variable, structure declaration, function name
   * @return DB :: VariableDefinition class
   */
  public VariableDefinition parseVariableDefinitionWithFfunctionType(Name name) {

    // Data type
    VariableDefinition varDef = parseVariableDefinition(name);
    if (varDef != null) {
      return varDef;
    }

    // Variable name
    String var_name = name.getValue();

    IXmlTypeTableChoice typeChoice = this.typeManager.findType(name);
    if (typeChoice instanceof FfunctionType) {
      varDef = parseVarDefFunctionType(var_name, (FfunctionType) typeChoice);
    }

    return varDef;
  }

  /**
   * Parse and generate DB :: VariableDefinition class from type definition element.
   *
   * @param var_name Variable name
   * @param basicType Defining references to primitive types and other type definition elements
   * @return DB :: VariableDefinition class
   */
  private VariableDefinition parseVarDefEnumType(String var_name, EnumType type) {
    // DB :: VariableDefinition class generation
    VariableDefinition varDef = new VariableDefinition(var_name);

    // Data type parser
    VariableTypeParser parserType = new VariableTypeParser(this.typeManager);
    parserType.setStackType(this.getStackType());
    VariableType varType = parserType.parseVarDefEnumType(type);
    varDef.setVariableType(varType);

    return varDef;
  }

  /**
   * Parse and generate DB :: VariableDefinition class from type definition element.
   *
   * @param var_name Variable name
   * @param basicType Defining references to primitive types and other type definition elements
   * @return DB :: VariableDefinition class
   */
  public VariableDefinition parseVarDefBasicType(String var_name, FbasicType basicType) {

    // DB :: VariableDefinition class generation
    VariableDefinition varDef = new VariableDefinition(var_name);

    // Data type
    VariableTypeParser parserType = new VariableTypeParser(this.typeManager);
    parserType.setStackType(this.getStackType());
    VariableType varType = parserType.parseVarDefBasicType(basicType);
    varDef.setVariableType(varType);

    // VariableDimension parser
    VariableDimensionParser dimsParser = new VariableDimensionParser(this.typeManager);

    // Array declaration
    List<IXmlNode> arrayElems = basicType.getIndexRangeOrArrayIndex();
    if (arrayElems != null && arrayElems.size() > 0) {
      VariableDimension dims = dimsParser.parseVariableDimension(arrayElems);
      // Set the array.
      varDef.setDimension(dims);
    }

    // Attribute
    Set<String> listAttr = new HashSet<String>();
    // Make it visible outside the PUBLIC module.
    if (XmlNodeUtil.isBoolean(basicType.isIsPublic())) {
      listAttr.add(this.ATTRIBUTE_PUBLIC);
    }
    // Prohibit references outside the PRIVATE module.
    if (XmlNodeUtil.isBoolean(basicType.isIsPrivate())) {
      listAttr.add(this.ATTRIBUTE_PRIVATE);
    }
    // Declare the POINTER language element as a pointer.
    if (XmlNodeUtil.isBoolean(basicType.isIsPointer())) {
      listAttr.add(this.ATTRIBUTE_POINTER);
    }
    // Enable the TARGET language element as a pointer destination.
    if (XmlNodeUtil.isBoolean(basicType.isIsTarget())) {
      listAttr.add(this.ATTRIBUTE_TARGET);
    }
    // OPTIONAL Declare the existence of a real argument as optional.
    if (XmlNodeUtil.isBoolean(basicType.isIsOptional())) {
      listAttr.add(this.ATTRIBUTE_OPTIONAL);
    }
    // Guarantee that the value of the language element is preserved between calls to the SAVE
    // procedure.
    if (XmlNodeUtil.isBoolean(basicType.isIsSave())) {
      listAttr.add(this.ATTRIBUTE_SAVE);
    }
    // PARAMETER Defines a named constant.
    if (XmlNodeUtil.isBoolean(basicType.isIsParameter())) {
      listAttr.add(this.ATTRIBUTE_PARAMETER);
    }
    // Declare an array that can be allocated during ALLOCATABLE execution.
    if (XmlNodeUtil.isBoolean(basicType.isIsAllocatable())) {
      listAttr.add(this.ATTRIBUTE_ALLOCATABLE);
    }
    // INTENT Defines how to use temporary arguments.
    if (basicType.getIntent() != null) {
      String intent =
          this.ATTRIBUTE_INTENT + "(" + basicType.getIntent().value().toLowerCase() + ")";
      listAttr.add(intent);
    }
    if (listAttr.size() > 0) {
      VariableAttribute attr = new VariableAttribute(listAttr);
      varDef.setVariableAttributes(attr);
    }

    return varDef;
  }

  /**
   * Parse and generate DB :: VariableDefinition class from structure elements.
   *
   * @param var_name Variable name
   * @param structType Structure definition
   * @return DB :: VariableDefinition class
   */
  public VariableDefinition parseVarDefStructType(String var_name, FstructType structType) {

    // DB :: VariableDefinition class generation
    VariableDefinition varDef = new VariableDefinition(var_name);

    VariableType varType = null;
    // Check if it is a nested structure
    if (this.containsStackType(var_name)) {
      varType = new VariableType(this.getStackType(var_name));
    } else {
      // Structure data type parsing
      VariableTypeParser parserType = new VariableTypeParser(this.typeManager);
      parserType.setStackType(this.getStackType());
      varType = parserType.parseVarDefStructType(structType);
    }

    // Set the data type of the structure
    varDef.setVariableType(varType);

    // Attribute
    Set<String> listAttr = new HashSet<String>();
    // Make it visible outside the PUBLIC module.
    if (XmlNodeUtil.isBoolean(structType.isIsPublic())) {
      listAttr.add(this.ATTRIBUTE_PUBLIC);
    }
    // Prohibit references outside the PRIVATE module.
    if (XmlNodeUtil.isBoolean(structType.isIsPrivate())) {
      listAttr.add(this.ATTRIBUTE_PRIVATE);
    }
    // SEQUENCE Places all components in the storage column in the same order as when they were
    // defined.
    if (XmlNodeUtil.isBoolean(structType.isIsSequence())) {
      listAttr.add(this.ATTRIBUTE_SEQUENCE);
    }
    // Can only be used when writing the PRIVATE definition in a module.
    if (XmlNodeUtil.isBoolean(structType.isIsInternalPrivate())) {
      listAttr.add(this.ATTRIBUTE_INTERNALPRIVATE);
    }
    if (listAttr.size() > 0) {
      VariableAttribute attr = new VariableAttribute(listAttr);
      varDef.setVariableAttributes(attr);
    }

    return varDef;
  }

  /**
   * Parse and generate DB :: VariableDefinition class from function definition element.
   *
   * @param var_name Variable name
   * @param basicType Function definition element
   * @return DB :: VariableDefinition class
   */
  private VariableDefinition parseVarDefFunctionType(String var_name, FfunctionType functionType) {

    // DB :: VariableDefinition class generation
    VariableDefinition varDef = new VariableDefinition(var_name);

    // Data type
    VariableTypeParser parserType = new VariableTypeParser(this.typeManager);
    parserType.setStackType(this.getStackType());
    VariableType varType = parserType.parseVarDefFunctionType(functionType);
    varDef.setVariableType(varType);

    // Attribute
    Set<String> listAttr = new HashSet<String>();
    // Make it visible outside the PUBLIC module.
    if (XmlNodeUtil.isBoolean(functionType.isIsPublic())) {
      listAttr.add(this.ATTRIBUTE_PUBLIC);
    }
    // Prohibit references outside the PRIVATE module.
    if (XmlNodeUtil.isBoolean(functionType.isIsPrivate())) {
      listAttr.add(this.ATTRIBUTE_PRIVATE);
    }
    // EXTERNAL
    if (XmlNodeUtil.isBoolean(functionType.isIsExternal())) {
      listAttr.add(this.ATTRIBUTE_EXTERNAL);
    }
    // INTERNAL
    if (XmlNodeUtil.isBoolean(functionType.isIsInternal())) {
      listAttr.add(this.ATTRIBUTE_INTERNAL);
    }
    // INTRINSIC
    if (XmlNodeUtil.isBoolean(functionType.isIsIntrinsic())) {
      listAttr.add(this.ATTRIBUTE_INTRINSIC);
    }
    // PROGRAM
    if (XmlNodeUtil.isBoolean(functionType.isIsProgram())) {
      listAttr.add(this.ATTRIBUTE_PROGRAM);
    }
    // Recursive
    if (XmlNodeUtil.isBoolean(functionType.isIsRecursive())) {
      listAttr.add(this.ATTRIBUTE_RECURSIVE);
    }
    if (listAttr.size() > 0) {
      VariableAttribute attr = new VariableAttribute(listAttr);
      varDef.setVariableAttributes(attr);
    }

    return varDef;
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
  @SuppressWarnings("unused")
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
