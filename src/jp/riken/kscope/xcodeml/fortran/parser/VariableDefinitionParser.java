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
package jp.riken.kscope.xcodeml.fortran.parser;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.language.VariableDimension;
import jp.riken.kscope.language.fortran.VariableAttribute;
import jp.riken.kscope.language.fortran.VariableType;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.xcodeml.fortran.XcodeMLTypeManager;
import jp.riken.kscope.xcodeml.fortran.utils.XmlNodeUtil;
import jp.riken.kscope.xcodeml.fortran.xml.EnumType;
import jp.riken.kscope.xcodeml.fortran.xml.IXmlNode;
import jp.riken.kscope.xcodeml.fortran.xml.IXmlTypeTableChoice;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FbasicType;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FfunctionType;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FstructDecl;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FstructType;
import jp.riken.kscope.xcodeml.fortran.xml.gen.Name;
import jp.riken.kscope.xcodeml.fortran.xml.gen.Var;

/**
 * VariableDefinitionパーサクラス
 * @author RIKEN
 */
public class VariableDefinitionParser {

    /** PUBLIC属性 */
    private final String ATTRIBUTE_PUBLIC = "public";
    /** PRIVATE属性 */
    private final String ATTRIBUTE_PRIVATE = "private";
    /** EXTERNAL属性 */
    private final String ATTRIBUTE_EXTERNAL = "external";
    /** INTERNAL属性 */
    private final String ATTRIBUTE_INTERNAL = "internal";
    /** INTRINSIC属性 */
    private final String ATTRIBUTE_INTRINSIC = "intrinsic";
    /** POINTER属性 */
    private final String ATTRIBUTE_POINTER = "pointer";
    /** TARGET属性 */
    private final String ATTRIBUTE_TARGET = "target";
    /** OPTIONAL属性 */
    private final String ATTRIBUTE_OPTIONAL = "optional";
    /** SAVE属性 */
    private final String ATTRIBUTE_SAVE = "save";
    /** PARAMETER属性 */
    private final String ATTRIBUTE_PARAMETER = "parameter";
    /** ALLOCATABLE属性 */
    private final String ATTRIBUTE_ALLOCATABLE = "allocatable";
    /** INTENT属性 */
    private final String ATTRIBUTE_INTENT = "intent";
    /** SEQUENCE属性 */
    private final String ATTRIBUTE_SEQUENCE = "sequence";
    /** INTERNAL PRIVATE属性 */
    private final String ATTRIBUTE_INTERNALPRIVATE = "private";
    /** PROGRAM */
    private final String ATTRIBUTE_PROGRAM = "program";
    /** RECURSIVE属性 */
    private final String ATTRIBUTE_RECURSIVE = "recursive";

    /** typeTable */
    private XcodeMLTypeManager typeManager;

    /** 構造体の入れ子リスト */
    private List<jp.riken.kscope.language.fortran.Type> stackType = new ArrayList<jp.riken.kscope.language.fortran.Type>();

    /**
     * コンストラクタ
     *
     * @param typeManager    typeTable
     */
    public VariableDefinitionParser(XcodeMLTypeManager typeManager) {
        this.typeManager = typeManager;
    }

    /**
     * XcodeML::NameクラスからDB::VariableDefinitionクラスをパース、生成する.<br/>
     * VariableDefinitionの変数名はnullとなる。
     * @param type_name           変数、構造体宣言
     * @return DB::VariableDefinitionクラス
     */
    public VariableDefinition parseVariableDefinition(String type_name) {

        // 変数名
        Var var = new Var();
        var.setType(type_name);

        return parseVariableDefinition(var);
    }



    /**
     * XcodeML::VarクラスからDB::VariableDefinitionクラスをパース、生成する.<br/>
     *
     * @param var           変数、構造体名
     * @return DB::VariableDefinitionクラス
     */
    public VariableDefinition parseVariableDefinition(Var var) {
        if (var == null) return null;

        // 変数名
        String var_name = var.getValue();

        // データ型
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
     * XcodeML::FstructDeclクラスからDB::VariableDefinitionクラスをパース、生成する.<br/>
     *
     * @param visitable           構造体要素
     * @return DB::VariableDefinitionクラス
     */
    public VariableDefinition parseVariableDefinition(FstructDecl visitable) {

        // 変数名
        Name name = visitable.getName();
        VariableDefinition varDef = parseVariableDefinition(name);

        return varDef;
    }

    /**
     * XcodeML::NameクラスからDB::VariableDefinitionクラスをパース、生成する.<br/>
     * 関数データ型は取得しない。
     *
     * @param name           変数、構造体宣言
     * @return DB::VariableDefinitionクラス
     */
    public VariableDefinition parseVariableDefinition(Name name) {

        // 変数名
        String var_name = name.getValue();

        // データ型
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
                varDef = parseVarDefStructType(var_name,
                        (FstructType) typeChoice);
            }
        }

        return varDef;
    }

    /**
     * XcodeML::NameクラスからDB::VariableDefinitionクラスをパース、生成する.<br/>
     * 関数データ型も取得する。
     *
     * @param name           変数、構造体宣言、関数名
     * @return DB::VariableDefinitionクラス
     */
    public VariableDefinition parseVariableDefinitionWithFfunctionType(Name name) {

        // データ型
        VariableDefinition varDef = parseVariableDefinition(name);
        if (varDef != null) {
            return varDef;
        }

        // 変数名
        String var_name = name.getValue();

        IXmlTypeTableChoice typeChoice = this.typeManager.findType(name);
        if (typeChoice instanceof FfunctionType) {
            varDef = parseVarDefFunctionType(var_name,
                    (FfunctionType) typeChoice);
        }

        return varDef;
    }

    /**
     * 型定義要素からDB::VariableDefinitionクラスをパース、生成する。
     *
     * @param var_name
     *            変数名
     * @param basicType
     *            プリミティブ型や他の型定義要素への参照の定義
     * @return DB::VariableDefinitionクラス
     */
    private VariableDefinition parseVarDefEnumType(String var_name, EnumType type) {
        // DB::VariableDefinitionクラス生成
        VariableDefinition varDef = new VariableDefinition(var_name);

        // データ型パーサ
        VariableTypeParser parserType = new VariableTypeParser(this.typeManager);
        parserType.setStackType(this.getStackType());
        VariableType varType = parserType.parseVarDefEnumType(type);
        varDef.setVariableType(varType);

        return varDef;
    }

    /**
     * 型定義要素からDB::VariableDefinitionクラスをパース、生成する。
     *
     * @param var_name
     *            変数名
     * @param basicType
     *            プリミティブ型や他の型定義要素への参照の定義
     * @return DB::VariableDefinitionクラス
     */
    public VariableDefinition parseVarDefBasicType(String var_name, FbasicType basicType) {

        // DB::VariableDefinitionクラス生成
        VariableDefinition varDef = new VariableDefinition(var_name);

        // データ型
        VariableTypeParser parserType = new VariableTypeParser(this.typeManager);
        parserType.setStackType(this.getStackType());
        VariableType varType = parserType.parseVarDefBasicType(basicType);
        varDef.setVariableType(varType);

        // VariableDimensionパーサ
        VariableDimensionParser dimsParser = new VariableDimensionParser(this.typeManager);

        // 配列宣言
        List<IXmlNode> arrayElems = basicType.getDefModelArraySubscript();
        if (arrayElems != null && arrayElems.size() > 0) {
            VariableDimension dims = dimsParser.parseVariableDimension(arrayElems);
            // 配列を設定する。
            varDef.setDimension(dims);
        }

        // 属性
        Set<String> listAttr = new HashSet<String>();
        // PUBLIC モジュール外で参照可能にします。
        if (XmlNodeUtil.isBoolean(basicType.isIsPublic())) {
            listAttr.add(this.ATTRIBUTE_PUBLIC);
        }
        // PRIVATE モジュール外での参照を禁止します。
        if (XmlNodeUtil.isBoolean(basicType.isIsPrivate())) {
            listAttr.add(this.ATTRIBUTE_PRIVATE);
        }
        // POINTER 言語要素をポインタとして宣言します。
        if (XmlNodeUtil.isBoolean(basicType.isIsPointer())) {
            listAttr.add(this.ATTRIBUTE_POINTER);
        }
        // TARGET 言語要素をポインタの指示先として使用可能にします。
        if (XmlNodeUtil.isBoolean(basicType.isIsTarget())) {
            listAttr.add(this.ATTRIBUTE_TARGET);
        }
        // OPTIONAL 実引き数の存在を省略可能として宣言します。
        if (XmlNodeUtil.isBoolean(basicType.isIsOptional())) {
            listAttr.add(this.ATTRIBUTE_OPTIONAL);
        }
        // SAVE 手続きの呼び出し間で、言語要素の値が保持されることを保証します。
        if (XmlNodeUtil.isBoolean(basicType.isIsSave())) {
            listAttr.add(this.ATTRIBUTE_SAVE);
        }
        // PARAMETER 名前付き定数を定義します。
        if (XmlNodeUtil.isBoolean(basicType.isIsParameter())) {
            listAttr.add(this.ATTRIBUTE_PARAMETER);
        }
        // ALLOCATABLE 実行中に割り付け可能な配列を宣言します。
        if (XmlNodeUtil.isBoolean(basicType.isIsAllocatable())) {
            listAttr.add(this.ATTRIBUTE_ALLOCATABLE);
        }
        // INTENT 仮引き数の使用方法を定義します。
        if (basicType.getIntent() != null) {
            String intent = this.ATTRIBUTE_INTENT + "(" + basicType.getIntent().value().toLowerCase() + ")";
            listAttr.add(intent);
        }
        if (listAttr.size() > 0) {
            VariableAttribute attr = new VariableAttribute(listAttr);
            varDef.setVariableAttributes(attr);
        }

        return varDef;
    }

    /**
     * 構造体要素からDB::VariableDefinitionクラスをパース、生成する。
     *
     * @param var_name            変数名
     * @param structType          構造体の定義
     * @return DB::VariableDefinitionクラス
     */
    public VariableDefinition parseVarDefStructType(String var_name, FstructType structType) {

        // DB::VariableDefinitionクラス生成
        VariableDefinition varDef = new VariableDefinition(var_name);

        VariableType varType = null;
        // 入れ子構造体であるかチェックする
        if (this.containsStackType(var_name)) {
            varType = new VariableType(this.getStackType(var_name));
        }
        else {
            // 構造体データ型のパース
            VariableTypeParser parserType = new VariableTypeParser(this.typeManager);
            parserType.setStackType(this.getStackType());
            varType = parserType.parseVarDefStructType(structType);
        }
        
        // 構造体のデータ型を設定する
        varDef.setVariableType(varType);

        // 属性
        Set<String> listAttr = new HashSet<String>();
        // PUBLIC モジュール外で参照可能にします。
        if (XmlNodeUtil.isBoolean(structType.isIsPublic())) {
            listAttr.add(this.ATTRIBUTE_PUBLIC);
        }
        // PRIVATE モジュール外での参照を禁止します。
        if (XmlNodeUtil.isBoolean(structType.isIsPrivate())) {
            listAttr.add(this.ATTRIBUTE_PRIVATE);
        }
        // SEQUENCE 記憶列内のすべての成分を定義時と同じ順序で配置します
        if (XmlNodeUtil.isBoolean(structType.isIsSequence())) {
            listAttr.add(this.ATTRIBUTE_SEQUENCE);
        }
        // PRIVATE 定義をモジュール中で書く場合にのみ使用できます。
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
     * 関数定義要素からDB::VariableDefinitionクラスをパース、生成する。
     *
     * @param var_name
     *            変数名
     * @param basicType
     *            関数定義要素
     * @return DB::VariableDefinitionクラス
     */
    private VariableDefinition parseVarDefFunctionType(String var_name, FfunctionType functionType) {

        // DB::VariableDefinitionクラス生成
        VariableDefinition varDef = new VariableDefinition(var_name);

        // データ型
        VariableTypeParser parserType = new VariableTypeParser(this.typeManager);
        parserType.setStackType(this.getStackType());
        VariableType varType = parserType.parseVarDefFunctionType(functionType);
        varDef.setVariableType(varType);

        // 属性
        Set<String> listAttr = new HashSet<String>();
        // PUBLIC モジュール外で参照可能にします。
        if (XmlNodeUtil.isBoolean(functionType.isIsPublic())) {
            listAttr.add(this.ATTRIBUTE_PUBLIC);
        }
        // PRIVATE モジュール外での参照を禁止します。
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
     * 構造体の入れ子リストを取得する
     * @return		構造体の入れ子リスト
     */
    public List<jp.riken.kscope.language.fortran.Type> getStackType() {
        return stackType;
    }

    /**
     * 構造体の入れ子リストを設定する
     * @param stackType		構造体の入れ子リスト
     */
    public void setStackType(List<jp.riken.kscope.language.fortran.Type> stackType) {
        this.stackType = stackType;
    }

    /**
     * 構造体の入れ子リストに追加する
     * @param type		追加構造体
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
     * 構造体の入れ子リストに構造体が追加済みであるかチェックする。
     * @param type		構造体名
     * @return    true=追加済み
     */
    private boolean containsStackType(String typeName) {
        if (StringUtils.isNullOrEmpty(typeName)) return false;
        if (this.stackType == null) return false;

        if (getStackType(typeName) == null) return false;

        return true;
    }

    /**
     * 構造体の入れ子リストから構造体名の構造体を取得する。
     * @param typeName		構造体名
     * @return    		構造体
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


