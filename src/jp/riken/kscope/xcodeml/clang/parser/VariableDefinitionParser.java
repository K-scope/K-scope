/*
 * K-scope
 * Copyright 2012-2015 RIKEN, Japan
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
package jp.riken.kscope.xcodeml.clang.parser;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.language.VariableDimension;
import jp.riken.kscope.language.fortran.VariableAttribute;
import jp.riken.kscope.language.fortran.VariableType;
import jp.riken.kscope.language.fortran.VariableType.PrimitiveDataType;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.xcodeml.clang.EnumPrimitiveType;
import jp.riken.kscope.xcodeml.clang.XcodeMLTypeManager;
import jp.riken.kscope.xcodeml.clang.utils.XmlNodeUtil;
import jp.riken.kscope.xcodeml.clang.xml.IXmlNode;
import jp.riken.kscope.xcodeml.clang.xml.IXmlTypeTableChoice;
import jp.riken.kscope.xcodeml.clang.xml.gen.ArrayType;
import jp.riken.kscope.xcodeml.clang.xml.gen.BaseType;
import jp.riken.kscope.xcodeml.clang.xml.gen.BasicType;
import jp.riken.kscope.xcodeml.clang.xml.gen.EnumType;
import jp.riken.kscope.xcodeml.clang.xml.gen.FunctionType;
import jp.riken.kscope.xcodeml.clang.xml.gen.Id;
import jp.riken.kscope.xcodeml.clang.xml.gen.PointerType;
import jp.riken.kscope.xcodeml.clang.xml.gen.StructType;
import jp.riken.kscope.xcodeml.clang.xml.gen.Name;
import jp.riken.kscope.xcodeml.clang.xml.gen.UnionType;
import jp.riken.kscope.xcodeml.clang.xml.gen.Var;
import jp.riken.kscope.xcodeml.clang.xml.gen.VarDecl;

/**
 * VariableDefinitionパーサクラス
 * @author RIKEN
 */
public class VariableDefinitionParser {


    /** typeTable */
    private XcodeMLTypeManager typeManager;

    /** 構造体の入れ子リスト */
    private List<jp.riken.kscope.language.fortran.Structure> stackType = new ArrayList<jp.riken.kscope.language.fortran.Structure>();

    /**
     * コンストラクタ
     *
     * @param typeManager    typeTable
     */
    public VariableDefinitionParser(XcodeMLTypeManager typeManager,
            List<jp.riken.kscope.language.fortran.Structure> stackType) {
        this.typeManager = typeManager;
        this.stackType = stackType;
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
        if (EnumPrimitiveType.isPrimitiveType(type_name)) {
            varDef = parseVariableDefinition(var_name, type_name);
        } else {
            IXmlTypeTableChoice[] typeChoices = this.typeManager.findTypes(type_name);
            varDef = parseVariableDefinition(var_name, typeChoices);
        }

        return varDef;
    }


    /**
     * XcodeML::VarDeclクラスからDB::VariableDefinitionクラスをパース、生成する.<br/>
     *
     * @param var           変数、構造体名
     * @return DB::VariableDefinitionクラス
     */
    public VariableDefinition parseVariableDefinition(VarDecl var_decl) {
        if (var_decl == null) return null;
        if (var_decl.getName() == null) return null;

        // 変数名
        String var_name = var_decl.getName().getValue();

        // データ型
        Id var_id = this.typeManager.findSymbol(var_name);
        VariableDefinition varDef = this.parseVariableDefinition(var_id);

        return varDef;
    }

    /**
     * XcodeML::IdクラスからDB::VariableDefinitionクラスをパース、生成する.<br/>
     * 関数データ型は取得しない。
     *
     * @param name           変数、構造体宣言
     * @return DB::VariableDefinitionクラス
     */
    public VariableDefinition parseVariableDefinition(Id id) {
        if (id == null) return null;
        if (id.getName() == null) return null;

        // 変数名
        String var_name = id.getName().getValue();
        String var_type = id.getType();

        // データ型
        VariableDefinition varDef = null;
        if (EnumPrimitiveType.isPrimitiveType(var_type)) {
            varDef = parseVariableDefinition(var_name, var_type);
        }
        else if (!StringUtils.isNullOrEmpty(var_type)) {
            IXmlTypeTableChoice[] typeChoices = this.typeManager.findTypes(var_type);
            varDef = parseVariableDefinition(var_name, typeChoices);
        }
        else {
            varDef = new VariableDefinition(var_name);
        }

        return varDef;
    }

    /**
     * 配列要素からDB::VariableDefinitionクラスをパース、生成する。
     *
     * @param var_name          変数名
     * @param arrayType            配列要素
     * @return DB::VariableDefinitionクラス
     */
    public VariableDefinition parseVariableDefinition(String var_name, ArrayType arrayType) {

        // DB::VariableDefinitionクラス生成
        VariableDefinition varDef = new VariableDefinition(var_name);

        // データ型
        VariableTypeParser parserType = new VariableTypeParser(this.typeManager, this.getStackType());
        VariableType varType = parserType.parseVarArrayType(arrayType);
        varDef.setVariableType(varType);

        // 属性を設定する
        this.parseAttributes(varDef, new IXmlTypeTableChoice[]{arrayType});

        return varDef;
    }


    /**
     * DB::VariableDefinitionの属性を設定する。
     * @param var_def            VariableDefinition:変数・構造体宣言
     * @param typeChoices        データ型定義要素リスト
     * @return DB::VariableDefinitionクラス
     */
    private VariableDefinition parseAttributes(VariableDefinition var_def, IXmlTypeTableChoice[] typeChoices) {
        if (var_def == null) return null;

        String name = var_def.get_name();
        Id var_id = this.typeManager.findSymbol(name);
        String sclass = null;
        if (var_id != null) {
            sclass = var_id.getSclass();
        }

        // 属性
        List<String> listAttr = new ArrayList<String>();
        if (sclass != null) listAttr.add(sclass);

        boolean is_volatile = false;
        boolean is_restrict = false;
        List<ArrayType> array_types = new ArrayList<ArrayType>();
        if (typeChoices != null) {
            for (IXmlTypeTableChoice type : typeChoices) {
                boolean is_pointer = false;
                boolean is_array = false;
                boolean is_const = false;
                // ポインタ
                if (type instanceof PointerType) is_pointer = true;
                // 配列
                if (type instanceof ArrayType) {
                    is_array = true;
                    array_types.add((ArrayType)type);
                }
                BaseType base_type = (BaseType)type;
                if (StringUtils.toBoolean(base_type.getIsConst()))  is_const = true;
                if (StringUtils.toBoolean(base_type.getIsVolatile()))  is_volatile = true;
                if (StringUtils.toBoolean(base_type.getIsRestrict()))  is_restrict = true;

                if (is_pointer && is_const) {
                    listAttr.add(VariableAttribute.ATTRIBUTE_CONST_POINTER);
                }
                else {
                    if (is_pointer)  listAttr.add(VariableAttribute.ATTRIBUTE_POINTER);
                    if (is_const)    listAttr.add(VariableAttribute.ATTRIBUTE_CONST);
                }
                if (is_array)    listAttr.add(VariableAttribute.ATTRIBUTE_ARRAY);
                if (is_volatile) listAttr.add(VariableAttribute.ATTRIBUTE_VOLATILE);
                if (is_restrict) listAttr.add(VariableAttribute.ATTRIBUTE_RESTRICT);
            }
        }

        // 配列宣言
        if (array_types != null && array_types.size() > 0) {
            // VariableDimensionパーサ
            VariableDimensionParser dimsParser = new VariableDimensionParser(this.typeManager);
            VariableDimension dims = dimsParser.parseVariableDimension(array_types);
            // 配列を設定する。
            var_def.setDimension(dims);
        }


        if (listAttr.size() > 0) {
            VariableAttribute attr = new VariableAttribute(listAttr);
            var_def.setVariableAttributes(attr);
        }

        return var_def;
    }

    /**
     * 基本データ型要素からDB::VariableDefinitionクラスをパース、生成する。
     *
     * @param var_name          変数名
     * @param basicType            基本データ型
     * @return DB::VariableDefinitionクラス
     */
    public VariableDefinition parseVariableDefinition(String var_name, BasicType basicType) {

        // DB::VariableDefinitionクラス生成
        VariableDefinition varDef = new VariableDefinition(var_name);

        // データ型
        VariableTypeParser parserType = new VariableTypeParser(this.typeManager, this.getStackType());
        VariableType varType = parserType.parseVarBasicType(basicType);
        varDef.setVariableType(varType);

        // 属性を設定する
        this.parseAttributes(varDef, new IXmlTypeTableChoice[]{basicType});

        return varDef;
    }

    /**
     * 構造体要素からDB::VariableDefinitionクラスをパース、生成する。
     *
     * @param var_name            変数名
     * @param structType          構造体の定義
     * @return DB::VariableDefinitionクラス
     */
    public VariableDefinition parseVariableDefinition(String var_name, StructType structType) {

        // DB::VariableDefinitionクラス生成
        VariableDefinition varDef = new VariableDefinition(var_name);

        VariableType varType = null;
        // 入れ子構造体であるかチェックする
        if (this.containsStackType(var_name)) {
            varType = new VariableType(this.getStackType(var_name));
        }
        else {
            // 構造体データ型のパース
            VariableTypeParser parserType = new VariableTypeParser(this.typeManager, this.getStackType());
            varType = parserType.parseStructType(structType);
        }

        // 構造体のデータ型を設定する
        varDef.setVariableType(varType);

        // 属性を設定する
        this.parseAttributes(varDef, new IXmlTypeTableChoice[]{structType});

        return varDef;
    }

    /**
     * 共同体要素からDB::VariableDefinitionクラスをパース、生成する。
     *
     * @param var_name            変数名
     * @param unionType          共同体要素
     * @return DB::VariableDefinitionクラス
     */
    public VariableDefinition parseVariableDefinition(String var_name, UnionType unionType) {

        // DB::VariableDefinitionクラス生成
        VariableDefinition varDef = new VariableDefinition(var_name);

        VariableType varType = null;
        // 入れ子構造体であるかチェックする
        if (this.containsStackType(var_name)) {
            varType = new VariableType(this.getStackType(var_name));
        }
        else {
            // 構造体データ型のパース
            VariableTypeParser parserType = new VariableTypeParser(this.typeManager, this.getStackType());
            varType = parserType.parseVarUnionType(unionType);
        }

        // 構造体のデータ型を設定する
        varDef.setVariableType(varType);

        // 属性を設定する
        this.parseAttributes(varDef, new IXmlTypeTableChoice[]{unionType});

        return varDef;
    }

    /**
     * 関数定義要素からDB::VariableDefinitionクラスをパース、生成する。
     *
     * @param var_name
     *            変数名
     * @param functionType
     *            関数定義要素
     * @return DB::VariableDefinitionクラス
     */
    public VariableDefinition parseVariableDefinition(String var_name, FunctionType functionType) {

        // DB::VariableDefinitionクラス生成
        VariableDefinition varDef = new VariableDefinition(var_name);

        // データ型
        VariableTypeParser parserType = new VariableTypeParser(this.typeManager, this.getStackType());
        VariableType varType = parserType.parseVarFunctionType(functionType);
        varDef.setVariableType(varType);

        // 属性を設定する
        this.parseAttributes(varDef, new IXmlTypeTableChoice[]{functionType});

        return varDef;
    }

    /**
     * ポインタ要素からDB::VariableDefinitionクラスをパース、生成する。
     *
     * @param var_name
     *            変数名
     * @param pointerType
     *            ポインタ要素
     * @return DB::VariableDefinitionクラス
     */
    public VariableDefinition parseVariableDefinition(String var_name, PointerType pointerType) {

        // DB::VariableDefinitionクラス生成
        VariableDefinition varDef = new VariableDefinition(var_name);

        // データ型
        VariableTypeParser parserType = new VariableTypeParser(this.typeManager, this.getStackType());
        VariableType varType = parserType.parseVarPointerType(pointerType);
        varDef.setVariableType(varType);

        // 属性を設定する
        this.parseAttributes(varDef, new IXmlTypeTableChoice[]{pointerType});

        return varDef;
    }

    /**
     * 構造体の入れ子リストを取得する
     * @return        構造体の入れ子リスト
     */
    public List<jp.riken.kscope.language.fortran.Structure> getStackType() {
        return this.stackType;
    }

    /**
     * 構造体の入れ子リストを設定する
     * @param stackType        構造体の入れ子リスト
     */
    public void setStackType(List<jp.riken.kscope.language.fortran.Structure> stackType) {
        this.stackType = stackType;
    }

    /**
     * 構造体の入れ子リストに追加する
     * @param type        追加構造体
     */
    @SuppressWarnings("unused")
    private void addStackType(jp.riken.kscope.language.fortran.Structure type) {
        if (type == null) return;
        if (this.stackType == null) {
            this.stackType = new ArrayList<jp.riken.kscope.language.fortran.Structure>();
        }
        this.stackType.add(type);
    }

    /**
     * 構造体の入れ子リストに構造体が追加済みであるかチェックする。
     * @param type        構造体名
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
     * @param typeName        構造体名
     * @return            構造体
     */
    private jp.riken.kscope.language.fortran.Structure getStackType(String typeName) {
        if (StringUtils.isNullOrEmpty(typeName)) return null;
        if (this.stackType == null) return null;

        for (jp.riken.kscope.language.fortran.Structure men : this.stackType) {
            if (typeName.equals(men.getName())) {
                return men;
            }
        }

        return null;
    }

    /**
     * データ型名からDB::VariableDefinitionクラスをパース、生成する。
     * @param var_name            変数名
     * @param type_name     プリミティブデータ型名
     * @return DB::VariableDefinitionクラス
     */
    public VariableDefinition parseVariableDefinition(String var_name, String type_name) {
        if (StringUtils.isNullOrEmpty(var_name)) return null;
        if (StringUtils.isNullOrEmpty(type_name)) return null;

        // DB::VariableDefinitionクラス生成
        VariableDefinition varDef = new VariableDefinition(var_name);

        // データ型パーサ
        VariableTypeParser parserType = new VariableTypeParser(this.typeManager, this.getStackType());

        if (EnumPrimitiveType.isPrimitiveType(type_name)) {
            VariableType varType = parserType.parseVarPrimitiveType(type_name);
            varDef.setVariableType(varType);
        } else {
            IXmlTypeTableChoice[] typeChoices = this.typeManager.findTypes(type_name);
            varDef = parseVariableDefinition(var_name, typeChoices);
        }

        // 属性を設定する
        this.parseAttributes(varDef, null);

        return varDef;
    }


    /**
     * データ型名からDB::VariableDefinitionクラスをパース、生成する。
     * @param var_name            変数名
     * @param type_name     プリミティブデータ型名
     * @return DB::VariableDefinitionクラス
     */
    public VariableDefinition parseVariableDefinition(Name name) {
        if (name == null) return null;

        String var_name = name.getValue();
        String type_name = name.getType();
        VariableDefinition varDef = this.parseVariableDefinition(var_name, type_name);
        return varDef;
    }

    /**
     * データ型リストからDB::VariableDefinitionクラスをパース、生成する。
     * @param var_name            変数名
     * @param typeChoices             データ型リスト
     * @return DB::VariableDefinitionクラス
     */
    public VariableDefinition parseVariableDefinition(String var_name, IXmlTypeTableChoice[] typeChoices) {
        if (StringUtils.isNullOrEmpty(var_name)) return null;
        if (typeChoices == null) return null;

        // DB::VariableDefinitionクラス生成
        VariableDefinition varDef = new VariableDefinition(var_name);

        // データ型パーサ
        VariableTypeParser parserType = new VariableTypeParser(this.typeManager, this.getStackType());

        VariableType varType = parserType.parseVarTypes(typeChoices);
        varDef.setVariableType(varType);

        // 属性を設定する
        this.parseAttributes(varDef, typeChoices);

        return varDef;
    }

}


