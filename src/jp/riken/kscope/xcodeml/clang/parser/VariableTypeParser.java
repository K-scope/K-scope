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

import jp.riken.kscope.language.BlockType;
import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.language.VariableDimension;
import jp.riken.kscope.language.fortran.Structure;
import jp.riken.kscope.language.fortran.VariableAttribute;
import jp.riken.kscope.language.fortran.VariableType;
import jp.riken.kscope.language.fortran.VariableType.PrimitiveDataType;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.xcodeml.clang.EnumPrimitiveType;
import jp.riken.kscope.xcodeml.clang.XcodeMLTypeManager;
import jp.riken.kscope.xcodeml.clang.xml.IXmlNode;
import jp.riken.kscope.xcodeml.clang.xml.IXmlTypeTableChoice;
import jp.riken.kscope.xcodeml.clang.xml.gen.ArrayType;
import jp.riken.kscope.xcodeml.clang.xml.gen.BaseType;
import jp.riken.kscope.xcodeml.clang.xml.gen.BasicType;
import jp.riken.kscope.xcodeml.clang.xml.gen.EnumType;
import jp.riken.kscope.xcodeml.clang.xml.gen.FunctionType;
import jp.riken.kscope.xcodeml.clang.xml.gen.PointerType;
import jp.riken.kscope.xcodeml.clang.xml.gen.StructType;
import jp.riken.kscope.xcodeml.clang.xml.gen.Id;
import jp.riken.kscope.xcodeml.clang.xml.gen.Name;
import jp.riken.kscope.xcodeml.clang.xml.gen.Symbols;
import jp.riken.kscope.xcodeml.clang.xml.gen.UnionType;
import jp.riken.kscope.xcodeml.clang.xml.gen.Var;

/**
 * VariableTypeパーサクラス
 * @author RIKEN
 */
public class VariableTypeParser {

    /** typeTable */
    private XcodeMLTypeManager typeManager;

    /** 構造体の入れ子リスト */
    private List<jp.riken.kscope.language.fortran.Structure> stackType;


    /**
     * コンストラクタ
     *
     * @param typeManager    typeTable
     */
    public VariableTypeParser(XcodeMLTypeManager typeManager,
            List<jp.riken.kscope.language.fortran.Structure> stackType) {
        this.typeManager = typeManager;
        this.stackType = stackType;
    }

    /**
     * XcodeML::NameクラスからDB::VariableTypeクラスをパース、生成する.
     * @param type_name           変数、構造体宣言
     * @return DB::VariableTypeクラス
     */
    public VariableType parseVariableType(String type_name) {
        if (type_name == null || type_name.isEmpty()) return null;

        // 変数名
        Var var = new Var();
        var.setType(type_name);

        return parseVariableType(var);
    }



    /**
     * XcodeML::VarクラスからDB::VariableTypeクラスをパース、生成する.<br/>
     *
     * @param var           変数、構造体名
     * @return DB::VariableTypeクラス
     */
    public VariableType parseVariableType(Var var) {

        // データ型
        VariableType varType = null;
        String type_name = var.getType();
        if (EnumPrimitiveType.isPrimitiveType(type_name)) {
            varType = parseVarPrimitiveType(type_name);
        }
        else {
            IXmlTypeTableChoice[] typeChoices = this.typeManager.findTypes(type_name);
            varType = parseVarTypes(typeChoices);
        }

        return varType;
    }


    /**
     * プリミティブデータ型名からDB::VariableTypeクラスをパース、生成する。
     *
     * @param type            プリミティブデータ型名
     * @return DB::VariableTypeクラス
     */
    public VariableType parseVarPrimitiveType(String type_name) {
        PrimitiveDataType primitive = PrimitiveDataType.findByTypeName(type_name);
        if (primitive == null || primitive == PrimitiveDataType.UNKOWN) {
            return null;
        }
        VariableType varType = new VariableType(primitive);

        return varType;
    }

    /**
     * 型定義要素からDB::VariableTypeクラスをパース、生成する。
     *
     * @param basicType         プリミティブ型や他の型定義要素への参照の定義
     * @return DB::VariableTypeクラス
     */
    public VariableType parseVarBasicType(BasicType basicType) {

        if (basicType == null) return null;
        if (basicType.getName() == null) return null;

        // データ型
        VariableType varType = null;
        String name = basicType.getName();
        if (EnumPrimitiveType.isPrimitiveType(name)) {
            varType = parseVarPrimitiveType(name);
        }
        else {
            IXmlTypeTableChoice typeChoice = this.typeManager.findType(name);
            if (typeChoice instanceof BasicType) {
                varType = parseVarBasicType((BasicType) typeChoice);
            } else if (typeChoice instanceof StructType) {
                varType = parseStructType((StructType) typeChoice);
            } else if (typeChoice instanceof UnionType) {
                varType = parseVarUnionType((UnionType) typeChoice);
            }
        }
        if (varType == null) return null;

        return varType;
    }

    /**
     * ポインタ要素からDB::VariableTypeクラスをパース、生成する。
     *
     * @param pointerType         ポインタ要素
     * @return DB::VariableTypeクラス
     */
    public VariableType parseVarPointerType(PointerType pointerType) {

        if (pointerType == null) return null;
        if (pointerType.getRef() == null) return null;

        // データ型
        VariableType varType = null;
        String ref = pointerType.getRef();
        if (EnumPrimitiveType.isPrimitiveType(ref)) {
            varType = parseVarPrimitiveType(EnumPrimitiveType.getClangDataType(ref));
        }
        else {
            IXmlTypeTableChoice typeChoice = this.typeManager.findType(ref);
            if (typeChoice instanceof BasicType) {
                varType = parseVarBasicType((BasicType) typeChoice);
            } else if (typeChoice instanceof StructType) {
                varType = parseStructType((StructType) typeChoice);
            } else if (typeChoice instanceof UnionType) {
                varType = parseVarUnionType((UnionType) typeChoice);
            }
        }
        if (varType == null) return null;

        return varType;
    }

    /**
     * 構造体要素からDB::VariableTypeクラスをパース、生成する。
     *
     * @param structType          構造体の定義
     * @return DB::VariableTypeクラス
     */
    public VariableType parseStructType(StructType structType) {

        // 構造体名
        String type_name = structType.getType();
        Id tagid = this.typeManager.findTagname(type_name);
        String tag_name = null;
        if (tagid != null && tagid.getName() != null) {
            tag_name = tagid.getName().getValue();
        }

        // 入れ子構造体であるかチェックする
        if (tag_name != null) {
            if (this.containsStackType(tag_name)) {
                // 定義済みであるので、作成済み構造体を返す。
                return new VariableType(this.getStackType(tag_name));
            }
        }

        // DB::Typeクラス生成
        jp.riken.kscope.language.fortran.Structure typeDef = new jp.riken.kscope.language.fortran.Structure(tag_name);
        this.addStackType(typeDef);

        // 構造体のデータ型を設定する
        VariableType varType = new VariableType(typeDef);

        // 変数宣言パーサ
        VariableDefinitionParser defParser = new VariableDefinitionParser(this.typeManager, this.getStackType());

        // 構造体メンバ
        Symbols symbols = structType.getSymbols();
        if (symbols == null)
            return varType;

        List<IXmlNode> ids = symbols.getIdOrPragmaOrText();
        for (IXmlNode id_elem : ids) {
            if (!(id_elem instanceof Id)) continue;
            if ( ((Id)id_elem).getName() == null) continue;

            String id_type = ((Id)id_elem).getType();
            String id_name = ((Id)id_elem).getName().getValue();

            // データ型
            VariableDefinition varMem = null;
            if (StringUtils.isNullOrEmpty(id_type)) {
                // 列挙体の場合は、typeはない。
                varMem = defParser.parseVariableDefinition((Id)id_elem);
            }
            else if (EnumPrimitiveType.isPrimitiveType(id_type)) {
                varMem = defParser.parseVariableDefinition(id_name, EnumPrimitiveType.getClangDataType(id_type));
            }
            else {
                IXmlTypeTableChoice[] typeidChoices = this.typeManager.findTypes(id_type);
                varMem = defParser.parseVariableDefinition(id_name, typeidChoices);
            }

            if (varMem != null) {
                // 構造体メンバ追加
                typeDef.add(varMem);
            }
        }

        return varType;
    }

    /**
     * 構造体要素からDB::VariableTypeクラスをパース、生成する。
     *
     * @param tag_name          構造体名
     * @return DB::VariableTypeクラス
     */
    public VariableType parseStructType(String  tag_name) {
        if (tag_name == null) return null;

        // 構造体名
        Id tagid = this.typeManager.findTagname(tag_name);
        return parseVariableType(tagid);
    }

    /**
     * シンボル:Id要素からDB::VariableTypeクラスをパース、生成する。
     * @param var_id		シンボル:Id要素
     * @return DB::VariableTypeクラス
     */
    public VariableType parseVariableType(Id var_id) {

        // データ型
        VariableType varType = null;
        String type_name = var_id.getType();
        if (EnumPrimitiveType.isPrimitiveType(type_name)) {
            varType = parseVarPrimitiveType(EnumPrimitiveType.getClangDataType(type_name));
        }
        else {
            IXmlTypeTableChoice[] typeChoices = this.typeManager.findTypes(type_name);
            varType = parseVarTypes(typeChoices);
        }

        return varType;
    }

    /**
     * 共同体要素からDB::VariableTypeクラスをパース、生成する。
     *
     * @param unionType          共同体の定義
     * @return DB::VariableTypeクラス
     */
    public VariableType parseVarUnionType(UnionType unionType) {

        // StructTypeとUnionTypeは同一構造であるので、StructTypeに変換する
        StructType structType = new StructType();
        structType.setType(unionType.getType());
        structType.setSymbols(unionType.getSymbols());

        // 構造体のVariableTypeクラスをパース、生成する。
        VariableType varType = this.parseStructType(structType);
        // ブロックタイプをUNIONに設定する.
        Structure structure = varType.getStructure();
        structure.setBlockType(BlockType.UNION);

        return varType;
    }


    /**
     * 列挙体要素からDB::VariableTypeクラスをパース、生成する。
     *
     * @param enumType          列挙体の定義
     * @return DB::VariableTypeクラス
     */
    public VariableType parseVarEnumType(EnumType enumType) {

        // StructTypeとenumTypeは同一構造であるので、StructTypeに変換する
        StructType structType = new StructType();
        structType.setType(enumType.getType());
        structType.setSymbols(enumType.getSymbols());

        // 構造体のVariableTypeクラスをパース、生成する。
        VariableType varType = this.parseStructType(structType);
        // ブロックタイプをENUMに設定する.
        Structure structure = varType.getStructure();
        structure.setBlockType(BlockType.ENUM);

        return varType;
    }


    /**
     * 関数定義要素からDB::VariableTypeクラスをパース、生成する。
     *
     * @param functionType           関数定義要素
     * @return DB::VariableTypeクラス
     */
    public VariableType parseVarFunctionType(FunctionType functionType) {

        // データ型
        VariableType varType = null;
        String refName = functionType.getReturnType();
        if (EnumPrimitiveType.isPrimitiveType(refName)) {
            varType = parseVarPrimitiveType(EnumPrimitiveType.getClangDataType(refName));
        }
        else {
            IXmlTypeTableChoice[] typeChoices = this.typeManager.findTypes(refName);
            varType = parseVarTypes(typeChoices);
        }

        return varType;
    }


    /**
     * データ型リストからDB::VariableTypeクラスをパース、生成する。
     * @param typeChoices             データ型リスト
     * @return DB::VariableTypeクラス
     */
    public VariableType parseVarTypes(IXmlTypeTableChoice[] typeChoices) {
        if (typeChoices == null) return null;

        IXmlTypeTableChoice top_type = typeChoices[0];
        IXmlTypeTableChoice last_type = typeChoices[typeChoices.length-1];

        VariableType varType = null;
        if (last_type instanceof BasicType) {
            varType = this.parseVarBasicType((BasicType)last_type);
        }
        else if (last_type instanceof StructType) {
            varType = this.parseStructType((StructType)last_type);
        }
        else if (last_type instanceof UnionType) {
            varType = this.parseVarUnionType((UnionType)last_type);
        }
        else if (last_type instanceof EnumType) {
            varType = this.parseVarEnumType((EnumType)last_type);
        }
        else if (top_type instanceof FunctionType) {
            varType = this.parseVarFunctionType((FunctionType)top_type);
        }
        varType = parseAttributes(varType, typeChoices);

        return varType;
    }

    /**
     * 構造体の入れ子リストを取得する
     * @return		構造体の入れ子リスト
     */
    public List<jp.riken.kscope.language.fortran.Structure> getStackType() {
        return stackType;
    }

    /**
     * 構造体の入れ子リストを設定する
     * @param stackType		構造体の入れ子リスト
     */
    public void setStackType(List<jp.riken.kscope.language.fortran.Structure> stackType) {
        this.stackType = stackType;
    }

    /**
     * 構造体の入れ子リストに追加する
     * @param type		追加構造体
     */
    private void addStackType(jp.riken.kscope.language.fortran.Structure type) {
        if (type == null) return;
        if (this.stackType == null) {
            this.stackType = new ArrayList<jp.riken.kscope.language.fortran.Structure>();
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
    private jp.riken.kscope.language.fortran.Structure getStackType(String typeName) {
        if (StringUtils.isNullOrEmpty(typeName)) return null;
        if (this.stackType == null) return null;

        for (jp.riken.kscope.language.fortran.Structure men : this.stackType) {
            if (typeName.equalsIgnoreCase(men.getName())) {
                return men;
            }
        }

        return null;
    }

    /**
     * 配列要素からDB::VariableTypeクラスをパース、生成する。
     *
     * @param arrayType         配列要素
     * @return DB::VariableTypeクラス
     */
    public VariableType parseVarArrayType(ArrayType arrayType) {

        if (arrayType == null) return null;
        if (arrayType.getElementType() == null) return null;

        // データ型
        VariableType varType = null;
        String elem_type = arrayType.getElementType();
        if (EnumPrimitiveType.isPrimitiveType(elem_type)) {
            varType = parseVarPrimitiveType(EnumPrimitiveType.getClangDataType(elem_type));
        }
        else {
            IXmlTypeTableChoice typeChoice = this.typeManager.findType(elem_type);
            if (typeChoice instanceof BasicType) {
                varType = parseVarBasicType((BasicType) typeChoice);
            } else if (typeChoice instanceof StructType) {
                varType = parseStructType((StructType) typeChoice);
            } else if (typeChoice instanceof UnionType) {
                varType = parseVarUnionType((UnionType) typeChoice);
            }
        }
        if (varType == null) return null;

        return varType;
    }

    /**
     * DB::VariableTypeの属性を設定する。
     * @param var_type            VariableType:変数・構造体データ型
     * @param typeChoices        データ型定義要素リスト
     * @return DB::VariableTypeクラス
     */
    private VariableType parseAttributes(VariableType var_type, IXmlTypeTableChoice[] typeChoices) {
        if (var_type == null) return null;

        // 属性
        List<String> listAttr = new ArrayList<String>();

        boolean is_pointer = false;
        boolean is_array = false;
        boolean is_const = false;
        boolean is_volatile = false;
        boolean is_restrict = false;
        boolean is_function = false;
        if (typeChoices != null) {
            for (IXmlTypeTableChoice type : typeChoices) {
                // ポインタ
                if (type instanceof PointerType) is_pointer = true;
                // 関数
                if (type instanceof FunctionType) is_function = true;
                // 配列
                if (type instanceof ArrayType) is_array = true;
                BaseType base_type = (BaseType)type;
                if (StringUtils.toBoolean(base_type.getIsConst()))  is_const = true;
                if (StringUtils.toBoolean(base_type.getIsVolatile()))  is_volatile = true;
                if (StringUtils.toBoolean(base_type.getIsRestrict()))  is_restrict = true;

                if (is_pointer)  listAttr.add("pointer");
                if (is_array)    listAttr.add("array");
                if (is_const)    listAttr.add("const");
                if (is_volatile) listAttr.add("volatile");
                if (is_restrict) listAttr.add("restrict");
                if (is_function) listAttr.add("function");
            }
        }

        if (listAttr.size() > 0) {
            VariableAttribute attr = new VariableAttribute(listAttr);
            var_type.setVariableAttributes(attr);
        }

        return var_type;
    }

}
