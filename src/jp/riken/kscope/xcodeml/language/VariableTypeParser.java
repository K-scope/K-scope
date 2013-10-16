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
 * VariableTypeパーサクラス
 * @author riken
 */
public class VariableTypeParser {

    /** typeTable */
    private XcodeMLTypeManager typeManager;

    /** 構造体の入れ子リスト */
    private List<jp.riken.kscope.language.fortran.Type> stackType;


    /**
     * コンストラクタ
     *
     * @param typeManager    typeTable
     */
    public VariableTypeParser(XcodeMLTypeManager typeManager) {
        this.typeManager = typeManager;
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
     * XcodeML::NameクラスからDB::VariableTypeクラスをパース、生成する.<br/>
     * 関数データ型は取得しない。
     *
     * @param name           変数、構造体宣言
     * @return DB::VariableTypeクラス
     */
    public VariableType parseVariableType(Name name) {

        // データ型
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
     * XcodeML::visitableクラスからDB::VariableTypeクラスをパース、生成する.
     *
     * @param visitable           構造体宣言
     * @return DB::VariableTypeクラス
     */
    public VariableType parseVariableType(FstructDecl visitable) {

        // 変数名
        Name name = visitable.getName();

        // データ型
        IXmlTypeTableChoice typeChoice = this.typeManager.findType(name);
        if (!(typeChoice instanceof FstructType)) {
            return null;
        }
        FstructType structType = (FstructType)typeChoice;

        return parseVarDefStructType(structType);
    }

    /**
     * XcodeML::NameクラスからDB::VariableTypeクラスをパース、生成する.<br/>
     * 関数データ型も取得する。
     *
     * @param name           変数、構造体宣言、関数名
     * @return DB::VariableTypeクラス
     */
    public VariableType parseVariableTypeWithFfunctionType(Name name) {

        // データ型
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
     * 型定義要素からDB::VariableTypeクラスをパース、生成する。
     *
     * @param type            プリミティブ型や他の型定義要素への参照の定義
     * @return DB::VariableTypeクラス
     */
    public VariableType parseVarDefEnumType(EnumType type) {
        // データ型
        String type_name = type.fortranName();

        PrimitiveDataType primitive = PrimitiveDataType.findTypeBy(type_name);
        VariableType varType = new VariableType(primitive);

        return varType;
    }

    /**
     * 型定義要素からDB::VariableTypeクラスをパース、生成する。
     *
     * @param basicType         プリミティブ型や他の型定義要素への参照の定義
     * @return DB::VariableTypeクラス
     */
    public VariableType parseVarDefBasicType(FbasicType basicType) {

        // データ型
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

        // ExprModelパーサ;
        ExpressionParser exprParser = new ExpressionParser(this.typeManager);

        // データバイト数
        Kind kindElem = basicType.getKind();
        if (kindElem != null) {
            // データ型種別のパース
            exprParser.setParseNode(kindElem);
            Expression expr = exprParser.getExpression();
            if (expr != null) {
                varType.setKind(expr);
            }
        }

        // CHARACTOR文字列長
        Len lenElem = basicType.getLen();
        if (lenElem != null) {
            exprParser.setParseNode(lenElem);
            Expression expr = exprParser.getExpression();
            if (expr != null) {
                varType.setLen(expr);
            }
            else {
                varType.setLen(new Expression("*"));
            }
        }

        return varType;
    }

    /**
     * 構造体要素からDB::VariableTypeクラスをパース、生成する。
     *
     * @param structType          構造体の定義
     * @return DB::VariableTypeクラス
     */
    public VariableType parseVarDefStructType(FstructType structType) {

        // 構造体名
        String structTypeName = structType.getType();
        String structName = this.typeManager.getAliasTypeName(structType.getType());

        // modify by @hira at 2013/02/01
        if (structName == null) {
            return null;
        }

        // 入れ子構造体であるかチェックする
        if (this.containsStackType(structName)) {
            // 定義済みであるので、作成済み構造体を返す。
            return new VariableType(this.getStackType(structName));
        }

        // DB::Typeクラス生成
        jp.riken.kscope.language.fortran.Type typeDef = new jp.riken.kscope.language.fortran.Type(structName);
        this.addStackType(typeDef);

        // 変数宣言パーサ
        VariableDefinitionParser defParser = new VariableDefinitionParser(this.typeManager);
        defParser.setStackType(this.getStackType());

        // 構造体メンバ
        Symbols symbols = structType.getSymbols();
        if (symbols == null)
            return null;

        List<Id> ids = symbols.getId();
        for (Id id_elem : ids) {
            String id_type = id_elem.getType();
            String id_name = id_elem.getName().getValue();

            // データ型
            EnumType refTypeId = EnumType.getTypeIdFromXcodemlTypeName(id_type);
            if (refTypeId == null || refTypeId.isPrimitive() == false) {
                IXmlTypeTableChoice typeidChoice = this.typeManager.findType(id_type);
                VariableDefinition varMem = null;
                if (typeidChoice instanceof FbasicType) {
                    String ref = ((FbasicType)typeidChoice).getRef();
                    varMem = defParser.parseVarDefBasicType(id_name, (FbasicType) typeidChoice);
                    /*
                    if (structTypeName.equals(ref)) {
                        // 構造体メンバ=同一構造体メンバ追加
                        varMem = new VariableType(typeDef);
                    }
                    else {
                        varMem = defParser.parseVarDefBasicType(id_name, (FbasicType) typeidChoice);
                    }
                    */
                } else if (typeidChoice instanceof FstructType) {
                    String ref = ((FstructType)typeidChoice).getType();
                    varMem = defParser.parseVarDefStructType(id_name, (FstructType) typeidChoice);
                    /*
                    if (structTypeName.equals(ref)) {
                        // 構造体メンバ=同一構造体メンバ追加
                        varMem = new VariableType(typeDef);
                    }
                    else {
                        varMem = parseVarDefStructType((FstructType) typeidChoice);
                    }
                    */
                }
                if (varMem != null) {
                    // 構造体メンバ追加
                    typeDef.add(varMem);
                }
            } else {
                PrimitiveDataType primitive = PrimitiveDataType.findTypeBy(refTypeId.fortranName());
                VariableType varType = new VariableType(primitive);
                typeDef.add(varType, id_name);
            }

        }

        // 構造体のデータ型を設定する
        VariableType varType = new VariableType(typeDef);

        return varType;
    }

    /**
     * 関数定義要素からDB::VariableTypeクラスをパース、生成する。
     *
     * @param functionType           関数定義要素
     * @return DB::VariableTypeクラス
     */
    public VariableType parseVarDefFunctionType(FfunctionType functionType) {

        // データ型
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
