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

import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

import jp.riken.kscope.exception.XcodeMLException;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.xcodeml.xml.EnumType;
import jp.riken.kscope.xcodeml.xml.IXmlTypeTableChoice;
import jp.riken.kscope.xcodeml.xml.gen.DefChoiceSclass;
import jp.riken.kscope.xcodeml.xml.gen.FbasicType;
import jp.riken.kscope.xcodeml.xml.gen.FfunctionType;
import jp.riken.kscope.xcodeml.xml.gen.FstructType;
import jp.riken.kscope.xcodeml.xml.gen.Id;
import jp.riken.kscope.xcodeml.xml.gen.Name;

/**
 * Type and Symbol manager.
 *
 * @author riken
 */
public class XcodeMLTypeManager {
    /** データ型テーブル */
    private TypeMap m_typeMap;
    /** シンボルスタック */
    private SymbolMapStack m_symbolMapStack;
    /** 参照型識別子スタック */
    private AliasMapStack m_aliasMapStack;
    /** 参照型識別子テーブル */
    private AliasMap m_reverseBasicRefMap;

    /**
     * データ型テーブルクラス
     * @author riken
     */
    private class TypeMap extends HashMap<String, IXmlTypeTableChoice> {
        /** シリアル番号 */
        private static final long serialVersionUID = 1L;

        /**
         * TypeMapを文字列にする
         * @return		TypeMap文字列
         */
        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append("[TypeMap, key(type name) -> value(class)]\n");
            for (Map.Entry<String, IXmlTypeTableChoice> entry : entrySet()) {
                sb.append(entry.getKey());
                sb.append(" -> ");
                sb.append(entry.getValue().getClass().getSimpleName());
                sb.append("\n");
            }
            return sb.toString();
        }
    }

    /**
     * シンボルテーブルクラス
     * @author riken
     */
    private class SymbolMap extends HashMap<String, Id> {
        /** シリアル番号 */
        private static final long serialVersionUID = 1L;

        /**
         * SymbolMapを文字列にする
         * @return		SymbolMap文字列
         */
        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append("[SymbolMap, key(symbol name) -> value(type name)]\n");
            for (Map.Entry<String, Id> entry : entrySet()) {
                sb.append(entry.getKey());
                sb.append(" -> ");
                sb.append(entry.getValue().getType());
                sb.append("\n");
            }
            return sb.toString();
        }
    }

    /**
     * シンボルスタッククラス
     * @author riken
     *
     */
    private class SymbolMapStack extends LinkedList<SymbolMap> {
        /** シリアル番号 */
        private static final long serialVersionUID = 1L;

        /**
         * SymbolMapStackを文字列にする
         * @return		SymbolMapStack文字列
         */
        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append("[SymbolMapStack]\n");
            for (SymbolMap symbolMap : m_symbolMapStack) {
                sb.append(symbolMap.toString());
            }
            return sb.toString();
        }
    }

    /**
     * 参照型識別子テーブルクラス
     * @author riken
     */
    private class AliasMap extends HashMap<String, String> {
        /** シリアル番号 */
        private static final long serialVersionUID = 1L;

        /**
         * AliasMapを文字列にする
         * @return		AliasMap文字列
         */
        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append("[AliasMap, key(type) -> value(alias type)]\n");
            for (Map.Entry<String, String> entry : entrySet()) {
                sb.append(entry.getKey());
                sb.append(" -> ");
                sb.append(entry.getValue());
                sb.append("\n");
            }
            return sb.toString();
        }
    }

    /**
     * 参照型識別子スタッククラス
     * @author riken
     */
    private class AliasMapStack extends LinkedList<AliasMap> {
        /** シリアル番号 */
        private static final long serialVersionUID = 1L;

        /**
         * AliasMapStackを文字列にする
         * @return		AliasMapStack文字列
         */
        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append("[AliasMapStack]\n");
            for (AliasMap aliasMap : m_aliasMapStack) {
                sb.append(aliasMap.toString());
            }
            return sb.toString();
        }
    }

    /**
     * データ型テーブルクラス
     * @author riken
     */
    public class TypeList extends LinkedList<IXmlTypeTableChoice> {
        /** シリアル番号 */
        private static final long serialVersionUID = 1L;

        /**
         * TypeListを文字列にする
         * @return		TypeList文字列
         */
        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append("[TypeList, element name -> type name]\n");
            for (IXmlTypeTableChoice typeChoice : this) {
                sb.append(typeChoice.getClass().getSimpleName());
                sb.append(" -> ");
                sb.append(typeChoice.getType());
                sb.append("\n");
            }
            return sb.toString();
        }
    }

    /**
     * コンストラクタ
     */
    public XcodeMLTypeManager() {
        m_typeMap = new TypeMap();
        m_symbolMapStack = new SymbolMapStack();
        m_aliasMapStack = new AliasMapStack();
        m_reverseBasicRefMap = new AliasMap();
    }

    /**
     * シンボルスタックを取得する
     * @return		シンボルスタック
     */
    private SymbolMap _getCurrentSymbolMap() {
        if (m_symbolMapStack.size() <= 0) {
            m_symbolMapStack.push(new SymbolMap());
        }
        return m_symbolMapStack.peekFirst();
    }

    /**
     * 参照型識別子スタックを取得する
     * @return		参照型識別子スタック
     */
    private AliasMap _getCurrentAliasMap() {
        if (m_aliasMapStack.size() <= 0) {
            m_aliasMapStack.push(new AliasMap());
        }
        return m_aliasMapStack.peekFirst();
    }

    /**
     * データ型要素のパースを開始する
     */
    public void enterScope() {
        m_symbolMapStack.push(new SymbolMap());
        m_aliasMapStack.push(new AliasMap());
    }

    /**
     * データ型要素のパースを終了する
     */
    public void leaveScope() {
        m_symbolMapStack.pop();
        m_aliasMapStack.pop();
    }

    /**
     * シンボルを追加する
     * @param id		シンボルID要素
     */
    public void addSymbol(Id id) {
        Name name = id.getName();
        if (name == null) {
            // Ignore invalid symbol name.
            return;
        }

        String symbolName = name.getValue();
        if (symbolName == null) {
            // Ignore invalid symbol name.
            return;
        }

        // Trim key word string.
        symbolName = symbolName.trim();
        if (symbolName.isEmpty() != false) {
            // Ignore invalid symbol name.
            return;
        }

        if (id.getSclass() == null)
            return;

        if (DefChoiceSclass.FTYPE_NAME.equals(id.getSclass())) {
            String typeName = id.getType();
            if (StringUtils.isNullOrEmpty(typeName) != false) {
                // Ignore invalid type name.
                return;
            }
            AliasMap aliasMap = _getCurrentAliasMap();
            assert (aliasMap != null);
            aliasMap.put(typeName, symbolName);
        } else if (!DefChoiceSclass.FCOMMON_NAME.equals(id.getSclass())
                && !DefChoiceSclass.FNAMELIST_NAME.equals(id.getSclass())) {
            SymbolMap symbolMap = _getCurrentSymbolMap();
            assert (symbolMap != null);
            symbolMap.put(symbolName, id);
        }
    }

    /**
     * シンボルを検索する
     * @param symbolName		シンボル名
     * @return		シンボルID要素
     */
    public Id findSymbol(String symbolName) {
        Id id = null;
        symbolName = symbolName.trim();
        for (SymbolMap symbolMap : m_symbolMapStack) {
            id = symbolMap.get(symbolName);
            if (id != null) {
                break;
            }
        }
        return id;
    }

    /**
     * データ型要素を追加する
     * @param type		データ型要素
     */
    public void addType(IXmlTypeTableChoice type) {
        String typeName = type.getType();
        if (StringUtils.isNullOrEmpty(typeName) != false) {
            // Ignore invalid type name.
            return;
        }

        // Trim key word string.
        typeName = typeName.trim();

        if (typeName.isEmpty() != false) {
            // Ignore invalid type name.
            return;
        }

        m_typeMap.put(typeName, type);

        if (type instanceof FbasicType && ((FbasicType) type).getRef() != null
                && ((FbasicType) type).getIndexRangeOrArrayIndex() == null) {
            m_reverseBasicRefMap.put(((FbasicType) type).getRef(),
                    type.getType());
        }
    }

    /**
     * タイプ要素を検索する
     * @param typeName		タイプ名
     * @return			タイプ要素
     */
    public IXmlTypeTableChoice findType(String typeName) {
        if (StringUtils.isNullOrEmpty(typeName) != false) {
            return null;
        }

        // Trim key word string.
        return m_typeMap.get(typeName.trim());
    }

    /**
     * タイプ要素を検索する
     * @param nameElem		タイプ名要素
     * @return			タイプ要素
     */
    public IXmlTypeTableChoice findType(Name nameElem) {
        if (nameElem == null) {
            return null;
        }

        String typeName = nameElem.getType();
        if (StringUtils.isNullOrEmpty(typeName) != false) {
            return findTypeFromSymbol(nameElem.getValue());
        }
        return findType(typeName);
    }

    /**
     * Find type element from symbol name.
     *
     * @param symbolName
     *            Symbol name.
     * @return IXmlTypeTableChoice interface or null.<br/>
     *         If null, type is not found.
     */
    public IXmlTypeTableChoice findTypeFromSymbol(String symbolName) {
        if (StringUtils.isNullOrEmpty(symbolName) != false) {
            return null;
        }

        Id id = findSymbol(symbolName);
        if (id == null) {
            return null;
        }

        return findType(id.getType());
    }

    /**
     * タイプを追加する
     *
     * @param typeId		タイプID
     * @param typeName		タイプ名
     */
    public void putAliasTypeName(String typeId, String typeName) {
        if (StringUtils.isNullOrEmpty(typeName)) {
            return;
        }

        if (StringUtils.isNullOrEmpty(typeId)) {
            return;
        }

        AliasMap currentAlias = _getCurrentAliasMap();

        currentAlias.put(typeId, typeName);
    }

    /**
     * Get alias of type name.
     *
     * @param typeName		タイプ名
     * @return When alias not found, return argument type name.
     */
    public String getAliasTypeName(String typeName) {
        if (StringUtils.isNullOrEmpty(typeName) != false) {
            return null;
        }

        // Trim key word string.
        typeName = typeName.trim();
        for (AliasMap aliasMap : m_aliasMapStack) {
            String aliasName = aliasMap.get(typeName);
            if (aliasName != null) {
                return aliasName;
            }
        }

        String inheritName = m_reverseBasicRefMap.get(typeName);
        if (inheritName != null) {
            return getAliasTypeName(inheritName);
        }

        // modify by @hira at 2013/02/01
        // throw new IllegalStateException("not found type name of '" + typeName + "'");
        return null;
    }

    /**
     * Get type reference list.
     *
     * @param typeName
     *            Type name.
     * @return First node of list is top level type.
     * @throws XcodeMLException thrown if FbasicType has cyclic definition.
     */
    public TypeList getTypeReferenceList(String typeName)
            throws XcodeMLException {
        TypeList typeList = new TypeList();

        if (StringUtils.isNullOrEmpty(typeName) != false) {
            // Return empty type list.
            return typeList;
        }

        IXmlTypeTableChoice typeChoice = findType(typeName);
        while (typeChoice != null) {
            typeList.addFirst(typeChoice);

            if (typeChoice instanceof FbasicType) {
                FbasicType basicType = (FbasicType) typeChoice;
                String refType = basicType.getRef();

                if (EnumType.DERIVED != EnumType
                        .getTypeIdFromXcodemlTypeName(refType))
                    break;

                typeChoice = findType(refType);

                if (typeList.contains(typeChoice))
                    throw new XcodeMLException("FbasicType"
                            + basicType.getType() + "has cyclic definition");

            } else if (typeChoice instanceof FstructType) {
                typeChoice = null;
            } else if (typeChoice instanceof FfunctionType) {
                typeChoice = null;
            } else {
                // Impossible.
                assert (false);
            }
        }

        return typeList;
    }

    /**
     * Gets a type id of the last type of a type reference list.
     *
     * @param typeName
     *            the top type of the type reference list.
     * @return if bottomType is not found returns null, else returns the last of
     *         the type reference list.
     */
    public String getBottomTypeName(String typeName) {
        if (typeName == null)
            throw new IllegalArgumentException();

        if (EnumType.DERIVED != EnumType.getTypeIdFromXcodemlTypeName(typeName))
            return typeName;

        TypeList typeList = null;

        try {
            typeList = getTypeReferenceList(typeName);
        } catch (XcodeMLException e) {
            return null;
        }

        if (typeList == null || typeList.size() == 0) {
            return null;
        }

        IXmlTypeTableChoice typeElem = typeList.getLast();

        if (typeElem instanceof FbasicType) {
            FbasicType basicTypeElem = (FbasicType) typeElem;
            return basicTypeElem.getRef();
        } else if (typeElem instanceof FstructType) {
            FstructType structTypeElem = (FstructType) typeElem;
            return structTypeElem.getType();
        } else if (typeElem instanceof FfunctionType) {
            FfunctionType functionTypeElem = (FfunctionType) typeElem;
            return functionTypeElem.getType();
        }

        // not reached.
        return null;
    }

    /**
     * return if xcodemlTypeName matchs the type includes its reference.
     *
     * @param xcodemlTypeName
     *            type name
     * @param type
     *            type
     * @return return true if matchs.
     */
    public boolean isTypeOf(String xcodemlTypeName, EnumType type) {
        return type.xcodemlName().equals(getBottomTypeName(xcodemlTypeName));
    }

    /**
     * return if xcodemlTypeName is not the type which is able to be decompiled
     * as Fortran token.
     *
     * @param xcodemlTypeName		タイプ名
     * @return			true=decompile
     */
    public boolean isDecompilableType(String xcodemlTypeName) {
        return isTypeOf(xcodemlTypeName, EnumType.VOID) == false
                && isTypeOf(xcodemlTypeName, EnumType.NUMERIC) == false
                && isTypeOf(xcodemlTypeName, EnumType.NUMERICALL) == false;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(m_typeMap.toString());
        sb.append(m_aliasMapStack.toString());
        sb.append(m_symbolMapStack.toString());
        return sb.toString();
    }
}
