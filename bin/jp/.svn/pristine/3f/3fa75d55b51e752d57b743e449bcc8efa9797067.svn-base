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
package jp.riken.kscope.xcodeml.xml;

/**
 * シンボルリストクラス
 * @author riken
 *
 */
public class XmlSymbol {
    /** シンボル名 */
    private String _symbolName;
    /** タイプID */
    private EnumType _typeId;
    /** 派生名 */
    private String _derivedName;

    /**
     * コンストラクタ
     * @param symbolName		シンボル名
     */
    public XmlSymbol(String symbolName) {
        this(symbolName, EnumType.VOID, null);
    }

    /**
     * コンストラクタ
     * @param symbolName		シンボル名
     * @param typeId		タイプID
     */
    public XmlSymbol(String symbolName, EnumType typeId) {
        this(symbolName, typeId, null);
    }

    /**
     * コンストラクタ
     * @param symbolName		シンボル名
     * @param typeId		タイプID
     * @param derivedName		派生名
     */
    public XmlSymbol(String symbolName, EnumType typeId, String derivedName) {
        _symbolName = symbolName;
        _typeId = typeId;
        _derivedName = derivedName;
    }

    /**
     * タイプIDを取得する
     * @return		タイプID
     */
    public EnumType getTypeId() {
        return _typeId;
    }

    /**
     * シンボル名を取得する
     * @return		シンボル名
     */
    public String getSymbolName() {
        return _symbolName;
    }

    /**
     * 派生名を取得する
     * @return		派生名
     */
    public String getDerivedName() {
        return _derivedName;
    }
}
