package jp.riken.kscope.xcodeml.clang;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

import jp.riken.kscope.exception.XcodeMLException;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.xcodeml.clang.utils.XmlNodeUtil;
import jp.riken.kscope.xcodeml.clang.xml.DefChoiceSclass;
import jp.riken.kscope.xcodeml.clang.xml.IXmlNode;
import jp.riken.kscope.xcodeml.clang.xml.IXmlTypeTableChoice;
import jp.riken.kscope.xcodeml.clang.xml.gen.ArrayType;
import jp.riken.kscope.xcodeml.clang.xml.gen.BasicType;
import jp.riken.kscope.xcodeml.clang.xml.gen.EnumType;
import jp.riken.kscope.xcodeml.clang.xml.gen.FunctionType;
import jp.riken.kscope.xcodeml.clang.xml.gen.PointerType;
import jp.riken.kscope.xcodeml.clang.xml.gen.StructType;
import jp.riken.kscope.xcodeml.clang.xml.gen.UnionType;
import jp.riken.kscope.xcodeml.clang.xml.gen.Id;
import jp.riken.kscope.xcodeml.clang.xml.gen.Name;



/**
 * Type and Symbol manager.
 *
 * @author RIKEN
 */
public class XcodeMLTypeManager {

    /** データ型テーブル */
    private TypeMap _typeMap;
    /** Symbol(ID)スタック : auto,param,extern,extern_def,static,register,moe,typedef_name */
    private SymbolMapStack  _mainSymbolStack;
    /** Symbol(ID)スタック : tagname(構造体タグ名) */
    private SymbolMapStack _tagSymbolStack;
    /** Symbol(ID)スタック : label,gccLabel(ラベル) */
    private SymbolMapStack _labelSymbolStack;

    /** 参照型識別子スタック */
    private AliasMapStack _aliasMapStack;
    /** 参照型識別子テーブル */
    private AliasMap _resolvedTypeList;

    /**
     * データ型テーブルクラス
     * @author RIKEN
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
     * @author RIKEN
     */
    private class SymbolMap extends HashMap<String, Id> {
        /** シリアル番号 */
        private static final long serialVersionUID = 1L;

        /**
         * Id/Name/text()と同じシンボル名が存在するかチェックする.
         * @param name		シンボル名
         * @return			一致Idが存在する.
         */
        public boolean containsName(String name) {
            Id id = this.findName(name);
            if (id == null) return false;
            return true;
        }

        /**
         * Id[@type]と同じシンボルタイプが存在するかチェックする.
         * @param type		シンボルタイプ
         * @return			一致Idが存在する.
         */
        public boolean containsType(String type) {
            Id id = this.findType(type);
            if (id == null) return false;
            return true;
        }

        /**
         * Id/Name/text()と同じシンボル名のIDを取得する
         * @param name		シンボル名
         * @return			シンボル名一致Id
         */
        public Id findName(String name) {
            if (name == null) return null;
            return this.get(name);
        }

        /**
         * Id[@type]と同じシンボルタイプのIdを取得する.
         * @param type		シンボルタイプ
         * @return			シンボルタイプ一致Id
         */
        public Id findType(String type) {
            if (type == null) return null;
            for (Map.Entry<String, Id> entry : this.entrySet()) {
                Id id = entry.getValue();
                if (id == null) continue;
                if (id.getType() == null) continue;
                String symbol_type = id.getType();
                if (type.equals(symbol_type)) {
                    return id;
                }
            }

            return null;
        }

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
     * @author RIKEN
     *
     */
    private class SymbolMapStack extends LinkedList<SymbolMap> {
        /** シリアル番号 */
        private static final long serialVersionUID = 1L;

        /**
         * シンボル名のIdを取得する。
         * @param name		シンボル名
         * @return		シンボルID
         */
        public Id findName(String name) {

            for (SymbolMap symbols : this) {
                Id id = symbols.findName(name);
                if (id == null) continue;
                String type = id.getType();
                if (type == null) continue;
                return id;
            }
            return null;
        }

        /**
         * シンボルタイプ名のIdを取得する。
         * @param name		シンボルタイプ名
         * @return		シンボルID
         */
        public Id findType(String type) {

            for (SymbolMap symbols : this) {
                Id id = symbols.findType(type);
                if (id == null) continue;
                return id;
            }
            return null;
        }
        /**
         * SymbolTableStackを文字列にする
         * @return		SymbolTableStack文字列
         */
        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append("[SymbolMapStack]\n");
            for (SymbolMap symbolMap : this) {
                sb.append(symbolMap.toString());
            }
            return sb.toString();
        }
    }

    /**
     * 参照型識別子テーブルクラス
     * @author RIKEN
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
     * @author RIKEN
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
            for (AliasMap aliasMap : this) {
                sb.append(aliasMap.toString());
            }
            return sb.toString();
        }
    }

    /**
     * データ型テーブルクラス
     * @author RIKEN
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
        this.initialize();
    }

    /**
     * 初期化を行う
     */
    public void initialize() {
        this._typeMap = new TypeMap();
        this._mainSymbolStack = new SymbolMapStack();
        this._tagSymbolStack = new SymbolMapStack();
        this._labelSymbolStack = new SymbolMapStack();
        this._aliasMapStack = new AliasMapStack();
        this._resolvedTypeList = new AliasMap();
    }

    /**
     * 初期化を行う
     */
    public void clearType() {
        this._typeMap = new TypeMap();
    }


    /**
     * 初期化を行う
     */
    public void clearSymbol() {
        this._mainSymbolStack = new SymbolMapStack();
        this._tagSymbolStack = new SymbolMapStack();
        this._labelSymbolStack = new SymbolMapStack();
    }

    /**
     * Symbol(ID)スタック : auto,param,extern,extern_def,static,register,moe,typedef_nameを取得する
     * @return		シンボルスタック
     */
    private SymbolMap _getCurrentMainSymbolMap() {
        if (this._mainSymbolStack.size() <= 0) {
            this._mainSymbolStack.push(new SymbolMap());
        }
        return this._mainSymbolStack.peekFirst();
    }

    /**
     * Symbol(ID)スタック : tagnameを取得する
     * @return		シンボルスタック
     */
    private SymbolMap _getCurrentTagSymbolMap() {
        if (this._tagSymbolStack.size() <= 0) {
            this._tagSymbolStack.push(new SymbolMap());
        }
        return this._tagSymbolStack.peekFirst();
    }

    /**
     * Symbol(ID)スタック : labelを取得する
     * @return		シンボルスタック
     */
    private SymbolMap _getCurrentLabelSymbolMap() {
        if (this._labelSymbolStack.size() <= 0) {
            this._labelSymbolStack.push(new SymbolMap());
        }
        return this._labelSymbolStack.peekFirst();
    }


    /**
     * データ型要素のパースを開始する.
     * Symbol(ID)スタックの先頭にSymbol(ID)テーブルを追加する.
     */
    public void enterScope() {
        this._mainSymbolStack.push(new SymbolMap());
        this._tagSymbolStack.push(new SymbolMap());
        this._labelSymbolStack.push(new SymbolMap());
        this._aliasMapStack.push(new AliasMap());
    }

    /**
     * データ型要素のパースを終了する
     * Symbol(ID)スタックの先頭のSymbol(ID)テーブルを削除する.
     */
    public void leaveScope() {
        this._mainSymbolStack.pop();
        this._tagSymbolStack.pop();
        this._labelSymbolStack.pop();
        this._aliasMapStack.pop();
    }

    /**
     * シンボルを追加する
     * @param id		シンボルID要素
     * @throws XcodeMLException
     */
    public void addSymbol(Id id) throws XcodeMLException {
        if (id == null) return;
        if (id.getName() == null) return;
        if (id.getType() == null) return;

        // id/name/text()
        Name name = id.getName();
        String symbolName = name.getValue();
        if (StringUtils.isTrimEmpty(symbolName)) return;
        symbolName = symbolName.trim();

        // id[@type]
        String typeName = id.getType();
        if (StringUtils.isTrimEmpty(typeName)) return;
        typeName = typeName.trim();

        // id[@sclass]
        String sclass = null;
        if (id.getSclass() != null) {
            sclass = id.getSclass();
            if (sclass != null) sclass = sclass.trim();
        }

        // ENUMメンバは対象外
        if (DefChoiceSclass.MOE.equals(sclass)) {
            return;
        }

        SymbolMap symbolMap = null;
        if (DefChoiceSclass.TAGNAME.equals(sclass)) {
            symbolMap = this._getCurrentTagSymbolMap();
        } else if (DefChoiceSclass.LABEL.equals(sclass)
                || DefChoiceSclass.GCCLABEL.equals(sclass)) {
            symbolMap = this._getCurrentLabelSymbolMap();
        }
        else {
            symbolMap = this._getCurrentMainSymbolMap();
        }
        if (symbolMap != null) {
            if (symbolMap.containsKey(symbolName)) {
                throw new XcodeMLException("Id"
                            + "[@sclass=" + sclass + "]/Name/text(" + symbolName + ") is duplicate.");
            }
            // シンボル追加
            symbolMap.put(symbolName, id);
        }

        return;
    }

    /**
     * シンボルを検索する
     * @param symbolName		シンボル名
     * @return		シンボルID要素
     */
    public Id findSymbol(String symbolName) {
        if (symbolName == null) return null;
        symbolName = symbolName.trim();

        Id id = null;
        id = this._mainSymbolStack.findName(symbolName);
        if (id != null) return id;
        id = this._tagSymbolStack.findName(symbolName);
        if (id != null) return id;
        id = this._labelSymbolStack.findName(symbolName);
        if (id != null) return id;

        return null;
    }

    /**
     * tagnameのシンボルタイプを検索する
     * @param type		シンボル名
     * @return		シンボルID要素
     */
    public Id findTagname(String type) {
        if (StringUtils.isNullOrEmpty(type)) return null;
        type = type.trim();

        Id id = this._tagSymbolStack.findType(type);
        return id;
    }


    /**
     * データ型要素を追加する
     * @param type		データ型要素
     * @throws XcodeMLException
     */
    public void addType(IXmlTypeTableChoice type) throws XcodeMLException {
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

        if (this._typeMap.containsKey(typeName)) {
            String elem_name = XmlNodeUtil.getElementName((IXmlNode)type);
            throw new XcodeMLException("typeTable/" + elem_name
                        + "[@" + type + "=" +  typeName + "] is duplicate.");
        }
        this._typeMap.put(typeName, type);

        return;
    }

    /**
     * タイプ要素を検索する
     * @param typeName		タイプ名
     * @return			タイプ要素
     */
    public IXmlTypeTableChoice[] findTypes(String typeName) {
        if (StringUtils.isTrimEmpty(typeName)) {
            return null;
        }

        typeName = typeName.trim();
        // BasicType, nullまで取得する.
        List<IXmlTypeTableChoice> list = new LinkedList<IXmlTypeTableChoice>();
        IXmlTypeTableChoice type = null;
        do {
            type = this.findType(typeName);
            if (type != null) {
                list.add(type);
                if (type instanceof ArrayType) {
                    typeName = ((ArrayType)type).getElementType();
                }
                else if (type instanceof FunctionType) {
                    typeName = ((FunctionType)type).getReturnType();
                }
                else if (type instanceof PointerType) {
                    typeName = ((PointerType)type).getRef();
                }
                else {
                    typeName = null;
                }
            }
        } while (type != null && !(type instanceof BasicType));

        if (list.size() <= 0) return null;

           return list.toArray(new IXmlTypeTableChoice[0]);
    }

    /**
     * タイプ要素を検索する
     * @param typeName		タイプ名
     * @return			タイプ要素
     */
    public IXmlTypeTableChoice findType(String typeName) {
        if (StringUtils.isTrimEmpty(typeName)) {
            return null;
        }

        // プリミティブデータ型であるかチェックする
        if (EnumPrimitiveType.isPrimitiveType(typeName)) {
            // BasicTypeとして返す。
            BasicType type = new BasicType();
            type.setType(typeName);
            type.setName(typeName);
            return type;
        }

        // Trim key word string.
        return this._typeMap.get(typeName.trim());
    }


    /**
     * タイプ要素を検索して、指定タイプ要素クラスの要素を取得する。
     * @param typeName		タイプ名
     * @param clazz        検索タイプ要素クラス
     * @return			タイプ要素
     */
    public IXmlTypeTableChoice findType(String typeName, Class<? extends IXmlNode> clazz) {

        IXmlTypeTableChoice[] types = this.findTypes(typeName);
        if (types == null || types.length <= 0) {
            return null;
        }

        for (IXmlTypeTableChoice type : types) {
            if (type.getClass().equals(clazz)) {
                return type;
            }
        }

        return null;
    }

    /**
     * タイプ要素を検索して、タイプ要素リストを取得する.
     * @param nameElem		タイプ名要素
     * @return			タイプ要素リスト
     */
    public IXmlTypeTableChoice[] findTypes(Id id) {
        if (id == null) {
            return null;
        }
        String type = id.getType();
        if (type == null) return null;
        return findTypes(type);
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
        if (StringUtils.isTrimEmpty(symbolName) != false) {
            return null;
        }

        Id id = findSymbol(symbolName);
        if (id == null) {
            return null;
        }

        return findType(id.getType());
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(this._typeMap.toString());
        sb.append(this._mainSymbolStack.toString());
        sb.append(this._tagSymbolStack.toString());
        sb.append(this._labelSymbolStack.toString());

        return sb.toString();
    }

    /**
     * const属性を取得する
     * @param typeName		type識別子
     * @return		true=const
     */
    public boolean isConst(String typeName) {

        boolean is_const = false;
        IXmlTypeTableChoice types[] = findTypes(typeName);

        // typeTableから検索する
        if (types != null && types.length > 0) {
            for (IXmlTypeTableChoice type : types) {
                if (type instanceof BasicType) {
                    is_const = XmlNodeUtil.isBoolean(((BasicType)type).getIsConst());
                    if (is_const) {
                        return true;
                    }
                }
            }
        }

        return false;
    }


    /**
     * restrict属性(C99:ポインタ変数の重複アクセスチェック)を取得する
     * @param typeName		type識別子
     * @return		true=restrict
     */
    public boolean IsRestrict(String typeName) {

        boolean is_restrict = false;
        IXmlTypeTableChoice types[] = findTypes(typeName);

        // typeTableから検索する
        if (types != null && types.length > 0) {
            for (IXmlTypeTableChoice type : types) {
                if (type instanceof BasicType) {
                    is_restrict = XmlNodeUtil.isBoolean(((BasicType)type).getIsRestrict());
                    if (is_restrict) {
                        return true;
                    }
                }
            }
        }

        return false;
    }

    /**
     * volatile属性(C89:最適化抑制)を取得する
     * @param typeName		type識別子
     * @return		true=volatile
     */
    public boolean isVolatile(String typeName) {

        boolean is_volatile = false;
        IXmlTypeTableChoice types[] = findTypes(typeName);

        // typeTableから検索する
        if (types != null && types.length > 0) {
            for (IXmlTypeTableChoice type : types) {
                if (type instanceof BasicType) {
                    is_volatile = XmlNodeUtil.isBoolean(((BasicType)type).getIsVolatile());
                    if (is_volatile) {
                        return true;
                    }
                }
            }
        }

        return false;
    }

    /**
     * static属性を取得する
     * @param typeName		type識別子
     * @return		true=static
     */
    public boolean isStatic(String typeName) {

        boolean is_static = false;
        IXmlTypeTableChoice types[] = findTypes(typeName);

        // typeTableから検索する
        if (types != null && types.length > 0) {
            for (IXmlTypeTableChoice type : types) {
                if (type instanceof ArrayType) {
                    is_static = XmlNodeUtil.isBoolean(((ArrayType)type).getIsStatic());
                }
                else if (type instanceof FunctionType) {
                    is_static = XmlNodeUtil.isBoolean(((FunctionType)type).getIsStatic());
                }
                if (is_static) {
                    return true;
                }
            }
        }
        // globalSymbols:idから検索する
        is_static = this.isSclassStatic(typeName);

        return false;
    }

    /**
     * inline属性を取得する
     * @param typeName		type識別子
     * @return		true=inline
     */
    public boolean isInline(String typeName) {

        boolean is_inline = false;
        IXmlTypeTableChoice types[] = findTypes(typeName);

        // typeTableから検索する
        if (types != null && types.length > 0) {
            for (IXmlTypeTableChoice type : types) {
                if (type instanceof FunctionType) {
                    is_inline = XmlNodeUtil.isBoolean(((FunctionType)type).getIsInline());
                }
                if (is_inline) {
                    return true;
                }
            }
        }

        return false;
    }

    /**
     * sclassに指定属性を持っているかチェックする
     * @param typeName		type識別子
     * @param has_sclass		チェック属性
     * @return		true=指定属性を持つ。
     */
    private boolean hasSclass(String typeName, String has_sclass) {

        boolean is_auto = false;
        IXmlTypeTableChoice types[] = findTypes(typeName);
        String id_type = typeName;

        // typeTableから検索する
        if (types != null && types.length > 0) {
            IXmlTypeTableChoice last_type = types[types.length-1];
            id_type = last_type.getType();
        }

        // globalSymbolsから検索する
        Id id = this.findSymbol(id_type);
        if (id != null) {
            String sclass = id.getSclass();
            if (sclass.equalsIgnoreCase(has_sclass)) {
                return true;
            }
        }

        return false;
    }

    /**
     * auto属性を取得する
     * @param typeName		type識別子
     * @return		true=auto
     */
    public boolean isSclassAuto(String typeName) {
        return this.hasSclass(typeName, "auto");
    }

    /**
     * param属性を取得する
     * @param typeName		type識別子
     * @return		true=param
     */
    public boolean isSclassParam(String typeName) {
        return this.hasSclass(typeName, "param");
    }

    /**
     * extern属性を取得する
     * @param typeName		type識別子
     * @return		true=extern
     */
    public boolean isSclassExtern(String typeName) {
        return this.hasSclass(typeName, "extern");
    }

    /**
     * extern_def'属性を取得する
     * @param typeName		type識別子
     * @return		true=extern_def'
     */
    public boolean isSclassExternDef(String typeName) {
        return this.hasSclass(typeName, "extern_def'");
    }

    /**
     * static属性を取得する
     * @param typeName		type識別子
     * @return		true=static
     */
    public boolean isSclassStatic(String typeName) {
        return this.hasSclass(typeName, "static'");
    }

    /**
     * register属性を取得する
     * @param typeName		type識別子
     * @return		true=register
     */
    public boolean isSclassRegister(String typeName) {
        return this.hasSclass(typeName, "register'");
    }

    /**
     * label属性を取得する
     * @param typeName		type識別子
     * @return		true=label
     */
    public boolean isSclassLabel(String typeName) {
        return this.hasSclass(typeName, "label'");
    }

    /**
     * tagname属性を取得する
     * @param typeName		type識別子
     * @return		true=tagname
     */
    public boolean isSclassTagname(String typeName) {
        return this.hasSclass(typeName, "tagname'");
    }

    /**
     * moe属性を取得する
     * @param typeName		type識別子
     * @return		true=moe
     */
    public boolean isSclassMoe(String typeName) {
        return this.hasSclass(typeName, "moe'");
    }

    /**
     * typedef_name属性を取得する
     * @param typeName		type識別子
     * @return		true=typedef_name
     */
    public boolean isSclassTypedefName(String typeName) {
        return this.hasSclass(typeName, "typedef_name'");
    }
}
