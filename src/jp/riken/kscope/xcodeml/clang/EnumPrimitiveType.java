package jp.riken.kscope.xcodeml.clang;

/**
 * プリミティブデータ型列挙クラス
 * @author RIKEN
 */
public enum EnumPrimitiveType
{
    VOID                    ("void",                    "void",                     null,   false),
    CHAR                    ("char",                    "char",                     null,   false),
    SHORT                   ("short",                   "short",                    null,   false),
    INT                     ("int",                     "int",                      null,   false),
    LONG                    ("long",                    "long",                     "l",    false),
    LONGLONG                ("long_long",               "long long",                "ll",   false),
    UCHAR                   ("unsigned_char",           "unsigned char",            null,   true),
    USHORT                  ("unsigned_short",          "unsigned short",           null,   true),
    UINT                    ("unsigned",                "unsigned int",             "u",    true),
    ULONG                   ("unsigned_long",           "unsigned long",            "ul",   true),
    ULONGLONG               ("unsigned_long_long",      "unsigned long long",       "ull",  true),
    FLOAT                   ("float",                   "float",                    null,    false),
    DOUBLE                  ("double",                  "double",                   null,   false),
    LONGDOUBLE              ("long_double",             "long double",              null,    false),
    BOOL                    ("bool",                    "bool",                    null,   true), /* _Bool is treated as unsigned */
    WCHAR                   ("wchar_t",                 "wchar_t",                  null,   false),
    FLOAT_COMPLEX           ("float_complex",           "float _Complex",           null,   false),
    DOUBLE_COMPLEX          ("double_complex",          "double _Complex",          null,   false),
    LONGDOUBLE_COMPLEX      ("long_double_complex",     "long double _Complex",     null,   false),
    FLOAT_IMAG              ("float_imaginary",         "float _Complex",           null,   false),
    DOUBLE_IMAG             ("double_imaginary",        "double _Complex",          null,   false),
    LONGDOUBLE_IMAG         ("long_double_imaginary",   "long double _Complex",     null,   false),
    GCC_BUILTIN_VA_LIST     ("__builtin_va_list",       "__builtin_va_list",        null,   false),
    ;

    /**
     * Xcodeml id[@type]記述文字列
     */
    private final String _xcodekeyword;

    /**
     * コード出力データ型
     */
    private final String _ckeyword;

    /**
     * 定数末尾への接尾語
     */
    private final String _literalSuffix;

    /**
     * unsignedフラグ
     */
    private final boolean _isUnsigned;

    /**
     * コンストラクタ
     * @param xcode			Xcodeml id[@type]記述文字列
     * @param ckeyword		C言語コード出力データ型
     * @param literalSuffix		定数末尾への接尾語
     * @param isUnigned			unsignedフラグ
     */
    private EnumPrimitiveType(String xcode, String ckeyword, String literalSuffix, boolean isUnigned)
    {
        _xcodekeyword = xcode;
        _ckeyword = ckeyword;
        _literalSuffix = literalSuffix;
        _isUnsigned = isUnigned;
    }

    /**
     * Xcodeml id[@type]記述文字列を取得する.
     * @return		Xcodeml id[@type]記述文字列
     */
    public final String getXcodeKeyword()
    {
        return _xcodekeyword;
    }


    /**
     * C言語コード出力データ型を取得する.
     * @return		C言語コード出力データ型
     */
    public final String getCCode()
    {
        return _ckeyword;
    }

    /**
     * 定数末尾への接尾語を取得する.
     * @return		定数末尾への接尾語
     */
    public final String getLiteralSuffix()
    {
        return _literalSuffix;
    }

    /**
     * unsignedフラグを取得する.
     * @return		unsignedフラグ
     */
    public final boolean isUnsigned()
    {
        return _isUnsigned;
    }

    /**
     * プリミティブデータ型であるかチェックする。
     * @param type		データ型タイプ
     * @return		true=プリミティブデータ型
     */
    public static boolean isPrimitiveType(String type) {
        for (EnumPrimitiveType value: EnumPrimitiveType.values()) {
            if (value.getXcodeKeyword().equals(type)) {
                return true;
            }
        }

        return false;
    }

    /**
     * _xcodekeywordからプリミティブデータ型を取得する
     * @param type		Xcodeml id[@type]記述文字列
     * @return		プリミティブデータ型列挙体
     */
    public static EnumPrimitiveType findXcodeKeyword(String type) {
        for (EnumPrimitiveType value: EnumPrimitiveType.values()) {
            if (value.getXcodeKeyword().equals(type)) {
                return value;
            }
        }

        return null;
    }

    /**
     * _xcodekeywordからプリミティブデータ型のC言語データ型を取得する
     * @param type		Xcodeml id[@type]記述文字列
     * @return		C言語データ型
     */
    public static String getClangDataType(String type) {
        EnumPrimitiveType primitive = EnumPrimitiveType.findXcodeKeyword(type);
        if (primitive == null) return null;
        return primitive.getCCode();
    }

}

