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

package jp.riken.kscope.data;

import java.io.File;
import java.util.ArrayList;

import javax.swing.filechooser.FileFilter;

import jp.riken.kscope.Message;
import jp.riken.kscope.properties.KscopeProperties;

/**
 * ソースファイルの形式列挙クラス。 ソースファイルの形式のタイプリストである。 また、拡張子フィルタ、拡張子によるソースファイル形式の判断、取得を行う。
 *
 * @author riken
 *
 */
public enum FILE_TYPE {
    /** 拡張子自動判定 */
    FILE_AUTO(Message.getString("file_type.enum.suffix-auto")), //拡張子自動判定
    /** Fortran:固定形式(72桁) */
    FORTRAN_FIXED_72(Message.getString("file_type.enum.fortran-fix-72")), //FORTRAN:固定形式(72桁)
    /** Fortran:固定形式(拡張桁数) */
    FORTRAN_FIXED_EXT(Message.getString("file_type.enum.fortran-fix-exp")), //FORTRAN:固定形式(拡張桁数)
    /** Fortran:自由形式 */
    FORTRAN_FREE(Message.getString("file_type.enum.fortran-free")), //FORTRAN:自由形式
    /** C言語 */
    CLANG(Message.getString("file_type.enum.c-lang")), //C言語
    /** XcodeML(XML) */
    XCODEML_XML(Message.getString("file_type.enum.xcodeml")), //XCODEML
    /** Fortran:全般 */
    FORTRANLANG(Message.getString("file_type.enum.fortran")), //Fortran
    /** 不明 */
    UNKNOWN(Message.getString("explore_panel.enum.unknown")); //不明

    /** Fortran:固定形式(72桁)拡張子 = null */
    public static String[] EXT_FORTRAN_FIXED72 = KscopeProperties.getExtFortranFixed72();
    /** Fortran:固定形式(拡張桁数) = {"f"} */
    public static String[] EXT_FORTRAN_FIXEDEXT = KscopeProperties.getExtFortranFixedExt();
    /** Fortran:自由形式拡張子 = {"f90", "f95"} */
    public static String[] EXT_FORTRAN_FREE = KscopeProperties.getExtFortranFree();
    /** C言語拡張子 = {"c", "h"} */
    public static String[] EXT_CLANG = KscopeProperties.getExtCLang();
    /** XcdoeML拡張子 = {"xml"} */
    public static String[] EXT_XCODEML = KscopeProperties.getExtXcodeml();

    /** ファイルタイプの表記文字列 */
    private final String m_text;

    /*
     * コンストラクタ
     *
     * @param text ファイルタイプの表記文字列
     */
    private FILE_TYPE(String text) {
        this.m_text = text;
    }

    /**
     * ファイルタイプの表記文字列を取得する。
     *
     * @return ファイルタイプの表記文字列
     */
    @Override
	public String toString() {
        return m_text;
    }

    /**
     * ファイルタイプの表記文字列リストを取得する。
     *
     * @return 表記文字列リスト
     */
    public static String[] getTextList() {
        FILE_TYPE type[] = FILE_TYPE.values();
        ArrayList<String> typeList = new ArrayList<String>();
        for (int i = 0; i < type.length; i++) {
            if (type[i] != UNKNOWN) {
                typeList.add(type[i].toString());
            }
        }
        return typeList.toArray(new String[0]);
    }

    /**
     * ソースファイルフィルタを作成する。 Windowsの場合、ファイルフィルタは拡張子の大文字・小文字を区別しない。
     * Linuxの場合、ファイルフィルタは拡張子の大文字・小文字を区別する。
     *
     * @return Fortranファイルフィルタ
     */
    public static FileFilter getSourceFilter() {
        FileFilter filter;
        filter = new FileFilter() {
            @Override
			public boolean accept(File file) {
                if (file.isDirectory())
                    return true;
                String file_name = file.getName();
                file_name = file_name.toLowerCase();
                for (int i = 0; EXT_FORTRAN_FIXED72 != null
                        && i < EXT_FORTRAN_FIXED72.length; i++) {
                    if (file_name.endsWith("." + EXT_FORTRAN_FIXED72[i]))
                        return true;
                }
                for (int i = 0; EXT_FORTRAN_FIXEDEXT != null
                        && i < EXT_FORTRAN_FIXEDEXT.length; i++) {
                    if (file_name.endsWith("." + EXT_FORTRAN_FIXEDEXT[i]))
                        return true;
                }
                for (int i = 0; EXT_FORTRAN_FREE != null
                        && i < EXT_FORTRAN_FREE.length; i++) {
                    if (file_name.endsWith("." + EXT_FORTRAN_FREE[i]))
                        return true;
                }
                for (int i = 0; EXT_CLANG != null && i < EXT_CLANG.length; i++) {
                    if (file_name.endsWith("." + EXT_CLANG[i]))
                        return true;
                }
                return false;
            }

            @Override
			public String getDescription() {
                String dest = Message.getString("file_type.enum.source.p"); //ソースファイル(
                for (int i = 0; EXT_FORTRAN_FIXED72 != null
                        && i < EXT_FORTRAN_FIXED72.length; i++) {
                    dest += "*." + EXT_FORTRAN_FIXED72[i] + ";";
                }
                for (int i = 0; EXT_FORTRAN_FIXEDEXT != null
                        && i < EXT_FORTRAN_FIXEDEXT.length; i++) {
                    dest += "*." + EXT_FORTRAN_FIXEDEXT[i] + ";";
                }
                for (int i = 0; EXT_FORTRAN_FREE != null
                        && i < EXT_FORTRAN_FREE.length; i++) {
                    dest += "*." + EXT_FORTRAN_FREE[i] + ";";
                }
                for (int i = 0; EXT_CLANG != null && i < EXT_CLANG.length; i++) {
                    dest += "*." + EXT_CLANG[i] + ";";
                }
                dest = dest.substring(0, dest.length() - 1);
                dest += ")";
                return dest;
            }
        };
        return filter;
    }

    /**
     * Fortranのファイルフィルタを作成する。 Windowsの場合、ファイルフィルタは拡張子の大文字・小文字を区別しない。
     * Linuxの場合、ファイルフィルタは拡張子の大文字・小文字を区別する。
     *
     * @return Fortranファイルフィルタ
     */
    public static FileFilter getFortranFilter() {
        FileFilter fort_filter;
        fort_filter = new FileFilter() {
            @Override
			public boolean accept(File file) {
                if (file.isDirectory())
                    return true;
                String file_name = file.getName();
                file_name = file_name.toLowerCase();
                for (int i = 0; EXT_FORTRAN_FIXED72 != null
                        && i < EXT_FORTRAN_FIXED72.length; i++) {
                    if (file_name.endsWith("." + EXT_FORTRAN_FIXED72[i]))
                        return true;
                }
                for (int i = 0; EXT_FORTRAN_FIXEDEXT != null
                        && i < EXT_FORTRAN_FIXEDEXT.length; i++) {
                    if (file_name.endsWith("." + EXT_FORTRAN_FIXEDEXT[i]))
                        return true;
                }
                for (int i = 0; EXT_FORTRAN_FREE != null
                        && i < EXT_FORTRAN_FREE.length; i++) {
                    if (file_name.endsWith("." + EXT_FORTRAN_FREE[i]))
                        return true;
                }
                return false;
            }

            @Override
			public String getDescription() {
                String dest = Message.getString("file_type.enum.fortran.p"); //"Fortranファイル("
                for (int i = 0; EXT_FORTRAN_FIXED72 != null
                        && i < EXT_FORTRAN_FIXED72.length; i++) {
                    dest += "*." + EXT_FORTRAN_FIXED72[i] + ";";
                }
                for (int i = 0; EXT_FORTRAN_FIXEDEXT != null
                        && i < EXT_FORTRAN_FIXEDEXT.length; i++) {
                    dest += "*." + EXT_FORTRAN_FIXEDEXT[i] + ";";
                }
                for (int i = 0; EXT_FORTRAN_FREE != null
                        && i < EXT_FORTRAN_FREE.length; i++) {
                    dest += "*." + EXT_FORTRAN_FREE[i] + ";";
                }
                dest = dest.substring(0, dest.length() - 1);
                dest += ")";
                return dest;
            }
        };
        return fort_filter;
    }

    /**
     * Fortran固定形式(72桁)のファイルフィルタを作成する。 Windowsの場合、ファイルフィルタは拡張子の大文字・小文字を区別しない。
     * Linuxの場合、ファイルフィルタは拡張子の大文字・小文字を区別する。
     *
     * @return Fortran固定形式ファイルフィルタ
     */
    public static FileFilter getFortranFixed72Filter() {
        FileFilter fort_filter;
        fort_filter = new FileFilter() {
            @Override
			public boolean accept(File file) {
                if (file.isDirectory())
                    return true;
                String file_name = file.getName();
                file_name = file_name.toLowerCase();
                for (int i = 0; EXT_FORTRAN_FIXED72 != null
                        && i < EXT_FORTRAN_FIXED72.length; i++) {
                    if (file_name.endsWith("."
                            + EXT_FORTRAN_FIXED72[i].toLowerCase()))
                        return true;
                }
                return false;
            }

            @Override
			public String getDescription() {
                String dest = Message.getString("file_type.enum.fortran-fix-72.p"); //"Fortran固定形式(72桁)ファイル(";
                for (int i = 0; EXT_FORTRAN_FIXED72 != null
                        && i < EXT_FORTRAN_FIXED72.length; i++) {
                    dest += "*." + EXT_FORTRAN_FIXED72[i] + ";";
                }
                dest = dest.substring(0, dest.length() - 1);
                dest += ")";
                return dest;
            }
        };
        return fort_filter;
    }

    /**
     * Fortran固定形式(桁数拡張)のファイルフィルタを作成する。 Windowsの場合、ファイルフィルタは拡張子の大文字・小文字を区別しない。
     * Linuxの場合、ファイルフィルタは拡張子の大文字・小文字を区別する。
     *
     * @return Fortran固定形式ファイルフィルタ
     */
    public static FileFilter getFortranFixedExtFilter() {
        FileFilter fort_filter;
        fort_filter = new FileFilter() {
            @Override
			public boolean accept(File file) {
                if (file.isDirectory())
                    return true;
                String file_name = file.getName();
                file_name = file_name.toLowerCase();
                for (int i = 0; EXT_FORTRAN_FIXEDEXT != null
                        && i < EXT_FORTRAN_FIXEDEXT.length; i++) {
                    if (file_name.endsWith("."
                            + EXT_FORTRAN_FIXEDEXT[i].toLowerCase()))
                        return true;
                }
                return false;
            }

            @Override
			public String getDescription() {
                String dest = Message.getString("file_type.enum.fortran-fix-exp.p"); //"Fortran固定形式(桁数拡張)ファイル(";
                for (int i = 0; EXT_FORTRAN_FIXEDEXT != null
                        && i < EXT_FORTRAN_FIXEDEXT.length; i++) {
                    dest += "*." + EXT_FORTRAN_FIXEDEXT[i] + ";";
                }
                dest = dest.substring(0, dest.length() - 1);
                dest += ")";
                return dest;
            }
        };
        return fort_filter;
    }

    /**
     * Fortran自由形式形式のファイルフィルタを作成する。 Windowsの場合、ファイルフィルタは拡張子の大文字・小文字を区別しない。
     * Linuxの場合、ファイルフィルタは拡張子の大文字・小文字を区別する。
     *
     * @return Fortran固定形式ファイルフィルタ
     */
    public static FileFilter getFortranFreeFilter() {
        FileFilter fort_filter;
        fort_filter = new FileFilter() {
            @Override
			public boolean accept(File file) {
                if (file.isDirectory())
                    return true;
                String file_name = file.getName();
                file_name = file_name.toLowerCase();
                for (int i = 0; EXT_FORTRAN_FREE != null
                        && i < EXT_FORTRAN_FREE.length; i++) {
                    if (file_name.endsWith("."
                            + EXT_FORTRAN_FREE[i].toLowerCase()))
                        return true;
                }
                return false;
            }

            @Override
			public String getDescription() {
                String dest = Message.getString("file_type.enum.fortran-free.p"); // "Fortran自由形式ファイル(";
                for (int i = 0; i < EXT_FORTRAN_FREE.length; i++) {
                    dest += "*." + EXT_FORTRAN_FREE[i];
                    if (i < EXT_FORTRAN_FREE.length - 1)
                        dest += ";";
                }
                dest += ")";
                return dest;
            }
        };
        return fort_filter;
    }

    /**
     * C言語のファイルフィルタを作成する。 Windowsの場合、ファイルフィルタは拡張子の大文字・小文字を区別しない。
     * Linuxの場合、ファイルフィルタは拡張子の大文字・小文字を区別する。
     *
     * @return C言語ファイルフィルタ
     */
    public static FileFilter getClangFilter() {
        FileFilter c_filter;
        c_filter = new FileFilter() {
            @Override
			public boolean accept(File file) {
                if (file.isDirectory())
                    return true;
                String file_name = file.getName();
                file_name = file_name.toLowerCase();
                for (int i = 0; EXT_CLANG != null && i < EXT_CLANG.length; i++) {
                    if (file_name.endsWith("." + EXT_CLANG[i].toLowerCase()))
                        return true;
                }
                return false;
            }

            @Override
			public String getDescription() {
                String dest = Message.getString("file_type.enum.c-lang.p"); //"C言語ファイル(";
                for (int i = 0; i < EXT_CLANG.length; i++) {
                    dest += "*." + EXT_CLANG[i];
                    if (i < EXT_CLANG.length - 1)
                        dest += ";";
                }
                dest += ")";
                return dest;
            }
        };
        return c_filter;
    }

    /**
     * XcodeMLのファイルフィルタを作成する。
     *
     * @return C言語ファイルフィルタ
     */
    public static FileFilter getXcodemlFilter() {
        FileFilter xml_filter;
        xml_filter = new FileFilter() {
            @Override
			public boolean accept(File file) {
                if (file.isDirectory())
                    return true;
                String file_name = file.getName();
                file_name = file_name.toLowerCase();
                for (int i = 0; EXT_XCODEML != null && i < EXT_XCODEML.length; i++) {
                    if (file_name.endsWith("." + EXT_XCODEML[i].toLowerCase()))
                        return true;
                }
                return false;
            }

            @Override
			public String getDescription() {
                String dest = Message.getString("file_type.enum.xcodeml.p"); //"XcodeML(";
                for (int i = 0; i < EXT_XCODEML.length; i++) {
                    dest += "*." + EXT_XCODEML[i];
                    if (i < EXT_XCODEML.length - 1)
                        dest += ";";
                }
                dest += ")";
                return dest;
            }
        };
        return xml_filter;
    }

    /**
     * Fortran固定形式(72桁)のファイルであるか判断する。
     *
     * @param file
     *            ソースファイル
     * @return true:Fortran固定形式/false:Fortran固定形式ではない。
     */
    public static boolean isFortranFixed72File(File file) {
        FileFilter filter = getFortranFixed72Filter();
        return filter.accept(file);
    }

    /**
     * Fortran固定形式(桁数拡張)のファイルであるか判断する。
     *
     * @param file
     *            ソースファイル
     * @return true:Fortran固定形式/false:Fortran固定形式ではない。
     */
    public static boolean isFortranFixedExtFile(File file) {
        FileFilter filter = getFortranFixedExtFilter();
        return filter.accept(file);
    }

    /**
     * Fortran自由形式のファイルであるか判断する。
     *
     * @param file
     *            ソースファイル
     * @return true:Fortran自由形式/false:Fortran自由形式ではない。
     */
    public static boolean isFortranFreeFile(File file) {
        FileFilter filter = getFortranFreeFilter();
        return filter.accept(file);
    }

    /**
     * Fortranのファイルであるか判断する。
     *
     * @param file
     *            ソースファイル
     * @return true:Fortran/false:Fortranではない。
     */
    public static boolean isFortranFile(File file) {
        FileFilter filter = getFortranFilter();
        return filter.accept(file);
    }

    /**
     * C言語のファイルであるか判断する。
     *
     * @param file
     *            ソースファイル
     * @return true:C言語/false:C言語ではない。
     */
    public static boolean isClangFile(File file) {
        FileFilter filter = getClangFilter();
        return filter.accept(file);
    }

    /**
     * XcdoeMLのファイルであるか判断する。
     *
     * @param file
     *            ソースファイル
     * @return true:XcdoeML/false:XcdoeMLではない。
     */
    public static boolean isXcodemlFile(File file) {
        FileFilter filter = getXcodemlFilter();
        return filter.accept(file);
    }

    /**
     * ソースファイルの拡張子からファイルタイプを自動判定する。
     * 拡張子から自動判定するのは、Fortran:固定形式(桁数拡張)->Fortran:自由形式->C言語の順番である。
     * Fortran:固定形式(72桁)は拡張子から自動判定は行わない。Fortran:固定形式(桁数拡張)とする。
     *
     * @param file
     *            ソースファイル
     * @return ファイルタイプ
     */
    public static FILE_TYPE getFileType(File file) {
        if (isFortranFixed72File(file))
            return FORTRAN_FIXED_72;
        else if (isFortranFixedExtFile(file))
            return FORTRAN_FIXED_EXT;
        else if (isFortranFreeFile(file))
            return FORTRAN_FREE;
        else if (isClangFile(file))
            return CLANG;
        else if (isXcodemlFile(file))
            return XCODEML_XML;
        else if (isFortranFile(file))
            return FORTRANLANG;

        return UNKNOWN;
    }

    /**
     * ファイルタイプを取得する。
     *
     * @param idx
     *            インデックス
     * @return ファイルタイプ
     */
    public static FILE_TYPE getFileType(int idx) {
        FILE_TYPE type[] = FILE_TYPE.values();
        return type[idx];
    }

    /**
     * Fortran:固定形式(72桁)の拡張子リストを設定する。
     *
     * @param exts
     *            拡張子リスト
     */
    public static void setExtensionsFixed72(String exts[]) {
        EXT_FORTRAN_FIXED72 = exts;
    }

    /**
     * Fortran:固定形式(桁数拡張)の拡張子リストを設定する。
     *
     * @param exts
     *            拡張子リスト
     */
    public static void setExtensionsFixedExt(String exts[]) {
        EXT_FORTRAN_FIXEDEXT = exts;
    }

    /**
     * Fortran:自由形式の拡張子リストを設定する。
     *
     * @param exts
     *            拡張子リスト
     */
    public static void setExtensionsFree(String exts[]) {
        EXT_FORTRAN_FREE = exts;
    }

    /**
     * C言語の拡張子リストを設定する。
     *
     * @param exts
     *            拡張子リスト
     */
    public static void setExtensionsClang(String exts[]) {
        EXT_CLANG = exts;
    }

    /**
     * XcodeMLの拡張子リストを設定する。
     *
     * @param exts
     *            拡張子リスト
     */
    public static void setExtensionsXcodeML(String exts[]) {
        EXT_XCODEML = exts;
    }

    /**
     * Fortran:固定形式(72桁)の拡張子リストを取得する。
     *
     * @return 拡張子リスト
     */
    public static String[] getExtensionsFixed72() {
        return EXT_FORTRAN_FIXED72;
    }

    /**
     * Fortran:固定形式(桁数拡張)の拡張子リストを取得する。
     *
     * @return 拡張子リスト
     */
    public static String[] getExtensionsFixedExt() {
        return EXT_FORTRAN_FIXEDEXT;
    }

    /**
     * Fortran:自由形式の拡張子リストを取得する。
     *
     * @return 拡張子リスト
     */
    public static String[] getExtensionsFree() {
        return EXT_FORTRAN_FREE;
    }

    /**
     * C言語の拡張子リストを取得する。
     *
     * @return 拡張子リスト
     */
    public static String[] getExtensionsClang() {
        return EXT_CLANG;
    }

    /**
     * XcodeMLの拡張子リストを取得する。
     *
     * @return 拡張子リスト
     */
    public static String[] getExtensionsXcodeml() {
        return EXT_XCODEML;
    }

    /**
     * 自身のファイルタイプのファイルフィルタを取得する。
     *
     * @return ファイルフィルタ
     */
    public FileFilter getFileFilter() {
        if (this.equals(FILE_AUTO)) {
            return getSourceFilter();
        } else if (this.equals(FORTRAN_FIXED_72)) {
            return getFortranFixed72Filter();
        } else if (this.equals(FORTRAN_FIXED_EXT)) {
            return getFortranFixedExtFilter();
        } else if (this.equals(FORTRAN_FREE)) {
            return getFortranFreeFilter();
        } else if (this.equals(CLANG)) {
            return getClangFilter();
        } else if (this.equals(XCODEML_XML)) {
            return getXcodemlFilter();
        } else if (this.equals(FORTRANLANG)) {
            return getFortranFilter();
        }
        return null;

    }
};
