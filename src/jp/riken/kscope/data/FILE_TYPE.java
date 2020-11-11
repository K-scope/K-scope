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
 * Source file format enumeration class. A type list of source file formats. In addition, the
 * extension filter and the source file format based on the extension are judged and acquired.
 *
 * @author RIKEN
 */
public enum FILE_TYPE {
  /** Automatic extension judgment */
  FILE_AUTO(Message.getString("file_type.enum.suffix-auto")), // Extension automatic judgment
  /** Fortran: Fixed format (72 digits) */
  FORTRAN_FIXED_72(
      Message.getString("file_type.enum.fortran-fix-72")), // FORTRAN: Fixed format (72 digits)
  /** Fortran: Fixed format (extended number of digits) */
  FORTRAN_FIXED_EXT(
      Message.getString(
          "file_type.enum.fortran-fix-exp")), // FORTRAN: Fixed format (extended number of digits)
  /** Fortran: Free format */
  FORTRAN_FREE(Message.getString("file_type.enum.fortran-free")), // FORTRAN: Free format
  /** C language */
  CLANG(Message.getString("file_type.enum.c-lang")), // C language
  /** XcodeML(XML) */
  XCODEML_XML(Message.getString("file_type.enum.xcodeml")), // XCODEML
  /** Fortran: General */
  FORTRANLANG(Message.getString("file_type.enum.fortran")), // Fortran
  /** Unknown */
  UNKNOWN(Message.getString("explore_panel.enum.unknown")); // unknown

  /** Fortran: Fixed format (72 digits) Extension = null */
  public static String[] EXT_FORTRAN_FIXED72 = KscopeProperties.getExtFortranFixed72();
  /** Fortran: Fixed format (extended number of digits) = {"f"} */
  public static String[] EXT_FORTRAN_FIXEDEXT = KscopeProperties.getExtFortranFixedExt();
  /** Fortran: Free-form extension = {"f90", "f95"} */
  public static String[] EXT_FORTRAN_FREE = KscopeProperties.getExtFortranFree();
  /** C language extension = {"c", "h"} */
  public static String[] EXT_CLANG = KscopeProperties.getExtCLang();
  /** XcdoeML extension = {"xml"} */
  public static String[] EXT_XCODEML = KscopeProperties.getExtXcodeml();

  /** File type notation string */
  private final String m_text;

  /*
   * Constructor
   *
   * @param text File type notation string
   */
  private FILE_TYPE(String text) {
    this.m_text = text;
  }

  /**
   * Get the notation string of the file type.
   *
   * @return File type notation string
   */
  @Override
  public String toString() {
    return m_text;
  }

  /**
   * Get the notation string list of the file type.
   *
   * @return Notation string list
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
   * Create a source file filter. On Windows, the file filter is case insensitive. For Linux, the
   * file filter is case sensitive.
   *
   * @return Fortran file filter
   */
  public static FileFilter getSourceFilter() {
    FileFilter filter;
    filter =
        new FileFilter() {
          @Override
          public boolean accept(File file) {
            if (file.isDirectory()) return true;
            String file_name = file.getName();
            file_name = file_name.toLowerCase();
            for (int i = 0; EXT_FORTRAN_FIXED72 != null && i < EXT_FORTRAN_FIXED72.length; i++) {
              if (file_name.endsWith("." + EXT_FORTRAN_FIXED72[i])) return true;
            }
            for (int i = 0; EXT_FORTRAN_FIXEDEXT != null && i < EXT_FORTRAN_FIXEDEXT.length; i++) {
              if (file_name.endsWith("." + EXT_FORTRAN_FIXEDEXT[i])) return true;
            }
            for (int i = 0; EXT_FORTRAN_FREE != null && i < EXT_FORTRAN_FREE.length; i++) {
              if (file_name.endsWith("." + EXT_FORTRAN_FREE[i])) return true;
            }
            for (int i = 0; EXT_CLANG != null && i < EXT_CLANG.length; i++) {
              if (file_name.endsWith("." + EXT_CLANG[i])) return true;
            }
            return false;
          }

          @Override
          public String getDescription() {
            String dest = Message.getString("file_type.enum.source.p"); // source file(
            for (int i = 0; EXT_FORTRAN_FIXED72 != null && i < EXT_FORTRAN_FIXED72.length; i++) {
              dest += "*." + EXT_FORTRAN_FIXED72[i] + ";";
            }
            for (int i = 0; EXT_FORTRAN_FIXEDEXT != null && i < EXT_FORTRAN_FIXEDEXT.length; i++) {
              dest += "*." + EXT_FORTRAN_FIXEDEXT[i] + ";";
            }
            for (int i = 0; EXT_FORTRAN_FREE != null && i < EXT_FORTRAN_FREE.length; i++) {
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
   * Create a Fortran file filter. On Windows, the file filter is case insensitive. For Linux, the
   * file filter is case sensitive.
   *
   * @return Fortran file filter
   */
  public static FileFilter getFortranFilter() {
    FileFilter fort_filter;
    fort_filter =
        new FileFilter() {
          @Override
          public boolean accept(File file) {
            if (file.isDirectory()) return true;
            String file_name = file.getName();
            file_name = file_name.toLowerCase();
            for (int i = 0; EXT_FORTRAN_FIXED72 != null && i < EXT_FORTRAN_FIXED72.length; i++) {
              if (file_name.endsWith("." + EXT_FORTRAN_FIXED72[i])) return true;
            }
            for (int i = 0; EXT_FORTRAN_FIXEDEXT != null && i < EXT_FORTRAN_FIXEDEXT.length; i++) {
              if (file_name.endsWith("." + EXT_FORTRAN_FIXEDEXT[i])) return true;
            }
            for (int i = 0; EXT_FORTRAN_FREE != null && i < EXT_FORTRAN_FREE.length; i++) {
              if (file_name.endsWith("." + EXT_FORTRAN_FREE[i])) return true;
            }
            return false;
          }

          @Override
          public String getDescription() {
            String dest = Message.getString("file_type.enum.fortran.p"); // "Fortran file ("
            for (int i = 0; EXT_FORTRAN_FIXED72 != null && i < EXT_FORTRAN_FIXED72.length; i++) {
              dest += "*." + EXT_FORTRAN_FIXED72[i] + ";";
            }
            for (int i = 0; EXT_FORTRAN_FIXEDEXT != null && i < EXT_FORTRAN_FIXEDEXT.length; i++) {
              dest += "*." + EXT_FORTRAN_FIXEDEXT[i] + ";";
            }
            for (int i = 0; EXT_FORTRAN_FREE != null && i < EXT_FORTRAN_FREE.length; i++) {
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
   * Create a Fortran fixed format (72 digit) file filter. On Windows, the file filter is case
   * insensitive. For Linux, the file filter is case sensitive.
   *
   * @return Fortran fixed format file filter
   */
  public static FileFilter getFortranFixed72Filter() {
    FileFilter fort_filter;
    fort_filter =
        new FileFilter() {
          @Override
          public boolean accept(File file) {
            if (file.isDirectory()) return true;
            String file_name = file.getName();
            file_name = file_name.toLowerCase();
            for (int i = 0; EXT_FORTRAN_FIXED72 != null && i < EXT_FORTRAN_FIXED72.length; i++) {
              if (file_name.endsWith("." + EXT_FORTRAN_FIXED72[i].toLowerCase())) return true;
            }
            return false;
          }

          @Override
          public String getDescription() {
            String dest =
                Message.getString(
                    "file_type.enum.fortran-fix-72.p"); // "Fortran fixed format (72 digits) file
            // (";
            for (int i = 0; EXT_FORTRAN_FIXED72 != null && i < EXT_FORTRAN_FIXED72.length; i++) {
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
   * Fortran Create a fixed format (extended number of digits) file filter. On Windows, the file
   * filter is case insensitive. For Linux, the file filter is case sensitive.
   *
   * @return Fortran fixed format file filter
   */
  public static FileFilter getFortranFixedExtFilter() {
    FileFilter fort_filter;
    fort_filter =
        new FileFilter() {
          @Override
          public boolean accept(File file) {
            if (file.isDirectory()) return true;
            String file_name = file.getName();
            file_name = file_name.toLowerCase();
            for (int i = 0; EXT_FORTRAN_FIXEDEXT != null && i < EXT_FORTRAN_FIXEDEXT.length; i++) {
              if (file_name.endsWith("." + EXT_FORTRAN_FIXEDEXT[i].toLowerCase())) return true;
            }
            return false;
          }

          @Override
          public String getDescription() {
            String dest =
                Message.getString(
                    "file_type.enum.fortran-fix-exp.p"); // "Fortran fixed format (extended number
            // of digits) file (";
            for (int i = 0; EXT_FORTRAN_FIXEDEXT != null && i < EXT_FORTRAN_FIXEDEXT.length; i++) {
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
   * Fortran Create a free-form file filter. On Windows, the file filter is case insensitive. For
   * Linux, the file filter is case sensitive.
   *
   * @return Fortran fixed format file filter
   */
  public static FileFilter getFortranFreeFilter() {
    FileFilter fort_filter;
    fort_filter =
        new FileFilter() {
          @Override
          public boolean accept(File file) {
            if (file.isDirectory()) return true;
            String file_name = file.getName();
            file_name = file_name.toLowerCase();
            for (int i = 0; EXT_FORTRAN_FREE != null && i < EXT_FORTRAN_FREE.length; i++) {
              if (file_name.endsWith("." + EXT_FORTRAN_FREE[i].toLowerCase())) return true;
            }
            return false;
          }

          @Override
          public String getDescription() {
            String dest =
                Message.getString("file_type.enum.fortran-free.p"); // "Fortran free format file (";
            for (int i = 0; i < EXT_FORTRAN_FREE.length; i++) {
              dest += "*." + EXT_FORTRAN_FREE[i];
              if (i < EXT_FORTRAN_FREE.length - 1) dest += ";";
            }
            dest += ")";
            return dest;
          }
        };
    return fort_filter;
  }

  /**
   * Create a C language file filter. On Windows, the file filter is case insensitive. For Linux,
   * the file filter is case sensitive.
   *
   * @return C language file filter
   */
  public static FileFilter getClangFilter() {
    FileFilter c_filter;
    c_filter =
        new FileFilter() {
          @Override
          public boolean accept(File file) {
            if (file.isDirectory()) return true;
            String file_name = file.getName();
            file_name = file_name.toLowerCase();
            for (int i = 0; EXT_CLANG != null && i < EXT_CLANG.length; i++) {
              if (file_name.endsWith("." + EXT_CLANG[i].toLowerCase())) return true;
            }
            return false;
          }

          @Override
          public String getDescription() {
            String dest = Message.getString("file_type.enum.c-lang.p"); // "C language file (";
            for (int i = 0; i < EXT_CLANG.length; i++) {
              dest += "*." + EXT_CLANG[i];
              if (i < EXT_CLANG.length - 1) dest += ";";
            }
            dest += ")";
            return dest;
          }
        };
    return c_filter;
  }

  /**
   * Create an XcodeML file filter.
   *
   * @return C language file filter
   */
  public static FileFilter getXcodemlFilter() {
    FileFilter xml_filter;
    xml_filter =
        new FileFilter() {
          @Override
          public boolean accept(File file) {
            if (file.isDirectory()) return true;
            String file_name = file.getName();
            file_name = file_name.toLowerCase();
            for (int i = 0; EXT_XCODEML != null && i < EXT_XCODEML.length; i++) {
              if (file_name.endsWith("." + EXT_XCODEML[i].toLowerCase())) return true;
            }
            return false;
          }

          @Override
          public String getDescription() {
            String dest = Message.getString("file_type.enum.xcodeml.p"); // "XcodeML(";
            for (int i = 0; i < EXT_XCODEML.length; i++) {
              dest += "*." + EXT_XCODEML[i];
              if (i < EXT_XCODEML.length - 1) dest += ";";
            }
            dest += ")";
            return dest;
          }
        };
    return xml_filter;
  }

  /**
   * Fortran Determines if the file is in fixed format (72 digits).
   *
   * @param file source file
   * @return true: Fortran fixed format / false: Not Fortran fixed format.
   */
  public static boolean isFortranFixed72File(File file) {
    FileFilter filter = getFortranFixed72Filter();
    return filter.accept(file);
  }

  /**
   * Fortran Determines if the file is in fixed format (extended number of digits).
   *
   * @param file source file
   * @return true: Fortran fixed format / false: Not Fortran fixed format.
   */
  public static boolean isFortranFixedExtFile(File file) {
    FileFilter filter = getFortranFixedExtFilter();
    return filter.accept(file);
  }

  /**
   * Fortran Determines if the file is in free format.
   *
   * @param file source file
   * @return true: Fortran free form / false: Fortran not free form.
   */
  public static boolean isFortranFreeFile(File file) {
    FileFilter filter = getFortranFreeFilter();
    return filter.accept(file);
  }

  /**
   * Determine if it is a Fortran file.
   *
   * @param file source file
   * @return true: Fortran / false: Not Fortran.
   */
  public static boolean isFortranFile(File file) {
    FileFilter filter = getFortranFilter();
    return filter.accept(file);
  }

  /**
   * Determine if the file is in C language.
   *
   * @param file source file
   * @return true: C language / false: Not C language.
   */
  public static boolean isClangFile(File file) {
    FileFilter filter = getClangFilter();
    return filter.accept(file);
  }

  /**
   * Determine if it is an XcdoeML file.
   *
   * @param file source file
   * @return true: XcdoeML / false: Not XcdoeML.
   */
  public static boolean isXcodemlFile(File file) {
    FileFilter filter = getXcodemlFilter();
    return filter.accept(file);
  }

  /**
   * Automatically determine the file type from the extension of the source file. The order of
   * automatic judgment from the extension is Fortran: fixed format (extended number of digits)->
   * Fortran: free format-> C language. Fortran: Fixed format (72 digits) is not automatically
   * judged from the extension. Fortran: Fixed format (extended number of digits).
   *
   * @param file source file
   * @return File type
   */
  public static FILE_TYPE getFileType(File file) {
    if (isFortranFixed72File(file)) return FORTRAN_FIXED_72;
    else if (isFortranFixedExtFile(file)) return FORTRAN_FIXED_EXT;
    else if (isFortranFreeFile(file)) return FORTRAN_FREE;
    else if (isClangFile(file)) return CLANG;
    else if (isXcodemlFile(file)) return XCODEML_XML;
    else if (isFortranFile(file)) return FORTRANLANG;

    return UNKNOWN;
  }

  /**
   * Get the file type.
   *
   * @param idx index
   * @return File type
   */
  public static FILE_TYPE getFileType(int idx) {
    FILE_TYPE type[] = FILE_TYPE.values();
    return type[idx];
  }

  /**
   * Fortran: Set a fixed format (72 digits) extension list.
   *
   * @param exts Extension list
   */
  public static void setExtensionsFixed72(String exts[]) {
    EXT_FORTRAN_FIXED72 = exts;
  }

  /**
   * Fortran: Set a fixed format (extended number of digits) extension list.
   *
   * @param exts Extension list
   */
  public static void setExtensionsFixedExt(String exts[]) {
    EXT_FORTRAN_FIXEDEXT = exts;
  }

  /**
   * Fortran: Set a free-form extension list.
   *
   * @param exts Extension list
   */
  public static void setExtensionsFree(String exts[]) {
    EXT_FORTRAN_FREE = exts;
  }

  /**
   * Set the C language extension list.
   *
   * @param exts Extension list
   */
  public static void setExtensionsClang(String exts[]) {
    EXT_CLANG = exts;
  }

  /**
   * Set the extension list of XcodeML.
   *
   * @param exts Extension list
   */
  public static void setExtensionsXcodeML(String exts[]) {
    EXT_XCODEML = exts;
  }

  /**
   * Fortran: Get a list of fixed format (72 digits) extensions.
   *
   * @return Extension list
   */
  public static String[] getExtensionsFixed72() {
    return EXT_FORTRAN_FIXED72;
  }

  /**
   * Fortran: Get a fixed format (extended number of digits) extension list.
   *
   * @return Extension list
   */
  public static String[] getExtensionsFixedExt() {
    return EXT_FORTRAN_FIXEDEXT;
  }

  /**
   * Fortran: Get a free-form extension list.
   *
   * @return Extension list
   */
  public static String[] getExtensionsFree() {
    return EXT_FORTRAN_FREE;
  }

  /**
   * Get the C language extension list.
   *
   * @return Extension list
   */
  public static String[] getExtensionsClang() {
    return EXT_CLANG;
  }

  /**
   * Get the XcodeML extension list.
   *
   * @return Extension list
   */
  public static String[] getExtensionsXcodeml() {
    return EXT_XCODEML;
  }

  /**
   * Get the file filter for your own file type.
   *
   * @return file filter
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
