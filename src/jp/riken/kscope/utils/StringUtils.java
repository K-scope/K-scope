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

package jp.riken.kscope.utils;

import java.awt.Color;
import java.awt.Font;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import jp.riken.kscope.properties.KscopeProperties;

/**
 * String manipulation utility class
 *
 * @author RIKEN
 */
public class StringUtils {

  /** Tab size when converting text tabs to HTML */
  private static int HTML_TABSIZE = 4;

  /**
   * Determine the character code of the character string. The judgment priority is EUC_JP>
   * SHIFT_JIS> UTF-8.
   *
   * @param byts Character code byte array
   * @return Judgment result Character code Character string "ASCII" ASCII "ISO-2022-JP" JIS
   *     "EUC_JP" EUC_JP "SHIFT_JIS" SHIFT-JIS "UTF-8" UTF-8 null Judgment is not possible
   */
  public static String getDetectedCharset(byte[] byts) {
    int len = byts.length;
    int sjis = 0;
    int sjis_ext = 0;
    int sjis_kana = 0;
    int euc = 0;
    int utf8 = 0;
    int jis = 0;
    int ascii = 0;
    boolean isJis = true;
    boolean isAscii = true;
    int b1, b2, b3, b4, b5, b6;
    int jp_char = 0;

    // ASCII, JIS judgment
    for (int i = 0; i < len; i++) {
      b1 = byts[i] & 0xFF;
      b2 = i < len - 1 ? byts[i + 1] & 0xFF : 0x00;
      b3 = i < len - 2 ? byts[i + 2] & 0xFF : 0x00;
      b4 = i < len - 3 ? byts[i + 3] & 0xFF : 0x00;
      b5 = i < len - 4 ? byts[i + 4] & 0xFF : 0x00;
      b6 = i < len - 5 ? byts[i + 5] & 0xFF : 0x00;

      if (b1 > 0x7F) {
        isJis = isAscii = false;
        jp_char = i;
        break;
      } else {
        if (b1 <= 0x06 || b1 == 0xFF) {
          // Control character
          return null;
        }
        if (isJis) {
          if (b1 == 0x1B) {
            if (b1 == 0x1B && b2 == 0x28 && b3 == 0x42) jis++;
            else if (b1 == 0x1B && b2 == 0x28 && b3 == 0x4A) jis++;
            else if (b1 == 0x1B && b2 == 0x28 && b3 == 0x49) jis++;
            else if (b1 == 0x1B && b2 == 0x24 && b3 == 0x40) jis++;
            else if (b1 == 0x1B && b2 == 0x24 && b3 == 0x42) jis++;
            else if (b1 == 0x1B && b2 == 0x24 && b3 == 0x28 && b4 == 0x44) jis++;
            else if (b1 == 0x1B
                && b2 == 0x26
                && b3 == 0x40
                && b4 == 0x1B
                && b5 == 0x24
                && b6 == 0x42) jis++;
          }
        }
        if (jis > 0) {
          return "ISO-2022-JP";
        }
        ascii++;
      }
    }
    if (isAscii) {
      return "ASCII";
    }

    // Shift-Jis judgment
    for (int i = jp_char; i < len; i++) {
      b1 = byts[i] & 0xFF;
      b2 = i < len - 1 ? byts[i + 1] & 0xFF : 0x00;
      b3 = i < len - 2 ? byts[i + 2] & 0xFF : 0x00;
      b4 = i < len - 3 ? byts[i + 3] & 0xFF : 0x00;

      if (b1 <= 0x7F) {
        continue;
      }

      // SHIFT-JIS If unused characters appear, it is not SHIFT-JIS.
      // Reference: http://charset.7jp.net/sjis.html
      if (b1 == 0x82) {
        if (b2 >= 0x40 && b2 <= 0x15) {
          i++;
          continue;
        }
        if (b2 >= 0x59 && b2 <= 0x5f) {
          i++;
          continue;
        }
        if (b2 >= 0x7a && b2 <= 0x80) {
          i++;
          continue;
        }
        if (b2 >= 0x9b && b2 <= 0x9e) {
          i++;
          continue;
        }
        if (b2 >= 0xf2 && b2 <= 0xfc) {
          i++;
          continue;
        }
      } else if (b1 == 0x83) {
        if (b2 >= 0x97 && b2 <= 0x9e) {
          i++;
          continue;
        }
        if (b2 >= 0xb7 && b2 <= 0xbe) {
          i++;
          continue;
        }
        if (b2 >= 0xd7 && b2 <= 0xfc) {
          i++;
          continue;
        }
      } else if (b1 == 0x84) {
        if (b2 >= 0x61 && b2 <= 0x6f) {
          i++;
          continue;
        }
        if (b2 >= 0x92 && b2 <= 0x9e) {
          i++;
          continue;
        }
        if (b2 >= 0xbf && b2 <= 0xfc) {
          i++;
          continue;
        }
      } else if (b1 == 0x85 || b1 == 0x86 || b1 == 0x87) {
        i++;
        continue;
      } else if (b1 == 0x88) {
        if (b2 >= 0x40 && b2 <= 0x9e) {
          i++;
          continue;
        }
      }

      if (b1 >= 0xA1 && b1 <= 0xDF) {
        sjis_kana++;
      } else {
        if ((b1 >= 0x81 && b1 <= 0x9F)
            && ((b2 >= 0x40 && b2 <= 0x7E) || (b2 >= 0x80 && b2 <= 0xFC))) {
          sjis++;
          i += 1;
        } else if ((b1 >= 0xE0 && b1 <= 0xEA)
            && ((b2 >= 0x40 && b2 <= 0x7E) || (b2 >= 0x80 && b2 <= 0xFC))) {
          // Do not count because it is a character that is not used very often. (Because there is a
          // possibility of garbled characters)
          // sjis++;
          i += 1;
        } else if ((b1 >= 0xED && b1 <= 0xEE)
            && ((b2 >= 0x40 && b2 <= 0x7E) || (b2 >= 0x80 && b2 <= 0xFC))) {
          // Do not count because it is a character that is not used very often. (Because there is a
          // possibility of garbled characters)
          // sjis++;
          i += 1;
        } else if ((b1 >= 0xE0 && b1 <= 0xFC)
            && ((b2 >= 0x40 && b2 <= 0x7E) || (b2 >= 0x80 && b2 <= 0xFC))) {
          // Not counted due to undefined characters, machine-dependent, and expansion.
          sjis_ext++;
          i += 1;
        } else {
          sjis = 0;
          break;
        }
      }
    }

    // EUC judgment
    for (int i = jp_char; i < len; i++) {
      b1 = byts[i] & 0xFF;
      b2 = i < len - 1 ? byts[i + 1] & 0xFF : 0x00;
      b3 = i < len - 2 ? byts[i + 2] & 0xFF : 0x00;
      b4 = i < len - 3 ? byts[i + 3] & 0xFF : 0x00;

      if (b1 <= 0x7F) {
        continue;
      }

      if ((b1 >= 0xA1 && b1 <= 0xFE) && (b2 >= 0xA1 && b2 <= 0xFE)) {
        euc += 1;
        i += 1;
      } else if ((b1 == 0x8E) && (b2 >= 0xA1 && b2 <= 0xDF)) {
        euc += 1;
        i += 1;
      } else if ((b1 == 0x8F) && (b2 >= 0xA1 && b2 <= 0xFE) && (b3 >= 0xA1 && b3 <= 0xFE)) {
        euc += 1;
        i += 2;
      } else {
        euc = 0;
        break;
      }
    }

    // UTF-8 judgment
    for (int i = jp_char; i < len; i++) {
      b1 = byts[i] & 0xFF;
      b2 = i < len - 1 ? byts[i + 1] & 0xFF : 0x00;
      b3 = i < len - 2 ? byts[i + 2] & 0xFF : 0x00;
      b4 = i < len - 3 ? byts[i + 3] & 0xFF : 0x00;

      if (b1 <= 0x7F) {
        continue;
      }

      if ((b1 >= 0xC2 && b1 <= 0xDF) && (b2 >= 0x80 && b2 <= 0xBF)) {
        utf8 += 1;
        i += 1;
      }
      // 3-byte character
      else if ((b1 == 0xE0) && (b2 >= 0xB8 && b2 <= 0xBB) && (b3 >= 0x80 && b3 <= 0xBF)) {
        utf8 += 1;
        i += 2;
      } else if ((b1 >= 0xE1 && b1 <= 0xE2)
          && (b2 >= 0x80 && b2 <= 0xBF)
          && (b3 >= 0x80 && b3 <= 0xBF)) {
        utf8 += 1;
        i += 2;
      } else if ((b1 == 0xE3) && (b2 >= 0x80 && b2 <= 0x8F) && (b3 >= 0x80 && b3 <= 0xBF)) {
        utf8 += 1;
        i += 2;
      } else if ((b1 == 0xE4) && (b2 >= 0xB8 && b2 <= 0xBF) && (b3 >= 0x80 && b3 <= 0xBF)) {
        utf8 += 1;
        i += 2;
      } else if ((b1 >= 0xE5 && b1 <= 0xE9)
          && (b2 >= 0x80 && b2 <= 0xBF)
          && (b3 >= 0x80 && b3 <= 0xBF)) {
        utf8 += 1;
        i += 2;
      } else if ((b1 == 0xED) && (b2 >= 0x80 && b2 <= 0x9F) && (b3 >= 0x80 && b3 <= 0xBF)) {
        // Do not count because 3-byte characters are undefined
        i += 2;
      } else if ((b1 == 0xEF) && (b2 >= 0xA7 && b2 <= 0xBF) && (b3 >= 0x80 && b3 <= 0xBF)) {
        if ((b1 == 0xEF) && (b2 == 0xBF) && (b3 == 0xBD)) {
          utf8 = 0;
          break;
        }
        utf8 += 1;
        i += 2;
      } else if ((b1 >= 0xE0 && b1 <= 0xEF)
          && (b2 >= 0x80 && b2 <= 0xBF)
          && (b3 >= 0x80 && b3 <= 0xBF)) {
        // Do not count because 3-byte characters are undefined
        i += 2;
      } else if ((b1 == 0xF0)
          && (b2 >= 0x90 && b2 <= 0xBF)
          && (b3 >= 0x80 && b3 <= 0xBF)
          && (b4 >= 0x80 && b4 <= 0xBF)) {
        // Do not count 4-byte characters because they are undefined
        i += 3;
      } else if ((b1 >= 0xF1 && b1 <= 0xF3)
          && (b2 >= 0x80 && b2 <= 0xBF)
          && (b3 >= 0x80 && b3 <= 0xBF)
          && (b4 >= 0x80 && b4 <= 0xBF)) {
        // Do not count 4-byte characters because they are undefined
        i += 3;
      } else if ((b1 == 0xF4)
          && (b2 >= 0x80 && b2 <= 0x8F)
          && (b3 >= 0x80 && b3 <= 0xBF)
          && (b4 >= 0x80 && b4 <= 0xBF)) {
        // Do not count 4-byte characters because they are undefined
        i += 3;
      } else if (b1 == 0xEF && b2 == 0xBB && b3 == 0xBF) {
        utf8 += 1;
        i += 2;
        // UTF-8 BOM
        return "UTF-8";
      } else {
        utf8 = 0;
        break;
      }
    }

    // Give priority to UTF-8 (UTF-8> SHIFT_JIS)
    if (euc >= sjis && euc >= utf8) return "EUC_JP";
    else if (utf8 >= euc && utf8 >= sjis) return "UTF-8";
    else if (sjis >= euc && sjis >= utf8) return "SHIFT_JIS";

    return null;
  }

  /**
   * Check if the string is an integer value
   *
   * @param str Integer value string
   * @return true: integer value
   */
  public static boolean isNumeric(String str) {
    if (str == null) return false;
    try {
      // Integer num = Integer.valueOf(str) ;
      Integer.valueOf(str);
    } catch (NumberFormatException e) {
      return false;
    }
    return true;
  }

  /**
   * Check if the string is a floating point number
   *
   * @param str Floating point numeric string
   * @return true: floating point number
   */
  public static boolean isFloat(String str) {
    if (str == null) return false;
    try {
      Float.valueOf(str);
    } catch (NumberFormatException e) {
      return false;
    }
    return true;
  }

  /**
   * Remove single and double quotes before and after the character.
   *
   * @param line Trim target string
   * @return String after trimming
   */
  public static String trimQuote(String line) {
    if (line == null || line.isEmpty()) return line;

    final String quote[] = {"'", "\""};
    String trim_str = line.trim();
    for (int i = 0; i < quote.length; i++) {
      if (trim_str.startsWith(quote[i]) && trim_str.endsWith(quote[i])) {
        return line.substring(1, line.length() - 1);
      }
    }

    return line;
  }

  /**
   * Check if the string is null or empty.
   *
   * @param s Check string
   * @return true = null or empty string
   */
  public static boolean isNullOrEmpty(String s) {
    return ((s == null) || (s.isEmpty() != false));
  }

  /**
   * Search for strings. <br>
   * (author rist_kobayashi)
   *
   * @param text All text to be searched
   * @param target Search string TreeMap with the @return key as the line number and the value as
   *     the list of positions from the beginning of the target line (TreeMap <line number,
   *     ArrayList <position from the beginning >>). However, the position at the beginning of the
   *     line is set to 0. <br>
   *     Returns an empty TreeMap if text is null or empty string, or target is null or empty
   *     string.
   */
  public static TreeMap<Integer, ArrayList<Integer>> searchString(String text, String target) {
    // Pre-conditions
    if (text == null || target == null || text.equals("") || target.equals("")) {
      return new TreeMap<Integer, ArrayList<Integer>>();
    }
    String[] lines = splitByNewlineCode(text);
    return searchString(lines, target);
  }

  /**
   * Search for strings <br>
   * (author rist_kobayashi)
   *
   * @param lines All lines to be searched
   * @param target Search string TreeMap with the @return key as the line number and the value as
   *     the list of positions from the beginning of the target line (TreeMap <line number,
   *     ArrayList <position from the beginning >>). However, the position at the beginning of the
   *     line is set to 0. <br>
   *     If lines is null, or target is null or an empty string, an empty TreeMap is returned.
   */
  public static TreeMap<Integer, ArrayList<Integer>> searchString(String[] lines, String target) {
    TreeMap<Integer, ArrayList<Integer>> result = new TreeMap<Integer, ArrayList<Integer>>();
    // Pre-conditions
    if (lines == null || target == null || target.equals("")) {
      return result;
    }

    // Search for search characters
    int lineNum = 0;
    for (String line : lines) {
      ArrayList<Integer> positions = new ArrayList<Integer>();
      lineNum += 1;
      int pos = 0;
      while (pos < line.length()) {
        pos = line.indexOf(target, pos);
        if (pos == -1) {
          break;
        }
        positions.add(pos);
        pos += 1;
      }
      if (positions.size() > 0) {
        result.put(lineNum, positions);
      }
    }

    return result;
  }

  /**
   * Split the string with a line feed code <br>
   * (author rist_kobayashi)
   *
   * @param text Text to be split
   * @return A string list separated by a line feed code. If text is null, it returns null.
   */
  public static String[] splitByNewlineCode(String text) {
    String newlineCode = "";
    String str = text;

    // Pre-conditions
    if (text == null) {
      return null;
    }

    // Determine line feed code
    if (!str.contains("\r")) {
      // When the line feed code is \ n
      newlineCode = "\n";
    } else if (!str.contains("\n")) {
      // When the line feed code is \ r
      newlineCode = "\r";
    } else {
      // When the line feed code is \ r \ n or \ n \ r
      // Convert the line feed code to \ n
      str = str.replace("\r", "\n");
      str = str.replace("\n\n", "\n");
      newlineCode = "\n";
    }

    return str.split(newlineCode);
  }

  /**
   * Convert HEX 2 digits to int.
   *
   * @param hex HEX 2-digit string
   * @return Converted number
   */
  public static int hex2Toint(String hex) {
    int value = 0;
    char hexDigit[] = hex.toCharArray();
    value = (Character.digit(hexDigit[0], 16)) * 16 + (Character.digit(hexDigit[1], 16));
    return value;
  }

  /**
   * Convert from int to 2 hexadecimal digits
   *
   * @param value int number
   * @return 2 hexadecimal digits
   */
  public static String intTohex2(int value) {
    char hex2[] = {
      Character.forDigit((value >> 4) & 0x0F, 16), Character.forDigit(value & 0x0F, 16)
    };
    String hex2Str = new String(hex2);
    return hex2Str.toUpperCase();
  }

  /**
   * Convert from 4 hexadecimal digits to int
   *
   * @param hex 4 hexadecimal digits
   * @return int number
   */
  public static int hex4Toint(String hex) {
    int value = 0;
    char HexDigit[] = hex.toCharArray();
    value =
        (Character.digit(HexDigit[0], 16)) * 16 * 16 * 16
            + (Character.digit(HexDigit[1], 16)) * 16 * 16
            + (Character.digit(HexDigit[2], 16)) * 16
            + (Character.digit(HexDigit[3], 16));
    return value;
  }

  /**
   * Convert from int to 4 hexadecimal digits
   *
   * @param value int number
   * @return 4 hexadecimal digits
   */
  public static String intTohex4(int value) {
    char hex4[] = {
      Character.forDigit((value >> 12) & 0x0F, 16),
      Character.forDigit((value >> 8) & 0x0F, 16),
      Character.forDigit((value >> 4) & 0x0F, 16),
      Character.forDigit(value & 0x0F, 16)
    };
    String hex4Str = new String(hex4);
    return hex4Str.toUpperCase();
  }

  /**
   * Delete half-width and full-width spaces before and after the character string.
   *
   * @param src Trim target string
   * @return Delete half-width and full-width spaces before and after the character string
   */
  public static String trim(String src) {
    if (src == null) return src;

    String line = src.trim();
    int len = line.length();
    int st = 0;
    char[] val = line.toCharArray();

    while (st < len && (val[st] <= ' ' || val[st] == '　')) {
      st++;
    }
    while (st < len && (val[len - 1] <= ' ' || val[len - 1] == '　')) {
      len--;
    }

    if (st > 0 || len < line.length()) {
      return line.substring(st, len);
    }

    return line;
  }

  /**
   * Create a java.awt.Color object from a color string. <br>
   * The color string is in the following format. <br>
   * 1. Field name of java.awt.Color definition such as RED, WHITE, BLUE <br>
   * 2. #RRGGBB: Starts with'#', 6 hexadecimal digits = html color specification 2. #RRGGBBAA:
   * Starts with'#', 8 hexadecimal digits = Transparent color
   *
   * @param color Color string
   * @return java .awt.Color object
   */
  public static Color parseColor(String color) {
    if (color == null || color.isEmpty()) return null;

    Color value = null;
    try {
      if (color.startsWith("#") && color.length() == 7) {
        String red = color.substring(1, 3);
        String green = color.substring(3, 5);
        String blue = color.substring(5);
        value =
            new Color(
                StringUtils.hex2Toint(red),
                StringUtils.hex2Toint(green),
                StringUtils.hex2Toint(blue));
        return value;
      } else if (color.startsWith("#") && color.length() == 9) {
        String red = color.substring(1, 3);
        String green = color.substring(3, 5);
        String blue = color.substring(5, 7);
        String alpha = color.substring(7);
        value =
            new Color(
                StringUtils.hex2Toint(red),
                StringUtils.hex2Toint(green),
                StringUtils.hex2Toint(blue),
                StringUtils.hex2Toint(alpha));
        return value;
      }
    } catch (Exception ex) {
      value = null;
    }

    try {
      Field field = Class.forName("java.awt.Color").getField(color);
      value = (Color) field.get(null);
    } catch (Exception ex) {
      value = null;
    }

    return value;
  }

  /**
   * Create HTML color code from java.awt.Color object. <br>
   * The color string is in the following format. <br>
   * 1. #RRGGBB: Starts with'#', 6 hexadecimal digits = html color specification
   *
   * @param color java.awt.Color object
   * @return HTML color code
   */
  public static String parseColorCode(Color color) {
    if (color == null) return null;

    int red = color.getRed();
    int green = color.getGreen();
    int blue = color.getBlue();
    int alpha = color.getAlpha();

    StringBuffer buf = new StringBuffer();
    buf.append("#");
    buf.append(StringUtils.intTohex2(red));
    buf.append(StringUtils.intTohex2(green));
    buf.append(StringUtils.intTohex2(blue));
    if (alpha != 255) {
      buf.append(StringUtils.intTohex2(alpha));
    }

    return buf.toString();
  }

  /**
   * Replace newline characters, whitespace, and tabs with HTML code
   *
   * @param content Original text string
   * @return HTML conversion string
   */
  public static String textTohtml(String content) {
    if (content == null || content.isEmpty()) return null;

    StringBuffer buf = new StringBuffer();
    boolean opening = false;
    for (int i = 0; i < content.length(); i++) {
      String str = content.substring(i, i + 1);
      // line feed
      if ("\r".equals(str)) {
        continue;
      }

      // Check tab characters
      if (!opening) {
        if ("<".equals(str)) {
          if (content.length() > i + 3) {
            String atag = content.substring(i, i + 3);
            if ("<a ".equals(atag)) {
              opening = true;
            }
          }
        }
      } else {
        if ("<".equals(str)) {
          if (content.length() > i + 4) {
            String atag = content.substring(i, i + 4);
            if ("</a>".equals(atag)) {
              opening = false;
              buf.append("</a>");
              i = i + 3;
              continue;
            }
          }
        }
      }

      if (opening) {
        buf.append(str);
        continue;
      }

      // Whitespace character
      if (" ".equals(str)) {
        buf.append("&nbsp;");
      }
      // Quotation
      else if ("\"".equals(str)) {
        buf.append("&quot;");
      }
      // Ampersand
      else if ("&".equals(str)) {
        buf.append("&amp;");
      }
      // less
      else if ("<".equals(str)) {
        buf.append("&lt;");
      }
      // Greater
      else if (">".equals(str)) {
        buf.append("&gt;");
      }
      // Copywriter
      else if ("@".equals(str)) {
        buf.append("&copy;");
      }
      // Newline character
      else if ("\n".equals(str)) {
        buf.append("<br/>\n");
      }
      // Tab character
      else if ("\t".equals(str)) {
        for (int j = 0; j < HTML_TABSIZE; j++) {
          buf.append("&nbsp;");
        }
      } else {
        buf.append(str);
      }
    }

    return buf.toString();
  }

  /**
   * Set the HTML color of the search target character from the search string. <br>
   * Enclose the search string in font and color HTML tags
   *
   * @param text Search target string
   * @param search Search string
   * @param forecolor Letter color
   * @param backcolor Background color
   * @param fontstyle Font style
   * @param sensitivecase Case sensitive (true = case sensitive)
   * @param regex Regular expressions
   * @param word Word search
   * @return HTML color setting string
   */
  public static String searchTextToHtml(
      String text,
      String search,
      Color forecolor,
      Color backcolor,
      int fontstyle,
      boolean sensitivecase,
      boolean regex,
      boolean word) {

    String contentText = textTohtml(text);
    String searchText = textTohtml(search);

    // Get the search target character position list from the search string
    List<Integer[]> list = getSearchTextList(contentText, searchText, sensitivecase, regex, word);
    if (list == null || list.size() <= 0) return text;

    // Replace search match character
    for (int i = list.size() - 1; i >= 0; i--) {
      int start = list.get(i)[0];
      int end = list.get(i)[1];
      // Search match character
      String findText = contentText.substring(start, end);
      // colouring
      String html = createHtmlColorTag(findText, forecolor, backcolor, fontstyle);
      contentText = contentText.substring(0, start) + html + contentText.substring(end);
    }
    contentText = "<html>" + contentText + "</html>";
    return contentText;
  }

  /**
   * Get the position information of the search target character from the search string. <br>
   *
   * @param text Search target string
   * @param search Search string
   * @param sensitivecase Case sensitive (true = case sensitive)
   * @param regex Regular expressions
   * @param word Word search
   * @return HTML color setting string
   */
  private static List<Integer[]> getSearchTextList(
      String text, String search, boolean sensitivecase, boolean regex, boolean word) {
    if (text == null) return null;
    if (search == null) return null;

    String contentText = text;
    String searchText = search;
    List<Integer[]> list = new ArrayList<Integer[]>();

    // Regular expressions
    if (regex) {
      int flags = 0;
      if (!sensitivecase) {
        // Do not distinguish between uppercase and lowercase letters
        flags = Pattern.CASE_INSENSITIVE;
      }
      flags += Pattern.MULTILINE;
      Pattern pattern = Pattern.compile(searchText, flags);
      Matcher m = pattern.matcher(contentText);
      while (m.find()) {
        // Create a match position list
        Integer[] findinfo = {m.start(), m.end()};
        list.add(findinfo);
      }
    } else {
      if (!sensitivecase) {
        // Convert to all lowercase as it is not case sensitive
        contentText = contentText.toLowerCase();
        searchText = searchText.toLowerCase();
      }

      // Perform a string search
      int fromIndex = 0;
      int start = -1;
      while ((start = contentText.indexOf(searchText, fromIndex)) != -1) {
        int end = start + searchText.length();
        fromIndex = end;
        // word search
        if (word) {
          // Check if the characters before and after the search result are delimiters
          if (start > 0) {
            if (!isDelimiter(contentText.substring(start - 1, start))) {
              continue;
            }
          }
          if (end < contentText.length()) {
            String endChar = contentText.substring(end, end + 1);
            if (endChar != null && !endChar.isEmpty()) {
              if (!isDelimiter(endChar)) {
                continue;
              }
            }
          }
        }
        // Create a match position list
        Integer[] findinfo = {start, end};
        list.add(findinfo);
      }
    }
    if (list.size() <= 0) return null;

    return list;
  }

  /**
   * Get whether the search target character exists from the search string. <br>
   *
   * @param text Search target string
   * @param search Search string
   * @param sensitivecase Case sensitive (true = case sensitive)
   * @param regex Regular expressions
   * @param word Word search
   * @return HTML color setting string
   */
  public static boolean existsSearchText(
      String text, String search, boolean sensitivecase, boolean regex, boolean word) {

    if (text == null) return false;
    if (search == null) return false;

    // Set dummy text color and background color
    String html =
        searchTextToHtml(
            text, search, Color.BLACK, Color.WHITE, Font.BOLD, sensitivecase, regex, word);

    return !(text.equals(html));
  }

  /**
   * Set the HTML color of the search target character from the search character string (variable
   * search) Check if it is the target character string. <br>
   * Not case sensitive, word search, quoted range not covered. <br>
   * *
   *
   * @param text Search target string
   * @param search Search string
   * @return true = HTML color setting target string
   */
  public static boolean existsSearchWord(String text, String search) {
    if (text == null) return false;
    if (search == null) return false;

    // Set dummy text color and background color
    String html = searchWordToHtml(text, search, Color.BLACK, Color.WHITE, Font.BOLD);

    return !(text.equals(html));
  }

  /**
   * Set the HTML color of the search target character from the search string (variable search).
   * <br>
   * Not case sensitive, word search, quoted range not covered. <br>
   * Enclose the search string in font and color HTML tags
   *
   * @param text Search target string
   * @param search Search string
   * @param forecolor Letter color
   * @param backcolor Background color
   * @param fontstyle Font style
   * @return HTML color setting string
   */
  public static String searchWordToHtml(
      String text, String search, Color forecolor, Color backcolor, int fontstyle) {
    if (text == null) return text;
    if (search == null) return text;

    try {
      String contentText = textTohtml(text);
      String searchText = textTohtml(search);

      StringBuffer buf = new StringBuffer();
      boolean htmltag = false;

      // Decompose the search target string into words and quotation marks
      LanguageTokenizer token = new LanguageTokenizer(contentText);
      // Delimiter settings
      String filldelimiters = KscopeProperties.DELIMITER_CHARS;
      String delimiters = "";
      for (int i = 0; i < filldelimiters.length(); i++) {
        char delim = filldelimiters.charAt(i);
        if (search.indexOf(delim) < 0) {
          delimiters += delim;
        }
      }
      token.useDelimiters(delimiters);
      token.eolIsSignificant(true);
      int ttype;
      boolean comment = false;
      while ((ttype = token.nextToken()) != LanguageTokenizer.LT_EOF) {
        switch (ttype) {
          case LanguageTokenizer.LT_EOL:
            break;
          case LanguageTokenizer.LT_EOF:
            break;
          case LanguageTokenizer.LT_WORD:
            if (comment) {
              // Comment
              buf.append(token.sval);
            } else if (searchText.equalsIgnoreCase(token.sval)) {
              String html = createHtmlColorTag(token.sval, forecolor, backcolor, fontstyle);
              buf.append(html);
              htmltag = true;
            } else {
              buf.append(token.sval);
            }
            break;
          case LanguageTokenizer.LT_QUOTE:
            buf.append(token.sval);
            break;
          case LanguageTokenizer.LT_DELIM:
            if (token.sval == "!") {
              // Make a comment below
              comment = true;
            }
            buf.append(token.sval);
            break;
          default:
            buf.append(token.sval);
            break;
        }
      }
      if (!htmltag) {
        // The search string does not exist.
        return text;
      }
      String html = buf.toString();
      html = "<html>" + html + "</html>";
      return html;

    } catch (Exception e) {
      e.printStackTrace();

      return text;
    }
  }

  /**
   * Apply HTML tags for font color, background color and style to the string.
   *
   * @param text Applicable string
   * @param forecolor Letter color
   * @param backcolor Background color
   * @param fontstyle Font style
   * @return HTML tag applicable string
   */
  private static String createHtmlColorTag(
      String text, Color forecolor, Color backcolor, int fontstyle) {

    // Style
    String style = "";

    // Letter color
    String attrColor = null;
    if (forecolor != null) {
      attrColor = parseColorCode(forecolor);
      attrColor = "color:" + attrColor + ";";
      style += attrColor;
    }
    // Background color
    String attrBackcolor = null;
    if (backcolor != null) {
      attrBackcolor = parseColorCode(backcolor);
      attrBackcolor = "background-color:" + attrBackcolor + ";";
      style += attrBackcolor;
    }
    // Bold
    String bold = null;
    if ((fontstyle & Font.BOLD) != 0) {
      bold = "font-weight: bold;";
      style += bold;
    }
    // italics
    String italic = null;
    if ((fontstyle & Font.ITALIC) != 0) {
      italic = "font-style: italic;";
      style += italic;
    }
    if (style == null || style.isEmpty()) return text;

    style = "style='" + style + "'";
    String html = "<span " + style + ">" + text + "</span>";

    return html;
  }

  /**
   * Decompose the string with a delimiter. <br>
   * The returned decomposed string list includes delimiters and spaces.
   *
   * @param text Search target string
   * @return Decomposition string list
   */
  public static List<String> tokenizer(String text) {
    return tokenizer(text, null);
  }

  /**
   * Decompose the string with a delimiter. <br>
   * The returned decomposed string list includes delimiters and spaces.
   *
   * @param text Search target string
   * @param unuseddelimiters Exclusion delimiter
   * @return Decomposition string list
   */
  public static List<String> tokenizer(String text, String[] unuseddelimiters) {
    if (text == null) return null;

    List<String> list = new ArrayList<String>();
    try {
      // Decompose the search target string into words and quotation marks
      LanguageTokenizer token = new LanguageTokenizer(text);
      // Delimiter settings
      token.useDelimiters(KscopeProperties.DELIMITER_CHARS);
      if (unuseddelimiters != null) {
        for (String unused : unuseddelimiters) {
          if (unused == null || unused.isEmpty()) continue;
          token.unusedDelimiter(unused);
        }
      }
      token.eolIsSignificant(true);
      while (token.nextToken() != LanguageTokenizer.LT_EOF) {
        list.add(token.sval);
      }
    } catch (Exception e) {
      e.printStackTrace();
    }

    if (list.size() <= 0) return null;

    return list;
  }

  /**
   * Decompose the string with a delimiter. <br>
   * The returned decomposed string list includes delimiters and spaces <br>
   * Quotation marks are not split as a single string.
   *
   * @param text Search target string
   * @param delimiters Delimiter
   * @return Decomposition string list
   */
  public static String[] tokenizerDelimit(String text, String delimiters) {
    if (text == null) return null;

    List<String> list = new ArrayList<String>();
    try {
      // Decompose the search target string into words and quotation marks
      LanguageTokenizer token = new LanguageTokenizer(text);
      // Delimiter settings
      token.useDelimiters(delimiters);
      token.eolIsSignificant(true);
      int ttype;
      while ((ttype = token.nextToken()) != LanguageTokenizer.LT_EOF) {
        switch (ttype) {
          case LanguageTokenizer.LT_EOL:
          case LanguageTokenizer.LT_EOF:
          case LanguageTokenizer.LT_DELIM:
            break;
          default:
            list.add(token.sval);
            break;
        }
      }
    } catch (Exception e) {
      e.printStackTrace();
    }

    if (list.size() <= 0) return null;

    return list.toArray(new String[0]);
  }

  /**
   * Check if the character is a delimiter.
   *
   * @param character Check character
   * @return true = Delimiter
   */
  private static boolean isDelimiter(String character) {
    if (character == null || character.isEmpty()) {
      return false;
    }
    return Character.isWhitespace(character.charAt(0))
        || KscopeProperties.DELIMITER_CHARS.indexOf(character) != -1;
  }

  /**
   * Remove spaces and parentheses before and after the string.
   *
   * @param arg_str Character string to be deleted
   * @return Character string after deletion
   */
  public static String delete_space(String arg_str) {
    int start_pos = arg_str.indexOf((String) "(");
    int last_pos = arg_str.lastIndexOf((String) ")");

    boolean string_in = false;
    StringBuffer arg_tmp = new StringBuffer();
    for (int i = start_pos + 1; i <= last_pos - 1; i++) {
      char ac = arg_str.charAt(i);
      if (ac == '\"' || ac == '\'') string_in = !(string_in);
      if (string_in) {
        arg_tmp.append(ac);
      } else {
        if (ac != ' ') {
          arg_tmp.append(ac);
        }
      }
    }
    return (arg_tmp.toString());
  }

  /**
   * Create a continuous string
   *
   * @param str Continuous characters
   * @param repeat Number of repetitions
   * @return Consecutive strings
   */
  public static String repeat(String str, int repeat) {
    if (str == null) {
      return null;
    }
    if (repeat <= 0) {
      return "";
    }
    if (repeat == 1) {
      return str;
    }

    StringBuffer buf = new StringBuffer();
    for (int i = 0; i < repeat; i++) {
      buf.append(str);
    }
    return buf.toString();
  }

  /**
   * Concatenate string arrays
   *
   * @param array String array
   * @param with Delimiter
   * @return Concatenated string
   */
  public static String join(String[] array, String with) {
    return join(array, with, 0);
  }

  /**
   * Concatenate string arrays
   *
   * @param array String array
   * @param with Delimiter
   * @param limit Upper limit of connection (unlimited with 0 or less setting)
   * @return Concatenated string
   */
  public static String join(String[] array, String with, int limit) {
    StringBuffer buf = new StringBuffer();
    int cnt = 0;
    for (String s : array) {
      if (limit > 0 && limit <= cnt) break;
      if (buf.length() > 0) buf.append(with);
      buf.append(s);
      cnt++;
    }
    return buf.toString();
  }

  /**
   * Convert InputStream to string (including line breaks)
   *
   * @param is InputStream
   * @return string
   * @throws IOException InputStream error
   */
  public static String convertString(InputStream is) throws IOException {
    BufferedReader reader = new BufferedReader(new InputStreamReader(is, "UTF-8"));
    StringBuilder sb = new StringBuilder();
    char[] b = new char[1024];
    int line;
    while (0 <= (line = reader.read(b))) {
      sb.append(b, 0, line);
    }
    return sb.toString();
  }

  /**
   * Check if it matches the regular expression
   *
   * @param str Search string
   * @param pattern Regular expression pattern
   * @return true = match
   */
  public static boolean patternMatch(String str, String pattern) {
    Pattern p = Pattern.compile(pattern);
    if (p == null) return false;
    Matcher m = p.matcher(str);

    return m.find();
  }

  /**
   * Escape the file pathname. <br>
   * Change'\'to'/'. <br>
   *
   * @param path File path name
   * @return Escape string
   */
  public static String escapeFilePath(String path) {
    if (path == null) return null;
    String escape = path.replaceAll("\\\\", "/");
    return escape;
  }
}
