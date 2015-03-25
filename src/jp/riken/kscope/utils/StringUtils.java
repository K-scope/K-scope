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
 * 文字列操作ユーティリティクラス
 *
 * @version    2015/03/01   	ブーリアン文字列から真偽値の変換メソッドの追加
 */
public class StringUtils {

    /**
     * テキストタブをHTML変換する時のタブサイズ
     */
    private static int HTML_TABSIZE = 4;

    /**
     * 文字列の文字コードを判別する。 判定優先順位は、EUC_JP > SHIFT_JIS > UTF-8とする。
     *
     * @param byts
     *            文字コードバイト配列
     * @return 判定結果文字コード文字列 "ASCII" ASCII "ISO-2022-JP" JIS "EUC_JP" EUC_JP
     *         "SHIFT_JIS" SHIFT-JIS "UTF-8" UTF-8 null 判定不可
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

        // ASCII, JIS判断
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
                    // 制御文字
                    return null;
                }
                if (isJis) {
                    if (b1 == 0x1B) {
                        if (b1 == 0x1B && b2 == 0x28 && b3 == 0x42)
                            jis++;
                        else if (b1 == 0x1B && b2 == 0x28 && b3 == 0x4A)
                            jis++;
                        else if (b1 == 0x1B && b2 == 0x28 && b3 == 0x49)
                            jis++;
                        else if (b1 == 0x1B && b2 == 0x24 && b3 == 0x40)
                            jis++;
                        else if (b1 == 0x1B && b2 == 0x24 && b3 == 0x42)
                            jis++;
                        else if (b1 == 0x1B && b2 == 0x24 && b3 == 0x28
                                && b4 == 0x44)
                            jis++;
                        else if (b1 == 0x1B && b2 == 0x26 && b3 == 0x40
                                && b4 == 0x1B && b5 == 0x24 && b6 == 0x42)
                            jis++;
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

        // Shift-Jis判断
        for (int i = jp_char; i < len; i++) {
            b1 = byts[i] & 0xFF;
            b2 = i < len - 1 ? byts[i + 1] & 0xFF : 0x00;
            b3 = i < len - 2 ? byts[i + 2] & 0xFF : 0x00;
            b4 = i < len - 3 ? byts[i + 3] & 0xFF : 0x00;

            if (b1 <= 0x7F) {
                continue;
            }

            // SHIFT-JIS未使用文字が表れたら、SHIFT-JISではない。
            // 参照 : http://charset.7jp.net/sjis.html
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
                    // あまり使用しない文字であるのでカウントしない。（文字化けの可能性があるため）
                    // sjis++;
                    i += 1;
                } else if ((b1 >= 0xED && b1 <= 0xEE)
                        && ((b2 >= 0x40 && b2 <= 0x7E) || (b2 >= 0x80 && b2 <= 0xFC))) {
                    // あまり使用しない文字であるのでカウントしない。（文字化けの可能性があるため）
                    // sjis++;
                    i += 1;
                } else if ((b1 >= 0xE0 && b1 <= 0xFC)
                        && ((b2 >= 0x40 && b2 <= 0x7E) || (b2 >= 0x80 && b2 <= 0xFC))) {
                    // 未定義文字、機種依存、拡張の為カウントしない。
                    sjis_ext++;
                    i += 1;
                } else {
                    sjis = 0;
                    break;
                }
            }
        }

        // EUC判断
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
            } else if ((b1 == 0x8F) && (b2 >= 0xA1 && b2 <= 0xFE)
                    && (b3 >= 0xA1 && b3 <= 0xFE)) {
                euc += 1;
                i += 2;
            } else {
                euc = 0;
                break;
            }
        }

        // UTF-8判断
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
            // 3バイト文字
            else if ((b1 == 0xE0) && (b2 >= 0xB8 && b2 <= 0xBB)
                    && (b3 >= 0x80 && b3 <= 0xBF)) {
                utf8 += 1;
                i += 2;
            } else if ((b1 >= 0xE1 && b1 <= 0xE2) && (b2 >= 0x80 && b2 <= 0xBF)
                    && (b3 >= 0x80 && b3 <= 0xBF)) {
                utf8 += 1;
                i += 2;
            } else if ((b1 == 0xE3) && (b2 >= 0x80 && b2 <= 0x8F)
                    && (b3 >= 0x80 && b3 <= 0xBF)) {
                utf8 += 1;
                i += 2;
            } else if ((b1 == 0xE4) && (b2 >= 0xB8 && b2 <= 0xBF)
                    && (b3 >= 0x80 && b3 <= 0xBF)) {
                utf8 += 1;
                i += 2;
            } else if ((b1 >= 0xE5 && b1 <= 0xE9) && (b2 >= 0x80 && b2 <= 0xBF)
                    && (b3 >= 0x80 && b3 <= 0xBF)) {
                utf8 += 1;
                i += 2;
            } else if ((b1 == 0xED) && (b2 >= 0x80 && b2 <= 0x9F)
                    && (b3 >= 0x80 && b3 <= 0xBF)) {
                // ３バイト文字の未定義の為カウントしない
                i += 2;
            } else if ((b1 == 0xEF) && (b2 >= 0xA7 && b2 <= 0xBF)
                    && (b3 >= 0x80 && b3 <= 0xBF)) {
                if ((b1 == 0xEF) && (b2 == 0xBF) && (b3 == 0xBD)) {
                    utf8 = 0;
                    break;
                }
                utf8 += 1;
                i += 2;
            } else if ((b1 >= 0xE0 && b1 <= 0xEF) && (b2 >= 0x80 && b2 <= 0xBF)
                    && (b3 >= 0x80 && b3 <= 0xBF)) {
                // ３バイト文字の未定義の為カウントしない
                i += 2;
            } else if ((b1 == 0xF0) && (b2 >= 0x90 && b2 <= 0xBF)
                    && (b3 >= 0x80 && b3 <= 0xBF) && (b4 >= 0x80 && b4 <= 0xBF)) {
                // ４バイト文字は未定義の為カウントしない
                i += 3;
            } else if ((b1 >= 0xF1 && b1 <= 0xF3) && (b2 >= 0x80 && b2 <= 0xBF)
                    && (b3 >= 0x80 && b3 <= 0xBF) && (b4 >= 0x80 && b4 <= 0xBF)) {
                // ４バイト文字は未定義の為カウントしない
                i += 3;
            } else if ((b1 == 0xF4) && (b2 >= 0x80 && b2 <= 0x8F)
                    && (b3 >= 0x80 && b3 <= 0xBF) && (b4 >= 0x80 && b4 <= 0xBF)) {
                // ４バイト文字は未定義の為カウントしない
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

        // UTF-8を優先とする (UTF-8 > SHIFT_JIS)
        if (euc >= sjis && euc >= utf8)
            return "EUC_JP";
        else if (utf8 >= euc && utf8 >= sjis)
            return "UTF-8";
        else if (sjis >= euc && sjis >= utf8)
            return "SHIFT_JIS";

        return null;
    }

    /**
     * 文字列が整数値であるかチェックする
     *
     * @param str
     *            整数値文字列
     * @return true:整数値
     */
    public static boolean isNumeric(String str) {
        if (str == null)
            return false;
        try {
            // Integer num = Integer.valueOf(str) ;
            Integer.valueOf(str);
        } catch (NumberFormatException e) {
            return false;
        }
        return true;
    }

    /**
     * 文字列が浮動小数点数値であるかチェックする
     *
     * @param str
     *            浮動小数点数値文字列
     * @return true:浮動小数点数値
     */
    public static boolean isFloat(String str) {
        if (str == null)
            return false;
        try {
            Float.valueOf(str);
        } catch (NumberFormatException e) {
            return false;
        }
        return true;
    }

    /**
     * 文字の前後のシングルクォート、ダブルクォートを削除する。
     *
     * @param line
     *            トリム対象文字列
     * @return トリム後の文字列
     */
    public static String trimQuote(String line) {
        if (line == null || line.isEmpty())
            return line;

        final String quote[] = { "'", "\"" };
        String trim_str = line.trim();
        for (int i = 0; i < quote.length; i++) {
            if (trim_str.startsWith(quote[i]) && trim_str.endsWith(quote[i])) {
                return line.substring(1, line.length() - 1);
            }
        }

        return line;
    }

    /**
     * 文字列がnullか空文字であるかチェックする。
     *
     * @param s
     *            チェック文字列
     * @return true=nullか空文字
     */
    public static boolean isNullOrEmpty(String s) {
        return ((s == null) || (s.isEmpty() != false));
    }

    /**
     * 文字列がnullか空文字, スペースのみであるかチェックする。
     *
     * @param s
     *            チェック文字列
     * @return true=nullか空文字、スペースのみ
     */
    public static boolean isTrimEmpty(String s) {
        if (s == null) return true;
        if (s.trim().isEmpty()) return true;
        return false;
    }

    /**
     * 文字列を検索する. <br/>
     * (author rist_kobayashi)
     *
     * @param text
     *            検索対象となる全テキスト
     * @param target
     *            検索する文字列
     * @return キーを行番号、値を対象行先頭からの位置のリストとしたTreeMap （TreeMap<行番号,
     *         ArrayList<先頭からの位置>>）。ただし、行頭の位置を0とする。<br>
     *         textがnullあるいは空文字、またはtargetがnullあるいは空文字の場合は、空のTreeMapを 返します。
     *
     *
     */
    public static TreeMap<Integer, ArrayList<Integer>> searchString(
            String text, String target) {
        // 事前条件
        if (text == null || target == null || text.equals("")
                || target.equals("")) {
            return new TreeMap<Integer, ArrayList<Integer>>();
        }
        String[] lines = splitByNewlineCode(text);
        return searchString(lines, target);
    }

    /**
     * 文字列を検索する<br/>
     * (author rist_kobayashi)
     *
     * @param lines
     *            検索対象となる全行
     * @param target
     *            検索する文字列
     * @return キーを行番号、値を対象行先頭からの位置のリストとしたTreeMap （TreeMap<行番号,
     *         ArrayList<先頭からの位置>>）。ただし、行頭の位置を0とする。<br>
     *         linesがnull、またはtargetがnullあるいは空文字の場合は、空のTreeMapを 返します。
     *
     */
    public static TreeMap<Integer, ArrayList<Integer>> searchString(
            String[] lines, String target) {
        TreeMap<Integer, ArrayList<Integer>> result = new TreeMap<Integer, ArrayList<Integer>>();
        // 事前条件
        if (lines == null || target == null || target.equals("")) {
            return result;
        }

        // 検索文字を探索する
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
     * 文字列を改行コードで分割する<br/>
     * (author rist_kobayashi)
     *
     * @param text
     *            分割対象テキスト
     * @return 改行コードで分割された文字列リスト。textがnullの時は、nullを返します。
     *
     */
    public static String[] splitByNewlineCode(String text) {
        String newlineCode = "";
        String str = text;

        // 事前条件
        if (text == null) {
            return null;
        }

        // 改行コードの決定
        if (!str.contains("\r")) {
            // 改行コードが\nの場合
            newlineCode = "\n";
        } else if (!str.contains("\n")) {
            // 改行コードが\rの場合
            newlineCode = "\r";
        } else {
            // 改行コードが\r\nあるいは\n\rの場合
            // 改行コードを\nに変換しておく
            str = str.replace("\r", "\n");
            str = str.replace("\n\n", "\n");
            newlineCode = "\n";
        }

        return str.split(newlineCode);
    }

    /**
     * HEX2桁を intに変換する。
     *
     * @param hex
     *            HEX2桁文字列
     * @return 変換数値
     */
    public static int hex2Toint(String hex) {
        int value = 0;
        char hexDigit[] = hex.toCharArray();
        value = (Character.digit(hexDigit[0], 16)) * 16
                + (Character.digit(hexDigit[1], 16));
        return value;
    }

    /**
     * intから16進2桁に変換
     *
     * @param value
     *            int数値
     * @return 16進2桁
     */
    public static String intTohex2(int value) {
        char hex2[] = { Character.forDigit((value >> 4) & 0x0F, 16),
                Character.forDigit(value & 0x0F, 16) };
        String hex2Str = new String(hex2);
        return hex2Str.toUpperCase();
    }

    /**
     * 16進数4桁からintに変換
     *
     * @param hex
     *            16進数4桁
     * @return int数値
     */
    public static int hex4Toint(String hex) {
        int value = 0;
        char HexDigit[] = hex.toCharArray();
        value = (Character.digit(HexDigit[0], 16)) * 16 * 16 * 16
                + (Character.digit(HexDigit[1], 16)) * 16 * 16
                + (Character.digit(HexDigit[2], 16)) * 16
                + (Character.digit(HexDigit[3], 16));
        return value;
    }

    /**
     * intから16進4桁に変換
     *
     * @param value
     *            int数値
     * @return 16進4桁
     */
    public static String intTohex4(int value) {
        char hex4[] = { Character.forDigit((value >> 12) & 0x0F, 16),
                Character.forDigit((value >> 8) & 0x0F, 16),
                Character.forDigit((value >> 4) & 0x0F, 16),
                Character.forDigit(value & 0x0F, 16) };
        String hex4Str = new String(hex4);
        return hex4Str.toUpperCase();
    }

    /**
     * 文字列の前後の半角、全角スペースを削除する。
     *
     * @param src
     *            トリム対象文字列
     * @return 前後の半角、全角スペースを削除文字列
     */
    public static String trim(String src) {
        if (src == null)
            return src;

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
     * カラー文字列からjava.awt.Colorオブジェクトを作成する.<br/>
     * カラー文字列は以下の形式とする.<br/>
     * 1. RED,WHITE,BLUE等のjava.awt.Color定義のフィールド名 <br/>
     * 2. #RRGGBB : '#'で始まり, 16進数6桁 = html色指定 2. #RRGGBBAA : '#'で始まり, 16進数8桁 =
     * 透過色
     *
     * @param color
     *            カラー文字列
     * @return java.awt.Colorオブジェクト
     */
    public static Color parseColor(String color) {
        if (color == null || color.isEmpty())
            return null;

        Color value = null;
        try {
            if (color.startsWith("#") && color.length() == 7) {
                String red = color.substring(1, 3);
                String green = color.substring(3, 5);
                String blue = color.substring(5);
                value = new Color(StringUtils.hex2Toint(red),
                        StringUtils.hex2Toint(green),
                        StringUtils.hex2Toint(blue));
                return value;
            } else if (color.startsWith("#") && color.length() == 9) {
                String red = color.substring(1, 3);
                String green = color.substring(3, 5);
                String blue = color.substring(5, 7);
                String alpha = color.substring(7);
                value = new Color(StringUtils.hex2Toint(red),
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
     * java.awt.ColorオブジェクトからHTMLカラーコードを作成する.<br/>
     * カラー文字列は以下の形式とする.<br/>
     * 1. #RRGGBB : '#'で始まり, 16進数6桁 = html色指定
     *
     * @param color
     *            java.awt.Colorオブジェクト
     * @return HTMLカラーコード
     */
    public static String parseColorCode(Color color) {
        if (color == null)
            return null;

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
     * 改行文字、空白、タブをHTMLコードに置換する
     *
     * @param content
     *            元テキスト文字列
     * @return HTML変換文字列
     */
    public static String textTohtml(String content) {
        if (content == null || content.isEmpty())
            return null;

        StringBuffer buf = new StringBuffer();
        boolean opening = false;
        for (int i = 0; i < content.length(); i++) {
            String str = content.substring(i, i + 1);
            // ラインフィード
            if ("\r".equals(str)) {
                continue;
            }

            // タブ文字をチェックする
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

            // 空白文字
            if (" ".equals(str)) {
                buf.append("&nbsp;");
            }
            // クォーテーション
            else if ("\"".equals(str)) {
                buf.append("&quot;");
            }
            // アンパサンド
            else if ("&".equals(str)) {
                buf.append("&amp;");
            }
            // 小なり
            else if ("<".equals(str)) {
                buf.append("&lt;");
            }
            // 大なり
            else if (">".equals(str)) {
                buf.append("&gt;");
            }
            // コピーライト
            else if ("@".equals(str)) {
                buf.append("&copy;");
            }
            // 改行文字
            else if ("\n".equals(str)) {
                buf.append("<br/>\n");
            }
            // タブ文字
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
     * 検索文字列から検索対象文字をHTML色設定を行う.<br/>
     * 検索文字列をfont,colorのHTMLタグで囲む
     *
     * @param text
     *            検索対象文字列
     * @param search
     *            検索文字列
     * @param forecolor
     *            文字色
     * @param backcolor
     *            背景色
     * @param fontstyle
     *            フォントスタイル
     * @param sensitivecase
     *            大文字・小文字の区別(true=大文字・小文字の区別を行う)
     * @param regex
     *            正規表現
     * @param word
     *            単語検索
     * @return HTML色設定文字列
     */
    public static String searchTextToHtml(String text, String search,
            Color forecolor, Color backcolor, int fontstyle,
            boolean sensitivecase, boolean regex, boolean word) {

        String contentText = textTohtml(text);
        String searchText = textTohtml(search);

        // 検索文字列から検索対象文字位置リストを取得する
        List<Integer[]> list = getSearchTextList(contentText, searchText,
                sensitivecase, regex, word);
        if (list == null || list.size() <= 0)
            return text;

        // 検索一致文字を置換する
        for (int i = list.size() - 1; i >= 0; i--) {
            int start = list.get(i)[0];
            int end = list.get(i)[1];
            // 検索一致文字
            String findText = contentText.substring(start, end);
            // カラーリング
            String html = createHtmlColorTag(findText, forecolor, backcolor,
                    fontstyle);
            contentText = contentText.substring(0, start) + html
                    + contentText.substring(end);
        }
        contentText = "<html>" + contentText + "</html>";
        return contentText;
    }

    /**
     * 検索文字列から検索対象文字の位置情報を取得する.<br/>
     *
     * @param text
     *            検索対象文字列
     * @param search
     *            検索文字列
     * @param sensitivecase
     *            大文字・小文字の区別(true=大文字・小文字の区別を行う)
     * @param regex
     *            正規表現
     * @param word
     *            単語検索
     * @return HTML色設定文字列
     */
    private static List<Integer[]> getSearchTextList(String text,
            String search, boolean sensitivecase, boolean regex, boolean word) {
        if (text == null)
            return null;
        if (search == null)
            return null;

        String contentText = text;
        String searchText = search;
        List<Integer[]> list = new ArrayList<Integer[]>();

        // 正規表現
        if (regex) {
            int flags = 0;
            if (!sensitivecase) {
                // 大文字・小文字の区別を行わない
                flags = Pattern.CASE_INSENSITIVE;
            }
            flags += Pattern.MULTILINE;
            Pattern pattern = Pattern.compile(searchText, flags);
            Matcher m = pattern.matcher(contentText);
            while (m.find()) {
                // 一致位置リストの作成
                Integer[] findinfo = { m.start(), m.end() };
                list.add(findinfo);
            }
        } else {
            if (!sensitivecase) {
                // 大文字・小文字の区別を行わないので、すべて小文字に変換する
                contentText = contentText.toLowerCase();
                searchText = searchText.toLowerCase();
            }

            // 文字列検索を行う
            int fromIndex = 0;
            int start = -1;
            while ((start = contentText.indexOf(searchText, fromIndex)) != -1) {
                int end = start + searchText.length();
                fromIndex = end;
                // 単語検索
                if (word) {
                    // 検索結果の前後の文字がデリミタであるかチェックする
                    if (start > 0) {
                        if (!isDelimiter(contentText
                                .substring(start - 1, start))) {
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
                // 一致位置リストの作成
                Integer[] findinfo = { start, end };
                list.add(findinfo);
            }
        }
        if (list.size() <= 0)
            return null;

        return list;
    }

    /**
     * 検索文字列から検索対象文字が存在するか取得する.<br/>
     *
     * @param text
     *            検索対象文字列
     * @param search
     *            検索文字列
     * @param sensitivecase
     *            大文字・小文字の区別(true=大文字・小文字の区別を行う)
     * @param regex
     *            正規表現
     * @param word
     *            単語検索
     * @return HTML色設定文字列
     */
    public static boolean existsSearchText(String text, String search,
            boolean sensitivecase, boolean regex, boolean word) {

        if (text == null)
            return false;
        if (search == null)
            return false;

        // ダミーの文字色、背景色を設定
        String html = searchTextToHtml(text, search, Color.BLACK, Color.WHITE,
                Font.BOLD, sensitivecase, regex, word);

        return !(text.equals(html));
    }

    /**
     * 検索文字列から検索対象文字をHTML色設定を行う(変数検索)対象文字列であるかチェックする.<br/>
     * 大文字・小文字の区別はしない、単語検索、引用符で囲まれた範囲は対象としない.<br/>
     * *
     *
     * @param text
     *            検索対象文字列
     * @param search
     *            検索文字列
     * @return true=HTML色設定対象文字列
     */
    public static boolean existsSearchWord(String text, String search) {
        if (text == null)
            return false;
        if (search == null)
            return false;

        // ダミーの文字色、背景色を設定
        String html = searchWordToHtml(text, search, Color.BLACK, Color.WHITE,
                Font.BOLD);

        return !(text.equals(html));

    }

    /**
     * 検索文字列から検索対象文字をHTML色設定を行う(変数検索).<br/>
     * 大文字・小文字の区別はしない、単語検索、引用符で囲まれた範囲は対象としない.<br/>
     * 検索文字列をfont,colorのHTMLタグで囲む
     *
     * @param text
     *            検索対象文字列
     * @param search
     *            検索文字列
     * @param forecolor
     *            文字色
     * @param backcolor
     *            背景色
     * @param fontstyle
     *            フォントスタイル
     * @return HTML色設定文字列
     */
    public static String searchWordToHtml(String text, String search,
            Color forecolor, Color backcolor, int fontstyle) {
        if (text == null)
            return text;
        if (search == null)
            return text;

        try {
            String contentText = textTohtml(text);
            String searchText = textTohtml(search);

            StringBuffer buf = new StringBuffer();
            boolean htmltag = false;

            // 検索対象文字列を単語、引用符分解を行う
            LanguageTokenizer token = new LanguageTokenizer(contentText);
            // デリミタの設定
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
                        // コメント
                        buf.append(token.sval);
                    } else if (searchText.equalsIgnoreCase(token.sval)) {
                        String html = createHtmlColorTag(token.sval, forecolor,
                                backcolor, fontstyle);
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
                        // 以下コメントとする
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
                // 検索文字列が存在しない。
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
     * 文字列に対してフォントカラー, 背景色, スタイルをHTMLタグを適用する.
     *
     * @param text
     *            適用文字列
     * @param forecolor
     *            文字色
     * @param backcolor
     *            背景色
     * @param fontstyle
     *            フォントスタイル
     * @return HTMLタグ適用文字列
     */
    private static String createHtmlColorTag(String text, Color forecolor,
            Color backcolor, int fontstyle) {

        // スタイル
        String style = "";

        // 文字色
        String attrColor = null;
        if (forecolor != null) {
            attrColor = parseColorCode(forecolor);
            attrColor = "color:" + attrColor + ";";
            style += attrColor;
        }
        // 背景色
        String attrBackcolor = null;
        if (backcolor != null) {
            attrBackcolor = parseColorCode(backcolor);
            attrBackcolor = "background-color:" + attrBackcolor + ";";
            style += attrBackcolor;
        }
        // 太字
        String bold = null;
        if ((fontstyle & Font.BOLD) != 0) {
            bold = "font-weight: bold;";
            style += bold;
        }
        // イタリック
        String italic = null;
        if ((fontstyle & Font.ITALIC) != 0) {
            italic = "font-style: italic;";
            style += italic;
        }
        if (style == null || style.isEmpty())
            return text;

        style = "style='" + style + "'";
        String html = "<span " + style + ">" + text + "</span>";

        return html;
    }

    /**
     * 文字列をデリミタで分解する.<br/>
     * 返す分解文字列リストには、デリミタ、空白も含む
     *
     * @param text
     *            検索対象文字列
     * @return 分解文字列リスト
     */
    public static List<String> tokenizer(String text) {
        return tokenizer(text, null);
    }

    /**
     * 文字列をデリミタで分解する.<br/>
     * 返す分解文字列リストには、デリミタ、空白も含む
     *
     * @param text
     *            検索対象文字列
     * @param unuseddelimiters
     *            除外デリミタ
     * @return 分解文字列リスト
     */
    public static List<String> tokenizer(String text, String[] unuseddelimiters) {
        if (text == null)
            return null;

        List<String> list = new ArrayList<String>();
        try {
            // 検索対象文字列を単語、引用符分解を行う
            LanguageTokenizer token = new LanguageTokenizer(text);
            // デリミタの設定
            token.useDelimiters(KscopeProperties.DELIMITER_CHARS);
            if (unuseddelimiters != null) {
                for (String unused : unuseddelimiters) {
                    if (unused == null || unused.isEmpty())
                        continue;
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

        if (list.size() <= 0)
            return null;

        return list;
    }

    /**
     * 文字列をデリミタで分解する.<br/>
     * 返す分解文字列リストには、デリミタ、空白も含む<br/>
     * 引用符は１つの文字列として分割しない.
     *
     * @param text
     *            検索対象文字列
     * @param delimiters
     *            デリミタ
     * @return 分解文字列リスト
     */
    public static String[] tokenizerDelimit(String text, String delimiters) {
        if (text == null)
            return null;

        List<String> list = new ArrayList<String>();
        try {
            // 検索対象文字列を単語、引用符分解を行う
            LanguageTokenizer token = new LanguageTokenizer(text);
            // デリミタの設定
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

        if (list.size() <= 0)
            return null;

        return list.toArray(new String[0]);
    }

    /**
     * 文字が区切り文字であるかチェックする。
     *
     * @param character
     *            チェック文字
     * @return true=区切り文字
     */
    private static boolean isDelimiter(String character) {
        if (character == null || character.isEmpty()) {
            return false;
        }
        return Character.isWhitespace(character.charAt(0))
                || KscopeProperties.DELIMITER_CHARS.indexOf(character) != -1;
    }

    /**
     * 文字列の前後のスペース、小括弧を削除する。
     *
     * @param arg_str
     *            削除対象文字列
     * @return 削除後文字列
     */
    public static String delete_space(String arg_str) {
        int start_pos = arg_str.indexOf((String) "(");
        int last_pos = arg_str.lastIndexOf((String) ")");

        boolean string_in = false;
        StringBuffer arg_tmp = new StringBuffer();
        for (int i = start_pos + 1; i <= last_pos - 1; i++) {
            char ac = arg_str.charAt(i);
            if (ac == '\"' || ac == '\'')
                string_in = !(string_in);
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
     * 連続した文字列を作成する
     *
     * @param str
     *            連続文字
     * @param repeat
     *            繰り返し回数
     * @return 連続した文字列
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
     * 文字列配列を連結する
     *
     * @param array
     *            文字列配列
     * @param with
     *            区切り文字
     * @return 連結した文字列
     */
    public static String join(String[] array, String with) {
        return join(array, with, 0);
    }

    /**
     * 文字列配列を連結する
     *
     * @param array
     *            文字列配列
     * @param with
     *            区切り文字
     * @param limit
     *            連結の上限（0以下の設定で無制限）
     * @return 連結した文字列
     */
    public static String join(String[] array, String with, int limit) {
        StringBuffer buf = new StringBuffer();
        int cnt = 0;
        for (String s : array) {
            if (limit > 0 && limit <= cnt)
                break;
            if (buf.length() > 0)
                buf.append(with);
            buf.append(s);
            cnt++;
        }
        return buf.toString();
    }

    /**
     * InputStreamを文字列に変換する（改行込）
     *
     * @param is
     *            InputStream
     * @return 文字列
     * @throws IOException
     *             InputStreamエラー
     */
    public static String convertString(InputStream is) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(is,
                "UTF-8"));
        StringBuilder sb = new StringBuilder();
        char[] b = new char[1024];
        int line;
        while (0 <= (line = reader.read(b))) {
            sb.append(b, 0, line);
        }
        return sb.toString();
    }

    /**
     * 正規表現に一致しているかチェック
     *
     * @param str
     *            検索文字列
     * @param pattern
     *            正規表現パターン
     * @return true=一致している
     */
    public static boolean patternMatch(String str, String pattern) {
        Pattern p = Pattern.compile(pattern);
        if (p == null)
            return false;
        Matcher m = p.matcher(str);

        return m.find();
    }

    /**
     * ファイルパス名をエスケープする.<br/>
     * '￥'を'/'に変更する.<br/>
     *
     * @param path
     *            ファイルパス名
     * @return エスケープ文字列
     */
    public static String escapeFilePath(String path) {
        if (path == null)
            return null;
        String escape = path.replaceAll("\\\\", "/");
        return escape;
    }


    /**
     * ブーリアン文字列から真偽値を取得する.
     * 'true','1'の場合はtrueとする。
     * @param value		ブーリアン文字列
     * @return   真偽値
     */
    public static boolean toBoolean(String value) {
        if (value == null) return false;
        value = value.trim();

        if ("true".equalsIgnoreCase(value)) return true;
        if ("1".equalsIgnoreCase(value)) return true;

        return false;
    }
}
