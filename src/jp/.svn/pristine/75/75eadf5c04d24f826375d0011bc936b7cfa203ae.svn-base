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

package jp.riken.kscope.parser;

import java.io.IOException;
import java.util.ArrayList;

import jp.riken.kscope.utils.LanguageTokenizer;

/**
 * ソースコードを文字列分解するクラス
 * @author riken
 *
 */
public class LineSpliter {
    /** ソースコード文字列 */
    private String m_line;

    /**
     * 分解して返すリストに分解文字も含めるかのフラグ 但し、空白は追加しない。 true:リストに分解文字を追加する。
     */
    private boolean m_listDelimiter = false;
    /** 文字列分解ユーティリティクラス */
    private LanguageTokenizer m_token;

    /**
     * コンストラクタ
     * @param line		コード行文字列
     */
    public LineSpliter(String line) {
        m_line = line;
        m_token = new LanguageTokenizer(line);

        // デフォルト設定
        // 区切り文字:'[' ']' ',' 'SPACE'
        m_token.useDelimiter("[");
        m_token.useDelimiter("]");
        m_token.useDelimiter(",");
        m_token.useDelimiter(" ");
        m_token.useDelimiter("::");
        m_token.useDelimiter(":");

        // 連続したSPACEは1文字とする。
        m_token.whitespaceChar(' ');

        // 小括弧を対の区切り文字とする。
        m_token.setParenthesisQuote(true);
    }

    /**
     * コード行を分解する。
     *
     * @return 分解文字列リスト
     */
    public String[] split() {

        try {

            ArrayList<String> list = new ArrayList<String>();
            String buf = new String();
            int ttype;
            while ((ttype = m_token.nextToken()) != LanguageTokenizer.LT_EOF) {
                switch (ttype) {
                case LanguageTokenizer.LT_EOL:
                case LanguageTokenizer.LT_EOF:
                    if (list.size() == 0)
                        return null;
                    return list.toArray(new String[0]);
                case LanguageTokenizer.LT_WORD:
                case LanguageTokenizer.LT_QUOTE:
                case LanguageTokenizer.LT_PARENTHESIS:
                    if (!buf.isEmpty()) {
                        list.add(buf.trim());
                        buf = new String();
                    }
                    if (m_token.sval != null && !m_token.sval.isEmpty()) {
                        list.add(m_token.sval.trim());
                    }
                    break;
                case LanguageTokenizer.LT_DELIM:
                    if (m_listDelimiter) {
                        // デリミタもリストに追加する。（SPACEは除外する）
                        if (m_token.sval != null
                                && !m_token.sval.trim().isEmpty()) {
                            list.add(m_token.sval.trim().trim());
                        }
                    }
                    break;
                default:
                    buf += m_token.sval;
                    break;
                }
            }

            if (!buf.isEmpty()) {
                list.add(buf.trim());
            }

            if (list.size() == 0)
                return null;
            return list.toArray(new String[0]);

        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }

    /**
     * 分解して返すリストに分解文字も含めるかのフラグを設定する。
     *
     * @param delim
     *            true:リストに分解文字を追加する。
     */
    public void setListDelimiter(boolean delim) {
        m_listDelimiter = delim;
    }

    /**
     * デリミタを追加する。
     *
     * @param delim
     *            デリミタ
     */
    public void useDelimiter(String delim) {
        m_token.useDelimiter(delim);
    }

    /**
     * デリミタを削除する。
     *
     * @param delim
     *            デリミタ
     */
    public void unusedDelimiter(String delim) {
        m_token.unusedDelimiter(delim);
    }

    /**
     * 小括弧を対の区切り文字とする。
     * @param flag		true=小括弧を対の区切り文字とする。
     */
    public void setParenthesisQuote(boolean flag) {
        m_token.setParenthesisQuote(flag);
    }

    /**
     * ソースコード文字列を取得する
     * @return m_line		ソースコード文字列
     */
    public String getLine() {
        return m_line;
    }

    /**
     * ソースコード文字列を設定する
     * @param line ソースコード文字列
     */
    public void setLine(String line) {
        this.m_line = line;
    }
}
