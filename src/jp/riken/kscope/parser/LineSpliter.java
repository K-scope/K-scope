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
 * A class that decomposes source code into strings
 *
 * @author RIKEN
 */
public class LineSpliter {
  /** Source code string */
  private String m_line;

  /**
   * Flag to include decomposed characters in the list to be decomposed and returned However, do not
   * add spaces. true: Add decomposition characters to the list.
   */
  private boolean m_listDelimiter = false;
  /** String decomposition utility class */
  private LanguageTokenizer m_token;

  /**
   * Constructor
   *
   * @param line Code line String
   */
  public LineSpliter(String line) {
    m_line = line;
    m_token = new LanguageTokenizer(line);

    // Default configuration
    // Delimiter:'['']'',''SPACE'
    m_token.useDelimiter("[");
    m_token.useDelimiter("]");
    m_token.useDelimiter(",");
    m_token.useDelimiter(" ");
    m_token.useDelimiter("::");
    m_token.useDelimiter(":");

    // Consecutive SPACE is one character.
    m_token.whitespaceChar(' ');

    // Use parentheses as a pair delimiter.
    m_token.setParenthesisQuote(true);
  }

  /**
   * Decompose lines of code.
   *
   * @return Decomposition string list
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
            if (list.size() == 0) return null;
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
              // Add the delimiter to the list as well. (Excludes SPACE)
              if (m_token.sval != null && !m_token.sval.trim().isEmpty()) {
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

      if (list.size() == 0) return null;
      return list.toArray(new String[0]);

    } catch (IOException e) {
      e.printStackTrace();
    }
    return null;
  }

  /**
   * Set a flag to include decomposed characters in the decomposed and returned list.
   *
   * @param delim true: Add decomposition characters to the list.
   */
  public void setListDelimiter(boolean delim) {
    m_listDelimiter = delim;
  }

  /**
   * Add a delimiter.
   *
   * @param delim Delimiter
   */
  public void useDelimiter(String delim) {
    m_token.useDelimiter(delim);
  }

  /**
   * Delete the delimiter.
   *
   * @param delim Delimiter
   */
  public void unusedDelimiter(String delim) {
    m_token.unusedDelimiter(delim);
  }

  /**
   * Use parentheses as a pair delimiter.
   *
   * @param flag true = Use parentheses as a pair delimiter.
   */
  public void setParenthesisQuote(boolean flag) {
    m_token.setParenthesisQuote(flag);
  }

  /**
   * Get the source code string
   *
   * @return m_line Source code string
   */
  public String getLine() {
    return m_line;
  }

  /**
   * Set the source code string
   *
   * @param line Source code string
   */
  public void setLine(String line) {
    this.m_line = line;
  }
}
