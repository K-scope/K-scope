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
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.exception.ParseException;
import jp.riken.kscope.utils.LanguageTokenizer;

/**
 * A class that splits multiple lines separated by semicolons
 *
 * @author hira
 */
public class MultiLineParser {
  /** source file */
  private SourceFile m_file;

  /**
   * Constructor
   *
   * @param file source file
   */
  public MultiLineParser(SourceFile file) {
    m_file = file;
  }

  /**
   * Parse multiple sentences.
   *
   * @param line Line of code
   * @param lineno File line number
   * @return Code line list
   * @throws ParseException Perth exception
   */
  public CodeLine[] parser(StringBuilder line, int lineno) throws ParseException {

    if (line == null) return null;
    if (line.toString().trim().length() <= 0) {
      line.delete(0, line.length());
      return null;
    }

    try {

      ArrayList<CodeLine> codeList = new ArrayList<CodeLine>();
      // Split the line with a semicolon (;).
      String src_code = line.toString();
      src_code = src_code.trim();
      StringBuilder bufCode = new StringBuilder();

      LanguageTokenizer token = new LanguageTokenizer(src_code);

      // Split by semicolon (;)
      token.useDelimiter(";");
      token.eolIsSignificant(true);
      int ttype;
      while ((ttype = token.nextToken()) != LanguageTokenizer.LT_EOF) {
        if (ttype == LanguageTokenizer.LT_EOL || ttype == LanguageTokenizer.LT_EOF) break;

        switch (ttype) {
          case LanguageTokenizer.LT_WORD:
          case LanguageTokenizer.LT_QUOTE:
            bufCode.append(token.sval);
            break;
          case LanguageTokenizer.LT_DELIM:
            if (token.sval == ";") {
              // Create a codeline object and add it to the list.
              String buf = bufCode.toString();
              buf = buf.trim();
              String fn = null;
              if (m_file != null) {
                fn = m_file.getPath();
              }
              CodeLine code = new CodeLine(m_file, buf, lineno, fn);
              codeList.add(code);

              // Clear the code buffer.
              bufCode = new StringBuilder();
            }
            break;
          default:
            bufCode.append(token.sval);
            break;
        }
      }
      if (bufCode.length() > 0) {
        // Create a codeline object and add it to the list.
        String buf = bufCode.toString().trim();
        String fn = null;
        if (m_file != null) {
          fn = m_file.getPath();
        }
        CodeLine code = new CodeLine(m_file, buf, lineno, fn);
        codeList.add(code);
      }

      line.delete(0, line.length());

      return (CodeLine[]) codeList.toArray(new CodeLine[0]);
    } catch (IOException e) {
      throw new ParseException(e, line.toString(), lineno);
    }
  }
}
