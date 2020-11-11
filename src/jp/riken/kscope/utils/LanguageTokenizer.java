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

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Iterator;

/**
 * Language syntax split class
 *
 * @author RIKEN
 */
public class LanguageTokenizer {

  /** Read source code reader */
  private Reader reader = null;

  /** Split character storage buffer */
  private StringBuffer buf = new StringBuffer();

  /** Delimiter list */
  private ArrayList<String> m_delimList = new ArrayList<String>();

  /** Next read flag */
  private int peekc = NEED_CHAR;

  private static final int NEED_CHAR = Integer.MAX_VALUE;
  private static final int SKIP_LF = Integer.MAX_VALUE - 1;

  /** Line number */
  private int LINENO = 1;

  /**
   * EOL (line feed) delimiter flag true: EOL (line feed) is used as a delimiter false: EOL (line
   * feed) is not used as a delimiter.
   */
  private boolean eolIsSignificantP = false;

  /** Comment out C language comment ("//") */
  private boolean slashSlashCommentsP = false;
  /** Comment C language comment ("/*") */
  private boolean slashStarCommentsP = false;
  /** Use parentheses'(',')' as pair delimiters */
  private boolean parenthesisQuote = false;

  /**
   * ACSII character clause setting list CT_WHITESPACE, CT_ALPHA, CT_QUOTE, CT_COMMENT are set by OR
   * value.
   */
  private byte ctype[] = new byte[256];
  /** Character to replace with space */
  private static final byte CT_WHITESPACE = 1;
  /** Characters to be words */
  private static final byte CT_ALPHA = 4;
  /** Characters to be enclosed words */
  private static final byte CT_QUOTE = 8;
  /** Characters to comment (unused) */
  private static final byte CT_COMMENT = 16;

  /** Current character phrase setting */
  public int ttype = LT_NOTHING;

  /** EOF (end of read reader) */
  public static final int LT_EOF = -1;

  /** EOL (end of read line) */
  public static final int LT_EOL = '\n';

  /** word string */
  public static final int LT_WORD = -3;

  /** Undecided */
  private static final int LT_NOTHING = -4;

  /** Enclosed word */
  public static final int LT_QUOTE = -9;

  /** Delimiter */
  public static final int LT_DELIM = -10;

  /** Parenthesized */
  public static final int LT_PARENTHESIS = -11;

  /** Phrase-delimited string */
  public String sval;

  /** Constructor */
  private LanguageTokenizer() {
    clearToken();
    // wordChars('a', 'z');
    // wordChars('A', 'Z');
    // wordChars(128 + 32, 255);
    whitespaceChars(0, ' ' - 1);
    quoteChar('"');
    quoteChar('\'');
    eolIsSignificant(true);
  }

  /**
   * Constructor
   *
   * @param r Read source reader
   */
  public LanguageTokenizer(Reader r) {
    this();
    if (r == null) {
      throw new NullPointerException();
    }
    reader = r;
  }

  /**
   * Constructor
   *
   * @param str Read source code string
   */
  public LanguageTokenizer(String str) {
    this();
    if (str == null) {
      throw new NullPointerException();
    }
    reader = new StringReader(str);
  }

  /** Reset phrase characters */
  public void resetSyntax() {
    for (int i = ctype.length; --i >= 0; ) ctype[i] = 0;
  }

  /** Make phrase characters the default settings */
  public void clearToken() {
    for (int i = ctype.length; --i >= 0; ) ctype[i] = CT_ALPHA;
  }

  /**
   * Specifies that all characters <i>c</i> in the range <code>
   * low&nbsp;&lt;=&nbsp;<i>c</i>&nbsp;&lt;=&nbsp;high</code> are word constituents. A word token
   * consists of a word constituent followed by zero or more word constituents or number
   * constituents.
   *
   * @param low the low end of the range.
   * @param hi the high end of the range.
   */
  public void wordChars(int low, int hi) {
    if (low < 0) low = 0;
    if (hi >= ctype.length) hi = ctype.length - 1;
    while (low <= hi) {
      ctype[low++] &= ~CT_WHITESPACE;
      ctype[low++] |= CT_ALPHA;
    }
  }

  /**
   * Set word letters
   *
   * @param low Word character code
   */
  public void wordChar(int low) {
    if (low < 0) low = 0;
    if (low >= ctype.length) low = ctype.length - 1;
    ctype[low] &= ~CT_WHITESPACE;
    ctype[low] |= CT_ALPHA;
  }

  /**
   * Set the space character range
   *
   * @param low Lower limit space character code
   * @param hi Maximum space character code
   */
  public void whitespaceChars(int low, int hi) {
    if (low < 0) low = 0;
    if (hi >= ctype.length) hi = ctype.length - 1;
    while (low <= hi) ctype[low++] = CT_WHITESPACE;
  }

  /**
   * Set space character
   *
   * @param ch Space character code
   */
  public void whitespaceChar(int ch) {
    if (ch >= 0 && ch < ctype.length) ctype[ch] |= CT_WHITESPACE;
  }

  /**
   * Set the character range that is not a clause character
   *
   * @param low Character code without lower limit setting
   * @param hi Character code with no upper limit
   */
  public void ordinaryChars(int low, int hi) {
    if (low < 0) low = 0;
    if (hi >= ctype.length) hi = ctype.length - 1;
    while (low <= hi) ctype[low++] = 0;
  }

  /**
   * Set characters that are not clause characters
   *
   * @param ch Character code without setting
   */
  public void ordinaryChar(int ch) {
    if (ch >= 0 && ch < ctype.length) ctype[ch] = 0;
  }

  /**
   * Set comment characters
   *
   * @param ch Comment character
   */
  public void commentChar(int ch) {
    if (ch >= 0 && ch < ctype.length) ctype[ch] = CT_COMMENT;
  }

  /**
   * Set the quotation mark character. Normally ("), (') is set.
   *
   * @param ch Quote character
   */
  public void quoteChar(int ch) {
    if (ch >= 0 && ch < ctype.length) ctype[ch] = CT_QUOTE;
  }

  /**
   * Cancel the quotation mark character setting.
   *
   * @param ch Quote character
   */
  public void unQuoteChar(int ch) {
    if (ch >= 0 && ch < ctype.length) ctype[ch] &= ~CT_QUOTE;
  }

  /**
   * Set whether to use the line feed character as a phrase delimiter
   *
   * @param flag true = Newline character is used as a phrase delimiter
   */
  public void eolIsSignificant(boolean flag) {
    eolIsSignificantP = flag;
  }

  /**
   * Set whether to comment C language comment ("/*")
   *
   * @param flag true = Make a C language comment ("/*") a comment
   */
  public void slashStarComments(boolean flag) {
    slashStarCommentsP = flag;
  }

  /**
   * Set whether to comment C language comment ("/*")
   *
   * @param flag true = Make a C language comment ("/*") a comment
   */
  public void slashSlashComments(boolean flag) {
    slashSlashCommentsP = flag;
  }

  /**
   * Read the next character
   *
   * @return Read character code
   */
  private int read() throws IOException {
    if (reader != null) return reader.read();
    else throw new IllegalStateException();
  }

  /**
   * Get the next clause delimiter
   *
   * @return Phrase delimiter code
   * @throws IOException Read error
   */
  public int nextToken() throws IOException {

    byte ct[] = ctype;
    sval = null;

    int c = peekc;
    if (c < 0) c = NEED_CHAR;
    if (c == SKIP_LF) {
      c = read();
      if (c < 0) return ttype = LT_EOF;
      if (c == '\n') c = NEED_CHAR;
    }
    if (c == NEED_CHAR) {
      c = read();
      if (c < 0) return ttype = LT_EOF;
    }
    ttype = c; /* Just to be safe */

    /*
     * Set peekc so that the next invocation of nextToken will read another
     * character unless peekc is reset in this invocation
     */
    peekc = NEED_CHAR;

    // Get the read character settings.
    int ctype = c < 256 ? ct[c] : CT_ALPHA;

    // Check CT_WHITESPACE.
    // Delete the setting character (space).
    boolean existWhite = false;
    while ((ctype & CT_WHITESPACE) != 0) {
      existWhite = true;
      if (c == '\r') {
        LINENO++;
        if (eolIsSignificantP) {
          peekc = SKIP_LF;
          return ttype = LT_EOL;
        }
        c = read();
        if (c == '\n') c = read();
      } else {
        if (c == '\n') {
          LINENO++;
          if (eolIsSignificantP) {
            return ttype = LT_EOL;
          }
        }
        c = read();
      }
      if (c < 0) return ttype = LT_EOF;
      ctype = c < 256 ? ct[c] : CT_ALPHA;
    }

    // If WHITESPACE exists, insert one space.
    if (existWhite) {
      appendBuffer(' ');
    }

    // Check CT_ALPHA.
    // Treat as one character string
    if ((ctype & CT_ALPHA) != 0) {
      do {
        appendBuffer(c);
        c = read();
        ctype = c < 0 ? CT_WHITESPACE : c < 256 ? ct[c] : CT_ALPHA;
      } while ((ctype & (CT_ALPHA)) != 0);
      peekc = c;
      sval = buf.toString();
      buf.delete(0, buf.length());
      // End of string delimiter
      return ttype = LT_WORD;
    }

    // When parentheses are paired delimiters
    if (parenthesisQuote && c == '(') {
      appendBuffer(c);
      int paren = 0;
      int d = read();
      while (d >= 0 && d != '\n' && d != '\r') {
        ctype = d < 256 ? ct[d] : CT_ALPHA;
        if ((ctype & CT_QUOTE) != 0) {
          // Get the characters enclosed in quotes.
          String quote = getQuote(d);
          appendBuffer(quote);
        } else if (d == '\\') {
          // If a backslash is present, do not see the next character.
          appendBuffer(d);
          c = read();
          appendBuffer(c);
        } else {
          c = d;
          // Check the right parenthesis.
          if (d == ')') {
            // Exit when the number of left parentheses and the number of right parentheses match.
            if (paren <= 0) {
              break;
            }
            // Decrement the parenthesis count.
            paren--;
          }
          // Check the left parenthesis.
          if (d == '(') {
            // Since the left parenthesis appears again, increment the parenthesis count.
            paren++;
          }
          appendBuffer(c);
        }
        d = read();
      }
      appendBuffer(d);

      peekc = NEED_CHAR;
      sval = buf.toString();
      buf.delete(0, buf.length());
      return LT_PARENTHESIS;
    }

    // Check CT_QUOTE.
    if ((ctype & CT_QUOTE) != 0) {
      // Get the characters enclosed in quotes.
      String quote = getQuote(c);

      peekc = NEED_CHAR;
      sval = quote;
      buf.delete(0, buf.length());
      return LT_QUOTE;
    }

    // Check the delimiter string.
    String delim = null;
    if ((delim = isDelimiter(c)) != null) {
      sval = delim;
      peekc = NEED_CHAR;
      buf.delete(0, buf.length());
      return LT_DELIM;
    }

    if (c == '/' && (slashSlashCommentsP || slashStarCommentsP)) {
      c = read();
      if (c == '*' && slashStarCommentsP) {
        int prevc = 0;
        while ((c = read()) != '/' || prevc != '*') {
          if (c == '\r') {
            LINENO++;
            c = read();
            if (c == '\n') {
              c = read();
            }
          } else {
            if (c == '\n') {
              LINENO++;
              c = read();
            }
          }
          if (c < 0) return ttype = LT_EOF;
          prevc = c;
        }
        return nextToken();
      } else if (c == '/' && slashSlashCommentsP) {
        while ((c = read()) != '\n' && c != '\r' && c >= 0)
          ;
        peekc = c;
        return nextToken();
      } else {
        /* Now see if it is still a single line comment */
        if ((ct['/'] & CT_COMMENT) != 0) {
          while ((c = read()) != '\n' && c != '\r' && c >= 0)
            ;
          peekc = c;
          return nextToken();
        } else {
          peekc = c;
          return ttype = '/';
        }
      }
    }

    sval = String.valueOf((char) c);
    return ttype = c;
  }

  /**
   * Return the current line number.
   *
   * @return the current line number of this stream tokenizer.
   */
  public int lineno() {
    return LINENO;
  }

  @Override
  public String toString() {
    String ret;
    switch (ttype) {
      case LT_EOF:
        ret = "EOF";
        break;
      case LT_EOL:
        ret = "EOL";
        break;
      case LT_WORD:
        ret = sval;
        break;
      case LT_NOTHING:
        ret = "NOTHING";
        break;
      default:
        {
          /*
           * ttype is the first character of either a quoted string or is an
           * ordinary character. ttype can definitely not be less than 0,
           * since those are reserved values used in the previous case
           * statements
           */
          if (ttype < 256 && ((ctype[ttype] & CT_QUOTE) != 0)) {
            ret = sval;
            break;
          }

          char s[] = new char[3];
          s[0] = s[2] = '\'';
          s[1] = (char) ttype;
          ret = new String(s);
          break;
        }
    }
    return "Token[" + ret + "], line " + LINENO;
  }

  /**
   * Set the delimiter.
   *
   * @param delim Delimiter
   */
  public void useDelimiter(String delim) {
    // Clear the first character of the delimiter from the type.
    char ch = delim.charAt(0);
    ordinaryChar(ch);
    m_delimList.add(delim);
  }

  /**
   * Set the delimiter.
   *
   * @param delims Delimiter character list
   */
  public void useDelimiters(String delims) {
    for (int i = 0; i < delims.length(); i++) {
      String delim = delims.substring(i, i + 1);
      useDelimiter(delim);
    }
  }

  /**
   * Cancel the delimiter setting.
   *
   * @param delim Delimiter
   */
  public void unusedDelimiter(String delim) {
    // The first character of the delimiter is the WORD character.
    char ch = delim.charAt(0);
    wordChar(ch);
    m_delimList.remove(delim);
  }

  /**
   * Check if it matches the delimiter string.
   *
   * @param c Currently read characters
   * @return Returns the delimiter string if it matches the delimiter string. / Returns null if they
   *     do not match.
   */
  private String isDelimiter(int c) {

    try {
      String delim = null;
      Iterator<String> itr = m_delimList.iterator();
      while (itr.hasNext()) {
        delim = itr.next();
        char ch = delim.charAt(0);
        if (c == ch) {
          // Mark to look ahead.
          reader.mark(delim.length());
          boolean match = true;
          for (int i = 1; i < delim.length(); i++) {
            if (reader.read() != delim.charAt(i)) {
              match = false;
              break;
            }
          }
          if (!match) {
            // Return to the mark position.
            reader.reset();
            continue;
          } else {
            return delim;
          }
        }
      }
      return null;
    } catch (IOException e) {
      e.printStackTrace();
      return null;
    }
  }

  /**
   * Set parentheses'(',')' as pair delimiters.
   *
   * @param paren true: Use parentheses'(',')' as pair delimiters.
   */
  public void setParenthesisQuote(boolean paren) {
    parenthesisQuote = paren;
    if (paren) {
      ctype['('] = 0;
      ctype[')'] = 0;
    } else {
      ctype['('] = CT_ALPHA;
      ctype[')'] = CT_ALPHA;
    }
  }

  /**
   * Get the quote string.
   *
   * @param c Character code
   * @return quote string
   */
  private String getQuote(int c) throws IOException {

    // Get the read character settings.
    byte ct[] = ctype;
    int ctype = c < 256 ? ct[c] : CT_ALPHA;
    if ((ctype & CT_QUOTE) == 0) return null;

    // Quote storage buffer
    StringBuffer quote_buf = new StringBuffer();
    appendBuffer(quote_buf, c);

    // Quote character
    int quote_type = c;

    // Get the next character
    int d = read();
    // Next, get the quotation marks and newline characters.
    while (d >= 0 && d != quote_type && d != '\n' && d != '\r') {
      if (d == '\\') {
        // If a backslash is present, do not see the next character.
        appendBuffer(quote_buf, c);
        c = read();
        d = read();

      } else {
        c = d;
        d = read();
      }
      appendBuffer(quote_buf, c);
    }
    appendBuffer(quote_buf, d);

    return quote_buf.toString();
  }

  /**
   * Add code to the split buffer.
   *
   * @param c Additional character code
   */
  private void appendBuffer(int c) {
    appendBuffer(this.buf, c);
  }

  /**
   * Add code to the split buffer.
   *
   * @param c Additional character code
   */
  private void appendBuffer(StringBuffer add_buf, int c) {
    if (c <= 0) return;

    if (c != '\n' && c != '\r') {
      add_buf.appendCodePoint(c);
    }
  }

  /**
   * Add a string to the split buffer.
   *
   * @param str Additional string
   */
  private void appendBuffer(String str) {
    if (str == null || str.isEmpty()) return;
    buf.append(str);
  }
}
