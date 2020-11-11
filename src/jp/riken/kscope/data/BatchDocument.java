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

import java.awt.Font;
import java.text.BreakIterator;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.swing.SwingUtilities;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Element;
import javax.swing.text.MutableAttributeSet;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import jp.riken.kscope.properties.KeywordProperties;
import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.utils.StringUtils;

/**
 * Styled text model
 *
 * @author RIKEN
 */
public class BatchDocument extends DefaultStyledDocument {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** End-of-line character */
  private static final char[] EOL_ARRAY = {'\n'};

  /** Line string list */
  private ArrayList<ElementSpec> batch = null;

  private SimpleAttributeSet defualtAttributeSet = new SimpleAttributeSet();

  /** Constructor */
  public BatchDocument() {
    batch = new ArrayList<ElementSpec>();
  }

  /**
   * Add a string.
   *
   * @param str Additional string
   * @param attr Style information
   */
  public void appendBatchString(String str, AttributeSet attr) {
    attr = attr.copyAttributes();
    char[] chars = str.toCharArray();
    // batch.add(new ElementSpec(attr, ElementSpec.ContentType, chars, 0, str.length()));
    addBachList(attr, ElementSpec.ContentType, chars, str.length());
  }

  /**
   * Add a line.
   *
   * @param str Additional line
   * @param attr Style information
   */
  public void appendBatchLineString(String str, AttributeSet attr) {
    appendBatchString(str, attr);
    appendBatchLineFeed(attr);
  }

  /**
   * Add a line ending string.
   *
   * @param attr Style information
   */
  public void appendBatchLineFeed(AttributeSet attr) {
    // Add a line ending character.
    // batch.add(new ElementSpec(attr, ElementSpec.ContentType, EOL_ARRAY, 0, 1));
    addBachList(attr, ElementSpec.ContentType, EOL_ARRAY, 1);

    Element paragraph = getParagraphElement(0);
    AttributeSet pattr = paragraph.getAttributes();
    // batch.add(new ElementSpec(null, ElementSpec.EndTagType));
    // batch.add(new ElementSpec(pattr, ElementSpec.StartTagType));
    addBachList(null, ElementSpec.EndTagType, null, 0);
    addBachList(pattr, ElementSpec.StartTagType, null, 0);
  }

  /**
   * Add the added elements (string + style) to DefaultStyledDocument at once.
   *
   * @param offs Additional offset
   * @throws BadLocationException Bad location error in document model
   */
  public void processBatchUpdates(int offs) throws BadLocationException {
    // Convert the added element list to an array.
    ElementSpec[] inserts = new ElementSpec[batch.size()];
    batch.toArray(inserts);

    // Add an array of element lists
    super.insert(offs, inserts);
  }

  /**
   * Add to document element list
   *
   * @param attr Style attribute
   * @param type type
   * @param txt Document text
   * @param len Text length
   */
  private void addBachList(AttributeSet attr, short type, char[] txt, int len) {
    if (len > 0) {
      batch.add(new ElementSpec(attr, type, txt, 0, len));
    } else {
      batch.add(new ElementSpec(attr, type));
    }
  }

  /**
   * Clear keyword highlighting
   *
   * @param startline Highlight start line index
   * @param endline Highlight end row index
   */
  public void clearKeywordAttributes(final int startline, final int endline) {

    SwingUtilities.invokeLater(
        new Runnable() {
          @Override
          public void run() {
            // Start and end caret positions
            Element root = getDefaultRootElement();
            int startOffset = root.getElement(startline - 1).getStartOffset();
            int endOffset = root.getElement(endline - 1).getEndOffset();
            int lineLength = endOffset - startOffset;

            // Clear the set style attribute.
            setCharacterAttributes(startOffset, lineLength, defualtAttributeSet, true);
          }
        });
  }

  /**
   * Highlight keywords
   *
   * @param properties Keyword setting
   * @param startline Highlight start line index
   * @param endline Highlight end row index
   */
  public void applyHighlighting(
      final KeywordProperties properties, final int startline, final int endline) {
    if (properties == null) return;

    // Start and end caret positions
    Element root = getDefaultRootElement();
    int startOffset = root.getElement(startline - 1).getStartOffset();
    int endOffset = root.getElement(endline - 1).getEndOffset();

    int count = properties.getKeywordCount();
    for (int i = 0; i < count; i++) {
      try {
        Keyword keyword = properties.getKeyword(i);
        if (!keyword.isEnabled()) continue;
        // Perform a keyword search.
        applyKeyword(keyword, startOffset, endOffset);

      } catch (Exception e) {
        e.printStackTrace();
      }
    }
  }

  /**
   * Search for the start position of a word
   *
   * @param properties Keyword setting information
   * @param startOffset Start document caret position
   * @param endOffset End document caret position
   * @throws Exception
   */
  @SuppressWarnings("unused")
  private void checkForTokens(KeywordProperties properties, int startOffset, int endOffset)
      throws Exception {
    int length = endOffset - startOffset + 1;
    String content = this.getText(startOffset, length);
    int startElement = startOffset;
    int endElement = endOffset;
    int start = 0;
    int end = endOffset - startOffset;
    // Search for words
    while (start <= end) {
      // Search for delimiters
      while (isDelimiter(content.substring(start, start + 1))) {
        if (start < end) {
          start++;
        } else {
          break;
        }
      }
      start = getOtherToken(properties, content, start, end, startElement);
    }

    // Regular expression search
    int count = properties.getKeywordCount();
    for (int i = 0; i < count; i++) {
      Keyword keyword = properties.getKeyword(i);
      if (!keyword.isEnabled()) continue;
      if (!keyword.isRegex()) continue;
      if (keyword.getKeyword() == null || keyword.getKeyword().isEmpty()) continue;
      String regex = keyword.getKeyword();
      int flags = keyword.isSensitivecase() ? 0 : Pattern.CASE_INSENSITIVE;
      flags += Pattern.MULTILINE;

      // Create style attributes from keyword information.
      MutableAttributeSet attr = createStyleAttributeSet(keyword);

      // Regular expression search
      Matcher m = Pattern.compile(regex, flags).matcher(content);
      while (m.find()) {
        if (m.groupCount() == 0) {
          // Apply style
          setCharacterAttributes(m.start() + startElement, m.end() - m.start(), attr, false);
        } else if (m.groupCount() >= 1) {
          // If grouped, exclude the first (whole).
          for (int j = 1; j <= m.groupCount(); j++) {
            // Apply style
            setCharacterAttributes(m.start(j) + startElement, m.end(j) - m.start(j), attr, false);
          }
        }
      }
    }
  }

  /**
   * Keywords
   *
   * @param keyword keyword setting
   * @param startOffset Start offset value
   * @param endOffset End offset value
   * @throws Exception Keyword setting error
   */
  public void applyKeyword(final Keyword keyword, final int startOffset, final int endOffset)
      throws Exception {

    SwingUtilities.invokeLater(
        new Runnable() {
          @Override
          public void run() {
            try {
              // Start and end caret positions
              int length = endOffset - startOffset;
              String content = BatchDocument.this.getText(startOffset, length);

              if (!keyword.isEnabled()) return;
              if (keyword.getKeyword() == null || keyword.getKeyword().isEmpty()) return;

              // Create style attributes from keyword information.
              MutableAttributeSet attr = createStyleAttributeSet(keyword);

              if (keyword.isRegex()) {
                // Regular expressions
                setRegexAttributes(
                    content, keyword.getKeyword(), startOffset, attr, keyword.isSensitivecase());
              } else if (keyword.isSearchVariable()) {
                // Variable / trace search
                setVariableAttributes(
                    content, keyword.getKeyword(), startOffset, attr, keyword.isSensitivecase());
              } else {
                //                        System.out.println("keyword=" + keyword.getKeyword() + ":
                // word=" + keyword.isSearchWord()
                //                                           + ": bold=" + keyword.getStyle() +
                // "color=" + keyword.getForecolor());
                // String search
                setFindAttributes(
                    content,
                    keyword.getKeyword(),
                    startOffset,
                    attr,
                    keyword.isSensitivecase(),
                    keyword.isSearchWord());
              }
            } catch (Exception ex) {
              ex.printStackTrace();
            }
          }
        });
  }

  /**
   * Set the highlight for the matching string by regular expression.
   *
   * @param content Search target string
   * @param regex regular expression
   * @param startOffset Start offset value
   * @param attr Applicable highlight settings
   * @param sensitivecase true = Case sensitive.
   */
  private void setRegexAttributes(
      String content,
      String regex,
      int startOffset,
      MutableAttributeSet attr,
      boolean sensitivecase) {
    if (content == null || content.isEmpty()) return;
    if (regex == null || regex.isEmpty()) return;
    if (attr == null) return;

    int flags = sensitivecase ? 0 : Pattern.CASE_INSENSITIVE;
    flags += Pattern.MULTILINE;

    // Regular expression search
    Matcher m = Pattern.compile(regex, flags).matcher(content);
    while (m.find()) {
      if (m.groupCount() == 0) {
        // Apply style
        setCharacterAttributes(m.start() + startOffset, m.end() - m.start(), attr, true);
      } else if (m.groupCount() >= 1) {
        // If grouped, exclude the first (whole).
        for (int j = 1; j <= m.groupCount(); j++) {
          // Apply style
          setCharacterAttributes(m.start(j) + startOffset, m.end(j) - m.start(j), attr, true);
        }
      }
    }
  }

  /**
   * Set the highlight for the matching character string by character search.
   *
   * @param content Search target string
   * @param find search string
   * @param startOffset Start offset value
   * @param attr Applicable highlight settings
   * @param sensitivecase true = Case sensitive.
   * @param word true = word search
   */
  private void setFindAttributes(
      String content,
      String find,
      int startOffset,
      MutableAttributeSet attr,
      boolean sensitivecase,
      boolean word) {
    if (content == null || content.isEmpty()) return;
    if (find == null || find.isEmpty()) return;
    if (attr == null) return;

    String contentText = content;
    String findText = find;
    if (!sensitivecase) {
      // It is not case sensitive, so convert it to all lowercase
      contentText = contentText.toLowerCase();
      findText = findText.toLowerCase();
    }
    // Perform a string search
    int fromIndex = 0;
    int start = -1;
    while ((start = contentText.indexOf(findText, fromIndex)) != -1) {
      int end = start + findText.length();
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

      // Apply style
      setCharacterAttributes(start + startOffset, findText.length(), attr, true);
    }
  }

  /**
   * Set the highlight for the matching character string by variable / trace search.
   *
   * @param content Search target string
   * @param name Variable name
   * @param startOffset Start offset value
   * @param attr Applicable highlight settings
   * @param sensitivecase true = Case sensitive.
   */
  private void setVariableAttributes(
      String content,
      String name,
      int startOffset,
      MutableAttributeSet attr,
      boolean sensitivecase) {
    if (content == null || content.isEmpty()) return;
    if (name == null || name.isEmpty()) return;
    if (attr == null) return;

    // Decompose the string with the delimiter
    String[] unuseddelimiters = {"%"};
    // If the variable name contains%, exclude% from the delimiter.
    if (name.indexOf("%") < 0) {
      unuseddelimiters = null;
    }
    List<String> list = StringUtils.tokenizer(content, unuseddelimiters);
    if (list == null || list.size() <= 0) return;

    int start = 0;
    for (String word : list) {
      if (word == null) {
        // \ r and \ n are set as null.
        start += 1;
        continue;
      }
      int len = word.length();
      if (len == 0) continue;
      if (word.trim().isEmpty()) {
        start += len;
        continue;
      }

      // Comment check
      if ("!".equals(word)) {
        // Since it is a comment until the end of the line, it does not apply after that
        break;
      }

      boolean match = false;
      if (!sensitivecase) {
        // uppercase letter. Insensitive to lowercase
        match = word.equalsIgnoreCase(name);
      } else {
        // uppercase letter. Case sensitive
        match = word.equals(name);
      }

      if (match) {
        // Apply style
        setCharacterAttributes(start + startOffset, len, attr, true);
      }

      // Start index
      start += len;
    }
  }

  /**
   * Set keywords from the start position of words Search for words
   *
   * @param properties Keyword setting information
   * @param content Search string
   * @param start Search string start position
   * @param end Search string end position
   * @param startElement Start document caret position
   * @return Next search string start position
   */
  private int getOtherToken(
      KeywordProperties properties, String content, int start, int end, int startElement) {
    int endOfToken = start + 1;
    while (endOfToken <= end) {
      if (isDelimiter(content.substring(endOfToken, endOfToken + 1))) {
        break;
      }
      endOfToken++;
    }
    String token = content.substring(start, endOfToken);
    Keyword keyword = properties.getKeyword(token);

    // Keyword is valid AND not a regular expression
    if (keyword != null && keyword.isEnabled() && !keyword.isRegex()) {
      // Create style attributes from keyword information.
      MutableAttributeSet attr = createStyleAttributeSet(keyword);
      // Apply style
      setCharacterAttributes(start + startElement, endOfToken - start, attr, false);
    }
    return endOfToken + 1;
  }

  /**
   * Check if the character is a delimiter.
   *
   * @param character Check character
   * @return true = Delimiter
   */
  private boolean isDelimiter(String character) {
    if (character == null || character.isEmpty()) {
      return false;
    }
    return Character.isWhitespace(character.charAt(0))
        || KscopeProperties.DELIMITER_CHARS.indexOf(character) != -1;
  }

  /**
   * Create style attributes from keyword information.
   *
   * @param keyword Keyword information
   * @return style attribute
   */
  private MutableAttributeSet createStyleAttributeSet(Keyword keyword) {
    if (keyword == null) return null;
    MutableAttributeSet attr = new SimpleAttributeSet();
    if (keyword.getForecolor() != null) {
      StyleConstants.setForeground(attr, keyword.getForecolor());
    }
    if (keyword.getBackgroundcolor() != null) {
      StyleConstants.setBackground(attr, keyword.getBackgroundcolor());
    }
    int style = keyword.getStyle();
    if ((style & Font.BOLD) != 0) {
      StyleConstants.setBold(attr, true);
    }
    if ((style & Font.ITALIC) != 0) {
      StyleConstants.setItalic(attr, true);
    }

    return attr;
  }

  /**
   * Get words in row and column positions
   *
   * @param rowIndex Caret row index
   * @param columnIndex Column index
   * @return Caret position word
   */
  public String getCaretWord(int rowIndex, int columnIndex) {

    try {
      // Get the string of the selected line
      Element root = getDefaultRootElement();
      int startOffset = root.getElement(rowIndex).getStartOffset();
      int endOffset = root.getElement(rowIndex).getEndOffset();
      String line = this.getText(startOffset, endOffset - startOffset);

      // BreakIterator
      BreakIterator boundary = BreakIterator.getWordInstance(Locale.ENGLISH);
      boundary.setText(line);
      int start = boundary.first();
      String word = null;
      for (int end = boundary.next();
          end != BreakIterator.DONE;
          start = end, end = boundary.next()) {

        if (start <= columnIndex && columnIndex <= end) {
          String str = line.substring(start, end);
          if (str != null && !str.trim().isEmpty()) {
            word = str.trim();
          }
        }
      }
      return word;

    } catch (BadLocationException ex) {
      ex.printStackTrace();
    }

    return null;
  }

  /**
   * Get the word for the caret position
   *
   * @param index Caret index
   * @return Caret position word
   */
  public String getCaretWord(int index) {

    // Get the string of the selected line
    Element root = getDefaultRootElement();

    /* Get the line number at the caret position */
    int rowIndex = root.getElementIndex(index);

    int startOffset = root.getElement(rowIndex).getStartOffset();
    int columnIndex = index - startOffset;

    return getCaretWord(rowIndex, columnIndex);
  }
}
