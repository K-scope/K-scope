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

import java.awt.Color;
import jp.riken.kscope.common.KEYWORD_TYPE;

/**
 * Source code keyword data class
 *
 * @author RIKEN
 */
public class Keyword {

  /** Keyword name */
  private String name;
  /** Keywords */
  private String keyword;
  /** Class mode (special mode that cannot be expressed by regular expression: unused) */
  private String classmode;
  /** Letter color */
  private Color forecolor;
  /** Background color */
  private Color backgroundcolor;
  /** Style (Font.PLAIN, Font.BOLD, Font.ITALIC) */
  private int style;
  /** Effectiveness */
  private boolean enabled = true;
  /** Case sensitive (true = case sensitive) */
  private boolean sensitivecase = false;
  /** Regular expressions */
  private boolean regex = false;
  /** Word search: Search in quotes and comments */
  private boolean searchWord = true;
  /** Variable search (= trace): Excludes search in quotes and comments */
  private boolean searchVariable;
  /** Keywords cannot be changed */
  private boolean keywordlock = false;
  /** Search target code line */
  private CodeLine searchLine;
  /** Keyword type: reserved word (default), text search, trace */
  private KEYWORD_TYPE type = KEYWORD_TYPE.KEYWORD;

  /**
   * Constructor
   *
   * @param type Keyword type
   */
  public Keyword(KEYWORD_TYPE type) {
    this.type = type;
  }

  /**
   * Get keyword name
   *
   * @return keyword name
   */
  public String getName() {
    return name;
  }

  /**
   * Set the keyword name
   *
   * @param name Keyword name
   */
  public void setName(String name) {
    this.name = name;
  }

  /**
   * Get keywords
   *
   * @return keyword
   */
  public String getKeyword() {
    return keyword;
  }

  /**
   * Set keywords
   *
   * @param keyword keyword
   */
  public void setKeyword(String keyword) {
    this.keyword = keyword;
  }

  /**
   * Get mode
   *
   * @return mode
   */
  public String getClassmode() {
    return this.classmode;
  }

  /**
   * Set the mode
   *
   * @param mode mode
   */
  public void setClassmode(String mode) {
    this.classmode = mode;
  }

  /**
   * Get highlight color
   *
   * @return highlight color
   */
  public Color getForecolor() {
    return forecolor;
  }

  /**
   * Set the highlight color
   *
   * @param forecolor highlight color
   */
  public void setForecolor(Color forecolor) {
    this.forecolor = forecolor;
  }

  /**
   * Get style
   *
   * @return style
   */
  public int getStyle() {
    return style;
  }

  /**
   * Set style
   *
   * @param style style
   */
  public void setStyle(int style) {
    this.style = style;
  }

  /**
   * Set keywords
   *
   * @param name Keyword name
   * @param keyword keyword
   * @param forecolor highlight color
   * @param style style
   */
  public void setKeyword(String name, String keyword, Color forecolor, int style) {
    this.name = name;
    this.keyword = keyword;
    this.forecolor = forecolor;
    this.style = style;
  }

  /**
   * Set the mode
   *
   * @param classmode mode
   * @param keyword keyword
   * @param forecolor highlight color
   * @param style style
   */
  public void setMode(String classmode, String keyword, Color forecolor, int style) {
    this.classmode = classmode;
    this.keyword = keyword;
    this.forecolor = forecolor;
    this.style = style;
  }

  /**
   * Get keyword valid / invalid
   *
   * @return enabled true = enabled
   */
  public boolean isEnabled() {
    return enabled;
  }

  /**
   * Enable / disable keywords
   *
   * @param enabled true = enabled
   */
  public void setEnabled(boolean enabled) {
    this.enabled = enabled;
  }

  /**
   * Get case sensitive
   *
   * @return Case sensitive (true = case sensitive)
   */
  public boolean isSensitivecase() {
    return sensitivecase;
  }

  /**
   * Set case sensitivity
   *
   * @param sensitivecase Case sensitive (true = case sensitive)
   */
  public void setCaseSensitive(boolean sensitivecase) {
    this.sensitivecase = sensitivecase;
  }

  /**
   * Get a regular expression
   *
   * @return regular expression
   */
  public boolean isRegex() {
    return regex;
  }

  /**
   * Set regular expression
   *
   * @param regex regular expression
   */
  public void setRegex(boolean regex) {
    this.regex = regex;
  }

  /**
   * Get keyword changeable
   *
   * @return Keyword cannot be changed
   */
  public boolean isKeywordlock() {
    return keywordlock;
  }

  /**
   * Set keyword change not possible
   *
   * @param keywordlock Keyword cannot be changed
   */
  public void setKeywordlock(boolean keywordlock) {
    this.keywordlock = keywordlock;
  }

  /**
   * Get the background color.
   *
   * @return backgroundcolor background color
   */
  public Color getBackgroundcolor() {
    return backgroundcolor;
  }

  /**
   * Set the background color
   *
   * @param backgroundcolor Background color
   */
  public void setBackgroundcolor(Color backgroundcolor) {
    this.backgroundcolor = backgroundcolor;
  }

  /**
   * Get a word search. <br>
   * Search in quotes and comments
   *
   * @return true = word search
   */
  public boolean isSearchWord() {
    return searchWord;
  }

  /**
   * Set up word search. <br>
   * Search in quotes and comments
   *
   * @param searchWord true = word search
   */
  public void setSearchWord(boolean searchWord) {
    this.searchWord = searchWord;
  }

  /**
   * Get variable search (= trace). <br>
   * Exclude search in quotes and comments
   *
   * @return true = variable search (= trace)
   */
  public boolean isSearchVariable() {
    return searchVariable;
  }

  /**
   * Set variable search (= trace). <br>
   * Exclude search in quotes and comments
   *
   * @param searchVariable true = Variable search (= trace)
   */
  public void setSearchVariable(boolean searchVariable) {
    this.searchVariable = searchVariable;
  }

  /**
   * Get the line of code to search
   *
   * @return Search target code line
   */
  public CodeLine getSearchLine() {
    return searchLine;
  }

  /**
   * Set the line of code to be searched
   *
   * @param searchLine Search target code line
   */
  public void setSearchLine(CodeLine searchLine) {
    this.searchLine = searchLine;
  }

  /**
   * Get keyword type
   *
   * @return type Keyword type
   */
  public KEYWORD_TYPE getType() {
    return type;
  }

  /**
   * Set keyword type
   *
   * @param type Keyword type
   */
  public void setType(KEYWORD_TYPE type) {
    this.type = type;
  }
}
