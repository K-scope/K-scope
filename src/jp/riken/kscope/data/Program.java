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

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;

/**
 * Extension setting data class
 *
 * @author RIKEN
 */
public class Program {

  /** Setting name */
  private String name;
  /**
   * Extension or regular expression <br>
   * For extensions, describe only the extension separated by commas.
   */
  private String pattern;
  /** Regular expressions */
  private boolean regex = false;
  /** extension */
  private boolean exts = false;
  /** External program */
  private String exename;
  /** Association program */
  private boolean relation;
  /** Options */
  private String option;

  /**
   * Get the setting name
   *
   * @return Setting name
   */
  public String getName() {
    return name;
  }

  /**
   * Set the setting name
   *
   * @param name Setting name
   */
  public void setName(String name) {
    this.name = name;
  }

  /**
   * Get the pattern
   *
   * @return pattern
   */
  public String getPattern() {
    return pattern;
  }

  /**
   * Set the pattern
   *
   * @param pattern pattern
   */
  public void setPattern(String pattern) {
    this.pattern = pattern;
  }

  /**
   * Get if the pattern is a regular expression
   *
   * @return true = regular expression
   */
  public boolean isRegex() {
    return regex;
  }

  /**
   * Set the pattern as a regular expression.
   *
   * @param regex true = regular expression
   */
  public void setRegex(boolean regex) {
    this.regex = regex;
  }

  /**
   * Get if the pattern is an extension
   *
   * @return true = extension
   */
  public boolean isExts() {
    return exts;
  }

  /**
   * Set the pattern as an extension
   *
   * @param exts true = extension
   */
  public void setExts(boolean exts) {
    this.exts = exts;
  }

  /**
   * Set the program path
   *
   * @return Program path
   */
  public String getExename() {
    return exename;
  }

  /**
   * Set the program path
   *
   * @param path Program path
   */
  public void setExename(String path) {
    this.exename = path;
  }

  /**
   * Make or get an association program
   *
   * @return true = Association program
   */
  public boolean isRelation() {
    return relation;
  }

  /**
   * Set as an association program
   *
   * @param relation Association program
   */
  public void setRelation(boolean relation) {
    this.relation = relation;
  }

  /**
   * Get the number of extensions set in the pattern
   *
   * @return Number of extensions
   */
  public int getPatternExtsCount() {
    String[] exts = getPatternExts();
    if (exts == null || exts.length <= 0) return 0;
    return exts.length;
  }

  /**
   * Get the extension set for the pattern
   *
   * @param index index
   * @return extension
   */
  public String getPatternExt(int index) {
    String[] exts = getPatternExts();
    if (exts == null || exts.length <= 0) return null;
    if (exts.length <= index) return null;

    return exts[index];
  }

  /**
   * Get the extension list set for the pattern
   *
   * @return Extension list
   */
  public String[] getPatternExts() {
    if (this.pattern == null || this.pattern.isEmpty()) return null;

    // Comma delimited decomposition
    String[] exts = this.pattern.split(",", 0);
    if (exts == null || exts.length <= 0) return null;

    List<String> list = new ArrayList<String>();
    for (int i = 0; i < exts.length; i++) {
      if (exts[i] == null) continue;
      exts[i] = exts[i].trim();
      if (exts[i].isEmpty()) continue;
      list.add(exts[i]);
    }

    return list.toArray(new String[0]);
  }

  /**
   * Get options
   *
   * @return option option
   */
  public String getOption() {
    return this.option;
  }

  /**
   * Set options
   *
   * @param value option
   */
  public void setOption(String value) {
    this.option = value;
  }

  /**
   * Check if the program matches the search character
   *
   * @param text Search character
   * @return true = Matching program
   */
  public boolean isMatchProgram(String text) {
    if (text == null || text.isEmpty()) return false;

    // Regular expressions
    if (this.isRegex()) {
      String pattern = this.getPattern();
      return isMatchRegex(text, pattern);
    } else {
      // extension
      int count = this.getPatternExtsCount();
      for (int i = 0; i < count; i++) {
        String ext = this.getPatternExt(i);
        // Search for extension files Regular expression
        String pattern = "^.+\\." + ext + "$";
        if (isMatchRegex(text, pattern)) {
          return true;
        }
      }
    }
    return false;
  }

  /**
   * Check if the search string matches with a regular expression
   *
   * @param text Search string
   * @param regexPattern Regular expression pattern
   * @return true = match
   */
  private boolean isMatchRegex(String text, String regexPattern) {
    if (text == null || text.isEmpty()) return false;
    if (regexPattern == null || regexPattern.isEmpty()) return false;

    // Regular expression matching
    java.util.regex.Pattern pattern =
        java.util.regex.Pattern.compile(
            regexPattern,
            java.util.regex.Pattern.CASE_INSENSITIVE + java.util.regex.Pattern.MULTILINE);
    Matcher m = pattern.matcher(text);
    return m.find();
  }
}
