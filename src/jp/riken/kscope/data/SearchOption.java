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

/**
 * Search condition class
 *
 * @author RIKEN
 */
public class SearchOption {

  /** Search string */
  private String searchText;
  /** Case sensitive (true = case sensitive) */
  private boolean sensitivecase;
  /** Regular expressions */
  private boolean regex;
  /** Word search */
  private boolean word;
  /** Variable search (= trace) */
  private boolean variable;
  /** Search node object class */
  private Class<?> searchClass;

  /**
   * Get the search string.
   *
   * @return Search string
   */
  public String getSearchText() {
    return searchText;
  }

  /**
   * Set the search string
   *
   * @param text Search string
   */
  public void setSearchText(String text) {
    this.searchText = text;
  }

  /**
   * Get case sensitive.
   *
   * @return true = case sensitive
   */
  public boolean isSensitivecase() {
    return sensitivecase;
  }

  /**
   * Set case sensitivity
   *
   * @param sensitivecase true = case sensitive
   */
  public void setSensitivecase(boolean sensitivecase) {
    this.sensitivecase = sensitivecase;
  }

  /**
   * Get a regular expression
   *
   * @return true = regular expression search
   */
  public boolean isRegex() {
    return regex;
  }

  /**
   * Set regular expression
   *
   * @param regex true = regular expression search
   */
  public void setRegex(boolean regex) {
    this.regex = regex;
  }

  /**
   * Get a word search.
   *
   * @return true = word search
   */
  public boolean isWord() {
    return word;
  }

  /**
   * Set up word search.
   *
   * @param word word search
   */
  public void setWord(boolean word) {
    this.word = word;
  }

  /**
   * Check if variable search (= trace)
   *
   * @return Variable search (= trace)
   */
  public boolean isVariable() {
    return this.variable;
  }

  /**
   * Set variable search (= trace).
   *
   * @param variable Variable search (= trace)
   */
  public void setVariable(boolean variable) {
    this.variable = variable;
  }

  /**
   * Get the object class of the search node
   *
   * @return Search node object class
   */
  public Class<?> getSearchClass() {
    return searchClass;
  }

  /**
   * Set the object class of the search node
   *
   * @param searchClass Object class of search node
   */
  public void setSearchClass(Class<?> searchClass) {
    this.searchClass = searchClass;
  }
}
