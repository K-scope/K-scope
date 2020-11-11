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
import java.util.List;
import jp.riken.kscope.common.KEYWORD_TYPE;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.utils.StringUtils;

/**
 * Source code variable memory data class
 *
 * @author RIKEN
 */
public class VariableMemory extends Keyword {

  /** Variables */
  private Variable variable;
  /** Access memory */
  private RequiredBF reqbf;

  /**
   * Constructor
   *
   * @param variable variable
   * @param reqbf Access memory
   */
  public VariableMemory(Variable variable, RequiredBF reqbf) {
    super(KEYWORD_TYPE.VARIABLE);
    this.variable = variable;
    this.reqbf = reqbf;
    // Set case sensitivity
    setCaseSensitive(false);
    // Set the regular expression
    setRegex(true);
    // Set up word search
    setSearchWord(false);
    // Set variable search
    setSearchVariable(true);
    // Variable regular expression
    String regex = createRegexPattern(variable);
    setKeyword(regex);
  }

  /**
   * Get variables.
   *
   * @return variable
   */
  public Variable getVariable() {
    return variable;
  }

  /**
   * Set variables
   *
   * @param variable variable
   */
  public void setVariable(Variable variable) {
    this.variable = variable;
  }

  /**
   * Convert the string representation of a variable to a regular expression. Add a blank regular
   * expression to the string representation of the variable.
   *
   * @param variable variable
   * @return Variable regular expression
   */
  private String createRegexPattern(Variable variable) {
    if (variable == null) return null;
    String statement = variable.getVariableString();
    List<String> list = StringUtils.tokenizer(statement);
    if (list == null || list.size() <= 0) return null;
    String buf = "";
    for (String word : list) {
      if (word.isEmpty()) continue;
      if (word.trim().isEmpty()) continue;
      if (!buf.isEmpty()) buf += "[ ]*";
      if (KscopeProperties.DELIMITER_CHARS.indexOf(word) >= 0) {
        buf += "\\" + word;
      } else {
        buf += word;
      }
    }
    return buf;
  }

  /** Get the background color of the variable */
  @Override
  public Color getBackgroundcolor() {
    if (reqbf == null) return null;
    return reqbf.getBackColor();
  }

  /** Get code line information for variables. */
  @Override
  public CodeLine getSearchLine() {
    if (this.variable == null) return null;
    IBlock parent = this.variable.getParentStatement();
    if (parent == null) return null;
    CodeLine start = parent.getStartCodeLine();
    CodeLine end = parent.getEndCodeLine();
    CodeLine line = new CodeLine(start, end);
    return line;
  }

  /**
   * Get access memory.
   *
   * @return Access memory
   */
  public RequiredBF getRequiredBF() {
    return reqbf;
  }

  /**
   * Set the access destination memory
   *
   * @param reqbf Access memory
   */
  public void setRequiredBF(RequiredBF reqbf) {
    this.reqbf = reqbf;
  }
}
