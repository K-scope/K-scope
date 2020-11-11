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

package jp.riken.kscope.language;

import java.io.Serializable;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;

/**
 * A class that holds line information in source code.
 *
 * @author RIKEN
 */
public class Statement implements Serializable {
  /** Serial number */
  private static final long serialVersionUID = 6565980998601169008L;
  /** Code line label default value */
  public static final String NO_LABEL = "no_label";
  /** Code line label */
  private String label = NO_LABEL;

  /**
   * Code line information Contains source code line character strings, file start / end line
   * numbers, and source file information.
   */
  CodeLine lineInfo;

  /**
   * Constructor
   *
   * @param lineInfo Code line information
   */
  public Statement(CodeLine lineInfo) {
    this.lineInfo = lineInfo;
    label = NO_LABEL;
  }

  protected void set_label(String str) {
    label = str;
  }

  // ++++++++++++++++++++++++++++++++++++++++++++

  /**
   * Get the code line string.
   *
   * @return code line string
   */
  public String get_statement() {
    return this.lineInfo.getStatement();
  }

  /**
   * Get the code line start file line number.
   *
   * @return code line start file line number
   */
  public Integer get_pos() {
    return this.lineInfo.getStartLine();
  }

  /**
   * Get the code line start file line number.
   *
   * @return code line start file line number
   */
  public Integer get_start_pos() {
    return this.lineInfo.getStartLine();
  }

  /**
   * Get the code line end file line number.
   *
   * @return Code line end file line number
   */
  public Integer get_end_pos() {
    return this.lineInfo.getEndLine();
  }

  /**
   * Get the source file.
   *
   * @return source file
   */
  public SourceFile get_sourcefile() {
    return this.lineInfo.getSourceFile();
  }

  /**
   * Get row label
   *
   * @return line label
   */
  public String get_label() {
    return label;
  }

  /**
   * Check if the row label exists.
   *
   * @return true: with line label / false: without line label
   */
  protected boolean is_labeled() {
    if (label == null) return false;
    return (!label.equals(NO_LABEL));
  }

  /**
   * Get code line information.
   *
   * @return lineInfo Code line information
   */
  public CodeLine getLineInfo() {
    return this.lineInfo;
  }
}
