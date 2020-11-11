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

/**
 * A class that represents an array of variables.
 *
 * @author RIKEN
 */
public class DimensionIndex implements Serializable {
  /** Serial number */
  private static final long serialVersionUID = -4025983357574098164L;

  private Expression start;
  private Expression end;

  /**
   * Constructor
   *
   * @param st Starting index
   * @param en End index
   */
  public DimensionIndex(Expression st, Expression en) {
    this.start = st;
    this.end = en;
  }
  // ++++++++++++++++++++++++++++++++++++++++++++

  /**
   * Set the start of the subscript.
   *
   * @param iStart Start of subscript
   */
  public void set_start(Expression iStart) {
    start = iStart;
  }

  /**
   * Set the end of the subscript.
   *
   * @param iEnd End of subscript
   */
  public void set_end(Expression iEnd) {
    end = iEnd;
  }

  /**
   * Returns the start of the subscript.
   *
   * @return Start of subscript
   */
  public Expression get_start() {
    return (start);
  }

  /**
   * Returns the end of the subscript.
   *
   * @return End of subscript
   */
  public Expression get_end() {
    return (end);
  }
}
