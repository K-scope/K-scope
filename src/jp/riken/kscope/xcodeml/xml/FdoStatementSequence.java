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

package jp.riken.kscope.xcodeml.xml;

import jp.riken.kscope.xcodeml.xml.gen.*;

/**
 * FdoStatement element (DO statement) class
 *
 * @author RIKEN
 */
public class FdoStatementSequence implements IXmlNode {
  /** DO variable */
  protected Var var;
  /** DO variable value range */
  protected IndexRange indexRange;

  /**
   * Constructor
   *
   * @param var DO variable
   * @param index DO variable value range
   */
  public FdoStatementSequence(Var var, IndexRange index) {
    this.var = var;
    indexRange = index;
  }

  /**
   * Get the DO variable
   *
   * @return DO variable
   */
  public Var getVar() {
    return var;
  }

  /**
   * Set the DO variable
   *
   * @param var DO variable
   */
  public void setVar(Var var) {
    this.var = var;
  }

  /**
   * Get the value range of a DO variable
   *
   * @return DO variable value range
   */
  public IndexRange getIndexRange() {
    return indexRange;
  }

  /**
   * Set the value range of the DO variable
   *
   * @param indexRange DO variable value range
   */
  public void setIndexRange(IndexRange indexRange) {
    this.indexRange = indexRange;
  }

  /**
   * Start searching for FdoStatement element (DO statement)
   *
   * @param visitor Xcode ML node search
   * @return Success or failure
   */
  @Override
  public boolean enter(IXmlVisitor visitor) {
    return (visitor.enter(this));
  }

  /**
   * End the search for the Fdo Statement element (DO statement)
   *
   * @param visitor Xcode ML node search
   */
  @Override
  public void leave(IXmlVisitor visitor) {
    visitor.leave(this);
  }
}
