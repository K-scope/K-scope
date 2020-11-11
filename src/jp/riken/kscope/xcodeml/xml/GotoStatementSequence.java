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

import jp.riken.kscope.xcodeml.xml.gen.Params;
import jp.riken.kscope.xcodeml.xml.gen.Value;

/**
 * GotoStatement element (GOTO statement) class
 *
 * @author RIKEN
 */
public class GotoStatementSequence implements IXmlNode {

  /** Sentence number sequence of calculated GOTO statement */
  protected Params params;
  /** Formula GOTO statement formula */
  protected Value value;

  /**
   * Constructor
   *
   * @param params Sentence number sequence of calculated GOTO statement
   * @param value Computational GOTO statement expression
   */
  public GotoStatementSequence(Params params, Value value) {
    this.params = params;
    this.value = value;
  }

  /**
   * Get the sentence number sequence of the calculated GOTO statement
   *
   * @return Statement number sequence of calculated GOTO statement
   */
  public Params getParams() {
    return params;
  }

  /**
   * Set the sentence number sequence of the calculated GOTO statement
   *
   * @param params Sentence number sequence of calculated GOTO statement
   */
  public void setParams(Params params) {
    this.params = params;
  }

  /**
   * Get the formula of the calculated GOTO statement
   *
   * @return Computational GOTO statement formula
   */
  public Value getValue() {
    return value;
  }

  /**
   * Set the formula of the calculated GOTO statement
   *
   * @param value Computational GOTO statement expression
   */
  public void setValue(Value value) {
    this.value = value;
  }

  /**
   * Start searching for GotoStatement element (GOTO statement)
   *
   * @param visitor Xcode ML node search
   * @return Success or failure
   */
  @Override
  public boolean enter(IXmlVisitor visitor) {
    return (visitor.enter(this));
  }

  /**
   * End the search for the GotoStatement element (GOTO statement)
   *
   * @param visitor Xcode ML node search
   */
  @Override
  public void leave(IXmlVisitor visitor) {
    visitor.leave(this);
  }
}
