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

import jp.riken.kscope.xcodeml.xml.gen.ValueList;
import jp.riken.kscope.xcodeml.xml.gen.VarList;

/**
 * FdataDecl element (DATA statement) class
 *
 * @author RIKEN
 */
public class FdataDeclSequence implements IXmlNode {

  /** Initialization item list */
  protected VarList varList;
  /** Initial value item list */
  protected ValueList valueList;

  /**
   * Constructor
   *
   * @param varList Initialization item list
   * @param valueList Initial value item list
   */
  public FdataDeclSequence(VarList varList, ValueList valueList) {
    this.varList = varList;
    this.valueList = valueList;
  }

  /**
   * Get the initialization item list
   *
   * @return Initialization item list
   */
  public VarList getVarList() {
    return varList;
  }

  /**
   * Set the initialization item list
   *
   * @param varList Initialization item list
   */
  public void setVarList(VarList varList) {
    this.varList = varList;
  }

  /**
   * Get the initial value item list
   *
   * @return Initial value item list
   */
  public ValueList getValueList() {
    return valueList;
  }

  /**
   * Get the initial value item list
   *
   * @param valueList Initial value item list
   */
  public void setValueList(ValueList valueList) {
    this.valueList = valueList;
  }

  /**
   * Start searching for FdataDecl element (DATA statement)
   *
   * @param visitor Xcode ML node search
   * @return Success or failure
   */
  @Override
  public boolean enter(IXmlVisitor visitor) {
    return (visitor.enter(this));
  }

  /**
   * End the search for the FdataDecl element (DATA statement)
   *
   * @param visitor Xcode ML node search
   */
  @Override
  public void leave(IXmlVisitor visitor) {
    visitor.leave(this);
  }
}
