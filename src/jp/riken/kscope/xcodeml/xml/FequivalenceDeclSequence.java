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

import jp.riken.kscope.xcodeml.xml.gen.VarList;
import jp.riken.kscope.xcodeml.xml.gen.VarRef;

/**
 * FequivalenceDecl element (EQUIVALENCE statement) class
 *
 * @author RIKEN
 */
public class FequivalenceDeclSequence implements IXmlNode {
  /** Combined entity */
  protected VarRef varRef;
  /** Combined entity arrangement */
  protected VarList varList;

  /**
   * Constructor
   *
   * @param varRef Join entity
   * @param varList Join entity list
   */
  public FequivalenceDeclSequence(VarRef varRef, VarList varList) {
    this.varRef = varRef;
    this.varList = varList;
  }

  /**
   * Get the combined entity
   *
   * @return Combined entity
   */
  public VarRef getVarRef() {
    return varRef;
  }

  /**
   * Set the combined entity
   *
   * @param varRef Join entity
   */
  public void setVarRef(VarRef varRef) {
    this.varRef = varRef;
  }

  /**
   * Get the combined entity list
   *
   * @return Combined entity list
   */
  public VarList getVarList() {
    return varList;
  }

  /**
   * Set the join entity list
   *
   * @param varList Join entity list
   */
  public void setVarList(VarList varList) {
    this.varList = varList;
  }

  /**
   * Start searching for FequivalenceDecl element (EQUIVALENCE statement)
   *
   * @param visitor Xcode ML node search
   * @return Success or failure
   */
  @Override
  public boolean enter(IXmlVisitor visitor) {
    return (visitor.enter(this));
  }

  /**
   * End the search for the FequivalenceDecl element (EQUIVALENCE statement)
   *
   * @param visitor Xcode ML node search
   */
  @Override
  public void leave(IXmlVisitor visitor) {
    visitor.leave(this);
  }
}
