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

import java.util.List;

/**
 * Array element class
 *
 * @author RIKEN
 */
public class DefModelArraySubscriptSequence implements IXmlNode {

  /** Array element list */
  protected List<IXmlNode> indexRangeOrArrayIndex;

  /**
   * Constructor
   *
   * @param index Array element list
   */
  public DefModelArraySubscriptSequence(List<IXmlNode> index) {
    this.indexRangeOrArrayIndex = index;
  }

  /**
   * Get a list of array elements
   *
   * @return Array element list
   */
  public IXmlNode[] getIndexRangeOrArrayIndex() {
    IXmlNode[] array = new IXmlNode[indexRangeOrArrayIndex.size()];
    return ((IXmlNode[]) indexRangeOrArrayIndex.toArray(array));
  }

  /**
   * Start parsing array elements
   *
   * @param visitor Parser Visitor
   * @return Success or failure
   */
  @Override
  public boolean enter(IXmlVisitor visitor) {
    return (visitor.enter(this));
  }

  /**
   * Exit parsing of array elements
   *
   * @param visitor Parser Visitor
   */
  @Override
  public void leave(IXmlVisitor visitor) {
    visitor.leave(this);
  }
}
