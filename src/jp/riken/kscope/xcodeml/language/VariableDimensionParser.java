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
package jp.riken.kscope.xcodeml.language;

import java.util.ArrayList;
import java.util.List;
import jp.riken.kscope.language.DimensionIndex;
import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.VariableDimension;
import jp.riken.kscope.xcodeml.XcodeMLTypeManager;
import jp.riken.kscope.xcodeml.xml.IXmlNode;
import jp.riken.kscope.xcodeml.xml.gen.ArrayIndex;
import jp.riken.kscope.xcodeml.xml.gen.IndexRange;

/**
 * VariableDimension parser class
 *
 * @author RIKEN
 */
public class VariableDimensionParser {

  /** typeTable */
  private XcodeMLTypeManager typeManager;

  /**
   * Constructor
   *
   * @param typeManager typeTable
   */
  public VariableDimensionParser(XcodeMLTypeManager typeManager) {
    this.typeManager = typeManager;
  }

  /**
   * Parse and generate DB :: VariableDimension class from the array definition element.
   *
   * @param arrayNodes Array definition
   * @return DB :: VariableDimension class
   */
  public VariableDimension parseVariableDimension(List<IXmlNode> arrayNodes) {

    if (arrayNodes == null || arrayNodes.size() <= 0) {
      return null;
    }
    // ExprModel parser;
    ExpressionParser exprParser = new ExpressionParser(this.typeManager);

    // Array declaration
    ArrayList<DimensionIndex> dim_list = new ArrayList<DimensionIndex>();
    for (IXmlNode elem : arrayNodes) {
      if (elem instanceof ArrayIndex) {
        // Array size only
        exprParser.setParseNode(elem);
        Expression expr = exprParser.getExpression();
        if (expr != null) {
          // The lower limit of the array is 1.
          DimensionIndex idx = new DimensionIndex(new Expression("1"), expr);
          dim_list.add(idx);
        }
      } else if (elem instanceof IndexRange) {
        // Array lower limit
        exprParser.setParseNode(((IndexRange) elem).getLowerBound());
        Expression exprLower = exprParser.getExpression();
        if (exprLower == null) {
          // No array size
          exprLower = new Expression("");
        }
        // Array upper limit
        exprParser.setParseNode(((IndexRange) elem).getUpperBound());
        Expression exprUpper = exprParser.getExpression();
        if (exprUpper == null) {
          // No array size
          exprUpper = new Expression("");
        }
        DimensionIndex idx = new DimensionIndex(exprLower, exprUpper);
        dim_list.add(idx);
      }
    }
    if (dim_list.size() <= 0) return null;

    // Array declaration
    VariableDimension dims = new VariableDimension(dim_list.toArray(new DimensionIndex[0]));
    return dims;
  }
}
