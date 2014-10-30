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
 * VariableDimensionパーサクラス
 * @author RIKEN
 */
public class VariableDimensionParser {

    /** typeTable */
    private XcodeMLTypeManager typeManager;

    /**
     * コンストラクタ
     * @param typeManager    typeTable
     */
    public VariableDimensionParser(XcodeMLTypeManager typeManager) {
        this.typeManager = typeManager;
    }

    /**
     * 配列定義要素からDB::VariableDimensionクラスをパース、生成する。
     *
     * @param arrayNodes          配列定義
     * @return DB::VariableDimensionクラス
     */
    public VariableDimension parseVariableDimension(List<IXmlNode> arrayNodes) {

        if (arrayNodes == null || arrayNodes.size() <= 0) {
            return null;
        }
        // ExprModelパーサ;
        ExpressionParser exprParser = new ExpressionParser(this.typeManager);

        // 配列宣言
        ArrayList<DimensionIndex> dim_list = new ArrayList<DimensionIndex>();
        for (IXmlNode elem : arrayNodes) {
            if (elem instanceof ArrayIndex) {
                // 配列サイズのみ
                exprParser.setParseNode(elem);
                Expression expr = exprParser.getExpression();
                if (expr != null) {
                    // 配列下限は１とする。
                    DimensionIndex idx = new DimensionIndex(new Expression("1"), expr);
                    dim_list.add(idx);
                }
            } else if (elem instanceof IndexRange) {
                // 配列下限
                exprParser.setParseNode(((IndexRange) elem).getLowerBound());
                Expression exprLower = exprParser.getExpression();
                if (exprLower == null) {
                    // 配列サイズなし
                    exprLower = new Expression("");
                }
                // 配列上限
                exprParser.setParseNode(((IndexRange) elem).getUpperBound());
                Expression exprUpper = exprParser.getExpression();
                if (exprUpper == null) {
                    // 配列サイズなし
                    exprUpper = new Expression("");
                }
                DimensionIndex idx = new DimensionIndex(exprLower, exprUpper);
                dim_list.add(idx);
            }
        }
        if (dim_list.size() <= 0) return null;

        // 配列宣言
        VariableDimension dims = new VariableDimension(dim_list.toArray(new DimensionIndex[0]));
        return dims;

    }
}
