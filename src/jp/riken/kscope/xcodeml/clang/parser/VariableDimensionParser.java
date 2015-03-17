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
package jp.riken.kscope.xcodeml.clang.parser;

import java.util.ArrayList;
import java.util.List;

import jp.riken.kscope.language.DimensionIndex;
import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.VariableDimension;
import jp.riken.kscope.xcodeml.clang.XcodeMLTypeManager;
import jp.riken.kscope.xcodeml.clang.xml.gen.ArrayType;

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
     * @param arrayNodes          配列定義リスト
     * @return DB::VariableDimensionクラス
     */
    public VariableDimension parseVariableDimension(List<ArrayType> arrayNodes) {

        if (arrayNodes == null || arrayNodes.size() <= 0) {
            return null;
        }
        // ExprModelパーサ;
        ExpressionParser exprParser = new ExpressionParser(this.typeManager);

        // 配列宣言
        ArrayList<DimensionIndex> dim_list = new ArrayList<DimensionIndex>();

        // 配列定義リストをelement_type->type順にソートする.
        this.sortArrayElements(arrayNodes);

        for (ArrayType elem : arrayNodes) {
            if (elem.getArraySize() != null) {
                // 式による配列定義
                exprParser.setParseNode(elem.getArraySize());
                Expression expr = exprParser.getExpression();
                if (expr != null) {
                    // 配列下限は0とする。
                    DimensionIndex idx = new DimensionIndex(new Expression("0"), expr);
                    dim_list.add(idx);
                }
            }
            else {
                // array_size属性による整数値
                String array_size = elem.getArraySizeAttribute();
                // 配列下限は0とする。
                DimensionIndex idx = new DimensionIndex(new Expression("0"), new Expression(array_size));
                dim_list.add(idx);
            }
        }
        if (dim_list.size() <= 0) return null;

        // 配列宣言
        VariableDimension dims = new VariableDimension(dim_list.toArray(new DimensionIndex[0]));

        return dims;
    }


    /**
     * 配列定義要素をtype,element_type属性値でソートする
     * element_type->typeにて関連付く様にソートする。
     * @param array_types          配列定義リスト
     * @return         ソート結果配列定義リスト
     */
    private List<ArrayType> sortArrayElements(List<ArrayType> array_types) {
        int i, j;
        for (i=0; i<array_types.size(); i++) {
            ArrayType array_src = array_types.get(i);
            String type_src = array_src.getType();
            String elem_src = array_src.getElementType();
            for (j=0; j<array_types.size(); j++) {
                if (i==j) continue;
                ArrayType array_dest = array_types.get(j);
                String type_dest = array_dest.getType();
                String elem_dest = array_dest.getElementType();
                if (elem_src.equals(type_dest)) {
                    if (i == j-1) continue;
                    this.order(array_types, j, i);
                    i--;
                    break;
                }
                if (elem_dest.equals(type_src)) {
                    if (i == j+1) continue;
                    this.order(array_types, j, i);
                    i--;
                    break;
                }
            }
        }
        return array_types;
    }

    /**
     * 配列定義リストの要素を入れ替える。入れ替えた要素は小さいインデックス位置と次に配置する。
     * @param list		配列定義リスト
     * @param index1	入替インデックス1
     * @param index2	入替インデックス2
     */
    private void order(List<ArrayType> list, int index1, int index2){

        ArrayType tmp1 = list.get(index1);
        ArrayType tmp2 = list.get(index2);
        if (index1 < index2) {
            list.remove(index2);
            list.set(index1, tmp2);
            list.add(index1+1, tmp1);
        }
        else {
            list.remove(index1);
            list.set(index2, tmp1);
            list.add(index2+1, tmp2);
        }
    }
}
