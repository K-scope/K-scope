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

import jp.riken.kscope.exception.XcodeMLException;
import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.xcodeml.clang.XcodeMLTypeManager;
import jp.riken.kscope.xcodeml.clang.utils.XmlNodeUtil;
import jp.riken.kscope.xcodeml.clang.xml.IXmlNode;
import jp.riken.kscope.xcodeml.clang.xml.gen.ArrayAddr;
import jp.riken.kscope.xcodeml.clang.xml.gen.ArrayRef;
import jp.riken.kscope.xcodeml.clang.xml.gen.BinaryExpression;
import jp.riken.kscope.xcodeml.clang.xml.gen.CoArrayRef;
import jp.riken.kscope.xcodeml.clang.xml.gen.IndexRange;
import jp.riken.kscope.xcodeml.clang.xml.gen.MemberAddr;
import jp.riken.kscope.xcodeml.clang.xml.gen.MemberArrayAddr;
import jp.riken.kscope.xcodeml.clang.xml.gen.MemberArrayRef;
import jp.riken.kscope.xcodeml.clang.xml.gen.MemberRef;
import jp.riken.kscope.xcodeml.clang.xml.gen.PointerRef;
import jp.riken.kscope.xcodeml.clang.xml.gen.SubArrayRef;
import jp.riken.kscope.xcodeml.clang.xml.gen.Value;
import jp.riken.kscope.xcodeml.clang.xml.gen.Var;
import jp.riken.kscope.xcodeml.clang.xml.gen.VarAddr;


/**
 * Variableパーサクラス
 * @author RIKEN
 */
public class VariableParser {

    /** typeTable */
    private XcodeMLTypeManager typeManager;

    /**
     * コンストラクタ
     * @param typeManager    typeTable
     */
    public VariableParser(XcodeMLTypeManager typeManager) {
        this.typeManager = typeManager;
    }


    /**
     * IXmlNode要素からDB::Variableクラスをパース、生成する。
     *
     * @param node          IXmlNode要素
     * @return DB::Variableクラス
     */
    public Variable getVariable(IXmlNode node) {

        if (node == null) {
            return null;
        }

        Variable var = null;
        if (node instanceof PointerRef) {
            var = getVariable((PointerRef)node);
        }
        else if (node instanceof MemberAddr) {
            var = getVariable((MemberAddr)node);
        }
        else if (node instanceof MemberRef) {
            var = getVariable((MemberRef)node);
        }
        else if (node instanceof MemberArrayAddr) {
            var = getVariable((MemberArrayAddr)node);
        }
        else if (node instanceof MemberArrayRef) {
            var = getVariable((MemberArrayRef)node);
        }
        else if (node instanceof CoArrayRef) {
            var = getVariable((CoArrayRef)node);
        }
        else if (node instanceof SubArrayRef) {
            var = getVariable((SubArrayRef)node);
        }
        else if (node instanceof ArrayRef) {
            var = getVariable((ArrayRef)node);
        }
        else if (node instanceof ArrayAddr) {
            var = getVariable((ArrayAddr)node);
        }
        else if (node instanceof Var) {
            var = getVariable((Var)node);
        }
        else if (node instanceof VarAddr) {
            var = getVariable((VarAddr)node);
        }
        else if (node instanceof Value) {
            var = getVariable((Value)node);
        }

        return var;
    }

    /**
     * Value要素からDB::Variableクラスをパース、生成する。
     *
     * @param node          Value要素
     * @return DB::Variableクラス
     */
    public Variable getVariable(Value node) {

        if (node == null) {
            return null;
        }

        // 変数名
        IXmlNode value = XmlNodeUtil.getXmlNodeChoice(node);

        // Variableクラス生成
        Variable var = getVariable(value);

        return var;
    }


    /**
     * Var要素からDB::Variableクラスをパース、生成する。
     *
     * @param node          Var要素
     * @return DB::Variableクラス
     */
    public Variable getVariable(Var node) {

        if (node == null) {
            return null;
        }

        // 変数名
        String value = node.getValue();

        // Variableクラス生成
        Variable var = new Variable(value);

        return var;
    }

    /**
     * MemberAddr要素からDB::Variableクラスをパース、生成する。
     *
     * @param node          MemberAddr要素
     * @return DB::Variableクラス
     */
    public Variable getVariable(MemberAddr node) {

        if (node == null) {
            return null;
        }

        // 構造体
        IXmlNode structure_node = XmlNodeUtil.getXmlNodeChoice(node);
        Variable structure_var = this.getVariable(structure_node);
        if (structure_var == null) return null;

        // 構造体メンバ
        String member_name = node.getMember();
        String member_type = node.getType();

        // 構造体メンバ名を付加した変数名に変更する
        String name = structure_var.getName();
        name = name + "." + member_name;

        // Variableクラス生成
        Variable var = new Variable(name);

        return var;
    }

    /**
     * MemberRef要素からDB::Variableクラスをパース、生成する。
     *
     * @param node          MemberRef要素
     * @return DB::Variableクラス
     */
    public Variable getVariable(MemberRef node) {

        if (node == null) {
            return null;
        }

        // 構造体
        IXmlNode structure_node = XmlNodeUtil.getXmlNodeChoice(node);
        Variable structure_var = this.getVariable(structure_node);
        if (structure_var == null) return null;

        // 構造体メンバ
        String member_name = node.getMember();
        String member_type = node.getType();

        // 構造体メンバ名を付加した変数名に変更する
        String name = structure_var.getName();
        name = name + "." + member_name;

        // Variableクラス生成
        Variable var = new Variable(name);

        return var;
    }


    /**
     * memberArrayAddr要素からDB::Variableクラスをパース、生成する。
     *
     * @param node          memberArrayAddr要素
     * @return DB::Variableクラス
     */
    public Variable getVariable(MemberArrayAddr node) {

        if (node == null) {
            return null;
        }

        // 構造体
        IXmlNode structure_node = XmlNodeUtil.getXmlNodeChoice(node);
        Variable structure_var = this.getVariable(structure_node);
        if (structure_var == null) return null;

        // 構造体メンバ
        String member_name = node.getMember();
        String member_type = node.getType();

        // 構造体メンバ名を付加した変数名に変更する
        String name = structure_var.getName();
        name = name + "." + member_name;

        // Variableクラス生成
        Variable var = new Variable(name);

        return var;
    }

    /**
     * memberArrayRef要素からDB::Variableクラスをパース、生成する。
     *
     * @param node          memberArrayRef要素
     * @return DB::Variableクラス
     */
    public Variable getVariable(MemberArrayRef node) {

        if (node == null) {
            return null;
        }

        // 構造体
        IXmlNode structure_node = XmlNodeUtil.getXmlNodeChoice(node);
        Variable structure_var = this.getVariable(structure_node);
        if (structure_var == null) return null;

        // 構造体メンバ
        String member_name = node.getMember();
        String member_type = node.getType();

        // 構造体メンバ名を付加した変数名に変更する
        String name = structure_var.getName();
        name = name + "." + member_name;

        // Variableクラス生成
        Variable var = new Variable(name);

        return var;
    }

    /**
     * ArrayAddr要素からDB::Variableクラスをパース、生成する。
     *
     * @param node          ArrayAddr要素
     * @return DB::Variableクラス
     */
    public Variable getVariable(ArrayAddr node) {

        if (node == null) {
            return null;
        }

        // 変数
        String name = node.getValue();

        // Variableクラス生成
        Variable var = new Variable(name);

        return var;
    }

    /**
     * ArrayRef要素からDB::Variableクラスをパース、生成する。
     *
     * @param node          ArrayRef要素
     * @return DB::Variableクラス
     */
    public Variable getVariable(ArrayRef node) {

        if (node == null) {
            return null;
        }

        // 変数
        ArrayAddr array_value = node.getArrayAddrInArrayRef();
        String name = array_value.getValue();

        // 配列
        List<IXmlNode> indexes = node.getExpressions();
        List<Expression> list = getIndexValues(indexes);

        // Variableクラス生成
        Variable var = getVariable(array_value);
        var.setDimensionIndexValue(list);

        return var;
    }

    /**
     * coArrayRef要素からDB::Variableクラスをパース、生成する。
     *
     * @param node          coArrayRef要素
     * @return DB::Variableクラス
     */
    public Variable getVariable(CoArrayRef node) {

        if (node == null) {
            return null;
        }

        // 変数
        IXmlNode var_node = null;
        if (node.getVarInCoArrayRef() != null) var_node = node.getVarInCoArrayRef();
        else if (node.getArrayRefInCoArrayRef() != null) var_node = node.getArrayRefInCoArrayRef();
        else if (node.getSubArrayRefInCoArrayRef() != null) var_node = node.getSubArrayRefInCoArrayRef();
        else if (node.getMemberRefInCoArrayRef() != null) var_node = node.getMemberRefInCoArrayRef();

        if (var_node == null) {
            return null;
        }

        // 配列
        List<IXmlNode> indexes = node.getExpressions();
        List<Expression> list = getIndexValues(indexes);

        // Variableクラス生成
        Variable var = getVariable(var_node);
        var.setDimensionIndexValue(list);

        return var;
    }

    /**
     * subArrayRef要素からDB::Variableクラスをパース、生成する。
     *
     * @param node          coArrayRef要素
     * @return DB::Variableクラス
     */
    public Variable getVariable(SubArrayRef node) {

        if (node == null) {
            return null;
        }

        // 変数
        ArrayAddr var_node = node.getArrayAddrInSubArrayRef();
        if (var_node == null) {
            return null;
        }

        // 配列
        List<IXmlNode> indexes = node.getIndexRangeOrIntConstantOrFloatConstant();
        IndexRange index_range = null;
        for (IXmlNode idx_node : indexes) {
            if (idx_node instanceof IndexRange) {
                index_range = (IndexRange) idx_node;
                break;
            }
        }
        List<Expression> list = null;
        if (index_range != null) {
            list = getIndexRangeValues(index_range);
        }
        else {
            list = getIndexValues(indexes);
        }

        // Variableクラス生成
        Variable var = getVariable(var_node);
        var.setDimensionIndexValue(list);

        return var;
    }

    /**
     * PointerRef要素からDB::Variableクラスをパース、生成する。
     *
     * @param node          PointerRef要素
     * @return DB::Variableクラス
     */
    public Variable getVariable(PointerRef node) {

        if (node == null) {
            return null;
        }

        // 変数
        IXmlNode var_node = XmlNodeUtil.getXmlNodeChoice(node);

        // Variableクラス生成
        Variable var = getVariable(var_node);

        return var;
    }


    /**
     * VarAddr要素からDB::Variableクラスをパース、生成する。
     *
     * @param node          VarAddr要素
     * @return DB::Variableクラス
     */
    public Variable getVariable(VarAddr node) {

        if (node == null) {
            return null;
        }

        // 変数名
        String value = node.getValue();

        // Variableクラス生成
        Variable var = new Variable(value);

        return var;
    }

    /**
     * 配列添字のリストを取得する
     * @param index		IndexRange要素
     * @return			配列添字リスト
     */
    private List<Expression> getIndexRangeValues(IndexRange index) {
        if (index == null) return null;

        // ExpressionParserモデルパーサ
        ExpressionParser exprParser = new ExpressionParser(this.typeManager, index);
        Expression expr = exprParser.getExpression();
        if (expr == null) return null;

        List<Expression> list = new ArrayList<Expression>();
        list.add(expr);

        return list;
    }

    /**
     * 配列添字のリストを取得する
     * @param index		IXmlNode要素リスト
     * @return			配列添字リスト
     */
    private List<Expression> getIndexValues(List<IXmlNode> indexes) {
        if (indexes == null) return null;

        // ExpressionParserモデルパーサ
        ExpressionParser exprParser = new ExpressionParser(this.typeManager);

        List<Expression> list = new ArrayList<Expression>();
        if (indexes != null && indexes.size() > 0) {
            for (IXmlNode index : indexes) {
                try {
                    Expression expr = exprParser.getExpression(index);
                    list.add(expr);
                } catch (XcodeMLException e) {
                    e.printStackTrace();
                }
            }
        }
        if (list.size() <= 0) return null;

        return list;
    }

}
