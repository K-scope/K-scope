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
package jp.riken.kscope.xcodeml.fortran.parser;

import java.util.ArrayList;
import java.util.List;

import jp.riken.kscope.exception.XcodeMLException;
import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.xcodeml.fortran.XcodeMLTypeManager;
import jp.riken.kscope.xcodeml.fortran.utils.XmlNodeUtil;
import jp.riken.kscope.xcodeml.fortran.xml.IXmlNode;
import jp.riken.kscope.xcodeml.fortran.xml.gen.ArrayIndex;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FarrayRef;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FcharacterRef;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FcoArrayRef;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FdoLoop;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FmemberRef;
import jp.riken.kscope.xcodeml.fortran.xml.gen.IndexRange;
import jp.riken.kscope.xcodeml.fortran.xml.gen.Value;
import jp.riken.kscope.xcodeml.fortran.xml.gen.Var;
import jp.riken.kscope.xcodeml.fortran.xml.gen.VarList;
import jp.riken.kscope.xcodeml.fortran.xml.gen.VarRef;


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
        if (node instanceof FcharacterRef) {
            var = getVariable((FcharacterRef)node);
        }
        else if (node instanceof FmemberRef) {
            var = getVariable((FmemberRef)node);
        }
        else if (node instanceof FcoArrayRef) {
            var = getVariable((FcoArrayRef)node);
        }
        else if (node instanceof FarrayRef) {
            var = getVariable((FarrayRef)node);
        }
        else if (node instanceof Var) {
            var = getVariable((Var)node);
        }
        else if (node instanceof Value) {
            var = getVariable((Value)node);
        }
        else if (node instanceof VarRef) {
            var = getVariable((VarRef)node);
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
     * FmemberRef要素からDB::Variableクラスをパース、生成する。
     *
     * @param node          FmemberRef要素
     * @return DB::Variableクラス
     */
    public Variable getVariable(FmemberRef node) {

        if (node == null) {
            return null;
        }

        // 変数
        VarRef varNode = node.getVarRef();
        String member = node.getMember();

        // Variableクラス生成
        Variable var = getVariable(varNode);

        // 構造体メンバ名を付加した変数名に変更する
        String name = var.getName();
        name = name + "%" + member;
        var.setName(name);

        return var;
    }

    /**
     * FarrayRef要素からDB::Variableクラスをパース、生成する。
     *
     * @param node          FarrayRef要素
     * @return DB::Variableクラス
     */
    public Variable getVariable(FarrayRef node) {

        if (node == null) {
            return null;
        }

        // 変数
        VarRef varNode = node.getVarRef();
        List<IXmlNode> indexes = node.getIndexRangeOrArrayIndexOrFarrayConstructor();

        // Variableクラス生成
        Variable var = getVariable(varNode);
        List<Expression> list = getIndexValues(indexes);
        var.setDimensionIndexValue(list);

        return var;
    }

    /**
     * FcoArrayRef要素からDB::Variableクラスをパース、生成する。
     *
     * @param node          FcoArrayRef要素
     * @return DB::Variableクラス
     */
    public Variable getVariable(FcoArrayRef node) {

        if (node == null) {
            return null;
        }

        // 変数
        VarRef varNode = node.getVarRef();
        List<ArrayIndex> indexes = node.getArrayIndex();

        // Variableクラス生成
        Variable var = getVariable(varNode);
        List<Expression> list = getArrayIndexValues(indexes);
        var.setDimensionIndexValue(list);

        return var;
    }

    /**
     * FcharacterRef要素からDB::Variableクラスをパース、生成する。
     *
     * @param node          FcharacterRef要素
     * @return DB::Variableクラス
     */
    public Variable getVariable(FcharacterRef node) {

        if (node == null) {
            return null;
        }

        // 変数
        VarRef varNode = node.getVarRef();
        // Variableクラス生成
        Variable var = getVariable(varNode);

        IndexRange indexRange = node.getIndexRange();
        List<Expression> indexes = getIndexRangeValues(indexRange);
        var.setDimensionIndexValue(indexes);

        return var;
    }


    /**
     * VarRef要素からDB::Variableクラスをパース、生成する。
     *
     * @param node          VarRef要素
     * @return DB::Variableクラス
     */
    public Variable getVariable(VarRef node) {

        if (node == null) {
            return null;
        }

        // 子要素の取得
        IXmlNode child = XmlNodeUtil.getXmlNodeChoice(node);
        Variable var = getVariable(child);

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


    /**
     * 配列添字のリストを取得する
     * @param index		ArrayIndex要素リスト
     * @return			配列添字リスト
     */
    private List<Expression> getArrayIndexValues(List<ArrayIndex> indexes) {
        if (indexes == null) return null;
        List<IXmlNode> list = new ArrayList<IXmlNode>();
        for (ArrayIndex index : indexes) {
            list.add(index);
        }
        return getIndexValues(list);
    }


    /**
     * VarList要素からDB::Variableクラスリストをパース、生成する。
     *
     * @param node          VarList要素
     * @return DB::Variableクラスリスト
     */
    public List<Variable> getVariableList(VarList node) {
        if (node == null) return null;

        List<IXmlNode> nodes = node.getVarRefOrFdoLoop();
        if (nodes == null) return null;

        List<Variable> list = new ArrayList<Variable>();
        for (IXmlNode xmlNode : nodes) {
            if (xmlNode instanceof FdoLoop) {
                List<Value> valList = ((FdoLoop)xmlNode).getValue();
                for (Value value : valList) {
                    Variable var = getVariable(value);
                    if (var != null) {
                        list.add(var);
                    }
                }
            }
            else {
                Variable var = getVariable(xmlNode);
                if (var != null) {
                    list.add(var);
                }
            }
        }
        if (list.size() <= 0) return null;

        return list;
    }
}
