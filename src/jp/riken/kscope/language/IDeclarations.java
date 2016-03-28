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
package jp.riken.kscope.language;

import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

/**
*
* 変数定義を含むブロックのインターフェース.<br>
* 変数定義・参照を取得、設定するインターフェイスを提供する.<br>
* @author RIKEN * @version    2015/09/01     新規作成
*/
public interface IDeclarations {

    /**
     * 指定した名前の変数宣言を返す。
     * @param var_name 変数名
     * @return 変数宣言。無ければnullを返す。
     */
    VariableDefinition get_variable(String var_name);

    /**
     * 自身に係わる変数宣言のマップを返す。
     * @return 変数宣言のマップ。
     */
    Map<String,VariableDefinition> getVariableDefinitionMap();

    /**
     * プログラム単位内で参照されている変数名とブロックリストのマップを返す。
     * @return 変数名とブロックリストのマップ
     */
    // Map<String, Set<IBlock>> getRefVariableNames();

    /**
     * プログラム単位内で参照されている変数のブロックリストを返す。
     * @param   var_name    変数名
     * @return 変数参照ブロックリスト
     */
    Set<IBlock> getRefVariableName(String var_name);

    /**
     * プログラム単位内で参照されている変数名と現れるブロックを追加する。
     * @param refVarName           変数名
     * @param blk            ブロック
     */
    void putRefVariableName(String refVarName, IBlock blk);

    /**
     * プログラム単位内で定義されている変数名の集合を取得する。
     * @return 変数名の集合
     */
    // Map<String, Set<IBlock>> getDefVariableNames();

    /**
     * プログラム単位内で定義されている変数のブロックリストを取得する。
     * @param   var_name    変数名
     * @return 変数定義ブロックリスト
     */
    Set<IBlock> getDefVariableName(String var_name);

    /**
     * プログラム単位内で定義されている変数名と現れるブロックを追加する。
     * @param defVarName            変数名
     * @param blk           ブロック
     */
    void putDefVariableName(String defVarName, IBlock blk);

    /**
     * プログラム単位で使用される変数マップを取得する。
     * @return    変数のマップ
     */
    HashMap<Variable, VariableDefinition> getVariableMap();

    /**
     * プログラム単位において指定された名前で使用される変数が存在すればその宣言を返す。
     * @param nm            変数名
     * @return 変数宣言。無ければnullを返す。
     */
    VariableDefinition[] searchVariableDefinitionFromMap(String nm);

    /**
     * プログラム単位において指定された名前で使用される変数が存在すればその変数を返す。
     * @param nm            変数名
     * @return 変数宣言。無ければnullを返す。
     */
    Variable[] searchVariableFromMap(String nm);

    /**
     * 変数宣言リストを作成する.
     */
    void createVariableMap();

    /**
     * プログラム単位で使用される変数名を追加する。
     * @param var            変数
     */
    void putVariableMap(Variable var);

    /**
     * プログラム単位で使用される変数名と宣言のマップを追加する。
     * @param var            変数
     * @param varDef            変数宣言
     */
    void putVariableMap(Variable var, VariableDefinition varDef);

    /**
     * プログラム単位内で、ある変数が参照・定義されているブロックのセットを返す。
     * @param name       変数名
     * @return ブロックのセット。無ければ空のセットを返す。
     */
    Set<IBlock> getRefDefBlocks(String name);

    /**
     * 変数宣言文を取得する。
     *
     * @return 変数宣言文リスト
     */
    VariableDefinition[] get_variables();

    /**
     * 変数宣言文を設定する。
     * @param varDef            変数宣言文
     */
    void set_variable_def(VariableDefinition varDef);

    /**
     * 指定された変数名と、指定されたブロックを参照としてセットする。
     * @param blk ブロック
     * @param var    変数
     */
    void addVariableToRef(IBlock blk, Variable var);

    /**
     * 手続呼出を参照としてセットする。
     * @param blk ブロック
     * @param func    手続呼出
     */
    void addFunctionToRef(IBlock blk, ProcedureUsage func);

    /**
     * 指定された式に含まれる全ての変数名と、指定されたブロックを参照としてセットする。
     * 指定された式に含まれる全ての変数名と、式に含まれる手続呼出を参照としてセットする。
     * @param blk ブロック
     * @param exp 式
     */
    void addExpressionToRef(IBlock blk, Expression exp);


    /**
     * 変数に変数定義をセットする
     */
    void setVariableDefinitions();


    /**
     * プログラム単位において指定された変数が存在すればその宣言を返す。
     * @param var            変数
     * @return 変数宣言。無ければnullを返す。
     */
    VariableDefinition getVariableMap(Variable var);

    /**
     * プログラム単位において指定された名前で使用される変数が存在すればその宣言を返す。
     *
     * @param nm
     *            変数名
     * @return 変数宣言。無ければnullを返す。
     */
    VariableDefinition[] searchVariablesDefinition(String nm);
}
