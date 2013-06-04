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
package jp.riken.kscope.common;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import jp.riken.kscope.Message;


/**
 * 構造ツリーフィルタタイプ
 * @author riken
 */
public enum FILTER_TYPE {
    // 構造ツリーフィルタタイプ
    /** サブルーチン・関数 */
    PROCEDURE(Message.getString("mainmenu.view.filter.subroutine-function"), //サブルーチン・関数
    		new Class<?>[]{jp.riken.kscope.language.Procedure.class}), 
    /** CALL文 */
    PROCEDUREUSAGE(Message.getString("filter_type.enum.call"), //CALL文
    		new Class<?>[]{jp.riken.kscope.language.ProcedureUsage.class}),
    /** DO文 */
    REPETITION(Message.getString("filter_type.enum.do"), //DO文 
    		new Class<?>[]{
    								jp.riken.kscope.language.Repetition.class,
            						jp.riken.kscope.language.ArrayExpression.class}),
    /** SELECT文 */
    SELECTION_SELECT(Message.getString("filter_type.enum.selection"), //SELECT,CASE文 
    		new Class<?>[]{
                                    jp.riken.kscope.language.Selection.class,
                                    jp.riken.kscope.language.Condition.class}),
    /** IF文 */
    SELECTION_IF(Message.getString("filter_type.enum.if"), //IF,WHERE,ELSE文
    		new Class<?>[]{
                                    jp.riken.kscope.language.Selection.class,
                                    jp.riken.kscope.language.Condition.class}),
    /** 代入文 */
    SUBSTITUTION(Message.getString("filter_type.enum.assign"), //代入文
    		new Class<?>[]{jp.riken.kscope.language.Substitution.class}),
    /** フロー制御文 */
    FLOW(Message.getString("filter_type.enum.flow"), //フロー制御文
    		new Class<?>[]{jp.riken.kscope.language.Return.class,
                                     jp.riken.kscope.language.Break.class,
                                     jp.riken.kscope.language.Continue.class,
                                     jp.riken.kscope.language.GoTo.class,
                                     jp.riken.kscope.language.Termination.class
                                     }),
    /** ディレクティブ文:OpenMP */
    DIRECTIVE_OPENML(Message.getString("mainmenu.view.filter.openmp"), //OpenMP
    		new Class<?>[]{jp.riken.kscope.language.Directive.class}),
    /** ディレクティブ文:OCL */
    DIRECTIVE_OCL(Message.getString("mainmenu.view.filter.ocl"), //OCL
    		new Class<?>[]{jp.riken.kscope.language.Directive.class}),
    /** デフォルト */
    DEFAULT(Message.getString("mainmenu.view.filter.default"), null), //デフォルト
    /** すべて表示(フィルタ無し) */
    ALL(Message.getString("filter_type.enum.show-all"), null), //すべて表示
    /** すべて非表示 */
    NONE(Message.getString("filter_type.enum.hide-all"), null), //すべて非表示
    /** 不明 */
    UNKNOWN(Message.getString("filter_type.enum.trace-unknown"), null); //トレース：不明

    /** フィルタ名 */
    private String name;
    /** フィルタクラス */
    private Class<?>[] filterClass;

    /**
     * コンストラクタ
     * @param name		フィルタ名
     * @param filterClass		フィルタクラス
     */
    private FILTER_TYPE(String name, Class<?>[] filterClass) {
        this.name = name;
        this.filterClass = filterClass;
    }

    /**
     * フィルタ名を取得する
     * @return		フィルタ名
     */
    public String getName() {
        return this.name;
    }

    /**
     * フィルタクラスを取得する
     * @return		フィルタクラス
     */
    public Class<?>[] getFilterClass() {
        return this.filterClass;
    }


    /**
     * フォートランクラスとフィルタが一致しているかチェックする
     * @param node			ノードオブジェクト
     * @return				true=一致
     */
    public boolean isFilter(Object node) {

        if (this == FILTER_TYPE.ALL) {
            // フィルタ適用無し（すべて表示）
            return true;
        }

        // フィルタクラス
        Class<?>[] filterClasses = this.getFilterClass();
        if (filterClasses == null) return true;

        // フィルタクラスであるか？
        boolean filter = false;
        for (Class<?> subclass : filterClasses) {
            if (subclass.isInstance(node)) {
                filter = true;
                break;
            }
        }
        if (filter == false) return false;

        // SELECT, IF文
        if (this == FILTER_TYPE.SELECTION_SELECT || this == FILTER_TYPE.SELECTION_IF) {
            jp.riken.kscope.language.Selection selection = null;
            if (node instanceof jp.riken.kscope.language.Selection) {
                selection = (jp.riken.kscope.language.Selection)node;
            }
            else if (node instanceof jp.riken.kscope.language.Condition) {
                jp.riken.kscope.language.Condition condition = (jp.riken.kscope.language.Condition)node;
                selection = (jp.riken.kscope.language.Selection)condition.get_mother();
            }
            if (selection != null) {
                if (this == FILTER_TYPE.SELECTION_SELECT) {
                    return (selection.isSelect());
                }
                else if (this == FILTER_TYPE.SELECTION_IF) {
                    return (selection.isIF() || (selection.isWHERE()));
                }
            }
            return false;
        }
        else if (this == FILTER_TYPE.DIRECTIVE_OPENML) {
            jp.riken.kscope.language.Directive directive = (jp.riken.kscope.language.Directive)node;
            String message = directive.getArgument();
            // OpenMP指示文の検索
            String regex = "^omp.*";
            int flags = Pattern.CASE_INSENSITIVE + Pattern.MULTILINE;
            // 正規表現検索
            Matcher m = Pattern.compile(regex, flags).matcher(message);
            return m.matches();
        }
        else if (this == FILTER_TYPE.DIRECTIVE_OCL) {
            jp.riken.kscope.language.Directive directive = (jp.riken.kscope.language.Directive)node;
            String message = directive.getArgument();
            // OCL指示文の検索
            String regex = "^ocl.*";
            int flags = Pattern.CASE_INSENSITIVE + Pattern.MULTILINE;
            // 正規表現検索
            Matcher m = Pattern.compile(regex, flags).matcher(message);
            return m.matches();
        }
        return true;
    }

}



