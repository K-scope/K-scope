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

package jp.riken.kscope.data;

/**
 * 検索条件クラス
 * @author RIKEN
 */
public class SearchOption {

    /** 検索文字列 */
    private String searchText;
    /** 大文字・小文字の区別(true=大文字・小文字の区別を行う) */
    private boolean sensitivecase;
    /** 正規表現 */
    private boolean regex;
    /** 単語検索 */
    private boolean word;
    /**  変数検索(=トレース) */
    private boolean variable;
    /** 検索ノードのオブジェクトクラス */
    private Class<?> searchClass;

    /**
     * 検索文字列を取得する.
     * @return		検索文字列
     */
    public String getSearchText() {
        return searchText;
    }

    /**
     * 検索文字列を設定する
     * @param text		検索文字列
     */
    public void setSearchText(String text) {
        this.searchText = text;
    }

    /**
     * 大文字・小文字の区別を取得する.
     * @return		true=大文字・小文字の区別する
     */
    public boolean isSensitivecase() {
        return sensitivecase;
    }

    /**
     * 大文字・小文字の区別を設定する
     * @param sensitivecase		true=大文字・小文字の区別する
     */
    public void setSensitivecase(boolean sensitivecase) {
        this.sensitivecase = sensitivecase;
    }

    /**
     * 正規表現を取得する
     * @return		true=正規表現検索
     */
    public boolean isRegex() {
        return regex;
    }

    /**
     * 正規表現を設定する
     * @param regex		true=正規表現検索
     */
    public void setRegex(boolean regex) {
        this.regex = regex;
    }

    /**
     * 単語検索を取得する.
     * @return		true=単語検索
     */
    public boolean isWord() {
        return word;
    }

    /**
     * 単語検索を設定する.
     * @param word		単語検索
     */
    public void setWord(boolean word) {
        this.word = word;
    }

    /**
     * 変数検索(=トレース)であるかチェックする
     * @return		変数検索(=トレース)
     */
    public boolean isVariable() {
        return this.variable;
    }

    /**
     * 変数検索(=トレース)を設定する.
     * @param variable		変数検索(=トレース)
     */
    public void setVariable(boolean variable) {
        this.variable = variable;
    }

    /**
     * 検索ノードのオブジェクトクラスを取得する
     * @return 検索ノードのオブジェクトクラス
     */
    public Class<?> getSearchClass() {
        return searchClass;
    }

    /**
     * 検索ノードのオブジェクトクラスを設定する
     * @param searchClass 		検索ノードのオブジェクトクラス
     */
    public void setSearchClass(Class<?> searchClass) {
        this.searchClass = searchClass;
    }

}


