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

import java.awt.Color;

import jp.riken.kscope.common.KEYWORD_TYPE;

/**
 * ソースコードのキーワードデータクラス
 * @author riken
 *
 */
public class Keyword {

    /** キーワード名 */
    private String name;
    /** キーワード */
    private String keyword;
    /** クラスモード（正規表現では表現できない特殊モード:未使用) */
    private String classmode;
    /** 文字色 */
    private Color forecolor;
    /** 背景色 */
    private Color backgroundcolor;
    /** スタイル(Font.PLAIN, Font.BOLD, Font.ITALIC) */
    private int style;
    /** 有効 */
    private boolean enabled = true;
    /** 大文字・小文字の区別(true=大文字・小文字の区別を行う) */
    private boolean sensitivecase = false;
    /** 正規表現 */
    private boolean regex = false;
    /** 単語検索 : 引用符,コメントの中でも検索対象とする*/
    private boolean searchWord = true;
    /**  変数検索(=トレース):引用符,コメントの中は検索対象外をする */
    private boolean searchVariable;
    /** キーワード変更不可 */
    private boolean keywordlock = false;
    /** 検索対象コード行 */
    private CodeLine searchLine;
    /** キーワードタイプ：予約語（デフォルト）、テキスト検索、トレース */
    private KEYWORD_TYPE type = KEYWORD_TYPE.KEYWORD;

    /**
     * コンストラクタ
     * @param type		キーワードタイプ
     */
    public Keyword(KEYWORD_TYPE type) {
        this.type = type;
    }


    /**
     * キーワード名を取得する
     * @return		キーワード名
     */
    public String getName() {
        return name;
    }

    /**
     * キーワード名を設定する
     * @param name		キーワード名
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * キーワードを取得する
     * @return		キーワード
     */
    public String getKeyword() {
        return keyword;
    }

    /**
     * キーワードを設定する
     * @param keyword		キーワード
     */
    public void setKeyword(String keyword) {
        this.keyword = keyword;
    }

    /**
     * モードを取得する
     * @return		モード
     */
    public String getClassmode() {
        return this.classmode;
    }

    /**
     * モードを設定する
     * @param mode		モード
     */
    public void setClassmode(String mode) {
        this.classmode = mode;
    }

    /**
     * ハイライト色を取得する
     * @return		ハイライト色
     */
    public Color getForecolor() {
        return forecolor;
    }

    /**
     * ハイライト色を設定する
     * @param forecolor		ハイライト色
     */
    public void setForecolor(Color forecolor) {
        this.forecolor = forecolor;
    }

    /**
     * スタイルを取得する
     * @return		スタイル
     */
    public int getStyle() {
        return style;
    }

    /**
     * スタイルを設定する
     * @param style		スタイル
     */
    public void setStyle(int style) {
        this.style = style;
    }

    /**
     * キーワードを設定する
     * @param name			キーワード名
     * @param keyword		キーワード
     * @param forecolor		ハイライト色
     * @param style			スタイル
     */
    public void setKeyword(String name, String keyword, Color forecolor, int style) {
        this.name = name;
        this.keyword = keyword;
        this.forecolor = forecolor;
        this.style = style;
    }


    /**
     * モードを設定する
     * @param classmode			モード
     * @param keyword		キーワード
     * @param forecolor		ハイライト色
     * @param style			スタイル
     */
    public void setMode(String classmode, String keyword, Color forecolor, int style) {
        this.classmode = classmode;
        this.keyword = keyword;
        this.forecolor = forecolor;
        this.style = style;
    }

    /**
     * キーワードの有効／無効を取得する
     * @return enabled		true=有効
     */
    public boolean isEnabled() {
        return enabled;
    }

    /**
     * キーワードの有効／無効を設定する
     * @param enabled    true=有効
     */
    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    /**
     * 大文字・小文字の区別を取得する
     * @return		大文字・小文字の区別(true=大文字・小文字の区別を行う)
     */
    public boolean isSensitivecase() {
        return sensitivecase;
    }

    /**
     * 大文字・小文字の区別を設定する
     * @param sensitivecase		大文字・小文字の区別(true=大文字・小文字の区別を行う)
     */
    public void setSensitivecase(boolean sensitivecase) {
        this.sensitivecase = sensitivecase;
    }

    /**
     * 正規表現を取得する
     * @return		正規表現
     */
    public boolean isRegex() {
        return regex;
    }

    /**
     * 正規表現を設定する
     * @param regex		正規表現
     */
    public void setRegex(boolean regex) {
        this.regex = regex;
    }

    /**
     * キーワード変更不可を取得する
     * @return		キーワード変更不可
     */
    public boolean isKeywordlock() {
        return keywordlock;
    }

    /**
     * キーワード変更不可を設定する
     * @param keywordlock		キーワード変更不可
     */
    public void setKeywordlock(boolean keywordlock) {
        this.keywordlock = keywordlock;
    }

    /**
     * 背景色を取得する。
     * @return backgroundcolor		背景色
     */
    public Color getBackgroundcolor() {
        return backgroundcolor;
    }

    /**
     * 背景色を設定する
     * @param backgroundcolor 背景色
     */
    public void setBackgroundcolor(Color backgroundcolor) {
        this.backgroundcolor = backgroundcolor;
    }

    /**
     * 単語検索を取得する.<br/>
      * 引用符,コメントの中でも検索対象とする
     * @return			true=単語検索
     */
    public boolean isSearchWord() {
        return searchWord;
    }

    /**
     * 単語検索を設定する.<br/>
      * 引用符,コメントの中でも検索対象とする
     * @param	searchWord			true=単語検索
     */
    public void setSearchWord(boolean searchWord) {
        this.searchWord = searchWord;
    }

    /**
     * 変数検索(=トレース)を取得する.<br/>
     * 引用符,コメントの中は検索対象外をする
     * @return		true=変数検索(=トレース)
     */
    public boolean isSearchVariable() {
        return searchVariable;
    }

    /**
     * 変数検索(=トレース)を設定する.<br/>
     * 引用符,コメントの中は検索対象外をする
     * @param searchVariable		true=変数検索(=トレース)
     */
    public void setSearchVariable(boolean searchVariable) {
        this.searchVariable = searchVariable;
    }

    /**
     * 検索対象コード行を取得する
     * @return		検索対象コード行
     */
    public CodeLine getSearchLine() {
        return searchLine;
    }

    /**
     * 検索対象コード行を設定する
     * @param searchLine		検索対象コード行
     */
    public void setSearchLine(CodeLine searchLine) {
        this.searchLine = searchLine;
    }

    /**
     * キーワードタイプを取得する
     * @return type		キーワードタイプ
     */
    public KEYWORD_TYPE getType() {
        return type;
    }

    /**
     * キーワードタイプを設定する
     * @param type 		キーワードタイプ
     */
    public void setType(KEYWORD_TYPE type) {
        this.type = type;
    }

}
