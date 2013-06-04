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

import jp.riken.kscope.Message;

/**
 * キーワードタイプ
 * @author riken
 */
public enum KEYWORD_TYPE {
    // キーワードタイプ
    /** 予約語、キーワード */
    KEYWORD(Message.getString("keyword_type.enum.reserved")), //予約語、キーワード
    /** テキスト検索 */
    SEARCH(Message.getString("keyword_type.enum.textsearch")), //テキスト検索
    /** トレース */
    TRACE(Message.getString("mainmenu.window.analysis.trace")), //トレース
    /** 変数アクセス先メモリ */
    VARIABLE(Message.getString("keyword_type.enum.variablememory")), //変数アクセス先メモリ
    /** 不明 */
    UNKNOWN(Message.getString("explore_panel.enum.unknown")); //不明

    /** タイプ名 */
    private String name;

    /**
     * コンストラクタ
     * @param name		タイプ名
     */
    private KEYWORD_TYPE(String name) {
        this.name = name;
    }

    /**
     * タイプ名を取得する
     * @return		タイプ名
     */
    public String getName() {
        return this.name;
    }

}



