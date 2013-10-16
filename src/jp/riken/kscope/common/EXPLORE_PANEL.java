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
 * エクスプローラ（ツリー）パネルの識別文字列
 * @author riken
 */
public enum EXPLORE_PANEL {
    // パネル一覧
    /** 構造パネル */
    LANGUAGE(Message.getString("mainmenu.window.explore.structure")), //構造
    /** モジュールパネル */
    MODULE(Message.getString("mainmenu.window.explore.module")), //モジュール
    /** ソースパネル */
    SOURCE(Message.getString("mainmenu.window.explore.source")), //ソース
    /** XMLパネル */
    XML(Message.getString("mainmenu.window.explore.xml")), //XML
    /** 不明 */
    UNKNOWN(Message.getString("explore_panel.enum.unknown")); //不明

    /** タブ名 */
    private String tabname;

    /**
     * コンストラクタ
     * @param tabname		タブ名
     */
    private EXPLORE_PANEL(String tabname) {
        this.tabname = tabname;
    }

    /**
     * タブ名を取得する
     * @return		タブ名
     */
    public String getTabName() {
        return this.tabname;
    }

}



