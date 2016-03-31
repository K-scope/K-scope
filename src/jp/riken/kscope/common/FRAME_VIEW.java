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
 * メインフレームのビュー画面識別列挙クラス
 * @author RIKEN
 */
public enum FRAME_VIEW {
    // ビュー一覧
    /** メインフレーム */
    MAIN_FRAME(Message.getString("frame_view.enum.main")), //メインフレーム
    /** エクスプローラビュー */
    EXPLORE_VIEW(Message.getString("mainmenu.window.explore")), //エクスプローラビュー
    /** ソースビュー */
    SOURCE_VIEW(Message.getString("mainmenu.window.source")), //ソースビュー
    /** 分析ビュー */
    ANALYSIS_VIEW(Message.getString("mainmenu.window.analysis")); //分析ビュー

    /** ビュー名 */
    private String viewname;

    /**
     * コンストラクタ
     * @param tabname		ビュー名
     */
    private FRAME_VIEW(String viewname) {
        this.viewname = viewname;
    }

    /**
     * ビュー名を取得する
     * @return		ビュー名
     */
    public String getViewName() {
        return this.viewname;
    }

}



