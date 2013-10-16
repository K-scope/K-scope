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
package jp.riken.kscope.gui;

import java.awt.Color;

import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;

/**
 * ソースビューのバーグラフインターフェイスクラス
 * @author riken
 */
public interface ISourceBargraph {

    /**
     * バーグラフの値を取得する.
     * Max=1.0 〜 Min=0.0
     * @return		バーグラフの値
     */
    public float getBarValue();

    /**
     * バーグラフ横に表示する文字列を取得する
     * @return		表示文字列
     */
    public String getBarText();

    /**
     * バーグラフの対象行情報を取得する
     * @return		対象行情報
     */
    public CodeLine getCodeLine();

    /**
     * バーグラフの対象ソースファイルを取得する
     * @return		対象ソースファイル
     */
    public SourceFile getSourceFile();

    /**
     * バーグラフの表示色を取得する
     * @return		バーグラフ表示色
     */
    public Color getBarColor();

    /**
     * バーグラフのタイプ名を取得する
     * @return		バーグラフタイプ名
     */
    public String getTypeName();
}
