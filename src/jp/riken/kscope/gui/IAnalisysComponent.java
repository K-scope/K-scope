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

import java.io.File;

import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.menu.MainMenu;
import jp.riken.kscope.properties.SourceProperties;


/**
 * 分析情報パネルコンポーネントインターフェイス.<br/>
 * 分析情報パネルコンポーネントのインターフェイス
 * @author RIKEN
 */
public interface IAnalisysComponent {

    /**
     * 分析情報のエクスポートを行う
     * @param   file    出力ファイル
     */
    public void export(File file);

    /**
     * パネルにアクションリスナを設定する.<br/>
     * メニューバーに作成済みのアクションリスナをパネルボタンに割り当てる。
     * @param menu		メインメニュー
     */
    public void setActionListener(MainMenu menu);

    /**
     * 分析情報パネル識別子を設定する
     * @return enumPanel		分析情報パネル識別子
     */
    public ANALYSIS_PANEL getEnumPanel();

    /**
     * モデルのクリアを行う。
     */
    public void clearModel();

    /**
     * タブのクローズを行う
     */
    public void closeTab();

    /**
     * 選択ソースコード行情報を取得する
     * @return		選択ソースコード行情報
     */
    public CodeLine getSelectedCodeLine();

    /**
     * 選択ブロックを取得する
     * @return		選択ブロック
     */
    public IBlock getSelectedBlock();

    /**
     * 選択付加情報を取得する
     * @return		選択付加情報
     */
    public IInformation getSelectedInformation();

    /**
     * ソースビュープロパティを設定する
     * @param properties		ソースビュープロパティ
     */
    void setSourceProperties(SourceProperties properties);

    /**
     * 選択項目をクリップボードにコピーする.
     */
	public void copyClipboard();
	
	/**
	 * エクスポートできる情報があるか判定する
	 * @return	true: 情報あり　false:情報無し
	 */
	public boolean isExportable();

}


