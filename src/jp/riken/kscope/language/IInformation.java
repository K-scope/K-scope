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


import jp.riken.kscope.information.InformationBlocks;
import jp.riken.kscope.information.TextInfo;

/**
 * 付加情報の取得、設定を行うインターフェイス
 * @author RIKEN
 */
public interface IInformation {
    /**
     * 付加情報を設定する.<br/>
     * @param info    付加情報
     */
    void setInformation(TextInfo info);

    /**
     * 付加情報を取得する.
     * @return        付加情報
     */
    TextInfo getInformation();

    /**
     * 名前空間（モジュール名.ルーチン名）を取得する。
     *
     * @return 名前空間（モジュール名.ルーチン名）
     */
    String getNamespace();

    /**
     * 開始位置を取得する。
     *
     * @return 開始位置
     */
    int getStartPos();
    /**
     * 開始位置を設定する。
     *
     * @param pos
     *         開始位置
     */
    void setStartPos(int pos);

    /**
     * 終了位置を取得する。
     *
     * @return 終了位置
     */
    int getEndPos();
    /**
     * 終了位置を設定する。
     *
     * @param pos
     *         終了位置
     */
    void setEndPos(int pos);

    /**
     * 付加情報をすべて削除する。
     */
    void clearInformation();

    /**
     * IDを取得する。
     * @return ID
     */
    String getID();

    /**
     * 構造IDを取得する。
     * @return 構造ID
     */
    String getLayoutID();

    /**
     * idにマッチした情報ブロックを検索する。
     * @param id          ID
     * @return 見つかった情報ブロック。見つからなかった場合はnullが返ります。
     */
    IInformation findInformationBlockBy(String id);

    /**
     * 付加情報ブロックコレクションを生成する。
     *
     * @return 付加情報ブロックコレクション
     */
    InformationBlocks createInformationBlocks();

    /**
     * 同一付加情報ブロックを検索する
     * @param block            IInformationブロック
     * @return        同一ブロック
     */
    IInformation[] searchInformationBlocks(IInformation block);

    /**
     * layoutIDにマッチした構造ブロックを検索する。
     * @param id    layoutID
     * @return 見つかった構造ブロック
     */
    IInformation findInformationLayoutID(String id);

 }
