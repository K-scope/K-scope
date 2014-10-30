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

package jp.riken.kscope.menu;

import java.awt.event.ActionListener;
import java.util.EventListener;


/**
 * ツリーポップアップメニューインターフェイス
 * @author RIKEN
 */
public interface ITreePopupMenu {

    /**
     * すべて収納アクションリスナを取得する
     * @return		すべて収納アクションリスナ
     */
    public ActionListener getActionTreeCollapseAll();

    /**
     * すべて展開アクションリスナを取得する
     * @return		すべて展開アクションリスナ
     */
    public ActionListener getActionTreeExpandAll();

    /**
     * 選択展開アクションリスナを取得する
     * @return		選択展開アクションリスナ
     */
    public ActionListener getActionTreeExpandSelect();

    /**
     * ファイルを開くリスナを取得する。
     * @return		ファイルを開くリスナ
     */
    public EventListener getActionOpenFile();

    /**
     * エクスプローラエクスポートアクションを取得する
     * @return		エクスプローラエクスポートアクション
     */
    public ActionListener getActionExportExplore();

}
