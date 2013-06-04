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

import jp.riken.kscope.common.ANALYSIS_PANEL;


/**
 * タブコンポーネントインターフェイス.<br/>
 * タブパイン、子要素のテキストパイン等すべてのタブを構成するコンポーネントのインターフェイス
 * @author riken
 */
public interface ITabComponent {

    /**
     * 親コンポーネントを取得する.<br/>
     * this.parentCompornentを取得する
     * @return	親コンポーネント
     */
    public ITabComponent getParentComponent();

    /**
     * 親コンポーネントを設定する.<br/>
     * this.parentCompornentに設定する。
     * @param component		親コンポーネント
     */
    public void setParentComponent(ITabComponent component);

    /**
     * フォーカスリスナを設定する
     * @param listener		フォーカスリスナ
     */
    public void addTabFocusListener(TabFocusListener listener);

    /**
     * タブを閉じる
     */
    public void closeTabComponent();

}


