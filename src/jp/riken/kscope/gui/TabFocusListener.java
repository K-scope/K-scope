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

import java.awt.Component;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

/**
 * フォーカスリスナ.<br/>
 * 現在フォーカスのあるコンポーネントを取得する。
 * @author riken
 *
 */
public class TabFocusListener implements FocusListener {

    /** 最終フォーカスコンポーネント */
    private ITabComponent lastTabComponent = null;

    /**
     * フォーカス取得イベント
     * @param event		イベント情報
     */
    @Override
    public void focusGained(FocusEvent event) {
        Component forcus = event.getComponent();
        if (forcus instanceof ITabComponent) {
            this.lastTabComponent = (ITabComponent)forcus;
        }
    }

    /**
     * フォーカス喪失イベント
     * @param event		イベント情報
     */
    @Override
    public void focusLost(FocusEvent event) {}

    /**
     * 最終フォーカスコンポーネントを取得する。
     * @return		最終フォーカスコンポーネント
     */
    public ITabComponent getLastTabComponent() {
        return this.lastTabComponent;
    }

}
