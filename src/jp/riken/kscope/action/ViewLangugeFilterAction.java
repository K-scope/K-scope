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
package jp.riken.kscope.action;

import java.awt.event.ActionEvent;

import javax.swing.JCheckBoxMenuItem;

import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.common.FILTER_TYPE;
import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.service.AppController;

/**
 * 構造ツリーフィルタアクションクラス
 * @author RIKEN
 */
public class ViewLangugeFilterAction extends ActionBase {

    /** フィルタタイプ */
    private FILTER_TYPE filter;

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     * @param filter     フィルタタイプ
     */
    public ViewLangugeFilterAction(AppController controller, FILTER_TYPE filter) {
        super(controller);
        this.filter = filter;
    }


    /**
     * アクション発生イベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {

        boolean checked = false;
        if (event.getSource() instanceof JCheckBoxMenuItem) {
            // チェックボックスメニュー
            JCheckBoxMenuItem item = (JCheckBoxMenuItem)event.getSource();
            checked = item.isSelected();
        }

        // 構造ツリーモデル
        if (this.filter != FILTER_TYPE.DEFAULT) {
            if (checked) {
                // 追加
                this.controller.addListLanguageFilter(this.filter);
            }
            else {
                // 削除
                this.controller.removeListLanguageFilter(this.filter);
            }
        }
        else {
            // 構造ツリーフィルタデフォルト設定
            FILTER_TYPE[] filters = KscopeProperties.LANGUGE_DEFAULTFILTERS;
            this.controller.setListLanguageFilter(filters);
        }

        // 構造ツリーパネルをアクティブにする。
        if (this.controller.getMainframe().getPanelExplorerView().getSelectedEnumPanel() != EXPLORE_PANEL.LANGUAGE) {
            this.controller.getMainframe().getPanelExplorerView().setSelectedPanel(EXPLORE_PANEL.LANGUAGE);
        }
    }

    /**
     * 構造ツリーフィルタタイプを取得する
     * @return		構造ツリーフィルタタイプ
     */
    public FILTER_TYPE getFilter() {
        return filter;
    }

    /**
     * 構造ツリーフィルタタイプを設定する.
     * @param filter		構造ツリーフィルタタイプ
     */
    public void setFilter(FILTER_TYPE filter) {
        this.filter = filter;
    }
}
