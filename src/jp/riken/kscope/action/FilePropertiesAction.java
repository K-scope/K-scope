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
import java.io.File;

import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.model.PropertiesTableModel;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.FileService;

/**
 * ファイルプロパティアクションクラス
 * @author RIKEN
 */
public class FilePropertiesAction extends ActionBase {

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     */
    public FilePropertiesAction(AppController controller) {
        super(controller);
    }

    /**
     * アクション発生イベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {

        // プロパティの設定を行う。
        setProperties();

        // プロパティタブをアクティブにする
        this.controller.getMainframe().getPanelAnalysisView().setSelectedPanel(ANALYSIS_PANEL.PROPARTIES);
    }

    /**
     * プロパティの設定を行う。
     */
    public void setProperties() {

        // 選択ファイルの取得を行う
        File[] files = this.controller.getMainframe().getPanelExplorerView().getSelectedNodeFiles();
        if (files == null || files.length <= 0) return;

        // ファイルのプロパティ設定モデルの取得する
        PropertiesTableModel model = this.controller.getPropertiesTableModel();

        // ファイルプロパティの取得を行う
        FileService service = new FileService();
        service.setFileProperties(files[0], model);

    }

}
