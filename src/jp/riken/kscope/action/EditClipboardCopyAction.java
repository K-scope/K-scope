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

import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.service.AppController;


/**
 * クリップボードコピーアクションクラス
 * @author RIKEN
 *
 */
public class EditClipboardCopyAction extends ActionBase {

    /** クリップボードコピー先ビュー */
    private FRAME_VIEW view;

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     * @param view 			クリップボードコピービュー
     */
    public EditClipboardCopyAction(AppController controller, FRAME_VIEW view) {
        super(controller);
        this.view = view;
    }

    /**
     * クリップボードコピーイベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {

        if (view == FRAME_VIEW.SOURCE_VIEW) {
            // ソースビューからクリップボードへコピーする
            this.controller.getMainframe().getPanelSourceView().copyClipboard();
        }
        else if (view == FRAME_VIEW.ANALYSIS_VIEW) {
            // 分析ビューからクリップボードへコピーする
            this.controller.getMainframe().getPanelAnalysisView().copyClipboard();
        }
        return;
    }

}

