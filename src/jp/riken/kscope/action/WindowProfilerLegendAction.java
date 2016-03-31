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

import java.awt.Frame;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;

import javax.swing.JComponent;

import jp.riken.kscope.dialog.ProfilerLegendDialog;
import jp.riken.kscope.properties.ProfilerProperties;
import jp.riken.kscope.service.AppController;

/**
 * プロファイラ凡例の表示アクションクラス
 * @author RIKEN
 */
public class WindowProfilerLegendAction extends ActionBase {

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     */
    public WindowProfilerLegendAction(AppController controller) {
        super(controller);
    }

    /**
     * アクションが実行可能であるかチェックする.<br/>
     * アクションの実行前チェック、メニューのイネーブルの切替を行う。<br/>
     * @return		true=アクションが実行可能
     */
    @Override
    public boolean validateAction() {
        return true;
    }

    /**
     * アクション発生イベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {

        // 親Frameの取得を行う。
        Frame frame = getWindowAncestor( event );

        // プロファイラ凡例ダイアログを表示する。
    	showLegendWindow(frame);
    }

    /**
     * プロファイラ凡例ダイアログを表示する。
     * @param    frame    親フレーム
     */
    public void showLegendWindow(Frame frame) {

        // プロファイラプロパティ
        ProfilerProperties properties = this.controller.getPropertiesProfiler();

        // プロファイラ凡例ダイアログを表示する。
    	ProfilerLegendDialog dialog = this.controller.getMainframe().getDialogProfilerLegend();
    	if (dialog == null) {
    		dialog = new ProfilerLegendDialog(frame, false);
    		// 表示位置を設定する
    		Rectangle dialogRect = dialog.getBounds();
    		JComponent view = this.controller.getMainframe().getPanelSourceView();
        	Point location = view.getLocationOnScreen();
        	int viewWidth = view.getWidth();
        	int viewHeight = view.getHeight();
    		dialog.setLocation(location.x + viewWidth - dialogRect.width,
    							location.y + viewHeight - dialogRect.height);
    		this.controller.getMainframe().setDialogProfilerLegend(dialog);
    	}
    	dialog.setProperties(properties);

        // ダイアログ表示
        dialog.showDialog();
    }

}


