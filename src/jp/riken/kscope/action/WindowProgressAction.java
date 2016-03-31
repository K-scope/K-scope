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

import jp.riken.kscope.dialog.ProgressDialog;
import jp.riken.kscope.service.AppController;

/**
 * プログレスダイアログの表示アクションクラス
 * @author RIKEN
 */
public class WindowProgressAction extends ActionBase {

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     */
    public WindowProgressAction(AppController controller) {
        super(controller);
    }

    /**
     * アクションが実行可能であるかチェックする.<br/>
     * アクションの実行前チェック、メニューのイネーブルの切替を行う。<br/>
     * @return		true=アクションが実行可能
     */
    @Override
    public boolean validateAction() {
        // スレッドタスクの実行状態をチェックする(true=スレッドが終了していない)
        return !this.controller.isThreadTaskDone();
    }

    /**
     * アクション発生イベント
     * @param event		イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {
        // メニューからのプログレスダイアログの表示の場合は、スレッド実行中のみ表示を行う。
        if (this.controller.isThreadTaskDone()) {
            // スレッド実行なし
            return;
        }

        // プログレスダイアログを表示する。
        showProgressDialog();
    }

    /**
     * プログレスダイアログを表示する。
     */
    public void showProgressDialog() {

        // プログレスダイアログを表示する。
        ProgressDialog dialog = this.controller.getMainframe().getDialogProgress();

        // スレッドタスクを設定する
        dialog.setThreadService(this.controller.getThreadFuture());

        // ダイアログ表示
        dialog.showDialog();
    }

    /**
     * プログレスダイアログを閉じるする。
     */
    public void closeProgressDialog() {

        // プログレスダイアログを表示する。
        ProgressDialog dialog = this.controller.getMainframe().getDialogProgress();

        // ダイアログを閉じる
        dialog.dispose();

    }

}


