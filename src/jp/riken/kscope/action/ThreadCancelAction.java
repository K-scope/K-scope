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

import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.FutureService;

/**
 * 実行中スレッドキャンセルアクション
 * @author riken
 */
public class ThreadCancelAction extends ActionBase {

    /**
     * コンストラクタ
     * @param controller	アプリケーションコントローラ
     */
    public ThreadCancelAction(AppController controller) {
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

        // 実行中のスレッドのタスク情報の取得
        FutureService<Integer> future = this.controller.getThreadFuture();
        if (future == null) return;
        if (future.isDone()) return;
        if (future.isCancelled()) return;

        // 実行中のスレッドをキャンセルする
        if (future != null) {
            future.cancel(true);
        }

    }

}
