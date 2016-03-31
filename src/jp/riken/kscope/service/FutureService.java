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
package jp.riken.kscope.service;

import java.util.concurrent.Callable;
import java.util.concurrent.FutureTask;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.lang.Integer;

import jp.riken.kscope.common.Constant;

/**
 * スレッドタスクサービスクラス
 * @author RIKEN
 * @param <Integer>		スレッド終了コード
 */
@SuppressWarnings("hiding")
public class FutureService<Integer> extends FutureTask<Integer> {

    /** スレッド終了通知 */
    private PropertyChangeSupport threadEvent;
    /** スレッド終了メッセージ */
    private String message;

    /**
     * コンストラクタ
     * @param callable		スレッド呼出クラス
     */
    public FutureService(Callable<Integer> callable) {
        super(callable);
    }

    /**
     * コンストラクタ
     * @param runnable		実行スレッド
     * @param result		パラメータ
     */
    public FutureService(Runnable runnable, Integer result) {
        super(runnable, result);
    }


    /**
     * コンストラクタ
     * @param runnable		実行スレッド
     */
    public FutureService(Runnable runnable) {
        super(runnable, null);
    }

    /**
     * スレッド終了通知リスナの登録を行う
     * @param listener		プロパティ変更リスナ
     */
    public void addPropertyChangeListener(PropertyChangeListener listener) {
        if (threadEvent == null) {
            threadEvent = new PropertyChangeSupport(this);
        }
        threadEvent.addPropertyChangeListener(listener);
    }

    @Override
    public boolean cancel(boolean mayInterruptIfRunning) {
        return super.cancel(mayInterruptIfRunning);
    }

    /**
     * スレッド実行終了
     */
    @Override
    @SuppressWarnings("unchecked")
    protected void done() {
        super.done();

        Integer result = null;
        if (this.isCancelled()) {
            result = (Integer) new java.lang.Integer(Constant.CANCEL_RESULT);
        }
        else if (this.isDone()) {
            try {
                result = this.get();
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }

        // PropertyChangeSupportを利用してスレッドの終了を通知する
        if (threadEvent != null) {
            this.threadEvent.firePropertyChange(Constant.PROPERTYNAME_THREADDONE, null, result);
        }
    }

	/**
	 * スレッド終了メッセージを取得する
	 * @return スレッド終了メッセージ
	 */
	public String getMessage() {
		return message;
	}

	/**
	 * スレッド終了メッセージを設定する.
	 * @param message スレッド終了メッセージ
	 */
	public void setMessage(String message) {
		this.message = message;
	}
}
