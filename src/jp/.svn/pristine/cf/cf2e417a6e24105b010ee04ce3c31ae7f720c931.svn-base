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
package jp.riken.kscope;

import java.util.Observable;
import java.util.Observer;

import javax.swing.SwingUtilities;

/**
 * アプリケーションの進捗メッセージ、進捗状況等のグローバル設定クラス
 *
 * @author riken
 */
public class Application {

    /** 進捗情報 */
    public static StatusPrint status = new StatusPrint();

    /**
     * コンストラクタ
     */
    private Application() {
    }

    /**
     * ステータス情報のオブザーバーの追加を行う
     *
     * @param o
     *            オブザーバー
     */
    public static void addStatus(Observer o) {
        status.addObserver(o);
    }

    /**
     * 進捗情報クラス
     *
     * @author riken
     *
     */
    public static class StatusPrint extends Observable {
        /** メインメッセージ */
        private StringBuffer messageMain = new StringBuffer();
        /** ロケーションメッセージ */
        private StringBuffer messageLocation = new StringBuffer();
        /** ステータスメッセージ */
        private StringBuffer messageStatus = new StringBuffer();
        /** プログレスバー:最小値 */
        private Integer progressMin = null;
        /** プログレスバー:最大値 */
        private Integer progressMax = null;
        /** プログレスバー:値 */
        private Integer progressValue = null;
        /** プログレスバー:開始 */
        private boolean progressStart = false;

        /**
         * ステータスの変更を通知する
         */
        private void notifyStatus() {
            SwingUtilities.invokeLater(new Runnable() {
                @Override
                public void run() {
                    if (countObservers() > 0) {
                        setChanged();
                        notifyObservers();
                        clearChanged();
                    } else {
                        String msg = "main message=" + messageMain.toString() + "\n";
                        msg += "location message=" + messageLocation.toString() + "\n";
                        msg += "status message=" + messageStatus.toString() + "\n";
                        msg += "progress:value=" + progressValue + ", min=" + progressMin + ", max=" + progressMax;
                        msg += "progress:start=" + (progressStart ? "start" : "stop");
                        System.out.println(msg);
                    }
                }
            });
        }

        /**
         * メインメッセージを設定する
         *
         * @param message
         *            メインメッセージ
         */
        public void setMessageMain(String message) {
            this.messageMain = new StringBuffer();
            if (message != null) {
                this.messageMain.append(message);
            }
            notifyStatus();
        }

        /**
         * メインメッセージを取得する
         *
         * @return メインメッセージ
         */
        public String getMessageMain() {
            return this.messageMain.toString();
        }

        /**
         * ステータスメッセージを設定する
         *
         * @param message
         *            ステータスメッセージ
         */
        public void setMessageStatus(String message) {
            this.messageStatus = new StringBuffer();
            if (message != null) {
                this.messageStatus.append(message);
            }
            notifyStatus();
        }

        /**
         * ステータスメッセージを取得する
         *
         * @return ステータスメッセージ
         */
        public String getMessageStatus() {
        	try {
				if (this.messageStatus == null) return null;
				if (this.messageStatus.length() <= 0) return null;
				return this.messageStatus.toString();
			} catch (Exception e) {
				return null;
			}
        }

        /**
         * ロケーションメッセージを設定する
         *
         * @param message
         *            ロケーションメッセージ
         */
        public void setMessageLocation(String message) {
            this.messageLocation = new StringBuffer();
            if (message != null) {
                this.messageLocation.append(message);
            }
            notifyStatus();
        }

        /**
         * ロケーションメッセージを設定する
         *
         * @param row
         *            行番号
         * @param col
         *            列番号
         * @param word
         *            ロケーションメッセージ
         */
        public void setMessageLocation(int row, int col, String word) {
            this.messageLocation = new StringBuffer();
            this.messageLocation.append(row);
            this.messageLocation.append(" : ");
            this.messageLocation.append(col);
            if (word != null) {
                this.messageLocation.append("  [");
                this.messageLocation.append(word);
                this.messageLocation.append("]");
            }
            notifyStatus();
        }

        /**
         * ロケーションメッセージを取得する
         *
         * @return ロケーションメッセージ
         */
        public String getMessageLocation() {
            return this.messageLocation.toString();
        }

        /**
         * プログレスバーを設定する
         *
         * @param orient
         *            値
         * @param min
         *            最小値
         * @param max
         *            最大値
         */
        public void setProgress(int orient, int min, int max) {
            this.progressValue = orient;
            this.progressMin = min;
            this.progressMax = max;
            this.progressStart = true;
            notifyStatus();
        }

        /**
         * プログレスバーを設定する。
         *
         * @param min
         *            最小値
         * @param max
         *            最大値
         */
        public void setProgress(int min, int max) {
            this.progressValue = null;
            this.progressMin = min;
            this.progressMax = max;
            notifyStatus();
        }

        /**
         * プログレスバーを設定する。
         *
         * @param value
         *            値
         */
        public void setProgressValue(int value) {
            this.progressValue = value;
            this.progressStart = true;
            notifyStatus();
        }

        /**
         * プログレスバーの開始状態を設定する
         *
         * @param start
         *            true=開始
         */
        public void setProgressStart(boolean start) {
            this.progressStart = start;
            if (!start) {
                // プログレスバーの設定のクリア
                this.progressValue = null;
                this.progressMin = null;
                this.progressMax = null;
            }
            notifyStatus();
        }

        /**
         * プログレスバー:値を取得する.
         *
         * @return プログレスバー:値
         */
        public Integer getProgressValue() {
            return this.progressValue;
        }

        /**
         * プログレスバー:最小値を取得する.
         *
         * @return プログレスバー:最小値
         */
        public Integer getProgressMin() {
            return this.progressMin;
        }

        /**
         * プログレスバー:最大値を取得する.
         *
         * @return プログレスバー:最大値
         */
        public Integer getProgressMax() {
            return this.progressMax;
        }

        /**
         * プログレスバーの開始状態を取得する
         *
         * @return true=開始
         */
        public boolean isProgressStart() {
            return this.progressStart;
        }
    }
}
