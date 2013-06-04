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

package jp.riken.kscope.utils;

import java.io.IOException;
import java.io.InputStream;
import java.util.logging.*;

/**
 * ログクラス
 *
 * @author hira
 *
 */
public class Logger {

    /**
     * コンストラクタ
     */
    private Logger() {
    }

    // logger instance
    private static java.util.logging.Logger _logger = null;

    /**
     * ログ出力をOFFにする。
     */
    public static void logOff() {
        _logger = null;
        return;
    }


    /**
     * ログ設定をログプロパティファイルから設定する。
     *
     * @param propFile
     *            ログプロパティファイル
     * @param appName
     *            アプリケーション名
     */
    public static void configure(InputStream propFile, String appName) {
        configure(propFile, appName, false);
        return;
    }

    /**
     * ログ設定をログプロパティファイルから設定する。
     *
     * @param propFile
     *            ログプロパティファイル
     * @param appName
     *            アプリケーション名
     * @param debugMode
     *            デバッグモードフラグ<br/>
     *            出力レベルをCONFIG出力レベルに変更する。
     */
    public static void configure(InputStream propFile, String appName, boolean debugMode) {
        try {
            LogManager man = LogManager.getLogManager();
            man.readConfiguration(propFile);

            // String pattern = man.getProperty(FileHandler.class.getName() +
            // ".pattern");
            // System.out.println(pattern);

            // Loggerオブジェクトの生成
            _logger = java.util.logging.Logger.getLogger(appName);

            if (debugMode) {
                _logger.setLevel(Level.CONFIG);
            }

            // ファイルハンドラの取得
            FileHandler fileHandler = null;
            Handler handlers[] = _logger.getHandlers();
            if (handlers == null || handlers.length == 0) {
                handlers = _logger.getParent().getHandlers();
            }
            if (handlers != null && handlers.length > 0) {
                for (int i = 0; i < handlers.length; i++) {
                    if (handlers[i] instanceof FileHandler) {
                        fileHandler = (FileHandler) handlers[i];
                        break;
                    }
                }
            }
            if (fileHandler == null) {
                fileHandler = new FileHandler("kscope.log");
                fileHandler.setFormatter(new LogFormatter());
                _logger.addHandler(fileHandler);
            }

            // FileHandler作成
            // FileHandler fileHandler = new FileHandler(logfolder + "/" +
            // pattern);
            // _logger.addHandler(fileHandler);

            _logger.info("Start Logging");

        } catch (IOException e) {
            e.printStackTrace();
            _logger = null;
        }
    }

    /**
     * デバッグモードフラグを設定する。<br/>
     * 出力レベルをCONFIGに変更する。
     *
     * @param debugMode
     *            デバッグモードフラグ
     */
    public static void setDebugMode(boolean debugMode) {
        if (_logger == null)
            return;
        if (debugMode) {
            _logger.setLevel(Level.CONFIG);
        }
        return;
    }

    /**
     * エラーレベルログを出力する。
     *
     * @param msg		エラーメッセージ
     */
    public static void error(String msg) {
        if (_logger == null)
            return;
        _logger.log(Level.SEVERE, msg);
    }

    /**
     * 例外スタックトレースを出力する。
     *
     * @param ex			エラー例外
     */
    public static void error(Exception ex) {
        if (_logger == null)
            return;
        if (ex == null)
            return;

        StringBuilder msg = new StringBuilder();
        msg.append(ex.toString() + "\n");
        StackTraceElement[] se = ex.getStackTrace();
        for (int i = 0; i < se.length; i++) {
            msg.append("\t" + "at " + se[i].toString() + "\n");
        }

        msg.append("\n");
        Throwable cause_ex = ex.getCause();
        if (cause_ex != null) {
            msg.append(cause_ex.toString() + "\n");
            StackTraceElement[] cause_se = cause_ex.getStackTrace();
            if (cause_se != null) {
                for (int i = 0; i < cause_se.length; i++) {
                    msg.append("\t" + "at " + cause_se[i].toString() + "\n");
                }
            }
        }

        _logger.log(Level.SEVERE, msg.toString());
    }

    /**
     * 警告レベルログを出力する。
     *
     * @param msg
     *            ログメッセージ
     */
    public static void warn(String msg) {
        if (_logger == null)
            return;
        _logger.log(Level.WARNING, msg);
    }

    /**
     * 情報レベルログを出力する。
     *
     * @param msg
     *            ログメッセージ
     */
    public static void info(String msg) {
        if (_logger == null)
            return;
        _logger.log(Level.INFO, msg);
    }

    /**
     * デバッグレベルログを出力する。
     *
     * @param msg
     *            ログメッセージ
     */
    public static void debug(String msg) {
        if (_logger == null)
            return;
        _logger.log(Level.CONFIG, msg);
    }

}
