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
package jp.riken.kscope.exception;

import jp.riken.kscope.utils.Logger;

/**
 * XcodeMLのXMLファイルの解析エラー
 * @author RIKEN
 */
public class XcodeMLException extends Exception {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /**
     * Creates XcodeMLException.
     */
    public XcodeMLException() {
    }

    /**
     * Creates XcodeMLException.
     *
     * @param msg
     *            the detail message.
     */
    public XcodeMLException(String msg) {
        super(msg);
    }

    /**
     * Creates XcodeMLException.
     *
     * @param cause
     *            the cause.
     */
    public XcodeMLException(Throwable cause) {
        super(cause);
    }

    /**
     * Creates XcodeMLException.
     *
     * @param msg
     *            the detail message.
     * @param cause
     *            the cause.
     */
    public XcodeMLException(String msg, Throwable cause) {
        super(msg, cause);
    }

    /**
     * エラー発生のスタックトレースを出力する。 標準エラー出力とログ出力を行う。
     */
    @Override
    public void printStackTrace() {
        // ログ出力
        Logger.error(this);

        // 標準出力
        super.printStackTrace();
    }
}
