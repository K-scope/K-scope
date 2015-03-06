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

import jp.riken.kscope.data.CodeLine;

/**
 * アプリケーション例外クラス GUIその他、アプリケーション全体の例外クラス アプリケーション例外は、例外発生箇所、又は呼出側にて例外処理,
 * 又はthrowsを行わなければならない。
 *
 * @author RIKEN
 *
 */
public class ApplicationException extends Exception {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;
    /** コード行情報 */
    private CodeLine m_errorCode;

    /**
     * コンストラクタ（エラーメッセージ）
     *
     * @param msg
     *            エラーメッセージ
     * @param line
     *            エラーコード行
     */
    public ApplicationException(String msg, CodeLine line) {
        super(msg);
        setCodeLine(line);
    }

    /**
     * コンストラクタ（エラーメッセージ）
     *
     * @param msg
     *            エラーメッセージ
     * @param line
     *            コード行文字列
     */
    public ApplicationException(String msg, String line) {
        super(msg);
        setCodeLine(line);
    }

    /**
     * コンストラクタ（例外）
     *
     * @param ex
     *            例外クラス
     * @param line
     *            エラーコード行
     */
    public ApplicationException(Exception ex, CodeLine line) {
        super(ex);
        setCodeLine(line);
    }

    /**
     * コンストラクタ（例外）
     *
     * @param ex
     *            例外クラス
     */
    public ApplicationException(Exception ex) {
        super(ex);
    }

    /**
     * コード行情報を設定する。
     *
     * @param line
     *            コード行情報
     */
    public void setCodeLine(CodeLine line) {
        this.m_errorCode = line;
    }

    /**
     * コード行文字列を設定する。
     *
     * @param line
     *            コード行文字列
     */
    public void setCodeLine(String line) {
        this.m_errorCode = new CodeLine(line);
    }

    /**
     * エラー発生コード行情報クラスを取得する。
     *
     * @return エラー発生コード行情報
     */
    public CodeLine getCodeLine() {
        return m_errorCode;
    }

    /**
     * エラー発生コード（文字列）を取得する。
     *
     * @return エラー発生コード（文字列）
     */
    public String getCodeInfo() {
        if (m_errorCode == null)
            return null;
        String info = m_errorCode.toString();
        info = info.replace('\n', ' ');
        return info;
    }

    /**
     * エラーメッセージを取得する。
     *
     * @return エラーメッセージ
     */
    @Override
    public String toString() {
        if (getCodeInfo() != null) {
            return super.toString() + "\n" + getCodeInfo();
        } else {
            return super.toString();
        }
    }

}
