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

import javax.xml.stream.XMLStreamException;

import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.utils.Logger;

/**
 * データベース例外クラス データベース部(jp.go.riken.ppa.language)の例外クラス
 * データベース例外は、例外発生箇所、又は呼出側にて例外処理を行う必要はないが 最上位の呼出側では例外発生をcatchし、適切な例外処理を行う必要がある。
 *
 * @author hira
 *
 */
public class LanguageException extends RuntimeException {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;
    /** コード行情報 */
    private CodeLine m_errorCode;
    /** エラーファイル */
    private SourceFile errorFile;

    /**
     * コンストラクタ（エラーメッセージ）
     * @param msg        エラーメッセージ
     */
    public LanguageException(String msg) {
        super(msg);
    }

    /**
     * コンストラクタ（エラーメッセージ）
     *
     * @param msg
     *            エラーメッセージ
     * @param line
     *            エラーコード行
     */
    public LanguageException(String msg, CodeLine line) {
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
    public LanguageException(String msg, String line) {
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
    public LanguageException(Exception ex, CodeLine line) {
        super(ex);
        setCodeLine(line);
    }

    /**
     * コンストラクタ（例外）
     * @param ex		例外クラス
     * @param file		エラーファイル
     */
    public LanguageException(Exception ex, SourceFile file) {
        super(ex);
        this.setErrorFile(file);
    }

    /**
     * コンストラクタ（例外）
     * @param ex		例外クラス
     * @param file		エラーファイル
     */
    public LanguageException(XMLStreamException ex, SourceFile file) {
        super(ex);
        this.setErrorFile(file);
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
        return super.toString() + "\n" + getCodeInfo();
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

    /**
     * エラー発生ファイルを取得する
     * @return		エラー発生ファイル
     */
    public SourceFile getErrorFile() {
        return errorFile;
    }

    /**
     * エラー発生ファイルを設定する
     * @param errorFile		エラー発生ファイル
     */
    public void setErrorFile(SourceFile errorFile) {
        this.errorFile = errorFile;
    }

    /**
     * エラーメッセージを取得する
     * @return		エラーメッセージ
     */
    @Override
    public String getMessage() {
         Throwable cause = getCause();
         if (cause != null) {
             return cause.getMessage();
         }
        return super.getMessage();
    }



}
