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
package jp.riken.kscope.data;

/**
 * エラー行情報クラス
 * @author RIKEN
 */
public class ErrorInfo {
    /** エラー行情報 */
    private CodeLine codeLine;
    /** エラーメッセージ */
    private String message;

    /**
     * コンストラクタ
     * @param line			エラー行情報
     * @param message		エラーメッセージ
     */
    public ErrorInfo(CodeLine line, String message) {
        this.codeLine = line;
        this.message = message;
    }

    /**
     * コンストラクタ
     * @param ex			例外情報
     */
    public ErrorInfo(Exception ex) {
        this.codeLine = null;
        if (ex != null) {
	        if (ex.getMessage() != null) {
	        	this.message = ex.getMessage();
	        }
	        else {
	        	this.message = ex.toString();
	        }
        }
    }


    /**
     * エラー行情報を取得する
     * @return		エラー行情報
     */
    public CodeLine getCodeLine() {
        return codeLine;
    }

    /**
     * エラーメッセージを取得する
     * @return		エラーメッセージ
     */
    public String getMessage() {
        return message;
    }
}
