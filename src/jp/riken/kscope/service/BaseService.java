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

import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.ErrorInfo;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.exception.LanguageException;
import jp.riken.kscope.model.ErrorInfoModel;

/**
 * サービス基底クラス
 * @author RIKEN
 */
public abstract class BaseService {
    /** エラーメッセージモデル */
    private ErrorInfoModel errorInfoModel;

    /**
     * コンストラクタ
     */
    public BaseService() {
    }

    /**
     * コンストラクタ
     * @param   errorModel  エラーモデル
     */
    public BaseService(ErrorInfoModel errorModel) {
        this.errorInfoModel = errorModel;
    }

    /**
     * エラーメッセージモデルを設定する。
     * @param errorInfoModel		エラーメッセージモデル
     */
    public void setErrorInfoModel(ErrorInfoModel errorInfoModel) {
        this.errorInfoModel = errorInfoModel;
    }

    /**
     * エラーメッセージモデルを取得する
     * @return errorMessage		エラーメッセージモデル
     */
    public ErrorInfoModel getErrorInfoModel() {
        return errorInfoModel;
    }

    /**
     * エラーメッセージを設定する
     * @param errorMessage 		エラーメッセージ
     */
    public void addErrorInfo(String errorMessage) {
        this.errorInfoModel.addErrorInfo(errorMessage);
    }

    /**
     * エラーメッセージを設定する
     * @param ex 		エラーメッセージ
     */
    public void addErrorInfo(Exception ex) {
        String message = ex.getMessage();
        if (message == null) {
            message = ex.toString();
        }
        this.errorInfoModel.addErrorInfo(message);
    }

    /**
     * エラーメッセージを設定する
     * @param file 		エラーファイル
     * @param errorMessage 		エラーメッセージ
     */
    public void addErrorInfo(SourceFile file, String errorMessage) {
        this.errorInfoModel.addErrorInfo(file, errorMessage);
    }

    /**
     * エラーメッセージを設定する
     * @param file 		エラーファイル
     * @param errorMessage 		エラーメッセージ
     * @param lineno 		エラー行番号
     */
    public void addErrorInfo(SourceFile file, String errorMessage, int lineno) {
        this.errorInfoModel.addErrorInfo(file, errorMessage, lineno);
    }

    /**
     * エラーメッセージを設定する
     * @param line 		エラー行情報
     * @param errorMessage 		エラーメッセージ
     */
    public void addErrorInfo(CodeLine line, String errorMessage) {
        this.errorInfoModel.addErrorInfo(line, errorMessage);
    }

    /**
     * エラーメッセージを設定する
     * @param lang_ex		エラー情報
     */
    public void addErrorInfo(LanguageException lang_ex) {
        String error_message = lang_ex.getMessage();
        if (lang_ex.getCodeLine() != null) {
            addErrorInfo(lang_ex.getCodeLine(), error_message);
        }
        else if (lang_ex.getErrorFile() != null) {
            addErrorInfo(lang_ex.getErrorFile(), error_message);
        }
        else {
            addErrorInfo(error_message);
        }
    }

    /**
     * エラーメッセージを設定する
     * @param infos          エラー情報リスト
     */
    public void addErrorInfos(ErrorInfo[] infos) {
    	if (infos == null) return;
        this.errorInfoModel.addErrorInfos(infos);
    }
}
