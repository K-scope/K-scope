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
 * Service base class
 *
 * @author RIKEN
 */
public abstract class BaseService {
  /** Error message model */
  private ErrorInfoModel errorInfoModel;

  /** Constructor */
  public BaseService() {}

  /**
   * Constructor
   *
   * @param errorModel Error model
   */
  public BaseService(ErrorInfoModel errorModel) {
    this.errorInfoModel = errorModel;
  }

  /**
   * Set the error message model.
   *
   * @param errorInfoModel Error message model
   */
  public void setErrorInfoModel(ErrorInfoModel errorInfoModel) {
    this.errorInfoModel = errorInfoModel;
  }

  /**
   * Get the error message model
   *
   * @return errorMessage Error message model
   */
  public ErrorInfoModel getErrorInfoModel() {
    return errorInfoModel;
  }

  /**
   * Set error message
   *
   * @param errorMessage Error message
   */
  public void addErrorInfo(String errorMessage) {
    this.errorInfoModel.addErrorInfo(errorMessage);
  }

  /**
   * Set error message
   *
   * @param ex error message
   */
  public void addErrorInfo(Exception ex) {
    String message = ex.getMessage();
    if (message == null) {
      message = ex.toString();
    }
    this.errorInfoModel.addErrorInfo(message);
  }

  /**
   * Set error message
   *
   * @param file Error file
   * @param errorMessage Error message
   */
  public void addErrorInfo(SourceFile file, String errorMessage) {
    this.errorInfoModel.addErrorInfo(file, errorMessage);
  }

  /**
   * Set error message
   *
   * @param file Error file
   * @param errorMessage Error message
   * @param lineno Error line number
   */
  public void addErrorInfo(SourceFile file, String errorMessage, int lineno) {
    this.errorInfoModel.addErrorInfo(file, errorMessage, lineno);
  }

  /**
   * Set error message
   *
   * @param line Error line information
   * @param errorMessage Error message
   */
  public void addErrorInfo(CodeLine line, String errorMessage) {
    this.errorInfoModel.addErrorInfo(line, errorMessage);
  }

  /**
   * Set error message
   *
   * @param lang_ex Error information
   */
  public void addErrorInfo(LanguageException lang_ex) {
    String error_message = lang_ex.getMessage();
    if (lang_ex.getCodeLine() != null) {
      addErrorInfo(lang_ex.getCodeLine(), error_message);
    } else if (lang_ex.getErrorFile() != null) {
      addErrorInfo(lang_ex.getErrorFile(), error_message);
    } else {
      addErrorInfo(error_message);
    }
  }

  /**
   * Set error message
   *
   * @param infos Error information list
   */
  public void addErrorInfos(ErrorInfo[] infos) {
    if (infos == null) return;
    this.errorInfoModel.addErrorInfos(infos);
  }
}
