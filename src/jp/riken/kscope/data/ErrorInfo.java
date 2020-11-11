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
 * Error line information class
 *
 * @author RIKEN
 */
public class ErrorInfo {
  /** Error line information */
  private CodeLine codeLine;
  /** Error message */
  private String message;

  /**
   * Constructor
   *
   * @param line Error line information
   * @param message Error message
   */
  public ErrorInfo(CodeLine line, String message) {
    this.codeLine = line;
    this.message = message;
  }

  /**
   * Constructor
   *
   * @param ex Exception information
   */
  public ErrorInfo(Exception ex) {
    this.codeLine = null;
    if (ex != null) {
      if (ex.getMessage() != null) {
        this.message = ex.getMessage();
      } else {
        this.message = ex.toString();
      }
    }
  }

  /**
   * Get error line information
   *
   * @return Error line information
   */
  public CodeLine getCodeLine() {
    return codeLine;
  }

  /**
   * Get error messages
   *
   * @return error message
   */
  public String getMessage() {
    return message;
  }
}
