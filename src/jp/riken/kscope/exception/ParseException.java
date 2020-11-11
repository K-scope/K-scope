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
import jp.riken.kscope.utils.Logger;

/**
 * Parsing exception class Exception class of the parsing part (jp.go.riken.ppa.parse) Parsing
 * exceptions are handled by the exception occurrence location or the caller. Or throws must be
 * done.
 *
 * @author RIKEN
 */
public class ParseException extends Exception {

  /** Serial number */
  private static final long serialVersionUID = 1L;
  /** Code line information */
  private CodeLine m_errorCode;

  /**
   * Constructor (error message)
   *
   * @param msg Error message
   * @param line Error code line
   */
  public ParseException(String msg, CodeLine line) {
    super(msg);
    setCodeLine(line);
  }

  /**
   * Constructor (error message)
   *
   * @param msg Error message
   * @param line Code line string
   */
  public ParseException(String msg, String line) {
    super(msg);
    setCodeLine(line);
  }

  /**
   * Constructor (exception)
   *
   * @param ex Exception class
   * @param line Error code line
   */
  public ParseException(Exception ex, CodeLine line) {
    super(ex);
    setCodeLine(line);
  }

  /**
   * Constructor (exception)
   *
   * @param ex Exception class
   */
  public ParseException(Exception ex) {
    super(ex);
  }

  /**
   * Constructor (exception)
   *
   * @param ex Exception class
   * @param line Code line string
   * @param lineno Code line number
   */
  public ParseException(Exception ex, String line, int lineno) {
    super(ex);
    setCodeLine(new CodeLine(line, lineno));
  }

  /**
   * Set code line information.
   *
   * @param line Code line information
   */
  public void setCodeLine(CodeLine line) {
    this.m_errorCode = line;
  }

  /**
   * Set the code line string.
   *
   * @param line Code line string
   */
  public void setCodeLine(String line) {
    this.m_errorCode = new CodeLine(line);
  }

  /**
   * Get the error occurrence code line information class.
   *
   * @return Error occurrence code line information
   */
  public CodeLine getCodeLine() {
    return m_errorCode;
  }

  /**
   * Get the error occurrence code (character string).
   *
   * @return Error occurrence code (character string)
   */
  public String getCodeInfo() {
    String info = m_errorCode.toString();
    info = info.replace('\n', ' ');
    return info;
  }

  /**
   * Get error messages.
   *
   * @return error message
   */
  @Override
  public String toString() {
    return super.toString() + "\n" + getCodeInfo();
  }

  /** Output the stack trace of the error occurrence. Outputs standard error and log. */
  @Override
  public void printStackTrace() {
    // Log output
    Logger.error(this);

    // Standard output
    super.printStackTrace();
  }
}
