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
 * Database exception class Exception class of database part (jp.go.riken.ppa.language) For database
 * exceptions, it is not necessary for the exception occurrence location or caller to handle the
 * exception, but the highest-level caller must catch the exception and handle the exception
 * appropriately.
 *
 * @author hira
 */
public class LanguageException extends RuntimeException {

  /** Serial number */
  private static final long serialVersionUID = 1L;
  /** Code line information */
  private CodeLine m_errorCode;
  /** Error file */
  private SourceFile errorFile;

  /**
   * Constructor (error message)
   *
   * @param msg error message
   */
  public LanguageException(String msg) {
    super(msg);
  }

  /**
   * Constructor (error message)
   *
   * @param msg Error message
   * @param line Error code line
   */
  public LanguageException(String msg, CodeLine line) {
    super(msg);
    setCodeLine(line);
  }

  /**
   * Constructor (error message)
   *
   * @param msg Error message
   * @param line Code line string
   */
  public LanguageException(String msg, String line) {
    super(msg);
    setCodeLine(line);
  }

  /**
   * Constructor (exception)
   *
   * @param ex Exception class
   * @param line Error code line
   */
  public LanguageException(Exception ex, CodeLine line) {
    super(ex);
    setCodeLine(line);
  }

  /**
   * Constructor (exception)
   *
   * @param ex exception class
   * @param file Error file
   */
  public LanguageException(Exception ex, SourceFile file) {
    super(ex);
    this.setErrorFile(file);
  }

  /**
   * Constructor (exception)
   *
   * @param ex exception class
   * @param file Error file
   */
  public LanguageException(XMLStreamException ex, SourceFile file) {
    super(ex);
    this.setErrorFile(file);
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
    if (m_errorCode == null) return null;
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

  /**
   * Get the error file
   *
   * @return Error occurrence file
   */
  public SourceFile getErrorFile() {
    return errorFile;
  }

  /**
   * Set the error occurrence file
   *
   * @param errorFile Error file
   */
  public void setErrorFile(SourceFile errorFile) {
    this.errorFile = errorFile;
  }

  /**
   * Get error messages
   *
   * @return error message
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
