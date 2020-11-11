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
 * XcodeML XML file parsing error
 *
 * @author RIKEN
 */
public class XcodeMLException extends Exception {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Creates XcodeMLException. */
  public XcodeMLException() {}

  /**
   * Creates XcodeMLException.
   *
   * @param msg the detail message.
   */
  public XcodeMLException(String msg) {
    super(msg);
  }

  /**
   * Creates XcodeMLException.
   *
   * @param cause the cause.
   */
  public XcodeMLException(Throwable cause) {
    super(cause);
  }

  /**
   * Creates XcodeMLException.
   *
   * @param msg the detail message.
   * @param cause the cause.
   */
  public XcodeMLException(String msg, Throwable cause) {
    super(msg, cause);
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
