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
 * Exception handling handler
 *
 * @author RIKEN
 */
public class ExceptionHandler {

  /**
   * Log exception message
   *
   * @param e exception
   * @throws ApplicationException Application error
   */
  public static void handleException(Exception e) throws ApplicationException {

    e.printStackTrace();

    // Log output
    Logger.error(e);

    // Wrap with application exception and throw
    throw new ApplicationException(e);
  }
}
