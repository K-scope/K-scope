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
package jp.riken.kscope.common;

/**
 * Constant class
 *
 * @author RIKEN
 */
public class Constant {

  /** Dialog OK, YES Return value */
  public static final int OK_DIALOG = 0;
  /** Dialog NO return value */
  public static final int NO_DIALOG = 1;
  /** Dialog CANCEL return value */
  public static final int CANCEL_DIALOG = 2;
  /** Dialog CANCEL return value */
  public static final int CLOSE_DIALOG = -1;
  /** Dialog DELETE return value */
  public static final int DELETE_DIALOG = 3;
  /** Dialog Return value to next: Display the next dialog */
  public static final int NEXT_DIALOG = 4;

  /** Thread exit code: Normal termination */
  public static final int SUCCESS_RESULT = 0;
  /** Thread exit code: Abnormal termination */
  public static final int ERROR_RESULT = 1;
  /** Thread exit code: Cancel end */
  public static final int CANCEL_RESULT = 2;

  /** Thread end notification property name */
  public static final String PROPERTYNAME_THREADDONE = "thread_done";
  /** Mouse scroll amount */
  public static final int VERTICALSCROLL_INCREMENT = 25;
}
