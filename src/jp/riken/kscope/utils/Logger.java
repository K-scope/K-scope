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

package jp.riken.kscope.utils;

import java.io.IOException;
import java.io.InputStream;
import java.util.logging.*;

/**
 * Log class
 *
 * @author hira
 */
public class Logger {

  /** Constructor */
  private Logger() {}

  // logger instance
  private static java.util.logging.Logger _logger = null;

  /** Turn off log output. */
  public static void logOff() {
    _logger = null;
    return;
  }

  /**
   * Set log settings from the log properties file.
   *
   * @param propFile Log property file
   * @param appName Application name
   */
  public static void configure(InputStream propFile, String appName) {
    configure(propFile, appName, false);
    return;
  }

  /**
   * Set log settings from the log properties file.
   *
   * @param propFile Log property file
   * @param appName Application name
   * @param debugMode Debug mode flag <br>
   *     Change the output level to the CONFIG output level.
   */
  public static void configure(InputStream propFile, String appName, boolean debugMode) {
    try {
      LogManager man = LogManager.getLogManager();
      man.readConfiguration(propFile);

      // String pattern = man.getProperty(FileHandler.class.getName() +
      // ".pattern");
      // System.out.println(pattern);

      // Generate Logger object
      _logger = java.util.logging.Logger.getLogger(appName);

      if (debugMode) {
        _logger.setLevel(Level.CONFIG);
      }

      // Get file handler
      FileHandler fileHandler = null;
      Handler handlers[] = _logger.getHandlers();
      if (handlers == null || handlers.length == 0) {
        handlers = _logger.getParent().getHandlers();
      }
      if (handlers != null && handlers.length > 0) {
        for (int i = 0; i < handlers.length; i++) {
          if (handlers[i] instanceof FileHandler) {
            fileHandler = (FileHandler) handlers[i];
            break;
          }
        }
      }
      if (fileHandler == null) {
        fileHandler = new FileHandler("kscope.log");
        fileHandler.setFormatter(new LogFormatter());
        _logger.addHandler(fileHandler);
      }

      // Create FileHandler
      // FileHandler fileHandler = new FileHandler(logfolder + "/" +
      // pattern);
      // _logger.addHandler(fileHandler);

      _logger.info("Start Logging");

    } catch (IOException e) {
      e.printStackTrace();
      _logger = null;
    }
  }

  /**
   * Set the debug mode flag. <br>
   * Change the output level to CONFIG.
   *
   * @param debugMode Debug mode flag
   */
  public static void setDebugMode(boolean debugMode) {
    if (_logger == null) return;
    if (debugMode) {
      _logger.setLevel(Level.CONFIG);
    }
    return;
  }

  /**
   * Output error level log.
   *
   * @param msg Error message
   */
  public static void error(String msg) {
    if (_logger == null) return;
    _logger.log(Level.SEVERE, msg);
  }

  /**
   * Output an exception stack trace.
   *
   * @param ex Error exception
   */
  public static void error(Exception ex) {
    if (_logger == null) return;
    if (ex == null) return;

    StringBuilder msg = new StringBuilder();
    msg.append(ex.toString() + "\n");
    StackTraceElement[] se = ex.getStackTrace();
    for (int i = 0; i < se.length; i++) {
      msg.append("\t" + "at " + se[i].toString() + "\n");
    }

    msg.append("\n");
    Throwable cause_ex = ex.getCause();
    if (cause_ex != null) {
      msg.append(cause_ex.toString() + "\n");
      StackTraceElement[] cause_se = cause_ex.getStackTrace();
      if (cause_se != null) {
        for (int i = 0; i < cause_se.length; i++) {
          msg.append("\t" + "at " + cause_se[i].toString() + "\n");
        }
      }
    }

    _logger.log(Level.SEVERE, msg.toString());
  }

  /**
   * Output warning level log.
   *
   * @param msg Log message
   */
  public static void warn(String msg) {
    if (_logger == null) return;
    _logger.log(Level.WARNING, msg);
  }

  /**
   * Output information level log.
   *
   * @param msg Log message
   */
  public static void info(String msg) {
    if (_logger == null) return;
    _logger.log(Level.INFO, msg);
  }

  /**
   * Output debug level log.
   *
   * @param msg Log message
   */
  public static void debug(String msg) {
    if (_logger == null) return;
    _logger.log(Level.CONFIG, msg);
  }
}
