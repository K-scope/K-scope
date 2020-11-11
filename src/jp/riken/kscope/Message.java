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
package jp.riken.kscope;

import java.util.Enumeration;

/** English and Japanese message classes. Singleton class */
public class Message {
  /** Property file */
  private static final String PROPERTIES_FILE = "jp.riken.kscope.message";
  /** Property resource */
  private java.util.ResourceBundle bundle = null;
  /** Message class instance */
  private static Message instance = new Message();

  private static boolean debug = (System.getenv("DEBUG") != null);
  private static boolean debug_l2 = false;
  private static boolean debug_l3 = false;

  /** Constructor */
  private Message() {
    if (debug) {
      debug_l2 = (System.getenv("DEBUG").equalsIgnoreCase("high"));
      debug_l3 = (System.getenv("DEBUG").equalsIgnoreCase("extreme"));
    }
    this.bundle = java.util.ResourceBundle.getBundle(PROPERTIES_FILE);
  }

  /**
   * Get a Message class instance.
   *
   * @return Message class instance
   */
  public static Message getInstance() {
    return instance;
  }

  /**
   * Create a resource from the properties file.
   *
   * @return property resource
   */
  private static java.util.ResourceBundle getBundle() {
    try {
      if (instance == null) instance = new Message();
      if (debug_l3) {
        System.out.println("ResourceBundle: ");
        for (Enumeration<String> e = instance.bundle.getKeys(); e.hasMoreElements(); ) {
          System.out.println(e.nextElement());
        }
      }
      return instance.bundle;
    } catch (Exception ex) {
      ex.printStackTrace();
    }
    return null;
  }

  /**
   * Get the key message.
   *
   * @param key key
   * @return message string
   */
  public static String getString(String key) {
    if (key == null || key.isEmpty()) return null;
    if (getBundle() == null) return null;
    try {
      String msg = getBundle().getString(key);
      return msg;
    } catch (Exception ex) {
      System.err.println("Error getting message for key " + key);
      StackTraceElement[] ste = ex.getStackTrace();
      int maxelements = 10;
      for (int i = 0; i < maxelements; i++) {
        System.err.println(ste[i].toString());
      }
      // ex.printStackTrace();
      return key;
    }
  }

  /**
   * Message from the key format string.
   *
   * @param key key
   * @param args Format parameters
   * @return message string
   */
  public static String getString(String key, Object... args) {
    if (key == null || key.isEmpty()) return null;
    if (getBundle() == null) return null;
    try {
      String format = getBundle().getString(key);
      String msg = String.format(format, (Object[]) args);
      return msg;
    } catch (Exception ex) {
      ex.printStackTrace();
      return key;
    }
  }

  /**
   * Get the key of the message.
   *
   * @param message message string
   * @return key
   */
  public static String getKey(String message) {
    if (message == null || message.isEmpty()) return null;
    if (getBundle() == null) return null;
    try {
      Enumeration<String> keys = getBundle().getKeys();
      while (keys.hasMoreElements()) {
        String key = keys.nextElement();
        String value = getBundle().getString(key);
        if (message.equals(value)) {
          return key;
        }
      }
    } catch (Exception ex) {
      ex.printStackTrace();
    }
    return null;
  }

  /**
   * Check if the message key exists.
   *
   * @param key key
   * @return true = key exists
   */
  public static boolean containsKey(String key) {
    if (key == null || key.isEmpty()) return false;
    if (getBundle() == null) return false;
    return getBundle().containsKey(key);
  }
}
