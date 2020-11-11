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
 * Project property setting value class
 *
 * @author RIKEN
 */
public class ProjectPropertyValue {
  private String type;
  private String key;
  private String value;
  private String name;
  private String message;
  private String commandline_option;
  private int order;

  /**
   * Constructor
   *
   * @param type type
   * @param key key
   * @param value value
   * @param name name
   * @param message message
   */
  public ProjectPropertyValue(
      String key, String type, String name, String value, String message, String CLO, int order) {
    this.type = type;
    this.key = key;
    this.value = value;
    this.name = name;
    this.message = message;
    this.commandline_option = CLO;
    this.order = order;
  }

  public void setProperty(String k, String v, String co, int order) {
    this.key = k;
    this.value = v;
    this.commandline_option = co;
    this.order = order;
  }

  /**
   * Get the type.
   *
   * @return type
   */
  public String getType() {
    return type;
  }

  /**
   * Set the type.
   *
   * @param type type
   */
  public void setType(String type) {
    this.type = type;
  }

  /**
   * Get the key.
   *
   * @return key
   */
  public String getKey() {
    return key;
  }

  /**
   * Set the key.
   *
   * @param key key
   */
  public void setKey(String key) {
    this.key = key;
  }

  /**
   * Get the value.
   *
   * @return value
   */
  public String getValue() {
    return value;
  }

  /**
   * Set the value.
   *
   * @param value value
   */
  public void setValue(String value) {
    this.value = value;
  }

  /**
   * Get the name.
   *
   * @return name
   */
  public String getName() {
    return name;
  }

  /**
   * Set the name.
   *
   * @param name name
   */
  public void setName(String name) {
    this.name = name;
  }

  /**
   * Get the message.
   *
   * @return message
   */
  public String getMessage() {
    return message;
  }

  /**
   * Set the message.
   *
   * @param message message
   */
  public void setMessage(String message) {
    this.message = message;
  }

  public String getCommandlineOption() {
    return this.commandline_option;
  }

  public int getOrder() {
    return this.order;
  }

  public String toString() {
    return "K:"
        + this.key
        + ", T:"
        + this.type
        + ", N:"
        + this.name
        + ", V:"
        + this.value
        + ", M:"
        + this.message
        + ", CL:"
        + this.commandline_option
        + ", O:"
        + this.order;
  }
}
