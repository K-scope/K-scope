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
 * Property value class
 *
 * @author RIKEN
 */
public class PropertyValue {

  /** Property key */
  private String key;
  /** Property name */
  private String name;
  /** Property type name */
  private String type;
  /** Property value */
  private Object value;
  /** Message */
  private String message;
  /** Options */
  private String option;

  /**
   * Constructor
   *
   * @param key Property key
   * @param name Property name
   * @param type Property type name
   * @param value Property value
   * @param message message
   */
  public PropertyValue(String key, String name, String type, Object value, String message) {
    super();
    this.key = key;
    this.name = name;
    this.type = type;
    this.value = value;
    this.message = message;
  }

  /**
   * Constructor
   *
   * @param key Property key
   * @param name Property name
   * @param type Property type name
   * @param value Property value
   * @param message message
   * @param option option
   */
  public PropertyValue(
      String key, String name, String type, Object value, String message, String option) {
    this(key, name, type, value, message);
    this.setOption(option);
  }

  /**
   * Get the property name.
   *
   * @return property name
   */
  public String getName() {
    return name;
  }

  /**
   * Set the property name.
   *
   * @param name Property name
   */
  public void setName(String name) {
    this.name = name;
  }

  /**
   * Get the property type name
   *
   * @return property type name
   */
  public String getType() {
    return type;
  }

  /**
   * Set the property type name.
   *
   * @param type Property type name
   */
  public void setType(String type) {
    this.type = type;
  }

  /**
   * Get the property value.
   *
   * @return property value
   */
  public Object getValue() {
    return value;
  }

  /**
   * Set property value
   *
   * @param value Property value
   */
  public void setValue(Object value) {
    this.value = value;
  }

  /**
   * Get the property key.
   *
   * @return property key
   */
  public String getKey() {
    return key;
  }

  /**
   * Set the property key.
   *
   * @param key Property key
   */
  public void setKey(String key) {
    this.key = key;
  }

  /**
   * Get a message
   *
   * @return message
   */
  public String getMessage() {
    return message;
  }

  /**
   * Set a message
   *
   * @param message message
   */
  public void setMessage(String message) {
    this.message = message;
  }

  /**
   * Get options
   *
   * @return option
   */
  public String getOption() {
    return option;
  }

  /**
   * Set options
   *
   * @param option option
   */
  public void setOption(String option) {
    this.option = option;
  }
}
