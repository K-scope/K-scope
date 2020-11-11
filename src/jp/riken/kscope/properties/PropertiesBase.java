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
package jp.riken.kscope.properties;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.Properties;

/**
 * Property base class
 *
 * @author RIKEN
 */
public abstract class PropertiesBase extends Properties {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Objects that support property change notifications */
  protected PropertyChangeSupport changes;

  /** Constructor */
  public PropertiesBase() {
    super();
    this.changes = new PropertyChangeSupport(this);
  }

  /** Property change event occurs */
  public abstract void firePropertyChange();

  /**
   * Add a listener. <br>
   *
   * @param listener Add listener
   */
  public void addPropertyChangeListener(PropertyChangeListener listener) {
    this.changes.addPropertyChangeListener(listener);
  }

  /**
   * Delete the listener. <br>
   *
   * @param listener Listener to delete
   */
  public void removePropertyChangeListener(PropertyChangeListener listener) {
    this.changes.removePropertyChangeListener(listener);
  }

  /**
   * Get String type data
   *
   * @param key --hash table key
   * @param defaultValue --default value
   * @return-Value in this property list with the specified key value
   */
  public String get(final String key, final String defaultValue) {
    return getProperty(key, defaultValue);
  }

  /**
   * Get boolean data
   *
   * @param key --hash table key
   * @param defaultValue --default value
   * @return-Value in this property list with the specified key value
   */
  public boolean getBoolean(final String key, final boolean defaultValue) {
    return new Boolean(getProperty(key, Boolean.toString(defaultValue)));
  }

  /**
   * Acquisition of int type data
   *
   * @param key --hash table key
   * @param defaultValue --default value
   * @return-Value in this property list with the specified key value
   */
  public int getInt(final String key, final int defaultValue) {
    String value = getProperty(key, Integer.toString(defaultValue));
    try {
      return Integer.parseInt(value);
    } catch (NumberFormatException e) {
      return 0;
    }
  }

  /**
   * String type data setting
   *
   * @param key --Key to be placed in the property list
   * @param value key-corresponding value
   */
  public void put(final String key, final String value) {
    setProperty(key, value);
  }

  /**
   * Setting boolean data
   *
   * @param key --Key to be placed in the property list
   * @param value key-corresponding value
   */
  public void putBoolean(final String key, final boolean value) {
    setProperty(key, Boolean.toString(value));
  }

  /**
   * Int type data setting
   *
   * @param key --Key to be placed in the property list
   * @param value key-corresponding value
   */
  public void putInt(final String key, final int value) {
    setProperty(key, Integer.toString(value));
  }

  /**
   * Get Object type data
   *
   * @param key --Key to be placed in the property list
   * @return-Value in this property list with the specified key value
   */
  public Object getObject(final String key) {
    return this.get(key);
  }

  /**
   * Object type data setting
   *
   * @param key --Key to be placed in the property list
   * @param value key-corresponding value
   */
  public void putObject(final String key, final Object value) {
    this.put(key, value);
  }
}
