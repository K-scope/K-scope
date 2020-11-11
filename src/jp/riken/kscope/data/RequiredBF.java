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

import java.awt.Color;
import jp.riken.kscope.common.ACCESSMEMORY_TYPE;

/**
 * Request Byte / FLOP data class
 *
 * @author RIKEN
 */
public class RequiredBF {

  /** Access type */
  private ACCESSMEMORY_TYPE type;
  /** Access name */
  private String name;
  /** Access background color */
  private Color backColor;
  /** Throughput: With store */
  private float mem_throughput_store;
  /** Throughput: No store */
  private float mem_throughput_nostore;
  /** Coefficient */
  private float coef;
  /** Request B / F calculation flag */
  private boolean reqbf;
  /** rate-determining */
  private boolean limits;
  /** Enabled / disabled flag */
  private boolean enabled;

  /**
   * Constructor
   *
   * @param type Access destination type
   */
  public RequiredBF(ACCESSMEMORY_TYPE type) {
    this.setType(type);
  }

  /**
   * Get the access destination background color.
   *
   * @return Access destination background color
   */
  public Color getBackColor() {
    return backColor;
  }

  /**
   * Set the access destination background color
   *
   * @param color Access destination background color
   */
  public void setBackColor(Color color) {
    this.backColor = color;
  }

  /**
   * Memory Throughput: Get with store.
   *
   * @return Memory Throughput: With store
   */
  public float getMemThroughputStore() {
    return mem_throughput_store;
  }

  /**
   * Memory Throughput: Set with store
   *
   * @param mem_throughput_store Memory Throughput: With store
   */
  public void setMemThroughputStore(float mem_throughput_store) {
    this.mem_throughput_store = mem_throughput_store;
  }

  /**
   * Memory Throughput: Get no store.
   *
   * @return Memory Throughput: No store
   */
  public float getMemThroughputNostore() {
    return mem_throughput_nostore;
  }

  /**
   * Memory Throughput: Set to no store.
   *
   * @param mem_throughput_nostore Memory Throughput: No store
   */
  public void setMemThroughputNostore(float mem_throughput_nostore) {
    this.mem_throughput_nostore = mem_throughput_nostore;
  }

  /**
   * Get the coefficient.
   *
   * @return coefficient
   */
  public float getCoef() {
    return coef;
  }

  /**
   * Set the coefficient.
   *
   * @param coef coefficient
   */
  public void setCoef(float coef) {
    this.coef = coef;
  }

  /**
   * Get the request B / F calculation flag.
   *
   * @return Request B / F calculation flag
   */
  public boolean isRequiredBF() {
    return reqbf;
  }

  /**
   * Set the request B / F calculation flag.
   *
   * @param bf Request B / F calculation flag
   */
  public void setRequiredBF(boolean bf) {
    this.reqbf = bf;
  }

  /**
   * Get the rate-determining flag.
   *
   * @return rate-determining flag
   */
  public boolean isLimiting() {
    return limits;
  }

  /**
   * Set the rate-determining flag.
   *
   * @param limits rate-determining flag
   */
  public void setLimiting(boolean limits) {
    this.limits = limits;
  }

  /**
   * Get access type
   *
   * @return Access type
   */
  public ACCESSMEMORY_TYPE getType() {
    return type;
  }

  /**
   * Set the access destination type
   *
   * @param type Access destination type
   */
  public void setType(ACCESSMEMORY_TYPE type) {
    this.type = type;
  }

  /**
   * Get the enable / disable flag.
   *
   * @return Enable / disable flag
   */
  public boolean isEnabled() {
    return enabled;
  }

  /**
   * Set the enable / disable flag.
   *
   * @param enabled Enable / disable flag
   */
  public void setEnabled(boolean enabled) {
    this.enabled = enabled;
  }

  /**
   * Set the access destination name
   *
   * @return Access name
   */
  public void setName(String name) {
    this.name = name;
  }

  /**
   * Get the access destination name
   *
   * @return Access name
   */
  public String getName() {
    return this.name;
  }

  /** Access name */
  @Override
  public String toString() {
    return getName();
  }
}
