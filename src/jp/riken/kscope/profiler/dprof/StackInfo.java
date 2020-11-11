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
package jp.riken.kscope.profiler.dprof;

/**
 * Holds stack information for call graph information
 *
 * @author RIKEN
 */
public class StackInfo {
  /** Nest level */
  private int nestLevel;
  /** Number of samples */
  private float sampNum;
  /** Cumulative sampling number */
  private float sumSampNum;
  /** Symbol name */
  private String symbolName;

  /**
   * Get the nesting level
   *
   * @return Nested level
   */
  public int getNestLevel() {
    return nestLevel;
  }

  /**
   * Get the number of samples
   *
   * @return Number of samples
   */
  public float getSampNum() {
    return sampNum;
  }

  /**
   * Get the cumulative number of samples
   *
   * @return Cumulative sampling number
   */
  public float getSumSampNum() {
    return sumSampNum;
  }

  /**
   * Get the symbol name
   *
   * @return Symbol name
   */
  public String getSymbolName() {
    return symbolName;
  }

  /**
   * Set the nesting level
   *
   * @param nestLevel Nest level to set
   */
  public void setNestLevel(int nestLevel) {
    this.nestLevel = nestLevel;
  }

  /**
   * Set the number of samplings
   *
   * @param sampNum Number of samplings to set
   */
  public void setSampNum(float sampNum) {
    this.sampNum = sampNum;
  }

  /**
   * Set the cumulative number of samplings
   *
   * @param sumSampNum Cumulative sampling number to set
   */
  public void setSumSampNum(float sumSampNum) {
    this.sumSampNum = sumSampNum;
  }

  /**
   * Set the symbol name
   *
   * @param symbolName Symbol name to set
   */
  public void setSymbolName(String symbolName) {
    this.symbolName = symbolName;
  }
}
