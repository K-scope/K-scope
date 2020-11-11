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
 * DPROF: Costline information class
 *
 * @author RIKEN
 */
public class LineInfo {
  private float sampNum;
  private int lineNo;
  private int symbolIndex;
  private int fileIndex;

  /**
   * Get the number of samplings
   *
   * @return Sampling count
   */
  public float getSampNum() {
    return sampNum;
  }

  /**
   * Get the line number
   *
   * @return line number
   */
  public int getLineNo() {
    return lineNo;
  }

  /**
   * Get the index of the symbol
   *
   * @return Symbol index
   */
  public int getSymbolIndex() {
    return symbolIndex;
  }

  /**
   * Get the index of the file
   *
   * @return file index
   */
  public int getFileIndex() {
    return fileIndex;
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
   * Set the line number
   *
   * @param lineNo Line number to set
   */
  public void setLineNo(int lineNo) {
    this.lineNo = lineNo;
  }

  /**
   * Set the index of the symbol
   *
   * @param symbolIndex Index of the symbol to be set
   */
  public void setSymbolIndex(int symbolIndex) {
    this.symbolIndex = symbolIndex;
  }

  /**
   * Set the index of the file
   *
   * @param fileIndex Index of the file to be set
   */
  public void setFileIndex(int fileIndex) {
    this.fileIndex = fileIndex;
  }
}
