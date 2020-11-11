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
 * Hold symbol information
 *
 * @author RIKEN
 */
public class SymbolInfo {
  private float sampNum;
  private float barrierSyncWaitNum;
  private float mpiLibCostNum;
  private int lineSymbolStart;
  private int lineSymbolEnd;
  private int fileIndex;
  private String symbolName;

  /**
   * Get the number of MPI library costs
   *
   * @return MPI library cost number
   */
  public float getMpiLibCostNum() {
    return mpiLibCostNum;
  }

  /**
   * Set the number of MPI library costs
   *
   * @param mpiLibCostNum Number of MPI library costs to set
   */
  public void setMpiLibCostNum(float mpiLibCostNum) {
    this.mpiLibCostNum = mpiLibCostNum;
  }

  /**
   * Get the number of samplings
   *
   * @return Sampling count
   */
  public float getSampNum() {
    return sampNum;
  }

  /**
   * Get the number of barrier synchronization waits
   *
   * @return Number of barrier synchronization waits
   */
  public float getBarrierSyncWaitNum() {
    return barrierSyncWaitNum;
  }

  /**
   * Get the symbol start line
   *
   * @return Symbol start line
   */
  public int getLineSymbolStart() {
    return lineSymbolStart;
  }

  /**
   * Get the symbol end line
   *
   * @return Symbol end line
   */
  public int getLineSymbolEnd() {
    return lineSymbolEnd;
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
   * Get the symbol name
   *
   * @return Symbol name
   */
  public String getSymbolName() {
    return symbolName;
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
   * Set the number of barrier synchronization waits
   *
   * @param barrierSyncWaitNum Number of barrier synchronization waits to set
   */
  public void setBarrierSyncWaitNum(float barrierSyncWaitNum) {
    this.barrierSyncWaitNum = barrierSyncWaitNum;
  }

  /**
   * Set the symbol start line
   *
   * @param lineSymbolStart Symbol start line to set
   */
  public void setLineSymbolStart(int lineSymbolStart) {
    this.lineSymbolStart = lineSymbolStart;
  }

  /**
   * Set the symbol end line
   *
   * @param lineSymbolEnd Symbol end line to set
   */
  public void setLineSymbolEnd(int lineSymbolEnd) {
    this.lineSymbolEnd = lineSymbolEnd;
  }

  /**
   * Set the index of the file
   *
   * @param fileIndex Index of the file to be set
   */
  public void setFileIndex(int fileIndex) {
    this.fileIndex = fileIndex;
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
