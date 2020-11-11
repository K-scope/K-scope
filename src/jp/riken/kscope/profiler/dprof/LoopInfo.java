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
 * DPROF: Cost loop information class
 *
 * @author RIKEN
 */
public class LoopInfo {
  private float sampNum;
  private float barrierSyncWaitNum;
  private float mpiLibCostNum;
  private int lineLoopStart;
  private int lineLoopEnd;
  private int nestLevel;
  private short loopType;
  private short parallelInfo;
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
   * Get the number of barrier synchronization waits
   *
   * @return Number of barrier synchronization waits
   */
  public float getBarrierSyncWaitNum() {
    return barrierSyncWaitNum;
  }

  /**
   * Get the number of MPI library costs
   *
   * @return MPI library cost number
   */
  public float getMpiLibCostNum() {
    return mpiLibCostNum;
  }

  /**
   * Get the loop start line
   *
   * @return Loop start line
   */
  public int getLineLoopStart() {
    return lineLoopStart;
  }

  /**
   * Get the loop end line
   *
   * @return Loop end line
   */
  public int getLineLoopEnd() {
    return lineLoopEnd;
  }

  /**
   * Get the nesting level
   *
   * @return Nested level
   */
  public int getNestLevel() {
    return nestLevel;
  }

  /**
   * Get loop type
   *
   * @return Loop type
   */
  public short getLoopType() {
    return loopType;
  }

  /**
   * Get parallelization information
   *
   * @return Parallelization information
   */
  public short getParallelInfo() {
    return parallelInfo;
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
   * Set the number of barrier synchronization waits
   *
   * @param barrierSyncWaitNum Number of barrier synchronization waits to set
   */
  public void setBarrierSyncWaitNum(float barrierSyncWaitNum) {
    this.barrierSyncWaitNum = barrierSyncWaitNum;
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
   * Set the loop start line
   *
   * @param lineLoopStart Loop start line to set
   */
  public void setLineLoopStart(int lineLoopStart) {
    this.lineLoopStart = lineLoopStart;
  }

  /**
   * Set the loop end line
   *
   * @param lineLoopEnd Loop end line to set
   */
  public void setLineLoopEnd(int lineLoopEnd) {
    this.lineLoopEnd = lineLoopEnd;
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
   * Set the loop type
   *
   * @param loopType Loop type to set
   */
  public void setLoopType(short loopType) {
    this.loopType = loopType;
  }

  /**
   * Set parallelization information
   *
   * @param parallelInfo Parallelization information to be set
   */
  public void setParallelInfo(short parallelInfo) {
    this.parallelInfo = parallelInfo;
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
