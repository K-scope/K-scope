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
 * Hold thread information
 *
 * @author RIKEN
 */
public class ThreadInfo {
  private int threadNo;
  private float elapsTime;
  private float userTime;
  private float systemTime;
  private float totalSampNum;
  private float barrierSyncWaitNum;
  private float mpiLibCostNum;
  private float mpiFuncElapsTime;
  private double[] paInfo;

  /**
   * Get the thread number
   *
   * @return thread number
   */
  public int getThreadNo() {
    return threadNo;
  }
  /**
   * Get the elapsed time
   *
   * @return elapsed time
   */
  public float getElapsTime() {
    return elapsTime;
  }
  /**
   * Get user time
   *
   * @return user time
   */
  public float getUserTime() {
    return userTime;
  }
  /**
   * Get system time
   *
   * @return system time
   */
  public float getSystemTime() {
    return systemTime;
  }
  /**
   * Get the total number of samplings
   *
   * @return Total sampling
   */
  public float getTotalSampNum() {
    return totalSampNum;
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
   * Get MPI function elapsed time
   *
   * @return MPI function elapsed time
   */
  public float getMpiFuncElapsTime() {
    return mpiFuncElapsTime;
  }
  /**
   * Get PA information
   *
   * @return PA information
   */
  public double[] getPaInfo() {
    return paInfo;
  }
  /**
   * Set the thread number
   *
   * @param threadNo Thread number to set
   */
  public void setThreadNo(int threadNo) {
    this.threadNo = threadNo;
  }
  /**
   * Set the elapsed time
   *
   * @param elapsTime Elapsed time to set
   */
  public void setElapsTime(float elapsTime) {
    this.elapsTime = elapsTime;
  }
  /**
   * Set user time
   *
   * @param userTime User time to set
   */
  public void setUserTime(float userTime) {
    this.userTime = userTime;
  }
  /**
   * Set system time
   *
   * @param systemTime System time to set
   */
  public void setSystemTime(float systemTime) {
    this.systemTime = systemTime;
  }
  /**
   * Set the total number of samplings
   *
   * @param totalSampNum Total sampling to set
   */
  public void setTotalSampNum(float totalSampNum) {
    this.totalSampNum = totalSampNum;
  }
  /**
   * Set the number of barrier synchronization waits
   *
   * @param barrierWaitSyncNum Number of barrier synchronization waits to set
   */
  public void setBarrierWaitSyncNum(float barrierWaitSyncNum) {
    this.barrierSyncWaitNum = barrierWaitSyncNum;
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
   * MPI function Set elapsed time
   *
   * @param mpiFuncElapsTime MPI function elapsed time to set
   */
  public void setMpiFuncElapsTime(float mpiFuncElapsTime) {
    this.mpiFuncElapsTime = mpiFuncElapsTime;
  }
  /**
   * Set PA information
   *
   * @param paInfo PA information to set
   */
  public void setPaInfo(double[] paInfo) {
    this.paInfo = paInfo;
  }
}
