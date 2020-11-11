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

import java.util.ArrayList;

/**
 * Hold call graph information
 *
 * @author RIKEN
 */
public class CallGraphInfo {
  private float totalSumSampNum;
  private ArrayList<StackInfo> stackInfo;

  /**
   * Get the total cumulative number of samples
   *
   * @return Total cumulative number of samples
   */
  public float getTotalSumSampNum() {
    return totalSumSampNum;
  }

  /**
   * Get a list of call graph information for each stack
   *
   * @return List of call graph information for each stack
   */
  public ArrayList<StackInfo> getStackInfo() {
    return stackInfo;
  }

  /**
   * Set the total cumulative number of samples
   *
   * @param totalSumSampNum Total cumulative number of samples to set
   */
  public void setTotalSumSampNum(float totalSumSampNum) {
    this.totalSumSampNum = totalSumSampNum;
  }

  /**
   * Set a list of call graph information for each stack
   *
   * @param stackInfo List of call graph information for each stack to be set
   */
  public void setStackInfo(ArrayList<StackInfo> stackInfo) {
    this.stackInfo = stackInfo;
  }
}
