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
package jp.riken.kscope.profiler.eprof;

import java.util.List;

/**
 * MPI information
 *
 * @author RIKEN
 */
public class MpiInfo {
  /** Number of MPI functions int */
  private int mpiCount;
  /** MPI function list */
  private List<MpiFunction> mpiFunctionList;

  /** Constructor */
  public MpiInfo() {}

  /**
   * Number of MPI functions
   *
   * @return Number of MPI functions
   */
  public int getMpiCount() {
    return mpiCount;
  }

  /**
   * Number of MPI functions
   *
   * @param count Number of MPI functions
   */
  public void setMpiCount(int count) {
    this.mpiCount = count;
  }

  /**
   * MPI function list
   *
   * @return MPI function list
   */
  public List<MpiFunction> getMpiFunctionList() {
    return mpiFunctionList;
  }

  /**
   * MPI function list
   *
   * @param list MPI function list
   */
  public void setMpiFunctionList(List<MpiFunction> list) {
    this.mpiFunctionList = list;
  }
}
