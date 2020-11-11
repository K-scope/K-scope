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

package jp.riken.kscope.profiler;

import java.io.File;

/**
 * Profiler file read class
 *
 * @author RIKEN
 */
public interface IProfilerReader {

  /**
   * Read from profiler file
   *
   * @param profilerfile Profiler file
   * @param endian Endian setting LITTLE_ENDIAN: 0x00 BIG_ENDIAN: 0x01;
   * @throws Exception Read exception
   */
  public void readFile(File profilerfile, int endian) throws Exception;

  /**
   * Read from profiler file
   *
   * @param profilerfile Profiler file
   * @throws Exception Read exception
   */
  public void readFile(File profilerfile) throws Exception;

  /**
   * Set the endianness of the file
   *
   * @param endian Endian setting LITTLE_ENDIAN: 0x00 BIG_ENDIAN: 0x01;
   */
  public void setEndian(int endian);

  /**
   * Read profiler file
   *
   * @return Read profiler file
   */
  public File getProfFile();

  /**
   * Cost information list: Get the line
   *
   * @return Cost information list: Line (Dprof)
   */
  public ProfilerDprofData[] getCostInfoLine();

  /**
   * Cost information list: Get a loop
   *
   * @return Cost information list: Loop (Dprof)
   */
  public ProfilerDprofData[] getCostInfoLoop();

  /**
   * Cost information list: Get the procedure
   *
   * @return Cost information list: Procedure (Dprof)
   */
  public ProfilerDprofData[] getCostInfoProcedure();

  /**
   * Get call graph information (Dprof)
   *
   * @return Call graph information (Dprof)
   */
  public ProfilerDprofData[] getDprofCallGraphInfo();

  /**
   * Get event counter information (Eprof)
   *
   * @return Event counter information (Eprof)
   */
  public ProfilerEprofData[] getEprofEventCounterInfo();

  /**
   * Get profiler magic key
   *
   * @return magic key
   */
  public String getFileType();

  /**
   * Get the PA event specification value (EPRF only). Cache Instructions MEM_access Performance
   * Statistics
   *
   * @return counterGroup PA event specification value (EPRF only)
   */
  public String getPaEventName();
}
