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
import java.util.List;

/**
 * File information record
 *
 * @author RIKEN
 */
public class FileRecord {

  /** File information list */
  private List<FileInfo> fileInfoList;

  /** Constructor */
  public FileRecord() {
    this.fileInfoList = new ArrayList<FileInfo>();
  }

  /**
   * Add file information
   *
   * @param info File information
   * @return true = success
   */
  public boolean addFileInfo(FileInfo info) {
    return this.fileInfoList.add(info);
  }

  /**
   * Get the number of file information
   *
   * @return Number of file information
   */
  public int getFileInfoCount() {
    if (fileInfoList == null) return 0;
    return this.fileInfoList.size();
  }

  /**
   * Get file information
   *
   * @param index File index
   * @return File information
   */
  public FileInfo getFileInfo(int index) {
    if (fileInfoList == null) return null;
    return this.fileInfoList.get(index);
  }
}
