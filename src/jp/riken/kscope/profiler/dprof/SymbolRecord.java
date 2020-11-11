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
 * Symbol information record class
 *
 * @author RIKEN
 */
public class SymbolRecord {
  /** Symbol information: Symbol information list (= number of threads) */
  private List<SymbolList> records;

  /** Constructor */
  public SymbolRecord() {
    records = new ArrayList<SymbolList>();
  }

  /**
   * Add symbol information list
   *
   * @param symbol Thread list
   * @return true = success
   */
  public boolean addSymbolList(SymbolList symbol) {
    return this.records.add(symbol);
  }

  /**
   * Get the symbol information record
   *
   * @return Symbol information record
   */
  public List<SymbolList> getSymbolRecord() {
    return this.records;
  }

  /**
   * Get the symbol information list of thread number (0 ~)
   *
   * @param threadid Thread number (0 ~)
   * @return Symbol information list
   */
  public SymbolList getSymbolList(int threadid) {
    if (this.records == null) return null;
    if (this.records.size() <= threadid) return null;
    return this.records.get(threadid);
  }

  /**
   * Get symbol information
   *
   * @param threadid Thread number (0 ~)
   * @param symbolid Symbol index
   * @return Symbol information
   */
  public SymbolInfo getSymbolInfo(int threadid, int symbolid) {
    SymbolList list = getSymbolList(threadid);
    if (list == null) return null;
    return list.getSymbolInfo(symbolid);
  }

  /**
   * Get the number of symbol information list (= number of threads)
   *
   * @return Number of symbol information lists
   */
  public int getSymbolListCount() {
    if (this.records == null) return 0;
    return this.records.size();
  }

  /**
   * Get the number of symbols
   *
   * @param threadid Thread number (0 ~)
   * @return Number of symbols
   */
  public int getSymbolInfoCount(int threadid) {
    SymbolList list = getSymbolList(threadid);
    if (list == null) return 0;
    return list.getSymbolInfoCount();
  }
}
