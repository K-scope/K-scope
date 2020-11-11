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

import java.util.ArrayList;
import java.util.List;
import jp.riken.kscope.common.PROFILERINFO_TYPE;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.IBlock;

/**
 * Profiler data base class
 *
 * @author RIKEN
 */
public abstract class ProfilerBaseData {

  /** Code line information */
  private CodeLine code;
  /** Block information */
  private List<IBlock[]> areas;
  /** Profiler information type identifier */
  private PROFILERINFO_TYPE infoType;

  /** Constructor */
  public ProfilerBaseData() {
    this.code = null;
    this.areas = null;
  }

  /**
   * Get code line information
   *
   * @return code line information
   */
  public CodeLine getCodeLine() {
    return code;
  }

  /**
   * Set code line information
   *
   * @param code Code line information
   */
  public void setCodeLine(CodeLine code) {
    this.code = code;
  }

  /**
   * Get block information
   *
   * @return block information
   */
  public List<IBlock[]> getAreas() {
    return this.areas;
  }

  /**
   * Set block information
   *
   * @param areas Block information
   */
  public void setAreas(List<IBlock[]> areas) {
    this.areas = new ArrayList<IBlock[]>();
    this.areas.addAll(areas);
  }

  /**
   * Get block information
   *
   * @return block information
   */
  public IBlock[] getBlocks() {
    if (this.areas == null || this.areas.size() <= 0) return null;
    IBlock[] blocks = this.areas.get(0);
    return blocks;
  }

  /**
   * Set block information
   *
   * @param blocks Block information
   */
  public void setBlocks(IBlock[] blocks) {
    if (blocks == null) return;
    this.areas = new ArrayList<IBlock[]>();
    this.areas.add(blocks);
  }

  /**
   * Get block information
   *
   * @return block information
   */
  public IBlock getBlock() {
    if (this.areas == null || this.areas.size() <= 0) return null;
    IBlock[] blocks = this.areas.get(0);
    return blocks[0];
  }

  /**
   * Set block information
   *
   * @param block Block information
   */
  public void setBlock(IBlock block) {
    if (block == null) return;
    this.areas = new ArrayList<IBlock[]>();
    IBlock[] blocks = new IBlock[] {block};
    this.areas.add(blocks);
  }

  /**
   * Get cost information type identifier
   *
   * @param type Cost information type identifier
   */
  public void setInfoType(PROFILERINFO_TYPE type) {
    this.infoType = type;
  }

  /**
   * Set cost information type identifier
   *
   * @return Cost information type identifier
   */
  public PROFILERINFO_TYPE getInfoType() {
    return this.infoType;
  }
}
