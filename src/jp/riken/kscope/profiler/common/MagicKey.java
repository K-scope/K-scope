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

package jp.riken.kscope.profiler.common;

/**
 * Holds magic key information.
 *
 * @author RIKEN
 */
public class MagicKey {
  private String id;
  private short add_mode;
  private short ver;

  /**
   * Get the file identification character
   *
   * @return File identifier "DPRF" or "RPRF" or "GPRF"
   */
  public String getId() {
    return id;
  }

  /**
   * Get address mode
   *
   * @return Address mode 0:32bit 1:64bit
   */
  public short getAdd_mode() {
    return add_mode;
  }

  /**
   * Get the version
   *
   * @return version <br>
   *     <br>
   *     ver (upper 1 byte: product type, lower 1 byte: version (serial number)) <br>
   *     0x0001 (RIKEN version, PL-PACK V2) <br>
   *     0x0101 (Pleiades version) <br>
   *     0x0102 (PQ V2) <br>
   *     0x0201 (EM64T version = PG V1, V2) <br>
   *     0x0301 (PW V3) <br>
   *     0x0302 (PG V3) <br>
   *     0x0401 (PETA compatible profiler, VarunaGE) <br>
   *     0x0411 (PETA Basic Profiler, VarunaGE) <br>
   *     0x0501 (PCC basic profiler) <br>
   *     0x0412 (PETA Basic Profiler, VarunaGE PT3) <br>
   */
  public short getVer() {
    return ver;
  }

  /**
   * Set the file identification character
   *
   * @param id File identification character to be set "DPRF" or "RPRF" or "GPRF"
   */
  public void setId(String id) {
    this.id = id;
  }

  /**
   * Set address mode
   *
   * @param add_mode Address mode to set 0: 32bit 1: 64bit
   */
  public void setAdd_mode(short add_mode) {
    this.add_mode = add_mode;
  }

  /**
   * Set the version
   *
   * @param ver Version to set
   */
  public void setVer(short ver) {
    this.ver = ver;
  }
}
