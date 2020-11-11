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
 * Holds PA identification information of common information
 *
 * @author RIKEN
 */
public class PaDiscrimInfo {
  private short cpu;
  private short event_nbr;
  private short pa_ver;
  private short reserve;

  /**
   * Get CPU type
   *
   * @return CPU type <br>
   *     <br>
   *     CPU type <br>
   *     0x0000 Itanium Madison (PQ V1) <br>
   *     0x0001 Itanium Montesito (Dual core) (PQ V2) <br>
   *     0x0002 AMD Opteron (Quad core) (PG V3) <br>
   *     0x0003 SPARC64 VII Jupiter (PW V3) <br>
   *     0x0004 Opteron (PG V3 collection-> analysis unified with 0x0002) <br>
   *     0x0005 Xeon Core 2 (PG V3 Woodcrest, Harpertown) <br>
   *     0x0006 Xeon Coer i7 (PG V3 Nehalem) <br>
   *     0x0007 SPARC64 VIII Venus (PETA) <br>
   *     0x0008 SPARC64 IX fx (PETA) <br>
   *     0x0009 Xeon CORE (TM) PROCESSOR 2XXX SERIES (PCC Sandy Brige) <br>
   */
  public short getCpu() {
    return cpu;
  }

  /**
   * Get the number of events
   *
   * @return Number of events
   */
  public short getEvent_nbr() {
    return event_nbr;
  }

  /**
   * Get PA version
   *
   * @return PA version (serial number)
   */
  public short getPa_ver() {
    return pa_ver;
  }

  /**
   * Get reserve
   *
   * @return reserve
   */
  public short getReserve() {
    return reserve;
  }

  /**
   * Set CPU type
   *
   * @param cpu CPU type to set
   */
  public void setCpu(short cpu) {
    this.cpu = cpu;
  }

  /**
   * Set the number of events
   *
   * @param event_nbr Number of events to set
   */
  public void setEvent_nbr(short event_nbr) {
    this.event_nbr = event_nbr;
  }

  /**
   * Set PA version
   *
   * @param pa_ver PA version to set (serial number)
   */
  public void setPa_ver(short pa_ver) {
    this.pa_ver = pa_ver;
  }

  /**
   * Set reserve
   *
   * @param reserve Reserve to set
   */
  public void setReserve(short reserve) {
    this.reserve = reserve;
  }
}
