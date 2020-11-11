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
package jp.riken.kscope.common;

import jp.riken.kscope.Message;

/**
 * Trace direction
 *
 * @author RIKEN
 */
public enum TRACE_DIR {
  // Trace direction
  /** Trace: Up */
  UP(Message.getString("trace_dir.enum.up")), // Trace: Up
  /** Trace: Down */
  DOWN(Message.getString("trace_dir.enum.down")), // Trace: Down
  /** Trace: In */
  IN(Message.getString("trace_dir.enum.in")), // Trace: In
  /** Trace: Out */
  OUT(Message.getString("trace_dir.enum.out")), // Trace: Out
  /** Trace: Forward */
  FORWARD(Message.getString("trace_dir.enum.forward")), // Trace: Forward
  /** Trace: Start */
  START(Message.getString("trace_dir.enum.start")), // Trace: Start
  /** Trace: End */
  END(Message.getString("trace_dir.enum.stop")), // Trace: End
  /** Trace: Refresh */
  REFRESH(Message.getString("trace_dir.enum.refresh")), // Trace: Refresh
  /** Unknown */
  UNKNOWN(Message.getString("trace_dir.enum.unknown")); // Trace: Unknown

  /** Trace direction name */
  private String tracename;

  /**
   * Constructor
   *
   * @param tabname Trace direction name
   */
  private TRACE_DIR(String tracename) {
    this.tracename = tracename;
  }

  /**
   * Get the trace direction name
   *
   * @return Trace direction name
   */
  public String getTraceName() {
    return this.tracename;
  }
}
