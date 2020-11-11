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

/**
 * MPI information: MPI function
 *
 * @author RIKEN
 */
public class MpiFunction {

  /** Index int of MPI function */
  private int mpiIndex;
  /** Number of calls int */
  private int callCount;
  /** Elapsed time float */
  private float elapsTime;
  /** Wait time float */
  private float waitTime;
  /** Message length long */
  private long messageLength;
  /** Number of times the message length is 0 bytes or more and less than 4 Kbytes int */
  private int countMessage4k;
  /** Number of times the message length is 4Kbyte or more and less than 64Kbyte int */
  private int countMessage64k;
  /** Number of times the message length is 64Kbyte or more and less than 1024Kbyte int */
  private int countMessage1024k;
  /** Number of times when the message length is 1024Kbyte or more int */
  private int countMessage1024kOver;

  /** Constructor */
  public MpiFunction() {}

  /**
   * Index of MPI function
   *
   * @return Index of MPI function
   */
  public int getMpiIndex() {
    return mpiIndex;
  }

  /**
   * Index of MPI function
   *
   * @param index Index of MPI function
   */
  public void setMpiIndex(int index) {
    this.mpiIndex = index;
  }

  /**
   * Number of calls
   *
   * @return Number of calls
   */
  public int getCallCount() {
    return callCount;
  }

  /**
   * Number of calls
   *
   * @param count number of calls
   */
  public void setCallCount(int count) {
    this.callCount = count;
  }

  /**
   * elapsed time
   *
   * @return elapsed time
   */
  public float getElapsTime() {
    return elapsTime;
  }

  /**
   * elapsed time
   *
   * @param time elapsed time
   */
  public void setElapsTime(float time) {
    this.elapsTime = time;
  }

  /**
   * Waiting time
   *
   * @return Waiting time
   */
  public float getWaitTime() {
    return waitTime;
  }

  /**
   * Waiting time
   *
   * @param time Wait time
   */
  public void setWaitTime(float time) {
    this.waitTime = time;
  }

  /**
   * Message length
   *
   * @return Message length
   */
  public long getMessageLength() {
    return messageLength;
  }

  /**
   * Message length
   *
   * @param length Message length
   */
  public void setMessageLength(long length) {
    this.messageLength = length;
  }

  /**
   * Number of times the message length is 0 bytes or more and less than 4 Kbytes
   *
   * @return Number of times the message length is 0 bytes or more and less than 4 Kbytes
   */
  public int getCountMessage4k() {
    return countMessage4k;
  }

  /**
   * Number of times the message length is 0 bytes or more and less than 4 Kbytes
   *
   * @param count Number of times the message length is 0 bytes or more and less than 4 Kbytes
   */
  public void setCountMessage4k(int count) {
    this.countMessage4k = count;
  }

  /**
   * Number of times the message length is 4Kbyte or more and less than 64Kbyte
   *
   * @return Number of times the message length is 4Kbyte or more and less than 64Kbyte
   */
  public int getCountMessage64k() {
    return countMessage64k;
  }

  /**
   * Number of times the message length is 4Kbyte or more and less than 64Kbyte
   *
   * @param count Number of times the message length is 4Kbyte or more and less than 64Kbyte
   */
  public void setCountMessage64k(int count) {
    this.countMessage64k = count;
  }

  /**
   * Number of times the message length is 64Kbyte or more and less than 1024Kbyte
   *
   * @return Number of times the message length is 64Kbyte or more and less than 1024Kbyte
   */
  public int getCountMessage1024k() {
    return countMessage1024k;
  }

  /**
   * Number of times the message length is 64Kbyte or more and less than 1024Kbyte
   *
   * @param count Number of times message length is 64Kbyte or more and less than 1024Kbyte
   */
  public void setCountMessage1024k(int count) {
    this.countMessage1024k = count;
  }

  /**
   * Number of times when the message length is 1024Kbyte or more
   *
   * @return Number of times when the message length is 1024Kbyte or more
   */
  public int getCountMessage1024kOver() {
    return countMessage1024kOver;
  }

  /**
   * Number of times when the message length is 1024Kbyte or more
   *
   * @param count Number of times when the message length is 1024Kbyte or more
   */
  public void setCountMessage1024kOver(int count) {
    this.countMessage1024kOver = count;
  }
}
