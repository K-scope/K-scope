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

import java.nio.ByteBuffer;
import jp.riken.kscope.profiler.utils.ProfilerReaderUtil;

/**
 * Profiler read base class
 *
 * @author RIKEN
 */
public abstract class BaseReader {

  /**
   * Get an int (4 bytes) value from the byte buffer
   *
   * @param byteBuf Byte buffer
   * @return int (4 bytes) value
   */
  protected int getInt(ByteBuffer byteBuf) {
    return ProfilerReaderUtil.convertInt(
        getByteArray(byteBuf, ProfilerReaderUtil.SIZEOF_INT), getEndian());
  }

  /**
   * Get the short (2 bytes) value from the byte buffer
   *
   * @param byteBuf Byte buffer
   * @return short (2 bytes) value
   */
  protected short getShort(ByteBuffer byteBuf) {
    return ProfilerReaderUtil.convertShort(
        getByteArray(byteBuf, ProfilerReaderUtil.SIZEOF_SHORT), getEndian());
  }

  /**
   * Get long (8 bytes) value from byte buffer
   *
   * @param byteBuf Byte buffer
   * @return long (8 bytes) value
   */
  protected long getLong(ByteBuffer byteBuf) {
    return ProfilerReaderUtil.convertLong(
        getByteArray(byteBuf, ProfilerReaderUtil.SIZEOF_LONG), getEndian());
  }

  /**
   * Get float (4 bytes) value from byte buffer
   *
   * @param byteBuf Byte buffer
   * @return float (4 bytes) value
   */
  protected float getFloat(ByteBuffer byteBuf) {
    return ProfilerReaderUtil.convertFloat(
        getByteArray(byteBuf, ProfilerReaderUtil.SIZEOF_FLOAT), getEndian());
  }

  /**
   * Get double (8 bytes) value from byte buffer
   *
   * @param byteBuf Byte buffer
   * @return double (8 bytes) value
   */
  protected double getDouble(ByteBuffer byteBuf) {
    return ProfilerReaderUtil.convertDouble(
        getByteArray(byteBuf, ProfilerReaderUtil.SIZEOF_DOUBLE), getEndian());
  }

  /**
   * Get a string from the byte buffer
   *
   * @param byteBuf Byte buffer
   * @param length String length
   * @return string
   */
  protected String getString(ByteBuffer byteBuf, int length) {
    return ProfilerReaderUtil.convertString(getByteArray(byteBuf, length), length, getEndian());
  }

  protected byte[] getByteArray(ByteBuffer byteBuf, int length) {
    byte[] temp = new byte[length];
    byteBuf.get(temp, 0, length);
    return temp;
  }

  /**
   * Get endian
   *
   * @return endian
   */
  public abstract int getEndian();
}
