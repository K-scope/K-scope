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

package jp.riken.kscope.profiler.utils;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import jp.riken.kscope.Message;

/**
 * Utility class that provides methods for converting byte strings to various data types
 *
 * @author RIKEN
 */
public class ProfilerReaderUtil {

  /** Constant representing little endian */
  public static final int LITTLE_ENDIAN = 0x00;

  /** Constant representing big endianness */
  public static final int BIG_ENDIAN = 0x01;

  /** Short data size (bytes) of profiler file */
  public static final int SIZEOF_SHORT = 0x02;

  /** Profiler file int type data size (bytes) */
  public static final int SIZEOF_INT = 0x04;

  /** Float type data size (bytes) of profiler file */
  public static final int SIZEOF_FLOAT = 0x04;

  /** Double type data size (bytes) of profiler file */
  public static final int SIZEOF_DOUBLE = 0x08;

  /** Long type data size (bytes) of profiler file */
  public static final int SIZEOF_LONG = 0x08;

  private static void setEndian(ByteBuffer byteBuf, int endian) {
    switch (endian) {
      case (LITTLE_ENDIAN):
        byteBuf.order(ByteOrder.LITTLE_ENDIAN);
        break;
      case (BIG_ENDIAN):
        byteBuf.order(ByteOrder.BIG_ENDIAN);
        break;
    }
  }

  /**
   * Converts a byte array to short type with the specified endian
   *
   * @param byteArray Byte array to convert
   * @param endian Endian specification ProfilerReaderUtil.LITTLE_ENDIAN or Specify
   *     ProfilerReaderUtil.BIG_ENDIAN
   * @return Value of short type variable after conversion
   */
  public static short convertShort(byte[] byteArray, int endian) {
    if (byteArray.length < SIZEOF_SHORT) {
      System.err.println(
          Message.getString(
              "profilerreaderutil.errout.bytearrayshort")); // Error: Cannot convert because the
      // length of the byte array is smaller
      // than the destination data type.
      return 0;
    }
    ByteBuffer byteBuf = ByteBuffer.allocate(SIZEOF_SHORT * byteArray.length);
    byteBuf.put(byteArray);
    setEndian(byteBuf, endian);
    byteBuf.put(byteArray);
    byteBuf.flip();
    return byteBuf.getShort();
  }

  /**
   * Convert byte array to int type with specified endian
   *
   * @param byteArray Byte array to convert
   * @param endian Endian specification ProfilerReaderUtil.LITTLE_ENDIAN or Specify
   *     ProfilerReaderUtil.BIG_ENDIAN
   * @return Value of int type variable after conversion
   */
  public static int convertInt(byte[] byteArray, int endian) {
    if (byteArray.length < SIZEOF_INT) {
      System.err.println(
          Message.getString(
              "profilerreaderutil.errout.bytearrayshort")); // Error: Cannot convert because the
      // length of the byte array is smaller
      // than the destination data type.
      return 0;
    }
    ByteBuffer byteBuf = ByteBuffer.allocate(SIZEOF_INT * byteArray.length);
    byteBuf.put(byteArray);
    setEndian(byteBuf, endian);
    byteBuf.put(byteArray);
    byteBuf.flip();
    return byteBuf.getInt();
  }

  /**
   * Convert byte array to long type with specified endian
   *
   * @param byteArray Byte array to convert
   * @param endian Endian specification ProfilerReaderUtil.LITTLE_ENDIAN or Specify
   *     ProfilerReaderUtil.BIG_ENDIAN
   * @return Value of long type variable after conversion
   */
  public static long convertLong(byte[] byteArray, int endian) {
    if (byteArray.length < SIZEOF_LONG) {
      System.err.println(
          Message.getString(
              "profilerreaderutil.errout.bytearrayshort")); // Error: Cannot convert because the
      // length of the byte array is smaller
      // than the destination data type.
      return 0;
    }
    ByteBuffer byteBuf = ByteBuffer.allocate(SIZEOF_LONG * byteArray.length);
    byteBuf.put(byteArray);
    setEndian(byteBuf, endian);
    byteBuf.put(byteArray);
    byteBuf.flip();
    return byteBuf.getLong();
  }

  /**
   * Converts a byte array to float type with the specified endian
   *
   * @param byteArray Byte array to convert
   * @param endian Endian specification ProfilerReaderUtil.LITTLE_ENDIAN or Specify
   *     ProfilerReaderUtil.BIG_ENDIAN
   * @return Value of float type variable after conversion
   */
  public static float convertFloat(byte[] byteArray, int endian) {
    if (byteArray.length < SIZEOF_FLOAT) {
      System.err.println(
          Message.getString(
              "profilerreaderutil.errout.bytearrayshort")); // Error: Cannot convert because the
      // length of the byte array is smaller
      // than the destination data type.
      return 0;
    }
    ByteBuffer byteBuf = ByteBuffer.allocate(SIZEOF_FLOAT * byteArray.length);
    byteBuf.put(byteArray);
    setEndian(byteBuf, endian);
    byteBuf.put(byteArray);
    byteBuf.flip();
    return byteBuf.getFloat();
  }

  /**
   * Convert byte array to double type with specified endian
   *
   * @param byteArray Byte array to convert
   * @param endian Endian specification ProfilerReaderUtil.LITTLE_ENDIAN or Specify
   *     ProfilerReaderUtil.BIG_ENDIAN
   * @return Value of double type variable after conversion
   */
  public static double convertDouble(byte[] byteArray, int endian) {
    if (byteArray.length < SIZEOF_DOUBLE) {
      System.err.println(
          Message.getString(
              "profilerreaderutil.errout.bytearrayshort")); // Error: Cannot convert because the
      // length of the byte array is smaller
      // than the destination data type.
      return 0;
    }
    ByteBuffer byteBuf = ByteBuffer.allocate(SIZEOF_DOUBLE * byteArray.length);
    byteBuf.put(byteArray);
    setEndian(byteBuf, endian);
    byteBuf.put(byteArray);
    byteBuf.flip();
    return byteBuf.getDouble();
  }

  /**
   * Convert byte array to String type with specified endian
   *
   * @param byteArray Byte array to convert
   * @param length Length of byte array to convert
   * @param endian Endian specification ProfilerReaderUtil.LITTLE_ENDIAN or Specify
   *     ProfilerReaderUtil.BIG_ENDIAN
   * @return Converted string
   */
  public static String convertString(byte[] byteArray, int length, int endian) {
    if (byteArray.length < length) {
      System.err.println(
          Message.getString(
              "profilerreaderutil.errout.bytearrayshort")); // Error: Cannot convert because the
      // length of the byte array is smaller
      // than the destination data type
      return "";
    }
    return new String(byteArray);
  }
}
