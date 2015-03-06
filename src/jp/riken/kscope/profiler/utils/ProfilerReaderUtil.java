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
 * バイト列を各種のデータ型に変換するためのメソッドを提供するユーティリティクラス
 *
 * @author RIKEN
 *
 */
public class ProfilerReaderUtil {

    /**
     * リトルエンディアンを表す定数
     */
    public static final int LITTLE_ENDIAN = 0x00;

    /**
     * ビッグエンディアンを表す定数
     */
    public static final int BIG_ENDIAN = 0x01;

    /**
     * プロファイラファイルのshort型のデータサイズ(バイト)
     */
    public static final int SIZEOF_SHORT = 0x02;

    /**
     * プロファイラファイルのint型のデータサイズ(バイト)
     */
    public static final int SIZEOF_INT = 0x04;

    /**
     * プロファイラファイルのfloat型のデータサイズ(バイト)
     */
    public static final int SIZEOF_FLOAT = 0x04;

    /**
     * プロファイラファイルのdouble型のデータサイズ(バイト)
     */
    public static final int SIZEOF_DOUBLE = 0x08;

    /**
     * プロファイラファイルのlong型のデータサイズ(バイト)
     */
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
     * 指定したエンディアンでバイト配列をshort型に変換する
     *
     * @param byteArray
     *            変換するバイト配列
     * @param endian
     *            エンディアン指定 ProfilerReaderUtil.LITTLE_ENDIAN または
     *            ProfilerReaderUtil.BIG_ENDIANを指定する
     * @return 変換後のshort型変数の値
     */
    public static short convertShort(byte[] byteArray, int endian) {
        if (byteArray.length < SIZEOF_SHORT) {
        	System.err.println(Message.getString("profilerreaderutil.errout.bytearrayshort")); //エラー: バイト配列の長さが変換先のデータ型より小さいため、変換できません。
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
     * 指定したエンディアンでバイト配列をint型に変換する
     *
     * @param byteArray
     *            変換するバイト配列
     * @param endian
     *            エンディアン指定 ProfilerReaderUtil.LITTLE_ENDIAN または
     *            ProfilerReaderUtil.BIG_ENDIANを指定する
     * @return 変換後のint型変数の値
     */
    public static int convertInt(byte[] byteArray, int endian) {
        if (byteArray.length < SIZEOF_INT) {
        	System.err.println(Message.getString("profilerreaderutil.errout.bytearrayshort")); //エラー: バイト配列の長さが変換先のデータ型より小さいため、変換できません。
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
     * 指定したエンディアンでバイト配列をlong型に変換する
     *
     * @param byteArray
     *            変換するバイト配列
     * @param endian
     *            エンディアン指定 ProfilerReaderUtil.LITTLE_ENDIAN または
     *            ProfilerReaderUtil.BIG_ENDIANを指定する
     * @return 変換後のlong型変数の値
     */
    public static long convertLong(byte[] byteArray, int endian) {
        if (byteArray.length < SIZEOF_LONG) {
        	System.err.println(Message.getString("profilerreaderutil.errout.bytearrayshort")); //エラー: バイト配列の長さが変換先のデータ型より小さいため、変換できません。
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
     * 指定したエンディアンでバイト配列をfloat型に変換する
     *
     * @param byteArray
     *            変換するバイト配列
     * @param endian
     *            エンディアン指定 ProfilerReaderUtil.LITTLE_ENDIAN または
     *            ProfilerReaderUtil.BIG_ENDIANを指定する
     * @return 変換後のfloat型変数の値
     */
    public static float convertFloat(byte[] byteArray, int endian) {
        if (byteArray.length < SIZEOF_FLOAT) {
        	System.err.println(Message.getString("profilerreaderutil.errout.bytearrayshort")); //エラー: バイト配列の長さが変換先のデータ型より小さいため、変換できません。
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
     * 指定したエンディアンでバイト配列をdouble型に変換する
     *
     * @param byteArray
     *            変換するバイト配列
     * @param endian
     *            エンディアン指定 ProfilerReaderUtil.LITTLE_ENDIAN または
     *            ProfilerReaderUtil.BIG_ENDIANを指定する
     * @return 変換後のdouble型変数の値
     */
    public static double convertDouble(byte[] byteArray, int endian) {
        if (byteArray.length < SIZEOF_DOUBLE) {
        	System.err.println(Message.getString("profilerreaderutil.errout.bytearrayshort")); //エラー: バイト配列の長さが変換先のデータ型より小さいため、変換できません。
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
     * 指定したエンディアンでバイト配列をString型に変換する
     *
     * @param byteArray
     *            変換するバイト配列
     * @param length
     *            変換するバイト配列の長さ
     * @param endian
     *            エンディアン指定 ProfilerReaderUtil.LITTLE_ENDIAN または
     *            ProfilerReaderUtil.BIG_ENDIANを指定する
     * @return 変換後の文字列
     */
    public static String convertString(byte[] byteArray, int length, int endian) {
        if (byteArray.length < length) {
        	System.err.println(Message.getString("profilerreaderutil.errout.bytearrayshort")); //エラー: バイト配列の長さが変換先のデータ型より小さいため、変換できません
            return "";
        }
        return new String(byteArray);
    }
}
