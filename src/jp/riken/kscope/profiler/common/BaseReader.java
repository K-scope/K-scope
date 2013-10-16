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
 * プロファイラ読込基底クラス
 * @author riken
 */
public abstract class BaseReader {

    /**
     * バイトバッファからint(4バイト)値を取得する
     * @param byteBuf		バイトバッファ
     * @return		int(4バイト)値
     */
    protected int getInt(ByteBuffer byteBuf) {
        return ProfilerReaderUtil.convertInt(getByteArray(byteBuf, ProfilerReaderUtil.SIZEOF_INT), getEndian());
    }

    /**
     * バイトバッファからshort(2バイト)値を取得する
     * @param byteBuf		バイトバッファ
     * @return		short(2バイト)値
     */
    protected short getShort(ByteBuffer byteBuf) {
        return ProfilerReaderUtil.convertShort(getByteArray(byteBuf, ProfilerReaderUtil.SIZEOF_SHORT), getEndian());
    }

    /**
     * バイトバッファからlong(8バイト)値を取得する
     * @param byteBuf		バイトバッファ
     * @return		long(8バイト)値
     */
    protected long getLong(ByteBuffer byteBuf) {
        return ProfilerReaderUtil.convertLong(getByteArray(byteBuf, ProfilerReaderUtil.SIZEOF_LONG), getEndian());
    }

    /**
     * バイトバッファからfloat(4バイト)値を取得する
     * @param byteBuf		バイトバッファ
     * @return		float(4バイト)値
     */
    protected float getFloat(ByteBuffer byteBuf) {
        return ProfilerReaderUtil.convertFloat(getByteArray(byteBuf, ProfilerReaderUtil.SIZEOF_FLOAT), getEndian());
    }

    /**
     * バイトバッファからdouble(8バイト)値を取得する
     * @param byteBuf		バイトバッファ
     * @return		double(8バイト)値
     */
    protected double getDouble(ByteBuffer byteBuf) {
        return ProfilerReaderUtil.convertDouble(getByteArray(byteBuf, ProfilerReaderUtil.SIZEOF_DOUBLE), getEndian());
    }

    /**
     * バイトバッファから文字列を取得する
     * @param byteBuf		バイトバッファ
     * @param length		文字列長
     * @return		文字列
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
     * エンディアンを取得する
     * @return  エンディアン
     */
    public abstract int getEndian();

}
