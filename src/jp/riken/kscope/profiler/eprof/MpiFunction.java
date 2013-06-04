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
 * MPI情報:MPI関数
 * @author riken
 */
public class MpiFunction {

    /** MPI関数のIndex	int */
    private int mpiIndex;
    /** 呼び出し回数	int */
    private int callCount;
    /** 経過時間	float */
    private float elapsTime;
    /** 待ち時間	float */
    private float waitTime;
    /** メッセージ長	long */
    private long messageLength;
    /** メッセージ長が 0byte以上～4Kbyte未満の回数	int */
    private int countMessage4k;
    /** メッセージ長が 4Kbyte以上～64Kbyte未満の回数	int */
    private int countMessage64k;
    /** メッセージ長が 64Kbyte以上～1024Kbyte未満の回数	int */
    private int countMessage1024k;
    /** メッセージ長が 1024Kbyte以上の場合の回数	int */
    private int countMessage1024kOver;

    /**
     * コンストラクタ
     */
    public MpiFunction() {

    }


    /**
     * MPI関数のIndex
     * @return		MPI関数のIndex
     */
    public int getMpiIndex() {
        return mpiIndex;
    }

    /**
     * MPI関数のIndex
     * @param index		MPI関数のIndex
     */
    public void setMpiIndex(int index) {
        this.mpiIndex = index;
    }

    /**
     * 呼び出し回数
     * @return		呼び出し回数
     */
    public int getCallCount() {
        return callCount;
    }

    /**
     * 呼び出し回数
     * @param count		呼び出し回数
     */
    public void setCallCount(int count) {
        this.callCount = count;
    }

    /**
     * 経過時間
     * @return		経過時間
     */
    public float getElapsTime() {
        return elapsTime;
    }

    /**
     * 経過時間
     * @param time		経過時間
     */
    public void setElapsTime(float time) {
        this.elapsTime = time;
    }

    /**
     * 待ち時間
     * @return		待ち時間
     */
    public float getWaitTime() {
        return waitTime;
    }

    /**
     * 待ち時間
     * @param time		待ち時間
     */
    public void setWaitTime(float time) {
        this.waitTime = time;
    }

    /**
     * メッセージ長
     * @return		メッセージ長
     */
    public long getMessageLength() {
        return messageLength;
    }

    /**
     * メッセージ長
     * @param length		メッセージ長
     */
    public void setMessageLength(long length) {
        this.messageLength = length;
    }

    /**
     * メッセージ長が 0byte以上～4Kbyte未満の回数
     * @return	メッセージ長が 0byte以上～4Kbyte未満の回数
     */
    public int getCountMessage4k() {
        return countMessage4k;
    }

    /**
     * メッセージ長が 0byte以上～4Kbyte未満の回数
     * @param count		メッセージ長が 0byte以上～4Kbyte未満の回数
     */
    public void setCountMessage4k(int count) {
        this.countMessage4k = count;
    }

    /**
     * メッセージ長が 4Kbyte以上～64Kbyte未満の回数
     * @return		メッセージ長が 4Kbyte以上～64Kbyte未満の回数
     */
    public int getCountMessage64k() {
        return countMessage64k;
    }

    /**
     * メッセージ長が 4Kbyte以上～64Kbyte未満の回数
     * @param count			メッセージ長が 4Kbyte以上～64Kbyte未満の回数
     */
    public void setCountMessage64k(int count) {
        this.countMessage64k = count;
    }

    /**
     * メッセージ長が 64Kbyte以上～1024Kbyte未満の回数
     * @return		メッセージ長が 64Kbyte以上～1024Kbyte未満の回数
     */
    public int getCountMessage1024k() {
        return countMessage1024k;
    }

    /**
     * メッセージ長が 64Kbyte以上～1024Kbyte未満の回数
     * @param count		メッセージ長が 64Kbyte以上～1024Kbyte未満の回数
     */
    public void setCountMessage1024k(int count) {
        this.countMessage1024k = count;
    }

    /**
     * メッセージ長が 1024Kbyte以上の場合の回数
     * @return		メッセージ長が 1024Kbyte以上の場合の回数
     */
    public int getCountMessage1024kOver() {
        return countMessage1024kOver;
    }


    /**
     * メッセージ長が 1024Kbyte以上の場合の回数
     * @param count		メッセージ長が 1024Kbyte以上の場合の回数
     */
    public void setCountMessage1024kOver(int count) {
        this.countMessage1024kOver = count;
    }

}

