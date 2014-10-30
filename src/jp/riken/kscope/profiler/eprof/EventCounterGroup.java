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
 * イベントカウンタグループ
 * @author RIKEN
 */
public class EventCounterGroup {

    /** カウンタグループ名 */
    private String groupname;
    /** カウンタ詳細番号 */
    private int detailno;
    /** 基本情報 */
    private BaseInfo baseInfo;
    /** MPI情報 */
    private MpiInfo mpiInfo;
    /** ハードウェアモニタ情報 */
    private HardwareMonitorInfo hardwareInfo;

    /**
     * コンストラクタ
     */
    public EventCounterGroup() {
    }


    /**
     * カウンタグループ名
     * @return		カウンタグループ名
     */
    public String getGroupname() {
        return groupname;
    }

    /**
     * カウンタグループ名
     * @param name		カウンタグループ名
     */
    public void setGroupname(String name) {
        this.groupname = name;
    }

    /**
     * カウンタ詳細番号
     * @return		カウンタ詳細番号
     */
    public int getDetailno() {
        return detailno;
    }

    /**
     * カウンタ詳細番号
     * @param no		カウンタ詳細番号
     */
    public void setDetailno(int no) {
        this.detailno = no;
    }

    /**
     * 基本情報
     * @return		基本情報
     */
    public BaseInfo getBaseInfo() {
        return baseInfo;
    }

    /**
     * 基本情報
     * @param info		基本情報
     */
    public void setBaseInfo(BaseInfo info) {
        this.baseInfo = info;
    }

    /**
     * MPI情報
     * @return		MPI情報
     */
    public MpiInfo getMpiInfo() {
        return mpiInfo;
    }

    /**
     * MPI情報
     * @param info		MPI情報
     */
    public void setMpiInfo(MpiInfo info) {
        this.mpiInfo = info;
    }

    /**
     * ハードウェアモニタ情報
     * @return		ハードウェアモニタ情報
     */
    public HardwareMonitorInfo getHardwareInfo() {
        return hardwareInfo;
    }

    /**
     * ハードウェアモニタ情報
     * @param info		ハードウェアモニタ情報
     */
    public void setHardwareInfo(HardwareMonitorInfo info) {
        this.hardwareInfo = info;
    }


}
