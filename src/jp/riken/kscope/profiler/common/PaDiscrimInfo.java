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
 * 共通情報のPA識別情報を保持する
 *
 * @author riken
 */
public class PaDiscrimInfo {
    private short cpu;
    private short event_nbr;
    private short pa_ver;
    private short reserve;

    /**
     * CPU種別を取得する
     * @return CPU種別
     *<br>
     *<br>
     * CPU種別<br>
     *0x0000    Itanium Madison                (PQ V1)<br>
     *0x0001    Itanium Montesito (Dual core)  (PQ V2)<br>
     *0x0002    AMD Opteron (Quad core)        (PG V3)<br>
     *0x0003    SPARC64 VII Jupiter             (PW V3)<br>
     *0x0004    Opteron                        (PG V3 収集->解析は 0x0002 で統一)<br>
     *0x0005    Xeon Core 2                    (PG V3 Woodcrest,Harpertown)<br>
     *0x0006    Xeon Coer i7                  (PG V3 Nehalem)<br>
      *0x0007    SPARC64 VIII Venus               (PETA)<br>
      *0x0008    SPARC64 IX fx                  (PETA)<br>
      *0x0009    Xeon CORE(TM) PROCESSOR 2XXX SERIES (PCC SandyBrige)<br>
      *
     */
    public short getCpu() {
        return cpu;
    }

    /**
     * イベント数を取得する
     * @return イベント数
     */
    public short getEvent_nbr() {
        return event_nbr;
    }

    /**
     * PAバージョンを取得する
     * @return PAバージョン(連番)
     */
    public short getPa_ver() {
        return pa_ver;
    }

    /**
     * リザーブを取得する
     * @return リザーブ
     */
    public short getReserve() {
        return reserve;
    }

    /**
     * CPU種別を設定する
     * @param cpu
     *            設定するCPU種別
     *
     */
    public void setCpu(short cpu) {
        this.cpu = cpu;
    }

    /**
     * イベント数を設定する
     * @param event_nbr
     *            設定するイベント数
     */
    public void setEvent_nbr(short event_nbr) {
        this.event_nbr = event_nbr;
    }

    /**
     * PAバージョンを設定する
     * @param pa_ver
     *            設定するPAバージョン(連番)
     */
    public void setPa_ver(short pa_ver) {
        this.pa_ver = pa_ver;
    }

    /**
     * リザーブを設定する
     * @param reserve
     *            設定するリザーブ
     */
    public void setReserve(short reserve) {
        this.reserve = reserve;
    }

}
