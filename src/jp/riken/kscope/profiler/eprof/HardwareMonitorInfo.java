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

import java.util.List;

/**
 *  ハードウェアモニタ情報
 * @author RIKEN
 */
public class HardwareMonitorInfo {
    /** 測定スレッド数 */
    private int threadCount;
    /** ハードウェアモニタ情報(PA情報)テーブルリスト */
    private List<HardwarePaTable> paInfo;

    /**
     * コンストラクタ
     */
    public HardwareMonitorInfo() {
    }

    /**
     * 測定スレッド数
     * @return 測定スレッド数
     */
    public int getThreadCount() {
        return threadCount;
    }

    /**
     * 測定スレッド数
     * @param count		測定スレッド数
     */
    public void setThreadCount(int count) {
        this.threadCount = count;
    }

    /**
     * ハードウェアモニタ情報(PA情報)テーブルリスト
     * @return ハードウェアモニタ情報(PA情報)テーブルリスト
     */
    public List<HardwarePaTable> getPaInfo() {
        return paInfo;
    }

    /**
     * ハードウェアモニタ情報(PA情報)テーブルリスト
     * @param list		ハードウェアモニタ情報(PA情報)テーブルリスト
     */
    public void setPaInfo(List<HardwarePaTable> list) {
        this.paInfo = list;
    }


}
