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
 * トレース方向
 * @author RIKEN
 */
public enum TRACE_DIR {
    // トレース方向
    /** トレース:アップ */
    UP(Message.getString("trace_dir.enum.up")), //トレース:アップ
    /** トレース:ダウン */
    DOWN(Message.getString("trace_dir.enum.down")), //トレース:ダウン
    /** トレース：イン */
    IN(Message.getString("trace_dir.enum.in")), //トレース：イン
    /** トレース：アウト */
    OUT(Message.getString("trace_dir.enum.out")), //トレース：アウト
    /** トレース：フォワード */
    FORWARD(Message.getString("trace_dir.enum.forward")), //トレース：フォワード
    /** トレース：開始 */
    START(Message.getString("trace_dir.enum.start")), //トレース：開始
    /** トレース：終了 */
    END(Message.getString("trace_dir.enum.stop")), //トレース：終了
    /** トレース：リフレッシュ */
    REFRESH(Message.getString("trace_dir.enum.refresh")), //トレース：リフレッシュ
    /** 不明 */
    UNKNOWN(Message.getString("trace_dir.enum.unknown")); //トレース：不明

    /** トレース方向名 */
    private String tracename;

    /**
     * コンストラクタ
     * @param tabname		トレース方向名
     */
    private TRACE_DIR(String tracename) {
        this.tracename = tracename;
    }

    /**
     * トレース方向名を取得する
     * @return		トレース方向名
     */
    public String getTraceName() {
        return this.tracename;
    }

}



