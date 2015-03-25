/*
 * K-scope
 * Copyright 2012-2015 RIKEN, Japan
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
package jp.riken.kscope.xcodeml.clang.xml;

/**
 * ステートメント文のインターフェイスクラス
 * @author RIKEN
 *
 */
public interface IBaseStatement {
    /**
     * 開始行番号を取得する
     * @return		開始行番号
     */
    public String getLineno();

    /**
     * 開始行番号を設定する
     * @param value		開始行番号
     */
    public void setLineno(String value);

    /**
     * 終了行番号を取得する
     * @return		終了行番号
     */
    public String getEndlineno();

    /**
     * 終了行番号を設定する
     * @param value		終了行番号
     */
    public void setEndlineno(String value);

    /**
     * 行番号を取得する
     * @return		行番号
     */
    public String getRawlineno();

    /**
     * 行番号を設定する
     * @param value		行番号
     */
    public void setRawlineno(String value);

    /**
     * ファイル名を取得する
     * @return		ファイル名
     */
    public String getFile();

    /**
     * ファイル名を設定する
     * @param value		ファイル名
     */
    public void setFile(String value);
}
