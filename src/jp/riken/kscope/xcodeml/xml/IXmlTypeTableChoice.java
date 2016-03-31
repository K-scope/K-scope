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
package jp.riken.kscope.xcodeml.xml;

/**
 * データ型インターフェイスクラス
 * @author RIKEN
 */
public interface IXmlTypeTableChoice {

    /**
     * データ型名を取得する
     * @return データ型名
     */
    String getType();

    /**
     * データ型名を設定する
     * @param type		データ型名
     */
    void setType(String type);

    /**
     * public属性をチェックする
     * @return		public属性
     */
    Boolean isIsPublic();

    /**
     * public属性を設定する
     * @param isPublic		public属性
     */
    void setIsPublic(Boolean isPublic);

    /**
     * private属性をチェックする
     * @return		private属性
     */
    Boolean isIsPrivate();

    /**
     * private属性を設定する
     * @param isPrivate	private属性
     */
    void setIsPrivate(Boolean isPrivate);

    /**
     * データ型を文字列にする
     * @return データ型文字列
     */
    @Override
    String toString();
}
