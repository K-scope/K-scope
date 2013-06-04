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

package jp.riken.kscope.language;

import java.util.Set;

import jp.riken.kscope.data.CodeLine;

/**
 * ブロックの行情報の取得を行うインターフェース。
 * @author RIKEN
 *
 */
public interface IBlock {

    /**
     * 開始行番号情報を取得する。
     * @return 開始行番号情報
     */
    CodeLine getStartCodeLine();

    /**
     * 終了行番号情報を取得する。
     * @return		終了行番号情報
     */
    CodeLine getEndCodeLine();

    /**
     * ブロックタイプを返す。
     * @return ブロックタイプ
     */
    BlockType getBlockType();

    /**
     * ブロックタイプを返す。
     * @return ブロックタイプ
     */
    IBlock getMotherBlock();

    /**
     * 式の変数リストを取得する.
     * @return		式の変数リスト
     */
	Set<Variable> getAllVariables();

}


