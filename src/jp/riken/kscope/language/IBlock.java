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

import java.util.List;
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
     * @return        終了行番号情報
     */
    CodeLine getEndCodeLine();

    /**
     * ブロックタイプを返す。
     * @return ブロックタイプ
     */
    BlockType getBlockType();

    /**
     * 親ブロックを習得する。
     *
     * @return 親ブロック
     */
    IBlock getMotherBlock();

    /**
     * 式の変数リストを取得する.
     * 子ブロックの変数リストも取得する。
     * @return        式の変数リスト
     */
    Set<Variable> getAllVariables();

    /**
     * 式の変数リストを取得する.
     * ブロックのみの変数リストを取得する。
     * @return        式の変数リスト
     */
    Set<Variable> getBlockVariables();

    /**
     * ファイルタイプ（C言語、Fortran)を取得する.
     * @return        ファイルタイプ（C言語、Fortran)
     */
    jp.riken.kscope.data.FILE_TYPE getFileType();


    /**
     * 子要素を返す。
     *
     * @return 子要素。無ければ空のリストを返す
     */
    List<IBlock> getChildren();


    /**
     * 行番号のブロックを検索する
     * @param line            行番号
     * @return        行番号のブロック
     */
    IBlock[] searchCodeLine(CodeLine line);

    /**
     * ファイルタイプがC言語であるかチェックする.
     * @return         true = C言語
     */
    boolean isClang();

    /**
     * ファイルタイプがFortranであるかチェックする.
     * @return         true = Fortran
     */
    boolean isFortran();

    /**
     * 親ブロックからIDeclarationsブロックを取得する.
     * @return    IDeclarationsブロック
     */
    IDeclarations getScopeDeclarationsBlock();


    /**
     * 子ブロックのIDeclarationsブロックを検索する.
     * @return    IDeclarationsブロックリスト
     */
    Set<IDeclarations> getDeclarationsBlocks();

    /**
     * Procedureブロックを習得する。
     * @return    Procedureブロック
     */
    Procedure getProcedureBlock();

    /**
     * Moduleブロックを習得する。
     * @return    Moduleブロック
     */
    Module getModuleBlock();

    /**
     * プロシージャ（関数）からブロックまでの階層文字列表記を取得する
     * @return      階層文字列表記
     */
    String toStringProcedureScope();

    /**
     * モジュールからブロックまでの階層文字列表記を取得する
     * @return      階層文字列表記
     */
    String toStringModuleScope();

    /**
     * ブロックの階層文字列表記を取得する
     * 階層文字列表記 : [main()]-[if (...)]-[if (...)]
     * CompoundBlock（空文）は省略する.
     * @param   module     true=Moduleまでの階層文字列表記とする
     * @return      階層文字列表記
     */
    String toStringScope(boolean module);

    /**
     * 関数呼出を含む自身の子ブロックのリストを返す。
     * @return 子ブロックのリスト
     */
    List<IBlock> getBlocks();

}


