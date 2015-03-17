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

import java.io.Serializable;

/**
 * Blockの型を表すenum。
 *
 * @author RIKEN
 *
 */
public enum BlockType implements Serializable {
    /** 動的にメモリ領域を割り当てる。 */
    DYNAMIC_ALLOCATION,
    /** 動的に割り当てられたメモリ領域を解放する。 */
    DYNAMIC_DEALLOCATION,
    /** 動的にpointer参照を破棄（pointer変数にnullを設定）する。 */
    DYNAMIC_NULLIFICATION,
    /** 繰り返し処理を抜ける。 */
    BREAK,
    /** 繰り返し処理の最後に飛ぶ。 */
    CONTINUE,
    /** プロブラムを終了する。 */
    TERMINATION,
    /** 何もしない。　 */
    DO_NOTHING,
    /** 一時停止する。　 */
    PAUSE,
    /** 移動する。　 */
    GOTO,
    /** ディレクティブ　 */
    DIRECTIVE,
    /** 初期値を与える */
    DATA,
    /** プログラム単位内での記憶領域共有を与える */
    EQUIVALENCE,
    /** プログラム単位間での記憶領域共有を与える */
    COMMON,
    /** 総称型のリスト */
    PROCEDURES,
    /** 分岐 */
    SELECTION,
    /** 条件ブロック */
    CONDITION,
    /** 実行文 */
    BODY,
    /** モジュール */
    MODULE,
    /** 手続き */
    PROCEDURE,
    /** 手続呼出 */
    PROCEDUREUSAGE,
    /** 反復 */
    REPETITION,
    /** リターン */
    RETURN,
    /** 代入文 */
    SUBSTITUTION,
    /** ユーザー定義 */
    USERDEFINITION,
    /** 変数宣言 */
    VARIABLEDEFINITION,
    /** USE文 */
    USE,
    /** Fortran:構造体 */
    TYPE,
    /** C:構造体 */
    STRUCT,
    /** C:共同体 */
    UNION,
    /** C:列挙体 */
    ENUM,
    /** 不明 */
    UNKNOWN;
}
