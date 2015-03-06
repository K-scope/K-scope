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

package jp.riken.kscope.language.utils;

import java.util.List;

import jp.riken.kscope.language.*;
import jp.riken.kscope.language.fortran.*;
import jp.riken.kscope.language.generic.*;

/**
 * Entryインターフェイスクラス.
 * データベースの探索を行う.
 * @author RIKEN
 */
public interface ILanguageEntry {

	/**
	 * 探索履歴リストを取得する.
	 * @return		探索履歴リスト
	 */
	public List<Object> getListVisit();

	/**
	 * 探索履歴リストを設定する
	 * @param list		探索履歴リスト
	 */
	public void setListVisit(List<Object> list);

	/**
	 * フォートランデータベースを取得する
	 * @return		フォートランデータベース
	 */
	public Fortran getLanguage();

	/**
	 * フォートランデータベースを設定する.
	 * @param language		フォートランデータベース
	 */
	public void setLanguage(Fortran language);

	/**
	 * モジュールを探索する.
	 * @param entry		モジュール
	 */
	public void entry(Module entry);

	/**
	 * サブルーチン、関数を探索する.
	 * @param entry		サブルーチン、関数
	 */
	public void entry(Procedure entry);

	/**
	 * 変数を探索する
	 * @param entry		変数
	 */
	public void entry(Variable entry);

	/**
	 * 構造体を探索する.
	 * @param entry		構造体
	 */
	public void entry(Type entry);

	/**
	 * 構造体を探索する.
	 * @param entry		構造体
	 */
	public void entry(Structure entry);

	/**
	 * BREAK文を探索する.
	 * @param entry		BREAK文
	 */
	public void entry(Break entry);

	/**
	 * COMMON文を探索する.
	 * @param entry		COMMAND文
	 */
	public void entry(Common entry);

	/**
	 * 条件式を探索する
	 * @param entry		条件式
	 */
	public void entry(Condition entry);

	/**
	 * CONTINUE文を探索する
	 * @param entry		CONTINUE文
	 */
	public void entry(Continue entry);

	/**
	 * DATA文を探索する.
	 * @param entry		DATA文
	 */
	public void entry(Data entry);

	/**
	 * DIRECTIVE文を探索する.
	 * @param entry		DIRECTIVE文
	 */
	public void entry(Directive entry);
	/**
	 * D何もしない制御文を探索する.
	 * @param entry		何もしない制御文
	 */
	public void entry(DoNothing entry);

	/**
	 * ALLOCATE文を探索する.
	 * @param entry		ALLOCATE文
	 */
	public void entry(DynamicAllocation entry);

	/**
	 * DEALLOCATE文を探索する.
	 * @param entry		DEALLOCATE文
	 */
	public void entry(DynamicDeallocation entry);

	/**
	 * NULLIFY文を探索する.
	 * @param entry		NULLIFY文
	 */
	public void entry(DynamicNullification entry);

	/**
	 * EQUIVALENCE文を探索する.
	 * @param entry		EQUIVALENCE文
	 */
	public void entry(Equivalence entry);

	/**
	 * サブルーチン、関数の本文を探索する.
	 * @param entry		サブルーチン、関数の本文
	 */
	public void entry(ExecutableBody entry);

	/**
	 * GOTO文を探索する.
	 * @param entry		GOTO文
	 */
	public void entry(GoTo entry);

	/**
	 * PAUSE文を探索する.
	 * @param entry		PAUSE文
	 */
	public void entry(Pause entry);

	/**
	 * CALL文、関数呼出を探索する.
	 * @param entry		CALL文、関数呼出
	 */
	public void entry(ProcedureUsage entry);

	/**
	 * 総称関数群(interface文)を探索する.
	 * @param entry		総称関数群(interface文)
	 */
	public void entry(Procedures entry);

	/**
	 * DO,WHILE文を探索する.
	 * @param entry		DO,WHILE文
	 */
	public void entry(Repetition entry);

	/**
	 * RETURN文を探索する
	 * @param entry		RETURN文
	 */
	public void entry(Return entry);

	/**
	 * SELECT文を探索する.
	 * @param entry 	SELECT文
	 */
	public void entry(Selection entry);

	/**
	 * 代入文を探索する.
	 * @param entry		代入文
	 */
	public void entry(Substitution entry);

	/**
	 * STOP文を探索する.
	 * @param entry		STOP文
	 */
	public void entry(Termination entry);

	/**
	 * ユーザーにより定義される処理ブロックを探索する.
	 * @param entry		ユーザーにより定義される処理ブロック
	 */
	public void entry(UserDefined entry);

	/**
	 * USE文を探索する.
	 * @param entry		USE文
	 */
	public void entry(UseState entry);

	/**
	 * MODULE PROCEDURE文を探索する.
	 * @param entry		MODULE PROCEDURE文
	 */
	public void entry(ProcedureWithNameOnly entry);

	/**
	 * 変数・構造体の宣言文を探索する.
	 * @param entry		変数・構造体の宣言文
	 */
	public void entry(VariableDefinition entry);

	/**
	 * 変数属性を探索する.
	 * @param entry		変数属性
	 */
	public void entry(VariableAttribute entry);

	/**
	 * 変数宣言の配列添字を探索する.
	 * @param entry		変数宣言の配列添字
	 */
	public void entry(VariableDimension entry);

	/**
	 * 変数の配列添字を探索する.
	 * @param entry		変数の配列添字
	 */
	public void entry(DimensionIndex entry);

	/**
	 * 式を探索する.
	 * @param entry		式
	 */
	public void entry(Expression entry);

	/**
	 * 総称関数を探索する.
	 * @param entry		総称関数
	 */
	public void entry(ProcedureItem entry);

	/**
	 * 変数データ型を探索する.
	 * @param entry		変数データ型
	 */
	public void entry(VariableType entry);

	/**
	 * UNION型データ型を探索する.
	 * @param entry		UNION型
	 */
	public void entry(Union entry);

}
