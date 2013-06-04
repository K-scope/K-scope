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

import java.util.ArrayList;
import java.util.List;

import jp.riken.kscope.language.*;
import jp.riken.kscope.language.fortran.*;
import jp.riken.kscope.language.generic.*;

/**
 * アクセス先メモリ設定変数探索クラス.
 * アクセス先メモリ設定変数を取得する.
 * @author riken
 */
public class VariableMemoryEntry implements ILanguageEntry {
	/** アクセス先メモリ設定変数 */
	private List<Variable> listVariable;
	/** Fortranデータベース */
	private Fortran language;

	/**
	 * コンストラクタ
	 * @param    Fortranデータベース
	 */
	public VariableMemoryEntry(Fortran language) {
		this.language = language;
		this.listVariable = new ArrayList<Variable>();
	}

	/**
	 * アクセス先メモリ設定変数を追加する.
	 */
	@Override
	public void entry(Variable entry) {
		if (entry == null) return;
		if (entry.getMemoryType() == null) return;
		// アクセス先メモリ設定変数を追加する.
		addVariable(entry);
		return;
	}

	@Override
	public void entry(Module entry) { }

	@Override
	public void entry(Procedure entry) { }

	@Override
	public void entry(ProcedureUsage entry) { }

	@Override
	public void entry(VariableDefinition entry) { }

	@Override
	public void entry(VariableType entry) { }

	@Override
	public void entry(Break entry) { }

	@Override
	public void entry(Common entry) { }

	@Override
	public void entry(Condition entry) { }

	@Override
	public void entry(Continue entry) { }

	@Override
	public void entry(Data entry) { }

	@Override
	public void entry(Directive entry) { }

	@Override
	public void entry(DoNothing entry) { }

	@Override
	public void entry(DynamicAllocation entry) { }

	@Override
	public void entry(DynamicDeallocation entry) { }

	@Override
	public void entry(DynamicNullification entry) { }

	@Override
	public void entry(Equivalence entry) { }

	@Override
	public void entry(ExecutableBody entry) { }

	@Override
	public void entry(GoTo entry) { }

	@Override
	public void entry(Pause entry) { }

	@Override
	public void entry(Procedures entry) { }

	@Override
	public void entry(Repetition entry) { }

	@Override
	public void entry(Return entry) { }

	@Override
	public void entry(Selection entry) { }

	@Override
	public void entry(Substitution entry) { }

	@Override
	public void entry(Termination entry) { }

	@Override
	public void entry(UserDefined entry) { }

	@Override
	public void entry(UseState entry) { }

	@Override
	public void entry(ProcedureWithNameOnly entry) { }

	@Override
	public void entry(VariableAttribute entry) { }

	@Override
	public void entry(VariableDimension entry) { }

	@Override
	public void entry(DimensionIndex entry) { }

	@Override
	public void entry(Expression entry) { }

	@Override
	public void entry(ProcedureItem entry) { }

	@Override
	public void entry(Type entry) { }

	@Override
	public void entry(Structure entry) {}

	@Override
	public void entry(Union entry) { }

	@Override
	public List<Object> getListVisit() {
		return null;
	}

	@Override
	public void setListVisit(List<Object> list) {
	}

	@Override
	public Fortran getLanguage() {
		return this.language;
	}

	@Override
	public void setLanguage(Fortran language) {
		this.language = language;
	}

	/**
	 * アクセス先メモリ設定変数を取得する.
	 * @return  アクセス先メモリ設定変数リスト
	 */
	public Variable[] getListVariable() {
		if (this.listVariable == null || this.listVariable.size() <= 0) {
			return null;
		}
		return listVariable.toArray(new Variable[0]);
	}

	/**
	 * アクセス先メモリ設定変数を追加する.
	 * @param var		アクセス先メモリ設定変数
	 */
	private void addVariable(Variable var) {
		if (this.listVariable.contains(var)) return;
		this.listVariable.add(var);
	}
}
