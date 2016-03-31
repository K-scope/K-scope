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

package jp.riken.kscope.language.utils;

import java.util.ArrayList;
import java.util.List;

import jp.riken.kscope.information.InformationBlock;
import jp.riken.kscope.information.InformationBlocks;
import jp.riken.kscope.language.*;
import jp.riken.kscope.language.fortran.*;
import jp.riken.kscope.language.generic.*;

/**
 * 付加情報探索クラス.
 * 指定ブロック配下のすべての付加情報を取得する.
 * @author RIKEN
 * @version    2015/03/15     探索履歴リストチェックメソッドの追加
 */
public class InformationEntry implements ILanguageEntry {
	/** 付加情報リスト */
	private List<IInformation> listInformation;
	/** Fortranデータベース */
	private Fortran language;

	/**
	 * コンストラクタ
	 * @param   language    Fortranデータベース
	 */
	public InformationEntry(Fortran language) {
		this.language = language;
		this.listInformation = new ArrayList<IInformation>();
	}

	/**
	 * アクセス先メモリ設定変数を追加する.
	 */
	@Override
	public void entry(Variable entry) { }

	@Override
	public void entry(Module entry) {
		addInformation(entry);
	}

	@Override
	public void entry(Procedure entry) {
		addInformation(entry);
	}

	@Override
	public void entry(ProcedureUsage entry) {
		addInformation(entry);
	}

	@Override
	public void entry(VariableDefinition entry) {
		addInformation(entry);
	}

	@Override
	public void entry(VariableType entry) { }

	@Override
	public void entry(Break entry) {
		addInformation(entry);
	}

	@Override
	public void entry(Common entry) {
		addInformation(entry);
	}

	@Override
	public void entry(Condition entry) {
		addInformation(entry);
	}

	@Override
	public void entry(Continue entry) {
		addInformation(entry);
	}

	@Override
	public void entry(Data entry) {
		addInformation(entry);
	}

	@Override
	public void entry(Directive entry) {
		addInformation(entry);
	}

	@Override
	public void entry(DoNothing entry) {
		addInformation(entry);
	}

	@Override
	public void entry(DynamicAllocation entry) {
		addInformation(entry);
	}

	@Override
	public void entry(DynamicDeallocation entry) {
		addInformation(entry);
	}

	@Override
	public void entry(DynamicNullification entry) {
		addInformation(entry);
	}

	@Override
	public void entry(Equivalence entry) {
		addInformation(entry);
	}

	@Override
	public void entry(ExecutableBody entry) {
		addInformation(entry);
	}

	@Override
	public void entry(GoTo entry) {
		addInformation(entry);
	}

	@Override
	public void entry(Pause entry) {
		addInformation(entry);
	}

	@Override
	public void entry(Procedures entry) {
		addInformation(entry);
	}

	@Override
	public void entry(Repetition entry) {
		addInformation(entry);
	}

	@Override
	public void entry(Return entry) {
		addInformation(entry);
	}

	@Override
	public void entry(Selection entry) {
		addInformation(entry);
	}

	@Override
	public void entry(Substitution entry) {
		addInformation(entry);
	}

	@Override
	public void entry(Termination entry) {
		addInformation(entry);
	}

	@Override
	public void entry(UserDefined entry) {
		addInformation(entry);
	}

	@Override
	public void entry(UseState entry) {
		addInformation(entry);
	}

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
	public void setListVisit(List<Object> list) { }

    @Override
    public boolean containsListVisit(Object obj) {
        return false;
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
	 * 付加情報を取得する.
	 * @return  付加情報リスト
	 */
	public IInformation[] getListInformation() {
		if (this.listInformation == null || this.listInformation.size() <= 0) {
			return null;
		}
		return listInformation.toArray(new IInformation[0]);
	}

	/**
	 * 付加情報(InformationBlocks)を取得する.
	 * @return  付加情報リスト
	 */
	public InformationBlocks getInformationBlocks() {
		if (this.listInformation == null || this.listInformation.size() <= 0) {
			return null;
		}
		InformationBlocks blocks = new InformationBlocks();
		for (IInformation info : this.listInformation) {
			if (info instanceof InformationBlock) {
				blocks.add((InformationBlock)info);
			}
			else {
				InformationBlock newblock = new InformationBlock(info.getInformation(), info, info);
				blocks.add(newblock);
			}
		}
		if (blocks.size() <= 0) return null;
		return blocks;
	}


	/**
	 * 付加情報を追加する.
	 * @param info		付加情報ブロック
	 */
	private void addInformation(IInformation info) {
		if (info == null) return;
		IInformation block = getInfoamationBlock(info);
		if (block != null) {
			if (!this.listInformation.contains(block)) {
				this.listInformation.add(block);
			}
		}
		if (info.getInformation() == null) return;
		if (info.getInformation().getContent() == null) return;
		if (info.getInformation().getContent().isEmpty()) return;
		if (this.listInformation.contains(info)) return;
		this.listInformation.add(info);
	}

	/**
	 * 付加情報を取得する.
	 * 付加情報ブロックから開始ブロックと一致する付加情報を取得する.
	 * @param info		付加情報ブロック
	 * @return 付加情報
	 */
	private IInformation getInfoamationBlock(IInformation info) {
		if (info == null) return null;
		InformationBlocks blocks = this.language.getInformationBlocks();
		for (InformationBlock block : blocks) {
			if (block.getStartBlock() == info) {
				if (block.getInformation() == null) return null;
				if (block.getInformation().getContent() == null) return null;
				if (block.getInformation().getContent().isEmpty()) return null;
				return block;
			}
		}
		return null;
	}

	/**
	 * 付加情報リストをクリアする.
	 */
	public void clearListInformation() {
		this.listInformation = new ArrayList<IInformation>();
	}
}

