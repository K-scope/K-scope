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
package jp.riken.kscope.properties;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jp.riken.kscope.common.ACCESSMEMORY_TYPE;
import jp.riken.kscope.data.Keyword;
import jp.riken.kscope.data.RequiredBF;
import jp.riken.kscope.data.VariableMemory;
import jp.riken.kscope.language.Variable;


/**
 * ソースコードの変数アクセス先メモリ設定クラス
 * @author RIKEN
 */
public class VariableMemoryProperties extends PropertiesBase {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** 変数(ハイライト)設定リスト */
    private List<VariableMemory> listVariable = new ArrayList<VariableMemory>();
    /** 要求Byte/FLOP設定プロパティ */
    RequiredBFProperties memoryProperties;

    /**
     * コンストラクタ
     * @param properties  要求Byte/FLOP設定プロパティ
     */
    public VariableMemoryProperties(RequiredBFProperties properties) {
    	this.memoryProperties = properties;
    }

    /**
     * プロパティ変更イベントを通知する。
     */
    @Override
    public void firePropertyChange() {
        this.changes.firePropertyChange(this.getClass().getName(), null, this);
    }


    /**
     * 変数(ハイライト)設定リストを取得する。
     * @return		変数ハイライト設定リスト
     */
    public List<VariableMemory> getListVariableMemory() {
        return this.listVariable;
    }

    /**
     * 変数(ハイライト)数を取得する。
     * @return		変数(ハイライト)数
     */
    public int getVariableCount() {
        if (listVariable == null || listVariable.size() <= 0) {return 0;}
        return listVariable.size();
    }

    /**
     * 変数(ハイライト)を取得する。
     * @param	index		インデックス
     * @return		変数(ハイライト)
     */
    public Keyword getVariableMemory(int index) {
        if (listVariable == null || listVariable.size() <= 0) {return null;}
        if (listVariable.size() <= index) {return null;}

        return listVariable.get(index);
    }

    /**
     * 変数(ハイライト)を設定する。
     * @param	index		インデックス
     * @param	variable		変数(ハイライト)
     */
    public void setVariableMemory(int index, VariableMemory variable) {
        if (listVariable == null || listVariable.size() <= 0) {return;}
        if (listVariable.size() <= index) {return;}
        listVariable.set(index, variable);
    }

    /**
     * 変数(ハイライト)を追加する。
     * @param	variable		変数(ハイライト)
     */
    public void addVariableMemory(VariableMemory variable) {
        if (listVariable == null) {
        	listVariable = new ArrayList<VariableMemory>();
        }
        listVariable.add(variable);
    }

    /**
     * 変数(ハイライト)を削除する。
     * @param	variable		変数(ハイライト)
     */
    public void removeVariableMemory(VariableMemory variable) {
        if (listVariable == null) return;
        listVariable.remove(variable);
    }

    /**
     * 変数(ハイライト)を削除する。
     * @param	index		インデックス
     */
    public void removeVariableMemory(int index) {
        if (listVariable == null) return;
        listVariable.remove(index);
    }

    /**
     * 変数(ハイライト)を削除する。
     * @param	variable		変数(ハイライト)
     */
    public void removeVariable(Variable variable) {
        if (listVariable == null) return;
        Iterator<VariableMemory> iter = this.listVariable.iterator();
        while(iter.hasNext()){
        	VariableMemory varmem = (VariableMemory)iter.next();
			Variable var = varmem.getVariable();
			if (var == variable) {
				iter.remove();
			}
        }
    }

    /**
     * 変数(ハイライト)リストをクリアする。
     */
    public void clearVariableMemory() {
    	listVariable = new ArrayList<VariableMemory>();
    }

    /**
     * 変数(ハイライト)設定の取得を行う.
     * @param variable		変数
     */
	public VariableMemory getVariableMemory(Variable variable) {
		if (variable == null) return null;
		if (this.listVariable == null || this.listVariable.size() <= 0) return null;
		for (VariableMemory varmem : this.listVariable) {
			Variable var = varmem.getVariable();
			if (var == variable) {
				return varmem;
			}
		}
		return null;
	}

    /**
     * 変数(ハイライト)設定が追加済みであるかチェックする.
     * @param variable		変数
     */
	public boolean containsVariableMemory(Variable variable) {
		if (variable == null) return false;
		VariableMemory varmem = getVariableMemory(variable);
		return (varmem != null);
	}

    /**
     * 変数の追加を行う.
     * @param variable		変数
     */
	public void addVariable(Variable variable) {
		if (variable == null) return;
		if (containsVariableMemory(variable)) {
			VariableMemory varmem = getVariableMemory(variable);
			RequiredBF mem = getMemoryband(variable);
			varmem.setMemoryband(mem);
		}
		else {
			// 変数メモリデータの生成
			VariableMemory varmem = createVariableMemory(variable);
			addVariableMemory(varmem);
		}
	}

	/**
	 * 変数メモリデータオブジェクトを生成する.
	 * @param variable		変数
	 * @return		変数メモリデータオブジェクト
	 */
	private VariableMemory createVariableMemory(Variable variable) {
		// 変数メモリデータの生成
		RequiredBF mem = getMemoryband(variable);
		VariableMemory varmem = new VariableMemory(variable, mem);

		return varmem;
	}


	/**
	 * アクセス先メモリを取得する.
	 * @param variable		変数
	 * @return		アクセス先メモリ
	 */
	private RequiredBF getMemoryband(Variable variable) {
		// 変数メモリデータの生成
		ACCESSMEMORY_TYPE memorytype = variable.getMemoryType();
		if (memorytype == null) {
			memorytype = ACCESSMEMORY_TYPE.getDefaultType();
			if (variable.getDefinition() != null) {
				memorytype = ACCESSMEMORY_TYPE.getDefaultType(variable.getDefinition());
			}
		}
		RequiredBF mem = memoryProperties.getRequiredBF(memorytype);
		return mem;
	}

	/**
	 * 設定済みリストから同一定義の変数を取得する.
	 * 同一定義、同一添字の変数リストを取得する.
	 * @param variable		検索変数
	 * @return			同一定義変数リスト
	 */
	public List<Variable> getEqualsVariableDefinition(Variable variable) {
		if (variable == null) return null;
		if (variable.getDefinition() == null) return null;
		if (this.listVariable == null || this.listVariable.size() <= 0) return null;
		List<Variable> list = new ArrayList<Variable>();
		for (VariableMemory varmem : this.listVariable) {
			Variable var = varmem.getVariable();
			if (variable.getDefinition() != var.getDefinition()) continue;
			if (variable.equalsVariable(var)) {
				list.add(var);
			}
		}
		if (list.size() <= 0) return null;
		return list;
	}

	/**
	 * 設定済みリストから変数リストを取得する.
	 * @return			変数リスト
	 */
	public List<Variable> getListVariable() {
		if (this.listVariable == null || this.listVariable.size() <= 0) return null;
		List<Variable> list = new ArrayList<Variable>();
		for (VariableMemory varmem : this.listVariable) {
			Variable var = varmem.getVariable();
			list.add(var);
		}
		if (list.size() <= 0) return null;
		return list;
	}
}


