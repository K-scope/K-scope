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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import jp.riken.kscope.common.ACCESSMEMORY_TYPE;
import jp.riken.kscope.information.InformationBlocks;

/**
 * 実行文に現れる変数を表現するクラス。
 *
 * @author RIKEN
 *
 */
public class Variable implements Serializable {
	/** シリアル番号 */
	private static final long serialVersionUID = 7677119337098266464L;
	/** 変数名 */
    private String name;
    /** 変数の値 */
    private String value;
    /** 配列添え字 */
    private List<Expression> indexValues;
    /** 変数定義 */
    private VariableDefinition def;
    /** アクセス先メモリ */
    private ACCESSMEMORY_TYPE memoryType;
    /** 変数の親ブロック : 変数が属する代入文(Substitution),構文 */
    private IBlock parentStatement;
    /** 一時設定アクセス先メモリ */
    private transient ACCESSMEMORY_TYPE temporaryMemoryType;

    /**
     * コンストラクタ。
     * @param varName
     *          変数名
     */
    public Variable(String varName) {
        name = varName;
        this.memoryType = null;
    }

    // ++++++++++++++++++++++++++++++++++++++++++++
    @Override
    public String toString() {
        return (name);
    }

    // ++++++++++++++++++++++++++++++++++++++++++++

    /**
     * 変数名のセット。
     * @param nm
     *          変数名
     */
    public void setName(String nm) {
        name = nm;
    }

    // ++++++++++++++++++++++++++++++++++++++++++++

    /**
     * 変数定義のセット。
     * @param varDef
     *          変数定義
     */
    public void setDefinition(VariableDefinition varDef) {
        def = varDef;
    }

    // ++++++++++++++++++++++++++++++++++++++++++++

    /**
     * 値のセット。
     * @param val
     *          変数の値
     */
    public void setValue(String val) {
        value = val;
    }

    // ++++++++++++++++++++++++++++++++++++++++++++
    /**
     * 配列添え字の値のセット。
     * @param val 添字のリスト
     */
    public void setDimensionIndexValue(List<Expression> val) {
        indexValues = val;
    }
    // ++++++++++++++++++++++++++++++++++++++++++++

    /**
     * 変数名の取得。
     * @return
     *      変数名
     */
    public String getName() {
        return (name);
    }

    // ++++++++++++++++++++++++++++++++++++++++++++

    /**
     * 値の取得。
     * @return 値
     */
    public String getValue() {
        return (value);
    }

    // ++++++++++++++++++++++++++++++++++++++++++++

    /**
     * 変数定義の取得。
     * @return 変数定義
     */
    public VariableDefinition getDefinition() {
        return (def);
    }

    /**
     * 配列添え字の値の取得
     * @return   配列添え字
     */
    public List<Expression> getDimensionIndexValue() {
        return this.indexValues;
    }

    /**
     * 変数のソースコード上での表示を再現して返す。
     *
     * @return 変数の表示
     */
    public String getVariableString() {
        StringBuilder st = new StringBuilder();
        st.append(name);
        if (this.indexValues != null) {
            st.append("(");
            for (Expression ex : indexValues) {
                st.append(ex.getLine());
                st.append(",");
            }
            st.replace(st.length() - 1, st.length(), ")");
            //System.out.println("variable name is " + name + "," + "indexValues are exist.");
            //System.out.println("VariableString is " + st.toString());
        } else {
            //System.out.println("variable name is " + name + "," + "indexValues are null.");
        }
        return st.toString();
    }
    /**
     * 付加情報ブロックコレクションを生成する。
     *
     * @return 付加情報ブロックコレクション
     */
    public InformationBlocks createInformationBlocks() {
        InformationBlocks result = new InformationBlocks();
        if (this.indexValues != null) {
            for (Expression expression : this.indexValues) {
                result.addAll(expression.createInformationBlocks());
            }
        }
        return result;
    }

    /**
     * idにマッチした情報ブロックを検索する。
     * @param id
     *          ID
     * @return 見つかった情報ブロック。見つからなかった場合はnullが返ります。
     */
    public IInformation findInformationBlockBy(String id) {
        IInformation result = null;
        if (this.indexValues != null) {
            for (Expression expression : this.indexValues) {
                result = expression.findInformationBlockBy(id);
                if (result != null) { break; }
            }
        }
        return result;
    }

    /**
     * 自身の添字に含まれる変数を全て含めたセットを返す。
     * @return 変数のセット
     */
    public Set<Variable> getAllVariables() {
        Set<Variable> set = new HashSet<Variable>();
        List<Expression> exps = this.indexValues;
        if (exps != null) {
            for (Expression exp: exps) {
                set.addAll(exp.getAllVariables());
            }
        }
        return set;
    }

    /**
     * 自身が配列式であるか判定する。
     * @return 真偽値。配列式であれば真
     */
    public boolean isArrayExpression() {
        if (this.indexValues == null) {
            return false;
        }
        for (Expression ex: this.indexValues) {
            if (ex.toString().contains(":")) {
                return true;
            }
        }
        return false;
    }


    /**
     * 式に含まれる全ての手続呼出のセットを返す。変数および手続呼出の添字も対象とする。 再帰呼び出し。
     * @return 手続呼出のセット。
     */
    public Set<ProcedureUsage> getAllFunctions() {
    	if (this.indexValues == null) return null;
    	if (this.indexValues.size() <= 0) return null;
        Set<ProcedureUsage> pus = new HashSet<ProcedureUsage>();
        for (Expression exp: this.indexValues) {
        	Set<ProcedureUsage> list = exp.getAllFunctions();
        	if (list != null && list.size() > 0) {
        		pus.addAll(exp.getAllFunctions());
        	}
        }
        if (pus.size() <= 0) return null;
        return pus;
    }

    /**
     * 同じ変数であるかチェックする.
     * @param destVar		変数クラス
     * @return		true=同じ
     */
	public boolean equalsVariable(Variable destVar) {
		if (destVar == null) return false;
		if (this.name != null) {
			if (!this.name.equalsIgnoreCase(destVar.name)) {
				return false;
			}
		}
		else if (destVar.name != null) {
			return false;
		}
		if (this.value != null) {
			if (!this.value.equalsIgnoreCase(destVar.value)) {
				return false;
			}
		}
		else if (destVar.value != null) {
			return false;
		}

		if (this.indexValues != null && destVar.indexValues != null) {
			if (this.indexValues.size() == destVar.indexValues.size()) {
	            for (int i=0; i<this.indexValues.size(); i++) {
	            	Expression thisExp = this.indexValues.get(i);
	            	Expression destExp = destVar.indexValues.get(i);
	            	if (thisExp == destExp) {
	            		continue;
	            	}
	            	else if (thisExp == null) {
	            		return false;
	            	}
	            	else if (!thisExp.equalsExpression(destExp)) {
	            		return false;
	            	}
	            }
			}
		}
		else if (this.indexValues != null || destVar.indexValues != null) {
			return false;
		}

		return true;
	}

	/**
	 * 同一ブロックを検索する
	 * @param block			IInformationブロック
	 * @return		同一ブロック
	 */
	public IInformation[] searchInformationBlocks(IInformation block) {
		if (block == null) return null;

		List<IInformation> list = new ArrayList<IInformation>();
        if (this.indexValues != null) {
            for (Expression expression : this.indexValues) {
            	IInformation[] infos = expression.searchInformationBlocks(block);
            	if (infos != null) {
    	        	list.addAll(Arrays.asList(infos));
    	        }
            }
        }
        if (list.size() <= 0) {
        	return null;
        }

		return list.toArray(new IInformation[0]);
	}

	/**
	 * メモリアクセス対象変数であるかチェックする.
	 * 実数型変数 : scaler and array
	 * 整数型変数 : array
	 * @return		true=メモリアクセス対象
	 */
	public boolean isMemoryAccess() {
		// 配列であること
		if (this.def == null) return false;
		if (this.def.getVariableType() == null) return false;
		if (this.def.getVariableType().isRealType()) {
			return true;
		}
		if (this.def.getVariableType().isIntegerType()) {
			if (this.def.get_dimension_size() > 0) {
				return true;
			}
		}
		return false;
	}

	/**
	 * アクセス先メモリを取得する.
	 * @return アクセス先メモリ
	 */
	public ACCESSMEMORY_TYPE getMemoryType() {
		return memoryType;
	}

	/**
	 * アクセス先メモリを設定する.
	 * @param memory アクセス先メモリ
	 */
	public void setMemoryType(ACCESSMEMORY_TYPE memory) {
		this.memoryType = memory;
	}

	/**
	 * アクセス先メモリ設定をクリアする.
	 */
	public void clearMemoryType() {
		this.memoryType = null;
		this.temporaryMemoryType = null;
	}

	/**
	 * 親ブロックを取得する.
	 * 親ブロックは変数が属する代入文(Substitution),構文とする.
	 * @return 親ブロック
	 */
	public IBlock getParentStatement() {
		return parentStatement;
	}

	/**
	 * 親ブロックを設定する.
	 * 親ブロックは変数が属する代入文(Substitution),構文とする.
	 * @param parent 親ブロック
	 */
	public void setParentStatement(IBlock parent) {
		this.parentStatement = parent;
		// 配列添え字に対して設定を行う
		if (this.indexValues != null) {
			for (Expression exp : this.indexValues) {
				if (exp == null) continue;
				exp.setParentStatement(parent);
			}
		}
	}

    /**
     * 変数添字式の加算カウントを返す.
     * 子変数、添字、関数引数もカウントする.
     * @return 加算カウント
     */
	public int getOperandAddCount() {
		int count = 0;
        List<Expression> exps = this.indexValues;
        if (exps != null) {
            for (Expression exp: exps) {
            	count = exp.getOperandAddCount();
            }
        }
        return count;
	}


    /**
     * 変数添字式の減算カウントを返す.
     * 子変数、添字、関数引数もカウントする.
     * @return 減算カウント
     */
	public int getOperandSubCount() {
		int count = 0;
        List<Expression> exps = this.indexValues;
        if (exps != null) {
            for (Expression exp: exps) {
            	count = exp.getOperandSubCount();
            }
        }
        return count;
	}

    /**
     * 変数添字式の乗算カウントを返す.
     * 子変数、添字、関数引数もカウントする.
     * @return 乗算カウント
     */
    public int getOperandMulCount() {
		int count = 0;
        List<Expression> exps = this.indexValues;
        if (exps != null) {
            for (Expression exp: exps) {
            	count = exp.getOperandMulCount();
            }
        }
        return count;
    }

    /**
     * 変数添字式の除算カウントを返す.
     * 子変数、添字、関数引数もカウントする.
     * @return 除算カウント
     */
    public int getOperandDivCount() {
		int count = 0;
        List<Expression> exps = this.indexValues;
        if (exps != null) {
            for (Expression exp: exps) {
            	count = exp.getOperandDivCount();
            }
        }
        return count;
    }
    /**
     * 変数添字式の累算カウントを返す.
     * 子変数、添字、関数引数もカウントする.
     * @return 累算カウント
     */
    public int getOperandPowCount() {
		int count = 0;
        List<Expression> exps = this.indexValues;
        if (exps != null) {
            for (Expression exp: exps) {
            	count = exp.getOperandPowCount();
            }
        }
        return count;
    }

	/**
	 * 一時設定アクセス先メモリを取得する
	 * @return 一時設定アクセス先メモリ
	 */
	public ACCESSMEMORY_TYPE getTemporaryMemoryType() {
		return temporaryMemoryType;
	}

	/**
	 * 一時設定アクセス先メモリを設定する
	 * @param memory    一時設定アクセス先メモリ
	 */
	public void setTemporaryMemoryType(ACCESSMEMORY_TYPE memory) {
		this.temporaryMemoryType = memory;
	}
}

