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
package jp.riken.kscope.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import jp.riken.kscope.common.ACCESSMEMORY_TYPE;
import jp.riken.kscope.data.RequiredBF;
import jp.riken.kscope.data.RequiredBFResult;
import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Substitution;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.language.fortran.VariableType;
import jp.riken.kscope.language.utils.LanguageVisitor;
import jp.riken.kscope.language.utils.OperationCounterUtils;
import jp.riken.kscope.language.utils.VariableMemoryEntry;
import jp.riken.kscope.model.RequiredBFModel;
import jp.riken.kscope.properties.RequiredBFProperties;
import jp.riken.kscope.properties.RequiredBFProperties.MEM_THROUGHPUT_CALC_MODE;
import jp.riken.kscope.properties.OperationProperties;
import jp.riken.kscope.properties.VariableMemoryProperties;
import jp.riken.kscope.utils.StringUtils;

/**
 * メモリ性能算出を行う
 * @author RIKEN
 */
public class AnalysisMemoryService extends AnalysisBaseService {

    /** 要求Byte/FLOP設定プロパティ */
    private RequiredBFProperties properitiesRequiredBF;
    /** 組込み関数演算カウントプロパティ */
    private OperationProperties propertiesOperation;
    /** 要求Byte/FLOPテーブルモデル */
    private RequiredBFModel modelRequiredBF;
    /** 要求Byte/FLOP算出結果 */
    private List<RequiredBFResult> requiedBFResults;
    /** 選択ブロック */
    private List<IBlock> blocks;
    /** 変数アクセス先メモリ設定プロパティ */
    private VariableMemoryProperties propertiesVariableMemory;

    /**
     * コンストラクタ
     */
    public AnalysisMemoryService() {
        super();
    }

	/**
	 * 要求Byte/FLOP設定プロパティを取得する.
	 * @return 要求Byte/FLOP設定プロパティ
	 */
	public RequiredBFProperties getProperitiesRequiredBF() {
		return properitiesRequiredBF;
	}

	/**
	 * 要求Byte/FLOP設定プロパティを設定する.
	 * @param properities 要求Byte/FLOP設定プロパティ
	 */
	public void setProperitiesRequiredBF(RequiredBFProperties properities) {
		this.properitiesRequiredBF = properities;
	}

	/**
	 * 組込み関数演算カウントプロパティを取得する.
	 * @return 組込み関数演算カウントプロパティ
	 */
	public OperationProperties getPropertiesOperand() {
		return propertiesOperation;
	}

	/**
	 * 組込み関数演算カウントプロパティを設定する.
	 * @param properities 組込み関数演算カウントプロパティ
	 */
	public void setPropertiesOperand(OperationProperties properities) {
		this.propertiesOperation = properities;
	}

	/**
	 * 選択ブロックを取得する.
	 * @return 選択ブロック
	 */
	public IBlock[] getBlocks() {
		return blocks.toArray(new IBlock[0]);
	}

	/**
	 * 選択ブロックを設定する.
	 * @param blocks 選択ブロック
	 */
	public void setBlocks(IBlock[] blocks) {
		if (this.blocks == null) {
			this.blocks = new ArrayList<IBlock>();
		}
		this.blocks.addAll(Arrays.asList(blocks));
	}

	/**
	 * 要求Byte/FLOPテーブルモデルを取得する.
	 * @return 要求Byte/FLOPテーブルモデル
	 */
	public RequiredBFModel getModelRequiredBF() {
		return modelRequiredBF;
	}

	/**
	 * 要求Byte/FLOPテーブルモデルを設定する.
	 * @param model 要求Byte/FLOPテーブルモデル
	 */
	public void setModelRequiredBF(RequiredBFModel model) {
		this.modelRequiredBF = model;
	}

	/**
	 * 要求Byte/FLOP算出結果を取得する.
	 * @return 要求Byte/FLOP算出結果
	 */
	public List<RequiredBFResult> getReqiedBFResults() {
		return requiedBFResults;
	}

	/**
	 * 要求Byte/FLOP算出結果を設定する.
	 * @param reqiedBFResults  要求Byte/FLOP算出結果
	 */
	public void setReqiedBFResults(List<RequiredBFResult> reqiedBFResults) {
		this.requiedBFResults = reqiedBFResults;
	}

	/**
	 * 要求Byte/FLOPを算出する.
	 * @param block    算出ブロック
	 * @return    要求Byte/FLOP算出結果
	 */
	public RequiredBFResult calcRequiredBF(IBlock block) {
		if (block == null) return null;

		// 演算数,Load,Storeをカウントする.
		OperationCounterUtils utils = new OperationCounterUtils();
		utils.setPropertiesOperation(this.propertiesOperation);
		utils.countBlock(block);

		RequiredBFResult result = new RequiredBFResult();
		// 算出ブロック
		result.setBlock(block);
		// 右辺変数
		List<Variable> rights = utils.getRightVariables();
		// 左辺変数
		List<Variable> lefts = utils.getLeftVariables();
		// Load, Store
		setStoreLoad(result, lefts, rights);
        // 演算数
		setOperation(result, utils.getAddFlop(),
						   utils.getSubFlop(),
						   utils.getMulFlop(),
						   utils.getDivFlop(),
						   utils.getPowFlop(),
						   utils.getFunctionFlop());
        // 要求Byte/FLOP,要求FLOP/Byte
		result.calcRequiredBF();
		// メモリスループット
		setMemThroughput(result, lefts, rights);
		// 実効Byte/FLOP,実効FLOP/Byte
		result.calcRequiredBF(this.properitiesRequiredBF.getFlopPerformance());
		// ピーク性能
		result.calcPeakPerformance();
		// アクセス先メモリカウント
		setAccessCount(result, lefts, rights);
		// BF算出単位
		result.setBFCalcType(this.properitiesRequiredBF.getBFCalcType());

		return result;
	}

	/**
	 * 演算数を設定する.
	 * @param result	算出結果
	 * @param add		加算(+)
	 * @param sub		減算(-)
	 * @param mul		乗算(*)
	 * @param div		除算(/)
	 * @param pow		累乗
	 * @param function	組込関数の加算(+) + 乗算(*)
	 */
	private void setOperation(RequiredBFResult result, int add, int sub, int mul, int div, int pow, int function) {
		if (result == null) return;

		// 演算数(FLOP) = add(F) + mul(F) + intrinsic(F)
		int op = add + sub + mul + div + pow + function;
		result.setOperation(op);
		// 浮動小数点データ型の変数に対する加算(+)の数
		result.setAddCount(add);
		// 浮動小数点データ型の変数に対する減算(-)の数
		result.setSubCount(sub);
		// 浮動小数点データ型の変数に対する乗算(*)の数
		result.setMulCount(mul);
		// 浮動小数点データ型の変数に対する除算(/)の数
		result.setDivCount(div);
		// 浮動小数点データ型の変数に対する累乗, 組込関数の加算(+) + 乗算(*)
		result.setIntrinsicCount(pow + function);

		return;
	}

	/**
	 * Store, Loadを算出結果に設定します.
	 * @param result		算出結果
	 * @param lefts			左辺変数リスト
	 * @param rights		右辺変数リスト
	 */
	private void setStoreLoad(RequiredBFResult result, List<Variable> lefts, List<Variable> rights) {
		if (result == null) return;
		// すべての変数リスト
		List<Variable> all = getMargeVariable(lefts, rights);
		// 変数バイト数
		int loadByte = getVariableMemoryByte(all);
		int leftByte = getVariableMemoryByte(lefts);
		// Load算出 = right + left
		result.setLoad(loadByte);
		// Store変数
		result.setStore(leftByte);
	}

	/**
	 * 左辺変数リストと右辺変数リストをマージします.
	 * 同一変数は除外します.
	 * @param lefts			左辺変数リスト
	 * @param rights		右辺変数リスト
	 * @return		マージ変数リスト
	 */
	private List<Variable> getMargeVariable(List<Variable> lefts, List<Variable> rights) {
		List<Variable> all = new ArrayList<Variable>();
		OperationCounterUtils utils = new OperationCounterUtils();
		if (lefts != null) {
			for (Variable var : lefts) {
				if (utils.getVariables(all, var) == null) {
					all.add(var);
				}
			}
		}
		if (rights != null) {
			for (Variable var : rights) {
				if (utils.getVariables(all, var) == null) {
					all.add(var);
				}
			}
		}
		if (all.size() <= 0) return null;
		return all;
	}

	/**
	 * メモリスループットを設定する.
	 * @param result		算出結果
	 * @param lefts			左辺変数リスト
	 * @param rights		右辺変数リスト
	 */
	private void setMemThroughput(RequiredBFResult result, List<Variable> lefts, List<Variable> rights) {
		if (result == null) return;

		// Store有り、無しの判定
		// 左辺変数と要求Byte/FLOP設定ダイアログのスループットモードから判断する.
		boolean isstore = isStore(lefts);

		// スループット(GB/s)
		float throughput = 0.0F;
		if (isstore) {
			throughput= this.properitiesRequiredBF.getMemThroughputStore();
		}
		else {
			throughput= this.properitiesRequiredBF.getMemThroughputNostore();
		}
		result.setThroughput(throughput);

		// メモリスループット算出モード
		result.setMemThroughputCalcMode(this.properitiesRequiredBF.getMemThroughputCalcMode());
		// 算出元のスループット値(GB/s)（ストア有り or ストアなし）
		RequiredBF memory = this.properitiesRequiredBF.getRequiredBF(ACCESSMEMORY_TYPE.MEMORY);
		RequiredBF l1 = this.properitiesRequiredBF.getRequiredBF(ACCESSMEMORY_TYPE.L1_CACHE);
		RequiredBF l2 = this.properitiesRequiredBF.getRequiredBF(ACCESSMEMORY_TYPE.L2_CACHE);
		RequiredBF register = this.properitiesRequiredBF.getRequiredBF(ACCESSMEMORY_TYPE.REGISTER);
		RequiredBF custom = this.properitiesRequiredBF.getRequiredBF(ACCESSMEMORY_TYPE.CUSTOM);
		if (isstore) {
			// 算出元のスループット値(GB/s)（ストア有り）
			result.setMemoryMBW(memory.getMemThroughputStore());
			result.setL1MBW(l1.getMemThroughputStore());
			result.setL2MBW(l2.getMemThroughputStore());
			result.setRegisterMBW(register.getMemThroughputStore());
			result.setCustomMBW(custom.getMemThroughputStore());
		}
		else {
			// 算出元のスループット値(GB/s)（ストアなし）
			result.setMemoryMBW(memory.getMemThroughputNostore());
			result.setL1MBW(l1.getMemThroughputNostore());
			result.setL2MBW(l2.getMemThroughputNostore());
			result.setRegisterMBW(register.getMemThroughputNostore());
			result.setCustomMBW(custom.getMemThroughputNostore());
		}
		// 係数
		result.setMemoryCoef(memory.getCoef());
		result.setL1Coef(l1.getCoef());
		result.setL2Coef(l2.getCoef());
		result.setRegisterCoef(register.getCoef());
		result.setCustomCoef(custom.getCoef());
	}

	/**
	 * メモリアクセス先メモリを設定する.
	 * @param result		算出結果
	 * @param lefts			左辺変数リスト
	 * @param rights		右辺変数リスト
	 */
	private void setAccessCount(RequiredBFResult result, List<Variable> lefts, List<Variable> rights) {
		if (result == null) return;
		int memory = 0;
		int l1 = 0;
		int l2 = 0;
		int register = 0;
		int custom = 0;
		if (lefts != null && lefts.size() > 0) {
			for (Variable var : lefts) {
				ACCESSMEMORY_TYPE type = getMemoryType(var);
				if (type == null) {
					type = ACCESSMEMORY_TYPE.getDefaultType(var);
				}
				if (type == ACCESSMEMORY_TYPE.MEMORY) memory++;
				else if (type == ACCESSMEMORY_TYPE.L1_CACHE) l1++;
				else if (type == ACCESSMEMORY_TYPE.L2_CACHE) l2++;
				else if (type == ACCESSMEMORY_TYPE.REGISTER) register++;
				else if (type == ACCESSMEMORY_TYPE.CUSTOM) custom++;
			}
		}
		if (rights != null && rights.size() > 0) {
			for (Variable var : rights) {
				ACCESSMEMORY_TYPE type = getMemoryType(var);
				if (type == null) {
					type = ACCESSMEMORY_TYPE.getDefaultType(var);
				}
				if (type == ACCESSMEMORY_TYPE.MEMORY) memory++;
				else if (type == ACCESSMEMORY_TYPE.L1_CACHE) l1++;
				else if (type == ACCESSMEMORY_TYPE.L2_CACHE) l2++;
				else if (type == ACCESSMEMORY_TYPE.REGISTER) register++;
				else if (type == ACCESSMEMORY_TYPE.CUSTOM) custom++;
			}
		}
		result.setMemoryCount(memory);
		result.setL1Count(l1);
		result.setL2Count(l2);
		result.setRegisterCount(register);
		result.setCustomCount(custom);
	}


	/**
	 * スループット算出のストア有り無しの判定を行う.
	 * @param lefts		左辺変数リスト
	 * @return			true=ストア有り
	 */
	private boolean isStore(List<Variable> lefts) {
		// Store有り、無しの判定
		boolean isstore = false;
		if (this.properitiesRequiredBF.getMemThroughputCalcMode() == MEM_THROUGHPUT_CALC_MODE.NOSTORE) {
			isstore = false;
		}
		else if (this.properitiesRequiredBF.getMemThroughputCalcMode() == MEM_THROUGHPUT_CALC_MODE.STORE) {
			isstore = true;
		}
		else if (this.properitiesRequiredBF.getMemThroughputCalcMode() == MEM_THROUGHPUT_CALC_MODE.AUTO) {
			int leftbyte = getVariableMemoryByte(lefts);
			if (leftbyte > 0) {
				isstore = true;
			}
		}
		return isstore;
	}

	/**
	 * 変数のバイト数を取得する.
	 * @param var		変数
	 * @return		バイト数
	 */
	private int getVariableByte(Variable var) {
		if (var == null) return 0;
		if (var.getDefinition() == null) return 0;
		if (var.getDefinition().getType() == null) return 0;
		if (!(var.getDefinition().getType() instanceof VariableType)) return 0;
		VariableType type = (VariableType)var.getDefinition().getType();
		if (type == null) return 0;

		// kind属性
		int kind = 0;
		if (type.getKind() != null) {
			if (StringUtils.isNumeric(type.getKind().toString())) {
				kind = Integer.parseInt(type.getKind().toString());
			}
		}
		// データ型のバイト数を求める.
		int byteValue = 0;
		if (type.getPrimitiveDataType() == VariableType.PrimitiveDataType.BYTE) {
			byteValue = 1;
		}
		else if (type.getPrimitiveDataType() == VariableType.PrimitiveDataType.INTEGER) {
			byteValue = 4;
			if (kind > 0) {
				byteValue = kind;
			}
			else if (this.properitiesRequiredBF.getDefaultSizeInteger() > 0) {
				byteValue = this.properitiesRequiredBF.getDefaultSizeInteger();
			}
		}
		else if (type.getPrimitiveDataType() == VariableType.PrimitiveDataType.REAL) {
			byteValue = 4;
			if (kind > 0) {
				byteValue = kind;
			}
			else if (this.properitiesRequiredBF.getDefaultSizeReal() > 0) {
				byteValue = this.properitiesRequiredBF.getDefaultSizeReal();
			}
		}
		else if (type.getPrimitiveDataType() == VariableType.PrimitiveDataType.DOUBLE_PRECISION) {
			byteValue = 8;
		}
		else if (type.getPrimitiveDataType() == VariableType.PrimitiveDataType.COMPLEX) {
			byteValue = 8;
		}
		else if (type.getPrimitiveDataType() == VariableType.PrimitiveDataType.DOUBLE_COMPLEX) {
			byteValue = 16;
		}

		return byteValue;
	}


	/**
     * 変数が右辺であるかチェックする.
	 * @param var		変数
     * @return        true=変数が存在する。
	 */
	@SuppressWarnings("unused")
	private boolean isRightVariable(Variable var) {
		if (var == null) return false;
		if (var.getParentStatement() == null) return false;
		IBlock parent = var.getParentStatement();
		if (!(parent instanceof Substitution)) return false;
		Substitution sub = (Substitution)parent;
        // 右辺
        Expression sub_exp = sub.getRightValue();
        List<Variable> exp_variables = sub_exp.getVariables();
        for (Variable exp_vasr : exp_variables) {
            if (exp_vasr == var) {
                return true;
            }
        }
		return false;
	}


	/**
	 * 変数が左辺であるかチェックする.
	 * @param var		変数
     * @return        true=変数が存在する。
	 */
    @SuppressWarnings("unused")
	private boolean isLeftVariable(Variable var) {
		if (var == null) return false;
		if (var.getParentStatement() == null) return false;
		IBlock parent = var.getParentStatement();
		if (!(parent instanceof Substitution)) return false;
		Substitution sub = (Substitution)parent;
        // 左辺
        Expression sub_exp = sub.getLeftValue();
        List<Variable> exp_variables = sub_exp.getVariables();
        for (Variable exp_vasr : exp_variables) {
            if (exp_vasr == var) {
                return true;
            }
        }
		return false;
	}


	/**
	 * 要求B/F対象の変数の数を取得する.
	 * @param  vars  変数リスト
	 * @return		要求B/F対象の変数の数
	 */
    @SuppressWarnings("unused")
	private int getVariableMemoryCount(List<Variable> vars) {
		if (vars == null) return 0;
		if (vars.size() <= 0) return 0;
		if (this.properitiesRequiredBF == null) return vars.size();
		int count = 0;
		for (Variable var : vars) {
			ACCESSMEMORY_TYPE type = getMemoryType(var);
			if (type == null) {
				type = ACCESSMEMORY_TYPE.getDefaultType(var);
			}
			if (type == null) continue;
			RequiredBF mem = this.properitiesRequiredBF.getRequiredBF(type);
			if (mem == null) continue;
			if (mem.isRequiredBF()) {
				count++;
			}
		}
		return count;
	}


	/**
	 * 要求B/F対象の変数Byteを取得する.
	 * @param  vars  変数リスト
	 * @return		要求B/F対象の変数Byte
	 */
	private int getVariableMemoryByte(List<Variable> vars) {
		if (vars == null) return 0;
		if (vars.size() <= 0) return 0;
		int varbyte = 0;
		for (Variable var : vars) {
			if (this.properitiesRequiredBF != null) {
				ACCESSMEMORY_TYPE type = getMemoryType(var);
				if (type == null) {
					// 設定済み変数からアクセス先メモリを取得する
					type = getAccessMemoryType(var);
					if (type == null) {
						// デフォルトのアクセス先メモリを取得する
						type = ACCESSMEMORY_TYPE.getDefaultType(var);
					}
				}
				if (type == null) continue;
				RequiredBF mem = this.properitiesRequiredBF.getRequiredBF(type);
				if (mem == null) continue;
				if (mem.isRequiredBF()) {
					varbyte += getVariableByte(var);
				}
			}
			else {
				varbyte += getVariableByte(var);
			}
		}
		return varbyte;
	}

	/**
	 * 変数のアクセス先メモリタイプを取得する.
	 * ソースビュー適用アクセス先メモリと一時設定アクセス先メモリから一時設定アクセス先メモリを優先して取得する.
	 * @param var	変数
	 * @return		アクセス先メモリ
	 */
	private ACCESSMEMORY_TYPE getMemoryType(Variable var) {
		if (var == null) return null;
		ACCESSMEMORY_TYPE type = var.getMemoryType();
		ACCESSMEMORY_TYPE temp = var.getTemporaryMemoryType();
		if (temp == null) return type;
		return temp;
	}

	/**
	 * 分析ビューに算出結果を追加する.
	 * @param  results    算出結果リスト
	 */
	public void setAnalysisPanel(RequiredBFResult[] results) {
		if (this.modelRequiredBF == null) return;
		if (this.properitiesRequiredBF != null) {
			// 算出単位を設定する
			this.modelRequiredBF.setUnitType(this.properitiesRequiredBF.getBFCalcType());
		}
		this.modelRequiredBF.addRequiredByteFlopResults(results);
	}

	/**
	 * 変数アクセス先メモリ設定プロパティを取得する.
	 * @return 変数アクセス先メモリ設定プロパティ
	 */
	public VariableMemoryProperties getPropertiesVariableMemory() {
		return propertiesVariableMemory;
	}

	/**
	 * 変数アクセス先メモリ設定プロパティを設定する.
	 * @param properties    変数アクセス先メモリ設定プロパティ
	 */
	public void setPropertiesVariableMemory(VariableMemoryProperties properties) {
		this.propertiesVariableMemory = properties;
	}

	/**
	 * 変数アクセス先メモリ設定プロパティに変数を設定する.
	 * 追加変数はデータベースから取得する.
	 * @param language   Fortranデータベース
	 */
	public void createVariableMemoryProperties(Fortran language) {

		VariableMemoryEntry entry = new VariableMemoryEntry(language);
        LanguageVisitor visitor = new LanguageVisitor(entry);
        visitor.entry();
        // アクセス先メモリの設定されている変数の取得
        Variable[] vars = entry.getListVariable();
        if (vars == null || vars.length <= 0) return;
        for (Variable var : vars) {
        	this.propertiesVariableMemory.addVariable(var);
        }
        propertiesVariableMemory.firePropertyChange();
	}

	/**
	 * 設定済み変数からアクセス先メモリを取得する.
	 * 同一添字、同一定義の変数を取得する.
	 * 設定済み変数が複数、且つ設定アクセス先メモリが異なる場合は、nullを返す
	 * @param var		検索変数
	 * @return			アクセス先メモリ
	 */
	private ACCESSMEMORY_TYPE getAccessMemoryType(Variable var) {
		// 設定済み変数からアクセス先メモリを取得する
		if (this.propertiesVariableMemory == null) return null;
		List<Variable> vars = this.propertiesVariableMemory.getEqualsVariableDefinition(var);
		if (vars == null || vars.size() <= 0) return null;

		// 設定アクセス先メモリが異なる場合は、nullを返す
		ACCESSMEMORY_TYPE type = null;
		for (Variable item : vars) {
			if (type == null) {
				type = item.getMemoryType();
			}
			if (type != item.getMemoryType()) {
				return null;
			}
		}

		return type;
	}
}



