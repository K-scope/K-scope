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

package jp.riken.kscope.data;

import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.properties.MemorybandProperties.THROUGHPUT_STORE_MODE;
import jp.riken.kscope.properties.MemorybandProperties.UNIT_TYPE;

/**
 * 要求Byte/FLOP算出結果
 * @author riken
 */
public class RequiredByteFlopResult {
	/** 算出ブロック */
	private IBlock block;
	/** Load変数の算出Byte */
	private int load;
	/** Store変数の算出Byte */
	private int store;
	/** 演算数(FLOP) = add(F) + mul(F) + intrinsic(F) */
	private int operand;
	/** 要求Byte/FLOP =(Load+Store) / FLOP */
	private float requiredBF; 		///<
	/** 要求FLOP/Byte = FLOP/(Load+Store) */
	private float requiredFB;
	/** スループット(GB/s) */
	private float throughput;
	/** 実効Byte/FLOP = Throughput(GB/s) / Performance(GFLOPS) */
	private float effectiveBF;
	/** 実効FLOP/Byte = Performance(GFLOPS) / Throughput(GB/s) */
	private float effectiveFB;
	/** ピーク性能比Byte/FLOP(%) = 要求Byte/FLOP / 実効Byte/FLOP */
	private float peakBF;
	/** ピーク性能比FLOP/Byte(%) = 要求FLOP/Byte / 実効Byte/FLOP */
	private float peakFB;
	/** アクセス先Memoryの変数の合計 */
	private int memoryCount;
	/** アクセス先L1の変数の合計 */
	private int l1Count;
	/** アクセス先L2の変数の合計 */
	private int l2Count;
	/** アクセス先Register の変数の合計 */
	private int registerCount;
	/** アクセス先Custom の変数の合計 */
	private int customCount;
	/** 浮動小数点データ型の変数に対する加算(+)の数 */
	private int addCount;
	/** 浮動小数点データ型の変数に対する減算(-)の数 */
	private int subCount;
	/** 浮動小数点データ型の変数に対する乗算(*)の数 */
	private int mulCount;
	/** 浮動小数点データ型の変数に対する除算(/)の数 */
	private int divCount;
	/** 浮動小数点データ型の変数に対する累乗, 組込関数の加算(+) + 乗算(*) */
	private int intrinsicCount;
	/** 演算性能 */
	private float performance;
	/** 算出元のMemoryスループット値(GB/s)（ストア有り or ストアなし） */
	private float memoryMBW;
	/** 算出元のL1スループット値(GB/s)	（ストア有り or ストアなし） */
	private float l1MBW;
	/** 算出元のL2スループット値(GB/s)	（ストア有り or ストアなし） */
	private float l2MBW;
	/** 算出元のRegisterスループット値(GB/s)（ストア有り or ストアなし） */
	private float registerMBW;
	/** 算出元のCustomスループット値(GB/s)（ストア有り or ストアなし） */
	private float customMBW;
	/** 算出元のMemory係数 */
	private float memoryCoef;
	/** 算出元のL2係数 */
	private float l1Coef;
	/** 算出元のL2係数 */
	private float l2Coef;
	/** 算出元のRegister係数 */
	private float registerCoef;
	/** 算出元のCustom係数 */
	private float customCoef;
	/** スループットストア設定 */
    private THROUGHPUT_STORE_MODE storeMode;
    /** 算出単位 */
    private UNIT_TYPE unitType;

	/**
	 * 算出ブロックを取得する.
	 * @return 算出ブロック
	 */
	public IBlock getBlock() {
		return block;
	}
	/**
	 * 算出ブロックを設定する.
	 * @param block 算出ブロック
	 */
	public void setBlock(IBlock block) {
		this.block = block;
	}

	/**
	 * Load変数の算出Byteを取得する.
	 * @return Load変数の算出Byte
	 */
	public int getLoad() {
		return load;
	}
	/**
	 * Load変数の算出Byteを設定する.
	 * @param load Load変数の算出Byte
	 */
	public void setLoad(int load) {
		this.load = load;
	}
	/**
	 * Store変数の算出Byteを取得する.
	 * @return Store変数の算出Byte
	 */
	public int getStore() {
		return store;
	}
	/**
	 * Store変数の算出Byteを設定する.
	 * @param store Store変数の算出Byte
	 */
	public void setStore(int store) {
		this.store = store;
	}
	/**
	 * 演算数(FLOP)を取得する.
	 * 演算数(FLOP) = add(F) + mul(F) + intrinsic(F)
	 * @return 演算数(FLOP)
	 */
	public int getOperand() {
		return operand;
	}
	/**
	 * 演算数(FLOP)を設定する.
	 * 演算数(FLOP) = add(F) + mul(F) + intrinsic(F)
	 * @param operand 演算数(FLOP)
	 */
	public void setOperand(int operand) {
		this.operand = operand;
	}
	/**
	 * 要求Byte/FLOPを取得する.
	 * 要求Byte/FLOP =(Load+Store) / FLOP
	 * @return 要求Byte/FLOP
	 */
	public float getRequiredBF() {
		return requiredBF;
	}
	/**
	 * 要求Byte/FLOPを設定する.
	 * 要求Byte/FLOP =(Load+Store) / FLOP
	 * @param requiredBF 要求Byte/FLOP
	 */
	public void setRequiredBF(float requiredBF) {
		this.requiredBF = requiredBF;
	}
	/**
	 * 要求FLOP/Byteを取得する.
	 * 要求FLOP/Byte = FLOP/(Load+Store)
	 * @return 要求FLOP/Byte
	 */
	public float getRequiredFB() {
		return requiredFB;
	}
	/**
	 * 要求FLOP/Byteを設定する.
	 * 要求FLOP/Byte = FLOP/(Load+Store)
	 * @param requiredFB 要求FLOP/Byte
	 */
	public void setRequiredFB(float requiredFB) {
		this.requiredFB = requiredFB;
	}
	/**
	 * スループット(GB/s)を取得する.
	 * @return スループット(GB/s)
	 */
	public float getThroughput() {
		return throughput;
	}
	/**
	 * スループット(GB/s)を設定する.
	 * @param throughput スループット(GB/s)
	 */
	public void setThroughput(float throughput) {
		this.throughput = throughput;
	}

	/**
	 * 実効Byte/FLOPを取得する.
	 * 実効Byte/FLOP = Throughput(GB/s) / Performance(GFLOPS)
	 * @return 実効Byte/FLOP
	 */
	public float getEffectiveBF() {
		return effectiveBF;
	}
	/**
	 * 実効Byte/FLOPを設定する.
	 * 実効Byte/FLOP = Throughput(GB/s) / Performance(GFLOPS)
	 * @param effectiveBF 実効Byte/FLOP
	 */
	public void setEffectiveBF(float effectiveBF) {
		this.effectiveBF = effectiveBF;
	}
	/**
	 * 実効FLOP/Byteを取得する.
	 * 実効FLOP/Byte = Performance(GFLOPS) / Throughput(GB/s)
	 * @return effectiveFB
	 */
	public float getEffectiveFB() {
		return effectiveFB;
	}
	/**
	 * 実効FLOP/Byteを設定する.
	 * 実効FLOP/Byte = Performance(GFLOPS) / Throughput(GB/s)
	 * @param effectiveFB セットする effectiveFB
	 */
	public void setEffectiveFB(float effectiveFB) {
		this.effectiveFB = effectiveFB;
	}

	/**
	 * ピーク性能比Byte/FLOP(%)を取得する.
	 * ピーク性能比Byte/FLOP(%) = 要求Byte/FLOP / 実効Byte/FLOP
	 * @return ピーク性能比Byte/FLOP(%)
	 */
	public float getPeakBF() {
		return peakBF;
	}
	/**
	 * ピーク性能比Byte/FLOP(%)を設定する.
	 * ピーク性能比Byte/FLOP(%) = 要求Byte/FLOP / 実効Byte/FLOP
	 * @param peakBF ピーク性能比Byte/FLOP(%)
	 */
	public void setPeakBF(float peak) {
		this.peakBF = peak;
	}
	/**
	 * ピーク性能比FLOP/Byte(%)を取得する.
	 * ピーク性能比FLOP/Byte(%) = 要求FLOP/Byte / 実効Byte/FLOP
	 * @return ピーク性能比FLOP/Byte(%)
	 */
	public float getPeakFB() {
		return peakFB;
	}
	/**
	 * ピーク性能比FLOP/Byte(%)を設定する.
	 * ピーク性能比FLOP/Byte(%) = 要求FLOP/Byte / 実効Byte/FLOP
	 * @param peakFB ピーク性能比FLOP/Byte(%)
	 */
	public void setPeakFB(float peak) {
		this.peakFB = peak;
	}
	/**
	 * アクセス先Memoryの変数の合計の取得を取得する.
	 * @return アクセス先Memoryの変数の合計
	 */
	public int getMemoryCount() {
		return memoryCount;
	}
	/**
	 * アクセス先Memoryの変数の合計を設定する.
	 * @param count アクセス先Memoryの変数の合計
	 */
	public void setMemoryCount(int count) {
		this.memoryCount = count;
	}
	/**
	 * アクセス先L1の変数の合計を取得する.
	 * @return l1Count		アクセス先L1の変数の合計
	 */
	public int getL1Count() {
		return l1Count;
	}
	/**
	 * アクセス先L1の変数の合計を設定する.
	 * @param count アクセス先L1の変数の合計
	 */
	public void setL1Count(int count) {
		this.l1Count = count;
	}
	/**
	 * アクセス先L2の変数の合計を取得する.
	 * @return アクセス先L2の変数の合計
	 */
	public int getL2Count() {
		return l2Count;
	}
	/**
	 * アクセス先L2の変数の合計を設定する.
	 * @param l2Count  アクセス先L2の変数の合計
	 */
	public void setL2Count(int count) {
		this.l2Count = count;
	}
	/**
	 * アクセス先Registerの変数の合計を取得する
	 * @return アクセス先Registerの変数の合計
	 */
	public int getRegisterCount() {
		return registerCount;
	}
	/**
	 * アクセス先Registerの変数の合計を設定する.
	 * @param count アクセス先Registerの変数の合計
	 */
	public void setRegisterCount(int count) {
		this.registerCount = count;
	}

	/**
	 * アクセス先Customの変数の合計を取得する
	 * @return アクセス先Customの変数の合計
	 */
	public int getCustomCount() {
		return customCount;
	}
	/**
	 * アクセス先Customの変数の合計を設定する.
	 * @param count アクセス先Customの変数の合計
	 */
	public void setCustomCount(int count) {
		this.customCount = count;
	}

	/**
	 * 浮動小数点データ型の変数に対する加算(+)の数を取得する.
	 * @return 浮動小数点データ型の変数に対する加算(+)の数
	 */
	public int getAddCount() {
		return addCount;
	}
	/**
	 * 浮動小数点データ型の変数に対する加算(+)の数を設定する.
	 * @param count 浮動小数点データ型の変数に対する加算(+)の数
	 */
	public void setAddCount(int count) {
		this.addCount = count;
	}

	/**
	 * 浮動小数点データ型の変数に対する減算(-)の数を取得する.
	 * @return 浮動小数点データ型の変数に対する減算(-)の数
	 */
	public int getSubCount() {
		return subCount;
	}
	/**
	 * 浮動小数点データ型の変数に対する減算(-)の数を設定する.
	 * @param count 浮動小数点データ型の変数に対する減算(-)の数
	 */
	public void setSubCount(int count) {
		this.subCount = count;
	}

	/**
	 * 浮動小数点データ型の変数に対する乗算(*)の数を取得する.
	 * @return 浮動小数点データ型の変数に対する乗算(*)の数
	 */
	public int getMulCount() {
		return mulCount;
	}

	/**
	 * 浮動小数点データ型の変数に対する乗算(*)の数を設定する.
	 * @param count 浮動小数点データ型の変数に対する乗算(*)の数
	 */
	public void setMulCount(int count) {
		this.mulCount = count;
	}

	/**
	 * 浮動小数点データ型の変数に対する除算(/)の数を取得する.
	 * @return 浮動小数点データ型の変数に対する除算(/)の数
	 */
	public int getDivCount() {
		return divCount;
	}
	/**
	 * 浮動小数点データ型の変数に対する除算(/)の数を設定する.
	 * @param count 浮動小数点データ型の変数に対する除算(/)の数
	 */
	public void setDivCount(int count) {
		this.divCount = count;
	}

	/**
	 * 浮動小数点データ型の変数に対する累乗, 組込関数の加算(+) + 乗算(*)を取得する.
	 * @return 浮動小数点データ型の変数に対する累乗, 組込関数の加算(+) + 乗算(*)
	 */
	public int getIntrinsicCount() {
		return intrinsicCount;
	}
	/**
	 * 浮動小数点データ型の変数に対する累乗, 組込関数の加算(+) + 乗算(*)を設定する.
	 * @param count 浮動小数点データ型の変数に対する累乗, 組込関数の加算(+) + 乗算(*)
	 */
	public void setIntrinsicCount(int count) {
		this.intrinsicCount = count;
	}
	/**
	 * 演算性能を取得する.
	 * @return 演算性能
	 */
	public float getPerformance() {
		return performance;
	}
	/**
	 * 演算性能を設定する.
	 * @param performance 演算性能
	 */
	public void setPerformance(float performance) {
		this.performance = performance;
	}
	/**
	 * スループットストア設定を取得する.
	 * @return スループットストア設定
	 */
	public THROUGHPUT_STORE_MODE getStoreMode() {
		return storeMode;
	}
	/**
	 * スループットストア設定を設定する.
	 * @param storeMode スループットストア設定
	 */
	public void setStoreMode(THROUGHPUT_STORE_MODE storeMode) {
		this.storeMode = storeMode;
	}
	/**
	 * 算出元のMemoryスループット値(GB/s)を取得する.
	 * @return 算出元のMemoryスループット値(GB/s)
	 */
	public float getMemoryMBW() {
		return memoryMBW;
	}
	/**
	 * 算出元のMemoryスループット値(GB/s)を設定する.
	 * @param value 算出元のMemoryスループット値(GB/s)
	 */
	public void setMemoryMBW(float value) {
		this.memoryMBW = value;
	}

	/**
	 * 算出元のL1スループット値(GB/s)を取得する.
	 * @return 算出元のL1スループット値(GB/s)
	 */
	public float getL1MBW() {
		return l1MBW;
	}

	/**
	 * 算出元のL1スループット値(GB/s)を設定する.
	 * @param value 算出元のL1スループット値(GB/s)
	 */
	public void setL1MBW(float value) {
		l1MBW = value;
	}

	/**
	 * 算出元のL2スループット値(GB/s)を取得する.
	 * @return 算出元のL2スループット値(GB/s)
	 */
	public float getL2MBW() {
		return l2MBW;
	}

	/**
	 * 算出元のL2スループット値(GB/s)を設定する.
	 * @param value 算出元のL2スループット値(GB/s)
	 */
	public void setL2MBW(float value) {
		l2MBW = value;
	}

	/**
	 * 算出元のRegisterスループット値(GB/s)を取得する.
	 * @return 算出元のRegisterスループット値(GB/s)
	 */
	public float getRegisterMBW() {
		return registerMBW;
	}

	/**
	 * 算出元のRegisterスループット値(GB/s)を設定する.
	 * @param value 算出元のRegisterスループット値(GB/s)
	 */
	public void setRegisterMBW(float value) {
		this.registerMBW = value;
	}

	/**
	 * 算出元のCustomスループット値(GB/s)を取得する.
	 * @return 算出元のCustomスループット値(GB/s)
	 */
	public float getCustomMBW() {
		return customMBW;
	}

	/**
	 * 算出元のCustomスループット値(GB/s)を設定する.
	 * @param value 算出元のCustomスループット値(GB/s)
	 */
	public void setCustomMBW(float value) {
		this.customMBW = value;
	}
	/**
	 * 算出元のMemory係数を取得する.
	 * @return 算出元のMemory係数
	 */
	public float getMemoryCoef() {
		return memoryCoef;
	}
	/**
	 * 算出元のMemory係数を設定する.
	 * @param coef 算出元のMemory係数
	 */
	public void setMemoryCoef(float coef) {
		this.memoryCoef = coef;
	}

	/**
	 * 算出元のL1係数を取得する.
	 * @return 算出元のL1係数
	 */
	public float getL1Coef() {
		return l1Coef;
	}

	/**
	 * 算出元のL1係数を設定する.
	 * @param coef 算出元のL1係数
	 */
	public void setL1Coef(float l1Coef) {
		this.l1Coef = l1Coef;
	}

	/**
	 * 算出元のL2係数を取得する.
	 * @return 算出元のL2係数
	 */
	public float getL2Coef() {
		return l2Coef;
	}
	/**
	 * 算出元のL2係数を設定する.
	 * @param coef 算出元のL2係数
	 */
	public void setL2Coef(float coef) {
		this.l2Coef = coef;
	}

	/**
	 * 算出元のRegister係数を取得する.
	 * @return 算出元のRegister係数
	 */
	public float getRegisterCoef() {
		return registerCoef;
	}
	/**
	 * 算出元のRegister係数を設定する.
	 * @param coef 算出元のRegister係数
	 */
	public void setRegisterCoef(float coef) {
		this.registerCoef = coef;
	}

	/**
	 * 算出元のCustom係数を取得する.
	 * @return 算出元のCustom係数
	 */
	public float getCustomCoef() {
		return customCoef;
	}
	/**
	 * 算出元のCustom係数を設定する.
	 * @param coef 算出元のCustom係数
	 */
	public void setCustomCoef(float coef) {
		this.customCoef = coef;
	}
	/**
	 * 算出単位を取得する.
	 * @return 算出単位
	 */
	public UNIT_TYPE getUnitType() {
		return unitType;
	}
	/**
	 * 算出単位を設定する.
	 * @param unitType 算出単位
	 */
	public void setUnitType(UNIT_TYPE type) {
		this.unitType = type;
	}

	/**
	 * 要求Byte/FLOP, 要求FLOP/Byteを設定済みLoad, Storeから算出します.
	 * Required = (Load + Store) / FLOP
	 * @return  計算結果:要求Byte/FLOP
	 */
	public float calculateRequired() {
		float flop = 0.0F;
		if (this.operand > 0) {
			flop = (float)(this.load + this.store) / (float)this.operand;
		}
		this.requiredBF = flop;
		if (flop > 0.0F) {
			this.requiredFB = 1.0F / flop;
		}
		else {
			this.requiredFB = 0.0F;
		}

		return flop;
	}

	/**
	 * 実効Byte/FLOP,実効FLOP/Byteを算出する.
	 * 実効Byte/FLOP = スループット / 演算性能
	 * @param performance   演算性能
	 * @return 実効Byte/FLOP
	 */
	public float calculateEffective(float performance) {
		this.performance = performance;
		if (performance == 0 || this.throughput == 0) {
			this.effectiveBF = 0.0F;
			this.effectiveFB = 0.0F;
			return 0.0F;
		}
		this.effectiveBF = this.throughput / performance;
		this.effectiveFB = performance / this.throughput;
		return this.effectiveBF;
	}

	/**
	 * ピーク性能比を算出する.
	 * ピーク性能比 = 実効B/F / 要求B/F
	 * @return ピーク性能比
	 */
	public float calculatePeak() {
		if (this.requiredBF == 0.0F) {
			this.peakBF = 0.0F;
		}
		else {
			this.peakBF = this.effectiveBF / this.requiredBF;
		}
		if (this.requiredFB == 0.0F) {
			this.peakBF = 0.0F;
		}
		else {
			this.peakFB = this.requiredFB / this.effectiveFB;
		}
		return this.peakBF;
	}

}
