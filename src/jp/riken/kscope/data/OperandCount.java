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

/**
 * 演算カウント
 * @author RIKEN
 *
 */
public class OperandCount {

    /** 演算子:+カウント */
    private Integer add;
    /** 演算子:-カウント */
    private Integer sub;
    /** 演算子:*カウント */
    private Integer mul;
    /** 演算子:/カウント */
    private Integer div;
    /** intrinsicカウント */
    private Integer intrinsic;
    /** ロードカウント */
    private Integer load;
    /** ストアカウント */
    private Integer store;
    /** F:カウント */
    private Integer f;
    /** F/(L+S) or (L+S)/F*/
    private Float fls;

    /**
     * カウント名.<br/>
     * 組込み関数名 or ループ、ブロック名
     */
    private String name;

    /**
     * コンストラクタ
     */
    public OperandCount() {
    }

    /**
     * 演算子:+カウントを取得する
     * @return		演算子:+カウント
     */
    public Integer getAdd() {
        return add;
    }

    /**
     * 演算子:+カウントを設定する
     * @param add		演算子:+カウント
     */
    public void setAdd(Integer add) {
        this.add = add;
    }

    /**
     * 演算子:-カウントを取得する
     * @return		演算子:-カウント
     */
    public Integer getSub() {
        return sub;
    }

    /**
     * 演算子:-カウントを設定する
     * @param sub		演算子:-カウント
     */
    public void setSub(Integer sub) {
        this.sub = sub;
    }


    /**
     * 演算子:*カウントを取得する
     * @return		演算子:*カウント
     */
    public Integer getMul() {
        return mul;
    }

    /**
     * 演算子:*カウントを設定する
     * @param mul		演算子:*カウント
     */
    public void setMul(Integer mul) {
        this.mul = mul;
    }

    /**
     * 演算子:/カウントを取得する
     * @return		演算子:/カウント
     */
    public Integer getDiv() {
        return div;
    }

    /**
     * 演算子:/カウントを設定する
     * @param div		演算子:/カウント
     */
    public void setDiv(Integer div) {
        this.div = div;
    }

    /**
     * intrinsicカウントを取得する
     * @return		intrinsicカウント
     */
    public Integer getIntrinsic() {
        return intrinsic;
    }

    /**
     * intrinsicカウントを設定する
     * @param intrinsic		intrinsicカウント
     */
    public void setIntrinsic(Integer intrinsic) {
        this.intrinsic = intrinsic;
    }


    /**
     * ロードカウントを取得する
     * @return		ロードカウント
     */
    public Integer getLoad() {
        return load;
    }

    /**
     * ロードカウントを設定する
     * @param load		ロードカウント
     */
    public void setLoad(Integer load) {
        this.load = load;
    }

    /**
     * ストアカウントを取得する
     * @return		ストアカウント
     */
    public Integer getStore() {
        return store;
    }

    /**
     * ストアカウントを設定する
     * @param store		ストアカウント
     */
    public void setStore(Integer store) {
        this.store = store;
    }

    /**
     * F(L+S)を取得する
     * @return		F(L+S)
     */
    public Float getFls() {
        return this.fls;
    }

    /**
     * F(L+S)を設定する
     * @param fls		F(L+S)
     */
    public void setFls(Float fls) {
        this.fls = fls;
    }

    /**
     * カウント名を取得する
     * @return 		カウント名
     */
    public String getName() {
        return name;
    }

    /**
     * カウント名を設定する
     * @param name 	カウント名
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * F:カウントを取得する
     * @return		F:カウント
     */
    public Integer getF() {
        return f;
    }

    /**
     * F:カウントを設定する
     * @param f		F:カウント
     */
    public void setF(Integer f) {
        this.f = f;
    }

}
