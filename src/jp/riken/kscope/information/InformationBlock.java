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

package jp.riken.kscope.information;

import java.io.Serializable;
import java.util.Set;

import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.BlockType;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.language.Variable;

/**
 * 情報ブロッククラス。<br>
 *
 * @author RIKEN
 *
 */
public class InformationBlock implements IInformation, IBlock, Serializable {
    /** シリアル番号 */
	private static final long serialVersionUID = -6653837061369941499L;

	private InformationBase information = null;
    private IInformation startBlock = null;
    private IInformation endBlock = null;

    /**
     * コンストラクタ。
     *
     * @param info
     *       設定する情報
     * @param start
     *       設定する開始位置
     * @param end
     *       設定する終了位置
     */
    public InformationBlock(InformationBase info,
            IInformation start, IInformation end) {
        this.information = info;
        this.startBlock = start;
        this.endBlock = end;
    }

    /**
     * 開始位置を取得する。
     *
     * @return 開始位置
     */
    public IInformation getStartBlock() {
        return startBlock;
    }

    /**
     * 開始位置を設定する。
     *
     * @param start 設定する開始位置
     */
    public void setStartBlock(IInformation start) {
        this.startBlock = start;
    }

    /**
     * 終了位置を取得する。
     *
     * @return 終了位置
     */
    public IInformation getEndBlock() {
        return endBlock;
    }

    /**
     * 終了位置を設定する。
     *
     * @param end 設定する終了位置
     */
    public void setEndBlock(IInformation end) {
        this.endBlock = end;
    }


    /**
     * 名前空間（モジュール名.ルーチン名）を取得する。
     *
     * @return 名前空間（モジュール名.ルーチン名）
     */
    public String getNamespace() {
        if (this.startBlock == null || this.endBlock == null) {
            return "";
        }
        return this.startBlock.getNamespace();
    }

    @Override
    public void setInformation(TextInfo info) {
        this.information = info;
    }

    @Override
    public TextInfo getInformation() {
        // 現状の設計では必ずTextInfo
    	if (this.information instanceof TextInfo) {
    		return (TextInfo) this.information;
    	}
    	return null;
    }

    @Override
    public int getStartPos() {
        return this.startBlock.getStartPos();
    }

    @Override
    public void setStartPos(int pos) {
        this.startBlock.setStartPos(pos);
    }

    @Override
    public int getEndPos() {
        return this.startBlock.getEndPos();
    }

    @Override
    public void setEndPos(int pos) {
        this.startBlock.setEndPos(pos);
    }

    @Override
    public void clearInformation() {
        this.information.setContent("");
    }

    // IDは不要
    @Override
    public String getID() {
        return null;
    }

    @Override
    public String toString() {
    	if (this.startBlock == this.endBlock) {
    		return this.startBlock.toString();
    	}
    	else {
    		return "[" + this.startBlock.toString() + "]  -  [" + this.endBlock.toString() + "]";
    	}
    }

    @Override
    public CodeLine getStartCodeLine() {
        if (this.startBlock instanceof IBlock) {
            IBlock blk = (IBlock) this.startBlock;
            return blk.getStartCodeLine();
        }
        return null;
    }

    @Override
    public CodeLine getEndCodeLine() {
        /****  暫定コード at 2012/03/21 by @hira   ****/
        if (this.endBlock != null && this.endBlock instanceof IBlock) {
            IBlock blk = (IBlock) this.endBlock;
            return blk.getEndCodeLine();
        }
        /****  暫定コード at 2012/03/21 by @hira   ****/

        if (this.startBlock instanceof IBlock) {
            IBlock blk = (IBlock) this.startBlock;
            return blk.getEndCodeLine();
        }
        return null;
    }

    @Override
    public BlockType getBlockType() {
        if (this.startBlock instanceof IBlock) {
            IBlock blk = (IBlock) this.startBlock;
            return blk.getBlockType();
        }
        return null;
    }

    @Override
    public IBlock getMotherBlock() {
        if (this.startBlock instanceof IBlock) {
            IBlock blk = (IBlock) this.startBlock;
            return blk.getMotherBlock();
        }
        return null;
    }

    /**
     * 構造IDを取得する.
     * 構造IDは不要であるので、nullを返す.
     * @return 構造ID
     */
    @Override
    public String getLayoutID() {
        return null;
    }


 	/**
 	 * 変数リストを取得する.
 	 */
 	@Override
 	public Set<Variable> getAllVariables() {
 		return null;
 	}
}
