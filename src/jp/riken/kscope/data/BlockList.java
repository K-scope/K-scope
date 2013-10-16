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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import jp.riken.kscope.language.BlockType;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Variable;

/**
 * Blockのリストクラス
 * @author riken
 */
public class BlockList implements IBlock {
	/** ブロックリスト */
	private List<IBlock> blocks;

	/**
	 * コンストラクタ
	 */
	public BlockList() {
		this.blocks = new ArrayList<IBlock>();
	}

	/**
	 * コンストラクタ
	 * @param  list    ブロックリスト
	 */
	public BlockList(IBlock[] list) {
		if (list != null && list.length > 0) {
			this.blocks = new ArrayList<IBlock>();
			this.blocks.addAll(Arrays.asList(list));
		}
	}

	/**
	 * ブロックリストを取得する.
	 * @return		ブロックリスト
	 */
	public List<IBlock> getBlocks() {
		return this.blocks;
	}

	/**
	 * ブロックリストを設定する.
	 * @param list		ブロックリスト
	 */
	public void setBlocks(List<IBlock> list) {
		this.blocks = list;
	}

	/**
	 * ブロックリスト数を取得する.
	 * @return		ブロックリスト数
	 */
	public int getBlockCount() {
		if (this.blocks == null) return 0;
		return this.blocks.size();
	}

	/**
	 * ブロックを追加する.
	 * @param block		追加ブロック
	 */
	public void addBlock(IBlock block) {
		if (this.blocks == null) {
			this.blocks = new ArrayList<IBlock>();
		}
		this.blocks.add(block);
	}

	/**
	 * 開始コード行情報を取得する.
	 * ブロックリストの最初のブロックの開始コード行情報を渡す.
	 */
	@Override
	public CodeLine getStartCodeLine() {
		if (this.blocks == null || this.blocks.size() <= 0) return null;
		return this.blocks.get(0).getStartCodeLine();
	}

	/**
	 * 終了コード行情報を取得する.
	 * ブロックリストの最後のブロックの終了コード行情報を渡す.
	 */
	@Override
	public CodeLine getEndCodeLine() {
		if (this.blocks == null || this.blocks.size() <= 0) return null;
		int index = this.blocks.size() - 1;
		return this.blocks.get(index).getEndCodeLine();
	}

	/**
	 * ブロックリストクラスのブロックタイプはUNKNOWNとする.
	 */
	@Override
	public BlockType getBlockType() {
		return BlockType.UNKNOWN;
	}

	@Override
	public IBlock getMotherBlock() {
		return null;
	}

	/**
	 * ブロックリストから変数リストを取得する.
	 */
	@Override
	public Set<Variable> getAllVariables() {
		if (this.blocks == null || this.blocks.size() <= 0) return null;
		Set<Variable> list = new HashSet<Variable>();
		for (IBlock block : this.blocks) {
			if (block == null) continue;
			Set<Variable> vars = block.getAllVariables();
			if (vars != null) {
	        	list.addAll(vars);
			}
        }
		if (list.size() <= 0) return null;
		return list;
	}

	/**
	 * ブロックリストの文字列表現を取得する.
	 * ブロックリストの開始 - 終了の文字列表現を取得する.
	 */
	@Override
	public String toString() {
		if (this.blocks == null || this.blocks.size() <= 0) return null;
		int last = this.blocks.size() - 1;
		String start = this.blocks.get(0).toString();
		String end = this.blocks.get(last).toString();
		if (this.blocks.size() == 1) {
			return start;
		}
		// [start] - [end]
		StringBuffer buf = new StringBuffer();
		buf.append("[");
		buf.append(start);
		buf.append("]");
		buf.append(" - ");
		buf.append("[");
		buf.append(end);
		buf.append("]");
		return buf.toString();
	}

	/**
	 * ブロックがリストに登録済みかチェックする.
	 * @param block		ブロック
	 * @return		true=登録済み
	 */
	public boolean contains(IBlock block) {
		if (this.blocks == null || this.blocks.size() <= 0) return false;
		return this.blocks.contains(block);
	}
}

