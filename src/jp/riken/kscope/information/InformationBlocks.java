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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import jp.riken.kscope.language.IInformation;

/**
 * 情報ブロックのコレクションクラス
 * @author RIKEN
 *
 */
public class InformationBlocks extends ArrayList<InformationBlock> {

    /** シリアル番号 */
	private static final long serialVersionUID = 4740835139890184660L;

	/**
     * 要素を追加する。ただし、要素内容が重複している場合は、追加しない。
     *
     * @param e
     *         追加対象要素
     * @return true: 追加に成功。false: 追加に失敗
     */
    @Override
    public boolean add(InformationBlock e) {
        boolean result = true;
        if (e == null) { return false; }
        if (!this.contains(e.getInformation(),
                e.getStartBlock(), e.getEndBlock())) {
            result = super.add(e);
        }
        return result;
    }

    /**
     * 要素群を追加する。ただし、内容が重複している要素は、追加しない。
     *
     * @param c
     *         追加対象要素群
     * @return true: 追加に成功。false: 追加に失敗
     */
    @Override
    public boolean addAll(Collection<? extends InformationBlock> c) {
        boolean result = true;
        if (c == null) { return false; }
        for (InformationBlock e : c) {
            if (!this.add(e)) { result = false; }
        }
        return result;
    }

    /**
     * 対象となる情報ブロックが含まれているかどうか。
     *
     * @param info
     *          情報
     * @param startBlock
     *          開始ブロック
     * @param endBlock
     *          終了ブロック
     * @return true : 対象となる情報ブロックが含まれている。
     *         false: 対象となる情報ブロックが含まれていない。
     */
    public boolean contains(InformationBase info,
            IInformation startBlock, IInformation endBlock) {
        boolean result = false;
        InformationBlock infoBlock = this.findObjectBy(startBlock, endBlock);
        if (infoBlock != null) {
            if (infoBlock.getInformation() == info) {
                result = true;
            }
        }
        return result;
    }

    /**
     * 対象となる情報ブロックを検索する。
     *
     * @param startBlock
     *          開始ブロック
     * @param endBlock
     *          終了ブロック
     * @return 見つかった情報ブロック。見つからなかった場合はnullが返る
     */
    public InformationBlock findObjectBy(
            IInformation startBlock, IInformation endBlock) {
        InformationBlock result = null;

        for (InformationBlock block : this) {
            if (block.getStartBlock() == startBlock
            && block.getEndBlock() == endBlock) {
                result = block;
                break;
            }
        }

        return result;
    }

    /**
     * 指定したブロックを開始に持つInformationBlockのリストを返す。
     * @param start 開始ブロック
     * @return 付加情報領域のリスト。無ければ空のリストを返す。
     */
    public List<InformationBlock> getStartWith(IInformation start) {
        List<InformationBlock> blocks = new ArrayList<InformationBlock>();
        for (InformationBlock bk: this) {
            if (bk.getStartBlock().equals(start)) {
                blocks.add(bk);
            }
        }
        return blocks;
    }

    /**
     * 対象となる情報ブロックを削除する。
     *
     * @param startBlock
     *          開始ブロック
     * @param endBlock
     *          終了ブロック
     * @return true: 削除された。false: 削除に失敗した
     */
    public boolean remove(
            IInformation startBlock, IInformation endBlock) {
        boolean result = true;
        InformationBlock infoBlock = this.findObjectBy(endBlock, endBlock);
        if (infoBlock != null) {
            result = this.remove(infoBlock);
        }
        return result;
    }
}
