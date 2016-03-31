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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.language.BlockType;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.language.ProcedureUsage;
import jp.riken.kscope.language.ProgramUnit;
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

    /**
     * 式の変数リストを取得する.
     * ブロックのみの変数リストを取得する。
     * @return        式の変数リスト
     */
    @Override
    public Set<Variable> getBlockVariables() {
        return null;
    }

    /**
     * 子要素を返す。
     *
     * @return 子要素。無ければ空のリストを返す
     */
    @Override
    public List<IBlock> getChildren() {
        if (this.startBlock instanceof IBlock) {
            IBlock blk = (IBlock) this.startBlock;
            return blk.getChildren();
        }
        return null;
    }

    /**
     * idにマッチした情報ブロックを検索する。
     * @param id          ID
     * @return 見つかった情報ブロック。見つからなかった場合はnullが返ります。
     */
    @Override
    public IInformation findInformationBlockBy(String id) {
        IInformation info = null;

        if (this.startBlock != null) {
            info = this.startBlock.findInformationBlockBy(id);
            if (info != null) {
                return info;
            }
        }
        if (this.endBlock != null) {
            info = this.endBlock.findInformationBlockBy(id);
            if (info != null) {
                return info;
            }
        }

        return null;
    }

    /**
     * 付加情報ブロックコレクションを生成する。
     *
     * @return 付加情報ブロックコレクション
     */
    @Override
    public InformationBlocks createInformationBlocks() {
        InformationBlocks result = new InformationBlocks();

        if (this.information != null) {
            InformationBlock block = new InformationBlock(this.information, this, this);
            result.add(block);
        }
        if (this.startBlock != null) {
            result.addAll(this.startBlock.createInformationBlocks());
        }
        if (this.endBlock != null) {
            result.addAll(this.endBlock.createInformationBlocks());
        }

        return result;
    }

    /**
     * 同一付加情報ブロックを検索する
     * @param block            IInformationブロック
     * @return        同一ブロック
     */
    @Override
    public IInformation[] searchInformationBlocks(IInformation block) {
        if (block == null) return null;

        List<IInformation> list = new ArrayList<IInformation>();
        if (this.startBlock != null) {
            IInformation[] infos = this.startBlock.searchInformationBlocks(block);
            if (infos != null) {
                list.addAll(Arrays.asList(infos));
            }
        }
        if (this.endBlock != null) {
            IInformation[] infos = this.endBlock.searchInformationBlocks(block);
            if (infos != null) {
                list.addAll(Arrays.asList(infos));
            }
        }

        if (list.size() <= 0) {
            return null;
        }
        return list.toArray(new IInformation[0]);
    }

    /**
     * layoutIDにマッチした構造ブロックを検索する。
     * @param id    layoutID
     * @return 見つかった構造ブロック
     */
    @Override
    public IInformation findInformationLayoutID(String id) {
        if (id == null || id.isEmpty()) return null;
        IInformation info = null;

        if (this.startBlock != null) {
            info = this.startBlock.findInformationLayoutID(id);
        }
        if (info != null) {
            return info;
        }

        if (this.endBlock != null) {
            info = this.endBlock.findInformationLayoutID(id);
        }

        return info;
    }

    /**
     * 行番号のブロックを検索する
     * @param line            行番号
     * @return        行番号のブロック
     */
    @Override
    public IBlock[] searchCodeLine(CodeLine line) {

        List<IBlock> list = new ArrayList<IBlock>();
        if (this.startBlock != null && this.startBlock instanceof IBlock) {
            IBlock[] childs = ((IBlock)this.startBlock).searchCodeLine(line);
            if (childs != null && childs.length > 0) {
                list.addAll(Arrays.asList(childs));
            }
        }
        if (this.endBlock != null && this.endBlock instanceof IBlock) {
            IBlock[] childs = ((IBlock)this.endBlock).searchCodeLine(line);
            if (childs != null && childs.length > 0) {
                list.addAll(Arrays.asList(childs));
            }
        }

        if (list.size() <= 0) return null;

        return list.toArray(new IBlock[0]);
    }

    /**
     * 親ブロックからIDeclarationsブロックを取得する.
     * @return    IDeclarationsブロック
     */
    @Override
    public ProgramUnit getScopeDeclarationsBlock() {
        if (this.startBlock != null && this.startBlock instanceof IBlock) {
            return ((IBlock)this.startBlock).getScopeDeclarationsBlock();
        }
        if (this.endBlock != null && this.endBlock instanceof IBlock) {
            return ((IBlock)this.endBlock).getScopeDeclarationsBlock();
        }

        return null;
    }

    /**
     * プロシージャ（関数）からブロックまでの階層文字列表記を取得する
     * 階層文字列表記 : [main()]-[if (...)]-[if (...)]
     * CompoundBlock（空文）は省略する.
     * @return      階層文字列表記
     */
    @Override
    public String toStringProcedureScope() {
        return this.toStringScope(false);
    }

    /**
     * モジュールからブロックまでの階層文字列表記を取得する
     * 階層文字列表記 : [main()]-[if (...)]-[if (...)]
     * CompoundBlock（空文）は省略する.
     * @return      階層文字列表記
     */
    @Override
    public String toStringModuleScope() {
        return this.toStringScope(true);
    }


    /**
     * ブロックの階層文字列表記を取得する
     * 階層文字列表記 : [main()]-[if (...)]-[if (...)]
     * CompoundBlock（空文）は省略する.
     * @param   module     true=Moduleまでの階層文字列表記とする
     * @return      階層文字列表記
     */
    @Override
    public String toStringScope(boolean module) {
        String statement = this.toString();
        statement = "[" + statement + "]";
        if (this.getMotherBlock() != null) {
            String buf = null;
            if (module) buf = this.getMotherBlock().toStringModuleScope();
            else buf = this.getMotherBlock().toStringProcedureScope();
            if (buf != null && !buf.isEmpty()) {
                statement = buf + "-" + statement;
            }
        }
        return statement;
    }

    /**
     * 関数呼出を含む自身の子ブロックのリストを返す。
     * @return 子ブロックのリスト
     */
    public List<IBlock> getBlocks() {
        if (this.startBlock instanceof IBlock) {
            IBlock blk = (IBlock) this.startBlock;
            return blk.getBlocks();
        }
        return null;
    }

    /**
     * 終了:END文を持つブロックは、終了:END文を返す
     * @return        終了:END文
     */
    @Override
    public String toEndString() {
        return null;
    }

    /**
     * 手続呼出しのリストを返す。
     * @return 手続呼出しのリスト
     */
    @Override
    public List<ProcedureUsage> getCalls() {
        return null;
    }
}
