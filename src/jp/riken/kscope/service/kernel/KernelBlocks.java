package jp.riken.kscope.service.kernel;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.BlockType;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.ProcedureUsage;
import jp.riken.kscope.language.ProgramUnit;
import jp.riken.kscope.language.Variable;

/**
 * 複数ブロックを持つカーネルファイルクラス.
 * F90ファイルに複数モジュール、外部サブルーチンを持つモジュールファイル用のクラス
 * @author RIKEN
 */
public class KernelBlocks implements IBlock, Collection<KernelBlock> {

    /** カーネル抽出ブロックリスト */
    private List<KernelBlock> kernel_blocks;

    /**
     * コンストラクタ
     */
    public KernelBlocks() {

    }

    /**
     * カーネルブロックを追加する
     * @param    block   追加カーネルブロック
     */
    public void addKernelBlock(KernelBlock block) {
        if (this.kernel_blocks == null) {
            this.kernel_blocks = new ArrayList<KernelBlock>();
        }
        this.kernel_blocks.add(block);
    }

    /**
     * 開始行番号情報を取得する。
     * @return 開始行番号情報
     */
    @Override
    public CodeLine getStartCodeLine() {
        if (this.kernel_blocks == null) return null;
        CodeLine line = null;
        for (KernelBlock block : this.kernel_blocks) {
            if (line == null) {
                line = block.getStartCodeLine();
            }
            else if (line.compareTo(block.getStartCodeLine()) > 0) {
                line = block.getStartCodeLine();
            }
        }

        return line;
    }

    /**
     * 終了行番号情報を取得する。
     * @return        終了行番号情報
     */
    @Override
    public CodeLine getEndCodeLine() {
        if (this.kernel_blocks == null) return null;
        CodeLine line = null;
        for (KernelBlock block : this.kernel_blocks) {
            if (line == null) {
                line = block.getEndCodeLine();
            }
            else if (line.compareTo(block.getStartCodeLine()) < 0) {
                line = block.getEndCodeLine();
            }
        }

        return line;
    }


    /**
     * ブロックタイプを返す。
     * @return ブロックタイプ
     */
    @Override
    public BlockType getBlockType() {
        if (this.kernel_blocks == null) return null;
        return BlockType.MODULE;
    }

    /**
     * 親ブロックを習得する。
     * @return 親ブロック
     */
    @Override
    public IBlock getMotherBlock() {
        return null;
    }

    /**
     * 変数リストを取得する.
     * 子ブロックの変数リストも取得する。
     * @return        変数リスト
     */
    @Override
    public Set<Variable> getAllVariables() {
        if (this.kernel_blocks == null) return null;

        Set<Variable> list = new HashSet<Variable>();
        for (KernelBlock block : this.kernel_blocks) {
            Set<Variable> block_vars = block.getAllVariables();
            if (block_vars == null) continue;
            list.addAll(block_vars);
        }
        if (list.size() <= 0) return null;

        return list;
    }

    /**
     * 変数リストを取得する.
     * ブロックのみの変数リストを取得する。
     * @return        変数リスト
     */
    @Override
    public Set<Variable> getBlockVariables() {
        if (this.kernel_blocks == null) return null;

        Set<Variable> list = new HashSet<Variable>();
        for (KernelBlock block : this.kernel_blocks) {
            Set<Variable> block_vars = block.getBlockVariables();
            if (block_vars == null) continue;
            list.addAll(block_vars);
        }
        if (list.size() <= 0) return null;

        return list;
    }

    /**
     * 子要素を返す.
     * @return 子要素
     */
    @Override
    public List<IBlock> getChildren() {
        return null;
    }

    /**
     * 行番号のブロックを検索する
     * @param line            行番号
     * @return        行番号のブロック
     */
    @Override
    public IBlock[] searchCodeLine(CodeLine line) {
        if (this.kernel_blocks == null) return null;

        for (KernelBlock block : this.kernel_blocks) {
            IBlock[] search = block.searchCodeLine(line);
            if (search == null) continue;
            return search;
        }

        return null;
    }

    /**
     * 親ブロックからIDeclarationsブロックを取得する.
     * @return    IDeclarationsブロック
     */
    @Override
    public ProgramUnit getScopeDeclarationsBlock() {
        return null;
    }

    /**
     * プロシージャ（関数）からブロックまでの階層文字列表記を取得する
     * @return      階層文字列表記
     */
    @Override
    public String toStringProcedureScope() {
        return null;
    }

    @Override
    public String toStringModuleScope() {
        return null;
    }

    /**
     * モジュールからブロックまでの階層文字列表記を取得する
     * @return      階層文字列表記
     */
    @Override
    public String toStringScope(boolean module) {
        return null;
    }

    /**
     * 関数呼出を含む自身の子ブロックのリストを返す。
     * @return 子ブロックのリスト
     */
    @Override
    public List<IBlock> getBlocks() {
        if (this.kernel_blocks == null) return null;

        List<IBlock> list = new ArrayList<IBlock>();
        for (KernelBlock block : this.kernel_blocks) {
            list.add(block);
        }
        if (list.size() <= 0) return null;

        return list;
    }

    @Override
    public String toEndString() {
        return null;
    }

    @Override
    public List<ProcedureUsage> getCalls() {
        if (this.kernel_blocks == null) return null;

        List<ProcedureUsage> list = new ArrayList<ProcedureUsage>();
        for (KernelBlock block : this.kernel_blocks) {
            List<ProcedureUsage> calls = block.getCalls();
            if (calls == null) continue;
            list.addAll(calls);
        }
        if (list.size() <= 0) return null;

        return list;
    }


    /**
     * ソースファイル情報を取得する。
     * @return ソースファイル
     */
    public SourceFile getSourceFile() {
        CodeLine line = this.getStartCodeLine();
        if (line == null) return null;
        return line.getSourceFile();
    }

    @Override
    public int size() {
        if (this.kernel_blocks == null) return 0;
        return this.kernel_blocks.size();
    }

    @Override
    public boolean isEmpty() {
        if (this.kernel_blocks == null) return true;
        return this.kernel_blocks.isEmpty();
    }

    @Override
    public boolean contains(Object o) {
        if (this.kernel_blocks == null) return false;
        return this.kernel_blocks.contains(o);
    }

    @Override
    public Iterator<KernelBlock> iterator() {
        if (this.kernel_blocks == null) return null;
        return this.kernel_blocks.iterator();
    }

    @Override
    public KernelBlock[] toArray() {
        if (this.kernel_blocks == null) return null;
        return this.kernel_blocks.toArray(new KernelBlock[0]);
    }

    @Override
    public Object[] toArray(Object[] a) {
        if (this.kernel_blocks == null) return null;
        return this.kernel_blocks.toArray(a);
    }

    @Override
    public boolean remove(Object o) {
        if (this.kernel_blocks == null) return false;
        return this.kernel_blocks.remove(o);
    }

    @Override
    public boolean containsAll(Collection c) {
        if (this.kernel_blocks == null) return false;
        return this.kernel_blocks.containsAll(c);
    }

    @Override
    public boolean addAll(Collection c) {
        if (this.kernel_blocks == null) return false;
        return this.kernel_blocks.containsAll(c);
    }

    @Override
    public boolean removeAll(Collection c) {
        if (this.kernel_blocks == null) return false;
        return this.kernel_blocks.removeAll(c);
    }

    @Override
    public boolean retainAll(Collection<?> c) {
        if (this.kernel_blocks == null) return false;
        return this.kernel_blocks.retainAll(c);
    }

    @Override
    public void clear() {
        if (this.kernel_blocks == null) return;
        this.kernel_blocks.clear();
    }

    @Override
    public boolean add(KernelBlock e) {
        this.addKernelBlock(e);
        return true;
    }

    /**
     * KernelContextを設定する.
     * @param context        KernelContext
     */
    public void setKernelContext(KernelContext context) {
        if (this.kernel_blocks == null) return;
        for (KernelBlock block : this.kernel_blocks) {
            block.setKernelContext(context);
        }
    }


}
