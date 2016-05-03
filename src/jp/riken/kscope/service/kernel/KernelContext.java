package jp.riken.kscope.service.kernel;

import java.util.ArrayList;
import java.util.List;

import org.apache.velocity.VelocityContext;

import jp.riken.kscope.language.Condition;
import jp.riken.kscope.language.ExecutableBody;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.Module;
import jp.riken.kscope.language.Program;
import jp.riken.kscope.properties.KernelProperties;

public class KernelContext extends VelocityContext {

    /** カーネル出力インデント */
    private int n_indent = 0;
    /** インデント幅 */
    private int indent_column = KernelProperties.DEFAULT_INDELT_COLUMN;
    /** Writer */
    private FortranFormattedWriter writer;
    /** カーネルプロパティ */
    private KernelProperties properties;

    /**
     * コンストラクタ
     */
    public KernelContext() {
        this.n_indent = 0;
        this.setLineWriter(null);
    }

    /**
     * インデントを取得する
     * @return    インデント
     */
    public int getIndent() {
        return this.n_indent;
    }

    /**
     * インデントを設定する.
     * @param indent        インデント
     */
    public void setIndent(int indent) {
        this.n_indent = indent;
    }

    /**
     * インデントをインクリメントする
     * @return        インデント
     */
    public int incrementIndent() {
        this.n_indent++;
        return this.n_indent;
    }

    /**
     * インデントをデクリメントする.
     * @return        インデント
     */
    public int decrementIndent() {
        this.n_indent--;
        return this.n_indent;
    }

    /**
     * インデント幅を設定する.
     * @param column        インデント幅
     */
    public void setIndentColumn(int column) {
        this.indent_column = column;
    }


    /**
     * ブロックのインデント幅を取得する.
     * @param root_block      ルートブロック
     * @param block        ブロック
     * @return            インデント幅
     */
    public int getCurrentIndentColumn(IBlock root_block, IBlock current) {
        if (current == null) return 0;
        if (root_block == null) return 0;

        int current_indent = 0;
        if (this.writer != null) {
            current_indent = this.writer.getCurrentIndent();
        }
        int indent = this.countOfIndent(root_block, current);
        if (indent >= 0) indent = indent*this.indent_column;
        else indent = 0;
        indent += current_indent;

        return indent;
    }


    /**
     * 親ブロックから子ブロックまでの階層数を取得する
     * @param parent_block    親ブロック
     * @param child_block     子ブロック
     * @return        階層数 : -1の場合は親子関係ではない。
     */
    private int countOfIndent(IBlock parent_block, IBlock child_block) {
        int indent = 0;
        if (parent_block == null) return -1;
        if (child_block == null) return -1;

        IBlock current_block = child_block;
        while (current_block != null) {
            if (parent_block == current_block) break;
            if (current_block instanceof ExecutableBody ) {
                current_block = ((ExecutableBody) current_block).getParent();
            }
            if (current_block instanceof Condition ) {
                current_block = ((Condition) current_block).getMotherBlock();
            }
            if (parent_block == current_block) break;
            if (parent_block.toStringModuleScope().equals(current_block.toStringModuleScope())) break;

            indent++;
            current_block = current_block.getMotherBlock();
            if (current_block instanceof Module ) {
                if (Program.NO_MODULE.equalsIgnoreCase(((Module) current_block).get_name())) {
                    // NO_MODULEはカウントしない。
                    break;
                }
            }
        }

        return indent;
    }


    /**
     * デフォルトインデント幅を取得する.
     * @return       インデント幅
     */
    public int getIndentColumn() {
        return this.indent_column;
    }

    /**
     * 親ブロックからのブロックのインデント幅を取得する.
     * @param block        ブロック
     * @return            インデント幅
     */
    public int getIndentColumnFromParent(IBlock kernel_block) {
        if (kernel_block == null) return 0;

        int count = 0;
        IBlock parent = kernel_block.getMotherBlock();
        while (parent != null) {
            count++;
            parent = parent.getMotherBlock();
        }

        return count*getIndentColumn();
    }

    /**
     * Writerを取得する.
     * @return writer        Writer
     */
    public FortranFormattedWriter getLineWriter() {
        return this.writer;
    }

    /**
     *  Writerを設定する.
     * @param writer     Writer
     */
    public void setLineWriter(FortranFormattedWriter writer) {
        this.writer = writer;
    }


    /**
     * テンプレートコンテキストにブロックリストを追加する
     * @param  context        テンプレートコンテキスト
     * @param key            キー名
     * @param values        選択ブロックリスト
     */
    public void putKernelBlocks(
                    String key,
                    List<? extends IBlock> values) {
        if (key == null) return;
        if (values == null) {
            this.put(key, values);
            return;
        }

        List<KernelBlock> list = new ArrayList<KernelBlock>();
        for (IBlock block : values) {
            // カーネルブロック生成
            KernelBlock kernel = new KernelBlock(block, block);
            kernel.setKernelContext(this);
            // カーネルリストに追加
            list.add(kernel);
        }

        this.put(key, list);
        return;
    }

    /**
     * テンプレートコンテキストにブロックリストを追加する
     * @param  context        テンプレートコンテキスト
     * @param key            キー名
     * @param blocks        選択ブロックリスト
     */
    public void putKernelBlocks(
                    String key,
                    KernelBlocks blocks) {
        if (key == null) return;
        if (blocks == null) {
            this.put(key, blocks);
            return;
        }

        // カーネルブロックにコンテキスト設定
        blocks.setKernelContext(this);

        this.put(key, blocks);
        return;
    }

    /**
     * テンプレートコンテキストにブロックリストを追加する
     * @param  context        テンプレートコンテキスト
     * @param key            キー名
     * @param values        選択ブロックリスト
     */
    public void putKernelBlock(
                    String key,
                    IBlock block) {
        if (key == null) return;
        if (block == null) {
            this.put(key, block);
            return;
        }

        // カーネルブロック生成
        KernelBlock kernel = new KernelBlock((IBlock)block, (IBlock)block);
        kernel.setKernelContext(this);
        this.put(key, kernel);
        return;
    }

    /**
     * テンプレートコンテキストにブロックリストを追加する
     * @param  context        テンプレートコンテキスト
     * @param key            キー名
     * @param values        選択ブロックリスト
     */
    public void putKernelBlock(
                    String key,
                    KernelBlock kernel) {
        if (key == null) return;
        if (kernel == null) {
            this.put(key, kernel);
            return;
        }

        // カーネルブロックにコンテキスト設定
        kernel.setKernelContext(this);

        this.put(key, kernel);
        return;
    }

    /**
     * カーネルプロパティを取得する。
     * @return properties        カーネルプロパティ
     */
    public KernelProperties getProperties() {
        return this.properties;
    }

    /**
     * カーネルプロパティを設定する
     * @param properties     カーネルプロパティ
     */
    public void setProperties(KernelProperties properties) {
        this.properties = properties;
    }
}


