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
package jp.riken.kscope.action;

import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JTable;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

import jp.riken.kscope.data.BlockList;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.information.InformationBlock;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.language.utils.LanguageUtils;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.utils.SwingUtils;

/**
 * 分析結果の該当箇所を開くアクションクラス
 * @author RIKEN
 */
public class ViewOpenAnalysisLineAction extends ActionBase implements MouseListener {

    /**
     * コンストラクタ
     * @param controller    アプリケーションコントローラ
     */
    public ViewOpenAnalysisLineAction(AppController controller) {
        super(controller);
    }


    /**
     * 分析結果の該当箇所を開く
     */
    private void openAnalysisLine() {
        // 選択ブロック情報を取得する
        IBlock block = this.controller.getMainframe().getPanelAnalysisView().getSelectedBlock();
        // 選択ブロックの構造ツリー、ソースビューの該当個所を選択状態にする
        viewSelectedBlock(block);

        return;
    }

    /**
     * 検索結果ツリーパスを開く
     * @param  path            検索結果ツリーパス
     */
    public void openSearchLine(TreePath path) {
        if (path == null) return;
        DefaultMutableTreeNode node = (DefaultMutableTreeNode) path.getLastPathComponent();
        if (node == null) return;
        CodeLine line = null;
        SourceFile file = null;
        if (node.getUserObject() instanceof CodeLine) {
            // ソースコード行
            line = (CodeLine)node.getUserObject();
            // ファイル検索の場合、１つ上のノードがファイルノードパス
            node = (DefaultMutableTreeNode) node.getParent();
        }
        else if (node.getUserObject() instanceof SourceFile) {
            // ノードがSourceFileの場合:エクスプローラノードを選択する
            // ツリーパスを作成する
            TreePath searchPath = SwingUtils.getTreePath(node);
            // ツリーパスを選択する
            this.controller.getMainframe().getPanelExplorerView().setSelectionPath(searchPath);

            // ソースファイル
            file = (SourceFile)node.getUserObject();
        }

        // ノードがコード行の場合
        if (line != null) {
            // 選択コード行を選択状態にする
            viewSourceLine(line);
        }
        else if (file != null) {
            // 選択ファイルを開く
            viewSourceFile(file);
        }
        // ノードがブロックの場合
        else if (node.getUserObject() != null && node.getUserObject() instanceof IBlock) {
            // 選択ノードを選択状態にする
            viewSelectedPath(path);
        }

        // ソースビューで検索文字列をハイライトする
        this.controller.setSearchKeywords();
    }


    /**
     * トレース結果該当個所を開く
     * @param        block        トレース結果ブロック
     */
    public void openTraceBlock(IBlock block) {
        // 選択ブロックの構造ツリー、ソースビューの該当個所を選択状態にする
        viewSelectedBlock(block);

        // ソースビューでトレース変数をハイライトする
        this.controller.setTraceKeywords();
    }

    /**
     * 指定ブロックを構造ツリー、ソースビューの該当個所を選択状態にする
     * @param block            選択ブロック
     */
    public void viewSelectedBlock(IBlock block) {
        if (block == null) return;

/*********  元コード  at 2012/03/21 by @hira
        if (block instanceof InformationBlock) {
            IInformation info = ((InformationBlock) block).getStartBlock();
            if (info instanceof IBlock) {
                block = (IBlock) info;
            }
        }

        // 構造ツリーを選択状態にする
        this.controller.getMainframe().getPanelExplorerView().setSelectedNode(block);

        // 選択コード行を選択状態にする
        viewSourceBlock(block);
**********************************************/

/*********  暫定コード:start  at 2012/03/21 by @hira   **********/
        IBlock[] blocks = new IBlock[2];
        if (block instanceof InformationBlock) {
            IInformation info = ((InformationBlock) block).getStartBlock();
            if (info instanceof IBlock) {
                blocks[0] = (IBlock) info;
            }
            info = ((InformationBlock) block).getEndBlock();
            if (info instanceof IBlock) {
                blocks[1] = (IBlock) info;
            }
        }
        else {
            blocks = null;
        }

        if (blocks == null) {
            // 構造ツリーを選択状態にする
            this.controller.getMainframe().getPanelExplorerView().setSelectedNode(block);
        }
        else {
            // 構造ツリーを選択状態にする
            this.controller.getMainframe().getPanelExplorerView().setSelectedNodeArea(blocks[0], blocks[1]);
        }

        // 選択コード行を選択状態にする
        viewSourceBlock(block);

/*********  暫定コード:end  at 2012/03/21 by @hira   **********/

    }

    /**
     * 指定ブロックをソースビューの該当個所を選択状態にする
     * @param code            選択コード範囲
     */
    public void viewSelectedSourceBlock(CodeLine code) {
        // 選択ソースコード行情報からファイルを開く
        try {
            this.controller.openSourceFile(code);
        } catch (Exception ex) {
            this.controller.getErrorInfoModel().addErrorInfo(code, ex.getMessage());
        }
        // 選択コード行を選択状態にする
        CodeLine[] lines = {code};
        this.controller.getMainframe().getPanelSourceView().setSelectedBlock(lines);
    }

    /**
     * 指定パスを構造ツリー、ソースビューの該当個所を選択状態にする
     * @param path            指定パス
     */
    public void viewSelectedPath(TreePath path) {
        if (path == null) return;

        // 構造ツリーを選択状態にする
        this.controller.getMainframe().getPanelExplorerView().setSelectionPath(path);

        // ノードがブロックの場合
        DefaultMutableTreeNode node = (DefaultMutableTreeNode) path.getLastPathComponent();
        if (node.getUserObject() != null && node.getUserObject() instanceof IBlock) {
            IBlock block = (IBlock)node.getUserObject();
            // 選択コード行を選択状態にする
            viewSourceBlock(block);
        }
    }

    /**
     * 指定ブロックのソースコード行を表示する
     * @param block        ブロック
     */
    public void viewSourceBlock(IBlock block) {

        if (block == null) return;

        // 選択ソースコード行情報
        CodeLine line = block.getStartCodeLine();
        if (line == null) return;

        // 選択ソースコード行情報からファイルを開く
        try {
            this.controller.openSourceFile(line);
        } catch (Exception ex) {
            ex.printStackTrace();
            String msg = ex.getMessage();
            if (msg == null) {
                msg = ex.toString();
            }
            this.controller.getErrorInfoModel().addErrorInfo(line, msg);
        }

/*********  元コード  at 2012/03/21 by @hira
        // 選択コード行を選択状態にする
        CodeLine[] lines = {line};
        this.controller.getMainframe().getPanelSourceView().setSelectedBlock(lines);
*******************/

/*********  暫定コード:start  at 2012/03/21 by @hira   **********/
        // 選択コード行を選択状態にする
        CodeLine[] lines = {line};
        if (block instanceof InformationBlock) {
            CodeLine infostartlines = null;
            CodeLine infoendlines = null;
            IInformation info = ((InformationBlock) block).getStartBlock();
            if (info instanceof IBlock) {
                infostartlines = ((IBlock) info).getStartCodeLine();
            }
            info = ((InformationBlock) block).getEndBlock();
            if (info instanceof IBlock) {
                infoendlines = ((IBlock) info).getStartCodeLine();
            }
            // 同一ファイル、開始行 < 終了行であるなら、開始ブロックから終了ブロックを連続してハイライトする。
            CodeLine infoblock = null;
            if (infostartlines.getSourceFile() != null && infoendlines.getSourceFile() != null) {
                if (infostartlines.getSourceFile().equals(infoendlines.getSourceFile())
                    && infostartlines.getStartLine() <= infoendlines.getEndLine()) {
                    // ブロックハイライト用のCodeLine生成
                    infoblock = new CodeLine(infostartlines);
                    int endlineno = infostartlines.getEndLine();
                    if (endlineno < infoendlines.getStartLine()) {
                        endlineno = infoendlines.getStartLine();
                    }
                    if (endlineno < infoendlines.getEndLine()) {
                        endlineno = infoendlines.getEndLine();
                    }
                    infoblock.setEndLine(infoendlines.getEndLine());
                }
            }
            if (infoblock != null) {
                lines = new CodeLine[] {infoblock};
            }
            else {
                // 開始ブロックと終了ブロックを別々にハイライトする
                lines = new CodeLine[] {infostartlines, infoendlines};
            }
        }
        else if (block instanceof BlockList) {
            List<IBlock> block_list = ((BlockList)block).getBlocks();
            List<CodeLine> line_list = new ArrayList<CodeLine>();
            if (block_list != null) {
                for (IBlock item : block_list) {
                    line_list.add(item.getStartCodeLine());
                }
            }
            if (line_list.size() > 0) {
                lines = line_list.toArray(new CodeLine[0]);
            }
        }
        this.controller.getMainframe().getPanelSourceView().setSelectedBlock(lines);

/*********  暫定コード:end  at 2012/03/21 by @hira   **********/

    }

    /**
     * 指定ブロックのソースコード行をクリアする
     */
    public void clearSourceBlock() {
        this.controller.getMainframe().getPanelSourceView().clearSelectedBlock();

        return;
    }


    /**
     * 指定ブロックのソースコード行を表示する
     * @param line        選択ソースコード行
     */
    public void viewSourceLine(CodeLine line) {

        // 選択ソースコード行
        if (line == null) return;

        // 選択ソースコード行情報からファイルを開く
        try {
            this.controller.openSourceFile(line);
        } catch (Exception ex) {
            this.controller.getErrorInfoModel().addErrorInfo(line, ex.getMessage());
        }

        // 選択コード行を選択状態にする
        this.controller.getMainframe().getPanelSourceView().setSelectedLine(line);

        LanguageUtils utils = new LanguageUtils(this.controller.getFortranLanguage());
        IBlock[] blocks = utils.getCodeLineBlocks(line);
        if (blocks != null && blocks.length > 0) {
            // 構造ツリーを選択状態にする
            this.controller.getMainframe().getPanelExplorerView().setSelectedNode(blocks[blocks.length-1]);
        }

    }


    /**
     * 指定ソースファイルを表示する
     * @param file        選択ソースファイル
     */
    public void viewSourceFile(SourceFile file) {

        // 選択ソースファイル
        if (file == null) return;

        // 選択ソースファイルを開く
        try {
            this.controller.openSourceFile(file);
        } catch (Exception ex) {
            this.controller.getErrorInfoModel().addErrorInfo(file, ex.getMessage());
        }
    }

    /**
     * アクション発生イベント
     * @param event        イベント情報
     */
    @Override
    public void actionPerformed(ActionEvent event) {
        // 選択ファイルを開く
        openAnalysisLine();
    }

    /**
     * マウスクリックイベント
     * @param event            イベント情報
     */
    @Override
    public void mouseClicked(MouseEvent event) {
        // ダブルクリックチェック
        if (SwingUtilities.isLeftMouseButton(event) && event.getClickCount() == 2) {
            if (event.getSource() instanceof JTree) {
                // 選択ファイルを開く
                openAnalysisLine();
            }
            else if (event.getSource() instanceof JTable) {
                // 選択ファイルを開く
                openAnalysisLine();
            }
        }
    }

    /**
     * マウスボタンダウンイベント
     * @param e        マウスイベント情報
     */
    @Override
    public void mousePressed(MouseEvent e) { }

    /**
     * マウスボタンアップイベント
     * @param e        マウスイベント情報
     */
    @Override
    public void mouseReleased(MouseEvent e) {}

    /**
     * マウスオーバーイベント
     * @param e        マウスイベント情報
     */
    @Override
    public void mouseEntered(MouseEvent e) {}

    /**
     * マウスアウトイベント
     * @param e        マウスイベント情報
     */
    @Override
    public void mouseExited(MouseEvent e) {}


    /**
     * トレース結果該当個所を開く
     * @param        blocks        トレース結果ブロックリスト
     */
    public void openTraceBlocks(IBlock[] blocks) {
        if (blocks == null || blocks.length <= 0) return;

        // 構造ツリーを選択状態にする
        this.controller.getMainframe().getPanelExplorerView().setSelectedBlocks(blocks);

        // 選択コード行を選択状態にする
        IBlock block = blocks[blocks.length-1];
        viewSourceBlock(block);
    }

    /**
     * 複数の指定ブロックを構造ツリー、ソースビューの該当個所を選択状態にする
     * @param blocks            選択ブロック
     */
    public void viewSelectedArea(IBlock[] blocks) {
        if (blocks == null) return;

        // 構造ツリーを選択状態にする
        if (blocks.length > 1) {
            this.controller.getMainframe().getPanelExplorerView().setSelectedNodeArea(blocks[0], blocks[blocks.length-1]);
        }
        else {
            this.controller.getMainframe().getPanelExplorerView().setSelectedNodes(blocks);
        }

        CodeLine blockcode = null;
        if (blocks.length > 0) {
            CodeLine start = new CodeLine(blocks[0].getStartCodeLine(), blocks[0].getEndCodeLine());
            CodeLine end = new CodeLine(blocks[blocks.length-1].getStartCodeLine(), blocks[blocks.length-1].getEndCodeLine());
            blockcode = new CodeLine(start, end);
        }
        if (blockcode != null) {
            // 選択ソースコード行情報からファイルを開く
            try {
                this.controller.openSourceFile(blockcode);
            } catch (Exception ex) {
                this.controller.getErrorInfoModel().addErrorInfo(blockcode, ex.getMessage());
            }
            // 選択ファイルのブロックを選択状態にする
            CodeLine[] codes = {blockcode};
            this.controller.getMainframe().getPanelSourceView().setSelectedBlock(codes);
        }

        return;
    }

    /**
     * 複数の指定ブロックを構造ツリー、ソースビューの該当個所を選択状態にする
     * @param areas            選択ブロック
     */
    public void viewSelectedAreas(List<IBlock[]> areas) {
        if (areas == null) return;

        // 構造ツリーを選択状態にする
        for (int i=0; i<areas.size(); i++) {
            IBlock[] blocks = areas.get(i);
            if (i == 0) {
                if (blocks.length > 1) {
                    this.controller.getMainframe().getPanelExplorerView().setSelectedNodeArea(blocks[0], blocks[blocks.length-1]);
                }
                else {
                    this.controller.getMainframe().getPanelExplorerView().setSelectedNodes(blocks);
                }
            }
            else {
                if (blocks.length > 1) {
                    this.controller.getMainframe().getPanelExplorerView().addSelectedNodeArea(blocks[0], blocks[blocks.length-1]);
                }
                else {
                    this.controller.getMainframe().getPanelExplorerView().addSelectedNodes(blocks);
                }
            }
        }

        // ソースビューのハイライトを行う。
        List<CodeLine> listcode = new ArrayList<CodeLine>();
        for (IBlock[] blocks : areas) {
            CodeLine blockcode = null;
            if (blocks != null && blocks.length > 0) {
                CodeLine start = new CodeLine(blocks[0].getStartCodeLine(), blocks[0].getEndCodeLine());
                CodeLine end = start;
                if (blocks[blocks.length-1] != null) {
                    end = new CodeLine(blocks[blocks.length-1].getStartCodeLine(), blocks[blocks.length-1].getEndCodeLine());
                }
                blockcode = new CodeLine(start, end);
                listcode.add(blockcode);
            }

        }

        if (listcode.size() > 0) {
            for (CodeLine code : listcode) {
                // 選択ソースコード行情報からファイルを開く
                try {
                    this.controller.openSourceFile(code);
                } catch (Exception ex) {
                    this.controller.getErrorInfoModel().addErrorInfo(code, ex.getMessage());
                }
            }
            // 選択ファイルのブロックを選択状態にする
            CodeLine[] codes = listcode.toArray(new CodeLine[0]);
            this.controller.getMainframe().getPanelSourceView().setSelectedBlock(codes);
        }

        return;
    }

}
