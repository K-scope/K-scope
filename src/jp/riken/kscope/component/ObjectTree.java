/*
 * K-scope
 * Copyright 2012-2015 RIKEN, Japan
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
package jp.riken.kscope.component;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.io.File;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;

import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.FILE_TYPE;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.information.InformationBlock;
import jp.riken.kscope.information.InformationBlocks;
import jp.riken.kscope.language.ArrayExpression;
import jp.riken.kscope.language.CompoundBlock;
import jp.riken.kscope.language.Condition;
import jp.riken.kscope.language.ExecutableBody;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProcedureUsage;
import jp.riken.kscope.language.Program;
import jp.riken.kscope.language.Repetition;
import jp.riken.kscope.language.Return;
import jp.riken.kscope.language.Selection;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.utils.ResourceUtils;

/**
 * オブジェクトツリークラス.<br/>
 * ノードオブジェクトに合わせてアイコンを表示する.
 * @author RIKEN
 * @version    2015/03/15     C言語対応によりC言語ファイルのアイコン追加
 */
public class ObjectTree extends JEntireRowTree {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** 付加情報ノード文字色 */
    private Color fontColorInformation;
    /** データベース */
    private Program languageDb;
    /** リンク切れ文字色 */
    private Color fontColorBrokenLink;
    /** イベントブロックフラグ : true=禁止*/
    private boolean eventBlocked;
    /** ノード検索済みProcedureリスト */
    private Map<Procedure, List<Object>> parentLists;

    /**
     * コンストラクタ
     */
    public ObjectTree() {
        super();
        this.eventBlocked = false;
        // ツリーのノード描画クラスの設定
        ObjectTreeCellRenderer renderer = new ObjectTreeCellRenderer();
        this.setCellRenderer( renderer);
    }


    /**
     * オブジェクトツリーノードのレンダリングクラス
     * @author RIKEN
     */
    public class ObjectTreeCellRenderer extends EntireRowCellRenderer {

        /** シリアル番号 */
        private static final long serialVersionUID = 1L;

        /** フォルダアイコン：クローズ */
        private Icon iconFolder;
        /** フォルダアイコン：展開 */
        private Icon iconExpanded;
        /** XMLファイルアイコン */
        private Icon iconXml;
        /** Fortranファイルアイコン */
        private Icon iconFortran;
        /** callブロックアイコン */
        private Icon callIcon;
        /** call nullブロックアイコン */
        private Icon callnullIcon;
        /** mpiブロックアイコン */
        private Icon mpiIcon;
        /** ifブロックアイコン */
        private Icon ifIcon;
        /** ifブロックアイコン */
        private Icon  doIcon;
        /** doブロックアイコン */
        private Icon  returnIcon;
        /** その他ブロックアイコン */
        @SuppressWarnings("unused")
        private Icon otherIcon;
        /** C言語ファイルアイコン */
        private Icon iconClang;
        /** ヘッダファイルアイコン */
        private Icon iconHeader;

        /**
         * コンストラクタ
         */
        public ObjectTreeCellRenderer() {

            /** フォルダアイコン：クローズ */
            iconFolder = ResourceUtils.getIcon("folder.gif");
            /** フォルダアイコン：展開 */
            iconExpanded = ResourceUtils.getIcon("folderexpand.gif");
            /** XMLファイルアイコン */
            iconXml = ResourceUtils.getIcon("xmldoc.gif");
            /** Fortranファイルアイコン */
            iconFortran = ResourceUtils.getIcon("fortran.gif");
            /** C言語ファイルアイコン */
            iconClang = ResourceUtils.getIcon("clang.gif");
            /** ヘッダファイルアイコン */
            iconHeader = ResourceUtils.getIcon("header.gif");

            callIcon = ResourceUtils.getIcon("call.gif");
            callnullIcon = ResourceUtils.getIcon("call_null.gif");
            mpiIcon = ResourceUtils.getIcon("mpi.gif");
            ifIcon = ResourceUtils.getIcon("if.gif");
            doIcon = ResourceUtils.getIcon("do.gif");
            returnIcon = ResourceUtils.getIcon("return.gif");

        }

        /**
         * ツリー内のノードを描画方法を決定するメソッド
         *
         * @param t
         *            ペイントしているツリー
         * @param value
         *            表示される値
         * @param selected
         *            ノードが選択された場合にtrue
         * @param expanded
         *            展開されている場合にtrue
         * @param leaf
         *            要素が葉の場合にtrue
         * @param row
         *            ノードのインデックス
         * @param hasFocus
         *            指定のノードにフォーカスがある場合にtrue
         * @return 指定の値を描画するpaint()メソッドがあるコンポーネント
         * @see javax.swing.tree.TreeCellRenderer
         * @see javax.swing.tree.DefaultTreeCellRenderer
         */
        @Override
        public Component getTreeCellRendererComponent(JTree t, Object value,
                boolean selected, boolean expanded, boolean leaf, int row,
                boolean hasFocus) {

            // ローカル変数
            String str;
            // 表示文字列にキャスト
            str = "" + value;
            // ツリー内のノードを取得
            JComponent c = (JComponent) super.getTreeCellRendererComponent(t, str, selected,
                    expanded, leaf, row, hasFocus);

            // 表示アイコンの設定
            Object obj = ((DefaultMutableTreeNode) value).getUserObject();
            if (obj instanceof ProcedureUsage) {
                if (obj.toString().toLowerCase().startsWith("call mpi_")) {
                    setIcon(mpiIcon);
                } else if (((ProcedureUsage) obj).getCallDefinition() != null){
                    setIcon(callIcon);
                } else {
                    setIcon(callnullIcon);
                }
            } else if (obj instanceof Selection) {
                setIcon(ifIcon);
            } else if (obj instanceof Condition) {
                setIcon(ifIcon);
            } else if (obj instanceof Repetition) {
                setIcon(doIcon);
            } else if (obj instanceof ArrayExpression) {
                setIcon(doIcon);
            } else if (obj instanceof Return) {
                setIcon(returnIcon);
            } else if (obj instanceof SourceFile) {
                SourceFile file = (SourceFile)obj;
                if (FILE_TYPE.isFortranFile(file.getFile())) {
                    this.setIcon(iconFortran);
                }
                else if (FILE_TYPE.isXcodemlFile(file.getFile())) {
                    this.setIcon(iconXml);
                }
                else if (FILE_TYPE.isClangFile(file.getFile())) {
                    this.setIcon(iconClang);
                }
            } else if (obj instanceof File) {
                File file = (File) ((DefaultMutableTreeNode) value).getUserObject();
                // フォルダ
                if (expanded) {
                    this.setIcon(iconExpanded);
                }
                else {
                    this.setIcon(iconFolder);
                }
                if (row > 0) {
                    // ルート以外は、フォルダ名のみ表示する
                    setText(file.getName());
                }
            }
            else if (obj instanceof IBlock) {
                setIcon(null);
                // ローカルにファイルがないヘッダ定義の場合リーフ文字色をグレーにする処理
                IBlock val = (IBlock)obj;
                CodeLine code = val.getStartCodeLine();
                if (code == null) {
                    code = val.getEndCodeLine();
                }
                if (code != null && (code.getSourceFile() == null || (code.getSourceFile() != null && code.getSourceFile().getFile() == null))) {
                    this.setForeground(fontColorBrokenLink);
                }
            }
            else {
                setIcon(null);
            }

            // 付加情報の存在するノードをフォント色の変更
            if (fontColorInformation != null && obj instanceof IInformation) {
                IInformation info = (IInformation)obj;
                if (info.getInformation() != null && !(info.getInformation().getContent().equals(""))) {
                    // ノードフォント色の変更
                    drawInformationColor(c);
                }
                // 複数範囲指定の付加情報に含まれているかチェックする
                if (containsInformationBlocks(info)) {
                    // ノードフォント色の変更
                    drawInformationColor(c);
                }
            }
            if ( c instanceof JLabel ) {
                JLabel label = ( JLabel ) c;
                int width = 60;
                if ( label.getIcon(  ) != null ) {
                    width += label.getIcon(  ).getIconWidth(  );
                }

                if ( label.getText(  ) != null ) {
                    width += SwingUtilities.computeStringWidth( label.getFontMetrics( label.getFont(  ) ), label.getText(  ) );
                }

                Dimension size  = new Dimension( width, (int) label.getPreferredSize().getHeight());
                label.setPreferredSize( size );
                label.revalidate();
                label.repaint();
                label.updateUI();
            }
            return c;
        }

        /**
         * 付加情報が設定されているノードの文字色を変更する
         * @param label		ノードコンポーネント
         */
        private void drawInformationColor(JComponent label) {
            if (fontColorInformation != null) {
                label.setForeground(fontColorInformation);
                label.setOpaque(true);
            }
        }

        /**
         * 複数範囲指定の付加情報に含まれているかチェックする.
         * @param info		チェックノード
         * @return			true=複数範囲指定の付加情報に含まれている
         */
        private boolean containsInformationBlocks(IInformation info) {
            if (languageDb == null) return false;
            if (info == null) return false;
            InformationBlocks infos = languageDb.getInformationBlocks();
            if (infos == null || infos.size() <= 0) return false;
            for (InformationBlock block : infos) {
                if (block.getInformation() == null
                    || block.getInformation().getContent() == null
                    || block.getInformation().getContent().isEmpty()) continue;
                if (block.getStartBlock() == info) {
                    return true;
                }
            }
            return false;
        }
    }



    /**
     * 選択ノードを設定する
     * @param blocks		選択ノードユーザオブジェクト
     */
    public void setSelectedChainNode(Object[] blocks) {
        if (blocks == null || blocks.length <= 0) return;

        TreeModel model = this.getModel();
        DefaultMutableTreeNode root = (DefaultMutableTreeNode)model.getRoot();

        // ツリーノードを順方向で列挙
        int count = 0;
        int maxMatch = 0;
        DefaultMutableTreeNode matchNode = null;
        Enumeration<?> depth = root.preorderEnumeration();
        while(depth.hasMoreElements()) {
            DefaultMutableTreeNode treeNode = (DefaultMutableTreeNode)depth.nextElement();

            // ノード検索を行う
            if (treeNode == null || treeNode.getUserObject() == null) {
                continue;
            }

            TreeNode[] objs = treeNode.getPath();
            if (objs == null || objs.length <= 0) continue;
            DefaultMutableTreeNode[] paths = new DefaultMutableTreeNode[objs.length];
            for (int i=0; i<objs.length; i++) {
                paths[i] = (DefaultMutableTreeNode) objs[i];
            }

            int match = matchChainNode(blocks, paths);
            if (maxMatch < match) {
                matchNode = treeNode;
                maxMatch = match;
            }
            count++;
        }

        if (matchNode != null) {
            TreePath path = new TreePath(matchNode.getPath());
            this.setSelectionPath(path);
            this.scrollPathToVisibleForVertical(path);
        }
        return;

    }

    /**
     * ブロックリストとパスノードリストの一致ノード数を取得する
     * @param blocks		ブロックリスト
     * @param paths			パスノードリスト
     * @return		一致個数
     */
    private int matchChainNode(Object[] blocks, DefaultMutableTreeNode[] paths) {
        if (blocks == null || blocks.length <= 0) return -1;
        if (paths == null || paths.length <= 0) return -1;
        //if (blocks.length > paths.length) return -1;

        int blockidx = 0;
        int pathidx = 0;
        while (blockidx < blocks.length && pathidx < paths.length) {
            if (blocks[blockidx] == paths[pathidx].getUserObject()) {
                blockidx++;
                pathidx++;
            }
            else {
                pathidx++;
            }
        }

        return blockidx+1;
    }

    /**
     * パスノードを選択状態にする
     * @param path		パス
     */
    @Override
    public void setSelectionPath(TreePath path) {
        if (!(path.getLastPathComponent() instanceof FilterTreeNode)) {
            super.setSelectionPath(path);
            return;
        }
        if (this.isVisible(path)) {
            super.setSelectionPath(path);
            return;
        }

        Object[] objs = path.getPath();
        List<Object> list = new ArrayList<Object>();
        //for (int i=objs.length-1; i>=0; i--) {
        for (int i=0; i<objs.length; i++) {
            if (!(objs[i] instanceof FilterTreeNode)) continue;
            FilterTreeNode node = (FilterTreeNode)objs[i];
            list.add(node);
            if (node.isPassed()) {
                //TreePath select = SwingUtils.getTreePath(node);
                TreePath select = new TreePath(list.toArray());
                super.setSelectionPath(select);
                //return;
            }
        }
    }

    /**
     * 選択ノードを展開する.
     * 構造ツリーは未展開なノードがあるので事前に展開する.
     * @param path		選択展開パス
     */
    public void expandSelectionPath(TreePath path) {
        Object[] objs = path.getPath();
        List<FilterTreeNode> list = new ArrayList<FilterTreeNode>();
        for (int i=0; i<objs.length; i++) {
            if (!(objs[i] instanceof DefaultMutableTreeNode)) continue;
            FilterTreeNode node = new FilterTreeNode(
                        ((DefaultMutableTreeNode)objs[i]).getUserObject());
            list.add(node);
            // TreePath select = SwingUtils.getTreePath(node);
            TreePath select = new TreePath(list.toArray());
            super.expandPath(select);
        }
    }

    /**
     * ソースビュープロパティを設定する
     * @param properties		ソースビュープロパティ
     */
    public void setSourceProperties(SourceProperties properties) {
        // ツリー選択ノード背景色
        if (properties.getBackgoundSelectNodeColor() != null) {
            this.setSelectionBackground(properties.getBackgoundSelectNodeColor());
        }
        // 付加情報ノードフォント色
        if (properties.getInformationNodeFontColor() != null) {
            this.setInformationNodeFontColor(properties.getInformationNodeFontColor());
        }

        // リンク切れ文字色
        if (properties.getBrokenLinkNodeFontColor() != null) {
            this.setBrokenLinkNodeFontColor(properties.getBrokenLinkNodeFontColor());
        }

        this.repaint();
    }


    /**
     * 付加情報ノードフォント色を設定する.<br>
     * @param color		付加情報ノードフォント色
     */
    public void setInformationNodeFontColor(Color color) {
        fontColorInformation = color;
    }

    /**
     * 選択ノードを展開する.
     * 構造ツリーは未展開なノードがあるので事前に展開する.
     * @param selectnode		選択ノードユーザオブジェクト
     */
    public void expandObjectPath(Object selectnode) {
        if (selectnode == null) return;

        // 選択ノードの階層を取得する
        clearParentLists();
        List<Object> parents = getLanguagePath(selectnode);
        if (parents == null || parents.size() <= 0) return;

        // ルートから展開を行う
        TreeModel model = this.getModel();
        DefaultMutableTreeNode node = (DefaultMutableTreeNode)model.getRoot();
        for (int i=parents.size()-1; i>=0; i--) {
            DefaultMutableTreeNode expandnode =  expandObjectPath(parents.get(i), node);
            if (expandnode != null) {
                node = expandnode;
            }
        }
    }

    /**
     * 選択ノードを設定する
     * @param selectnode		選択ノードユーザオブジェクト
     */
    public void setSelectedNode(Object selectnode) {
        if (selectnode == null) return;

        // 選択ノードの階層を取得する
        clearParentLists();
        List<Object> parents = getLanguagePath(selectnode);

        TreeModel model = this.getModel();
        DefaultMutableTreeNode root = (DefaultMutableTreeNode)model.getRoot();

        // ツリーノードを順方向で列挙
        TreePath lastpath = null;
        this.eventBlocked = true;
        Enumeration<?> depth = root.preorderEnumeration();
        NODE_LOOP:while (depth.hasMoreElements()) {
            DefaultMutableTreeNode treeNode = (DefaultMutableTreeNode)depth.nextElement();
            // ノード検索を行う
            if (treeNode == null || treeNode.getUserObject() == null) {
                continue;
            }
            if (treeNode.getUserObject().equals(selectnode)) {
                if (treeNode instanceof FilterTreeNode) {
                    if (!((FilterTreeNode)treeNode).isPassed()) {
                        break;
                    }
                }
                TreePath path = new TreePath(treeNode.getPath());
                this.setSelectionPath(path);
                this.scrollPathToVisibleForVertical(path);
                lastpath = path;
                break;
            }
            if (parents != null) {
                for (Object obj : parents) {
                    if (treeNode.getUserObject().equals(obj)) {
                        if (lastpath != null) {
                            Object lastObj = ((DefaultMutableTreeNode)lastpath.getLastPathComponent()).getUserObject();
                            if (lastObj == obj) {
                                break NODE_LOOP;
                            }
                        }
                        TreePath path = new TreePath(treeNode.getPath());
                        this.setSelectionPath(path);
                        this.scrollPathToVisibleForVertical(path);
                        lastpath = path;
                        break;
                    }
                }
            }
        }
        this.eventBlocked = false;
        if (lastpath != null) {
            // ツリーノード変更イベントを発生させる.
            super.setSelectionPath(null);
            super.setSelectionPath(lastpath);
            super.scrollPathToVisibleForVertical(lastpath);
        }
        return;
    }

    /**
     * データベース構造階層を取得する.
     * 階層リストは子から親のリストである.
     * @param block		データベース構成ブロック
     * @return			データベース構造階層リスト
     */
    private List<Object> getLanguagePath(Object block) {
        if (block == null) return null;

        // 選択ノードの階層を取得する
        List<Object> parents = new ArrayList<Object>();
        Object child = block;
        while (child != null) {
            if (child instanceof ExecutableBody) {
                parents.add(child);
                child = ((ExecutableBody)child).getParent();
                if (child instanceof Procedure) {
                    if (((Procedure) child).isProgram()) {
                        parents.add(child);
                        break;
                    }
                }
            }
            else if (child instanceof Procedure) {
                if (((Procedure) child).isProgram()) {
                    break;
                }
                // 検索済みProcedure
                List<Object> searched = getParentLists((Procedure)child);
                if (searched != null) {
                    parents.add(child);
                    parents.addAll(searched);
                    child = searched.get(searched.size()-1);
                    continue;
                }
                if (parents.size() > 0) {
                    if (parents.get(parents.size()-1) != child) {
                        parents.add(child);
                    }
                    else {
                        break;
                    }
                }
                Set<ProcedureUsage> calls = ((Procedure)child).getCallMember();
                if (calls != null && calls.size() > 0) {
                    ProcedureUsage[] array = calls.toArray(new ProcedureUsage[0]);
                    List<Object> listMax = null;
                    List<Object> listProg = null;
                    for (ProcedureUsage useage : array) {
                        // 循環参照となっていないかチェックする.
                        if (isRecursiveBlock(parents, useage)) {
                            return null;
                        }
                        List<Object> listPath = getLanguagePath(useage);
                        if (listPath == null) continue;
                        if (listMax == null) listMax = listPath;
                        else if (listMax.size() < listPath.size()) listMax = listPath;
                        if (listPath.get(listPath.size()-1) instanceof Procedure) {
                            Object last = listPath.get(listPath.size()-1);
                            if (((Procedure) last).isProgram()) {
                                if (listProg == null) listProg = listPath;
                                else if (listProg.size() > listPath.size()) listProg = listPath;
                            }
                        }
                    }
                    if (listProg != null ) {
                        Object last = listProg.get(listProg.size()-1);
                        if (last instanceof Procedure) {
                            if (((Procedure) last).isProgram()) {
                                addParentLists((Procedure)child, listMax);
                                child = listProg.get(listProg.size()-1);
                                parents.addAll(listProg);
                                return parents;
                            }
                        }
                    }
                    if (listMax != null ) {
                        addParentLists((Procedure)child, listMax);
                        child = listMax.get(listMax.size()-1);
                        parents.addAll(listMax);
                    }
                    else {
                        child = null;
                    }
                }
                else {
                    child = null;
                }
            }
            else if (child instanceof IBlock) {
                parents.add(child);
                child = ((IBlock)child).getMotherBlock();
            }
            else {
                child = null;
            }
        }
        if (parents.size() <= 0) return null;

        return parents;
    }

    /**
     * ノード範囲を選択する
     * @param startnode		選択開始ノード
     * @param endnode		選択終了ノード
     */
    @Override
    public void setSelectedNodeArea(Object startnode, Object endnode) {
        expandObjectPath(startnode);
        setSelectedNode(startnode);
        expandObjectPath(endnode);
        setSelectedNode(endnode);

        super.setSelectedNodeArea(startnode, endnode);

        return;
    }

    /**
     * データベースを設定する.
     * @param language データベース
     */
    public void setLanguageDb(Program language) {
        this.languageDb = language;
    }


    /**
     * 選択ノードを設定する
     * @param selectnodes		選択ノードユーザオブジェクト
     */
    public void setSelectedNodes(Object[] selectnodes) {
        if (selectnodes == null) return;
        for (Object obj : selectnodes) {
            expandObjectPath(obj);
            setSelectedNode(obj);
        }
        super.setSelectedNodes(selectnodes, false);
        return;
    }

    /**
     * リンク切れ文字色を設定する.<br>
     * @param color		リンク切れ文字色
     */
    public void setBrokenLinkNodeFontColor(Color color) {
        fontColorBrokenLink = color;
    }

    @Override
    protected void fireValueChanged(TreeSelectionEvent e) {
        if (!this.eventBlocked) {
            super.fireValueChanged(e);
        }
    }


    /**
     * 循環参照となっていないかチェックする.
     * @param list		ブロックリスト
     * @param block			呼出ブロック
     * @return				true=循環参照
     */
    private boolean isRecursiveBlock(List<Object> list, Object block) {
        if (list == null) return false;
        if (block == null) return false;
        for (Object obj : list) {
            if (obj == block) {
                return true;
            }
        }
        return false;
    }

    /**
     * ノード検索済みProcedureリストに追加する.
     * @param proc		ノード検索Procedure
     * @param objs		検索結果親リスト
     */
    private void addParentLists(Procedure proc, List<Object> objs) {
        if (this.parentLists == null) {
            this.parentLists = new java.util.HashMap<Procedure, List<Object>>();
        }
        this.parentLists.put(proc, objs);
    }

    /**
     * ノード検索済みProcedureリストをクリアする
     */
    private void clearParentLists() {
        this.parentLists = null;
        this.parentLists = new java.util.HashMap<Procedure, List<Object>>();
    }

    /**
     * ノード検索済みProcedureリストから検索結果親リストを取得する.
     * @param proc		ノード検索Procedure
     * @return   検索結果親リスト
     */
    private List<Object> getParentLists(Procedure proc) {
        if (this.parentLists == null) return null;
        return this.parentLists.get(proc);
    }

    /**
     * 選択ノードの変更イベントを発生させる
     */
    public void fireSelectNodeChanged() {
        TreePath[] paths = this.getSelectionPaths();
        super.setSelectionPaths(null);
        super.setSelectionPaths(paths);
    }
}


