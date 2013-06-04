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
package jp.riken.kscope.component;

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.List;

import javax.swing.JComponent;
import javax.swing.JTree;
import javax.swing.JViewport;
import javax.swing.event.TreeExpansionEvent;
import javax.swing.event.TreeWillExpandListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.ExpandVetoException;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;

import jp.riken.kscope.language.Block;
import jp.riken.kscope.language.Procedure;
import jp.riken.kscope.language.ProcedureUsage;
import jp.riken.kscope.language.Selection;
import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.service.LanguageService;
import jp.riken.kscope.utils.SwingUtils;

/**
 * 行ハイライトツリーコンポーネント
 * @author riken
 */
public class JEntireRowTree extends javax.swing.JTree implements TreeWillExpandListener {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** ツリー選択背景色(=cornflowerblue) */
    private static Color SELECTION_BACKGROUND = new Color(0x66, 0x99, 0xFF);
    /** ツリー選択文字色 */
    private static Color SELECTION_FORECOLOR = new Color(255, 255, 255);

    /** 現在選択ノード */
    private List<TreePath> currentSelected;
    /** 現在展開ノード */
    private List<TreePath> currentExpanded;

    /**
     * ツリーのLook & Feelクラス.<br/>
     * ツリーの行選択によりノード選択とする。
     */
    private EntireRowTreeUI treeUI;

    /**
     * コンストラクタ
     */
    public JEntireRowTree() {

        // ツリーのLook & Feel生成
        treeUI = new EntireRowTreeUI();
        this.setUI(treeUI);

        // 透明設定
        this.setOpaque(false);

        // ツリーのノード描画クラスの設定
        EntireRowCellRenderer renderer = new EntireRowCellRenderer();
        this.setCellRenderer( renderer);

        this.addTreeWillExpandListener(this);
    }

    /**
     * ドキュメントの更新時にツリーのLook & Feelを設定する.<br/>
     * コンストラクタで設定したTreeUIが解除されるので、毎回設定する。
     */
    @Override
    public void updateUI() {
        super.updateUI();
        this.setUI(treeUI);
    }

    /**
     * コンポーネントの描画を行う
     * @param g		グラフィックス
     */
    @Override
    protected void paintComponent(Graphics g) {

        try {
            // 背景色を描画する
            g.setColor(getBackground());
            g.fillRect(0,0,getWidth(),getHeight());

            if (getSelectionCount()>0) {
                int count = getSelectionCount();
                int[] list = getSelectionRows();
                if (count > 0 && list != null) {
                    // 選択行全体を塗りつぶす。
                    for(int i: list) {
                        Rectangle r = getRowBounds(i);
                        g.setColor(SELECTION_BACKGROUND);
                        g.fillRect(0, r.y, getWidth(), r.height);
                    }
                }
            }
            super.paintComponent(g);

            // ボーダを描画する
            if (getLeadSelectionPath()!=null) {
                Rectangle r = getRowBounds(getRowForPath(getLeadSelectionPath()));
                if (r != null) {
                    g.setColor(SELECTION_BACKGROUND.darker());
                    g.drawRect(0, r.y, getWidth()-1, r.height-1);
                }
            }
        }
        catch (Exception ex) {
//            ex.printStackTrace();
        }
    }

    /**
     * 選択行の背景色を設定する.<br>
     * デフォルト=cornflowerblue
     * @param color			背景色
     */
    public void setSelectionBackground(Color color) {
        JEntireRowTree.SELECTION_BACKGROUND = color;
    }

    /**
     * 選択行の文字色を設定する.<br>
     * デフォルト=white
     * @param color		文字色
     */
    public void setSelectionForeColor(Color color) {
        JEntireRowTree.SELECTION_FORECOLOR = color;
    }

    /**
     * ツリーのLook & Feelクラス.<br/>
     * ツリーの行選択によりノード選択とする。
     * @author riken
     *
     */
    private class EntireRowTreeUI extends javax.swing.plaf.basic.BasicTreeUI {

        /**
         * ノードのボーダを取得する.<br/>
         * ノードのボーダをノード幅からツリー幅に変更する。
         * @param tree		ツリー
         * @param path		ノードパス
         * @return			ノードのボーダ
         */
        @Override
        public Rectangle getPathBounds(JTree tree, TreePath path) {
            if(tree != null && treeState != null) {
                return getPathBounds(path, tree.getInsets(), new Rectangle());
            }
            return null;
        }

        /**
         * ノードのボーダを取得する.<br/>
         * ノードのボーダをノード幅からツリー幅に変更する。
         * @param path			ノードパス
         * @param insets		余白
         * @param bounds		ボーダ
         * @return			ノードボーダ
         */
        private Rectangle getPathBounds(TreePath path, Insets insets, Rectangle bounds) {
            bounds = treeState.getBounds(path, bounds);
            if(bounds != null) {
                bounds.x = insets.left;
                bounds.width = tree.getWidth();
                bounds.y += insets.top;
            }
            return bounds;
        }

        /**
         * 垂直線をペイントします.<br/>
         * Macの場合は、垂直線をペイントしない。
         * @param g			Graphics
         * @param c			ツリーコンポーネント
         * @param x			X位置
         * @param top		TOP位置
         * @param bottom	BOTTOM位置
         */
        @Override
        protected void paintVerticalLine(Graphics g, JComponent c, int x,
                int top, int bottom) {
            if (!KscopeProperties.isMac()) {
                super.paintVerticalLine(g, c, x, top, bottom);
            }
        }

        /**
         * 水平線をペイントします.<br/>
         * Macの場合は、水平線をペイントしない。
         * @param g			Graphics
         * @param c			ツリーコンポーネント
         * @param y			Y位置
         * @param left		LEFT位置
         * @param right		RIGHT位置
         */
        @Override
        protected void paintHorizontalLine(Graphics g, JComponent c, int y,
                int left, int right) {
            if (!KscopeProperties.isMac()) {
                super.paintHorizontalLine(g, c, y, left, right);
            }
        }
    }

    /**
     * ツリーのノード描画クラス
     * @author riken
     */
    public class EntireRowCellRenderer extends DefaultTreeCellRenderer {
        /** シリアル番号 */
        private static final long serialVersionUID = 1L;

        /**
         * ノードコンポーネントを取得する.<br/>
         * ノードコンポーネントの背景色、文字色を描画する
         * @param tree			ツリー
         * @param value			ノード値
         * @param selected		選択状態
         * @param expanded		展開状態
         * @param leaf			葉状態
         * @param row			行番号
         * @param hasFocus		フォーカス
         * @return				ノードコンポーネント
         */
        @Override
        public Component getTreeCellRendererComponent(JTree tree, Object value, boolean selected, boolean expanded, boolean leaf, int row, boolean hasFocus) {
            Component comp = super.getTreeCellRendererComponent(
                                    tree, value, selected,
                                    expanded, leaf, row, hasFocus);
            if (comp instanceof JComponent) {
                JComponent label = (JComponent)comp;
                label.setBackground(selected?SELECTION_BACKGROUND:tree.getBackground());
                label.setForeground(selected?SELECTION_FORECOLOR:tree.getForeground());
                label.setOpaque(true);
            }

            return comp;
        }
    }


    /**
     * 選択ノードを設定する
     * @param selectnodes		選択ノードユーザオブジェクト
     */
    public void addSelectedNodes(Object[] selectnodes) {
        setSelectedNodes(selectnodes, true);
        return;
    }

    /**
     * 選択ノードを設定する
     * @param selectnodes		選択ノードユーザオブジェクト
     * @param addflag		選択範囲の追加フラグ (true=選択範囲の追加)
     */
    protected void setSelectedNodes(Object[] selectnodes, boolean addflag) {
        if (selectnodes == null) return;

        TreeModel model = this.getModel();
        DefaultMutableTreeNode root = (DefaultMutableTreeNode)model.getRoot();

        List<TreePath> selectpaths = new ArrayList<TreePath>();
        // ツリーノードを順方向で列挙
        Enumeration<?> depth = root.preorderEnumeration();
        while(depth.hasMoreElements()) {
            DefaultMutableTreeNode treeNode = (DefaultMutableTreeNode)depth.nextElement();

            // ノード検索を行う
            if (treeNode == null || treeNode.getUserObject() == null) {
                continue;
            }
            for (int i=0; i<selectnodes.length; i++) {
                if (selectnodes[i] == null) continue;
                if (treeNode.getUserObject().equals(selectnodes[i])) {
                    TreePath path = new TreePath(treeNode.getPath());
                    // ノードが非表示の場合は、親ノードを追加する
                    if (treeNode instanceof FilterTreeNode) {
                        if (!((FilterTreeNode)treeNode).isPassed()) {
                            path = path.getParentPath();
                        }
                    }
                    selectpaths.add(path);
                    break;
                }
            }
            if (selectpaths.size() >= selectnodes.length) {
                break;
            }
        }
        if (selectpaths.size() > 0) {
            if (addflag) {
                this.addSelectionPaths(selectpaths.toArray(new TreePath[0]));
            }
            else {
                this.setSelectionPath(selectpaths.get(0));
                this.scrollPathToVisibleForVertical(selectpaths.get(0));
                this.setSelectionPaths(selectpaths.toArray(new TreePath[0]));
            }
        }

        return;
    }


    /**
     * ノード範囲を選択する
     * @param startnode		選択開始ノード
     * @param endnode		選択終了ノード
     */
    public void setSelectedNodeArea(Object startnode, Object endnode) {
        setSelectedNodeArea(startnode, endnode, false);
        return;
    }


    /**
     * ノード範囲を選択する
     * @param startnode		選択開始ノード
     * @param endnode		選択終了ノード
     */
    public void addSelectedNodeArea(Object startnode, Object endnode) {
        setSelectedNodeArea(startnode, endnode, true);
        return;
    }


    /**
     * ノード範囲を選択する
     * @param startnode		選択開始ノード
     * @param endnode		選択終了ノード
     * @param addflag		選択範囲の追加フラグ (true=選択範囲の追加)
     */
    private void setSelectedNodeArea(Object startnode, Object endnode, boolean addflag) {
        if (startnode == null && endnode == null) return;

        // 最初に選択開始ノード,選択終了ノードを別々に選択状態にする。
        if (addflag) {
            addSelectedNodes(new Object[]{startnode, endnode});
        }
        else {
            setSelectedNodes(new Object[]{startnode, endnode}, false);
        }
        if (startnode == null || endnode == null) return;

        TreeModel model = this.getModel();
        DefaultMutableTreeNode root = (DefaultMutableTreeNode)model.getRoot();

        List<TreePath> selectpaths = null;
        // ツリーノードを順方向で列挙
        Enumeration<?> depth = root.preorderEnumeration();
        while(depth.hasMoreElements()) {
            DefaultMutableTreeNode treeNode = (DefaultMutableTreeNode)depth.nextElement();

            // ノード検索を行う
            if (treeNode == null || treeNode.getUserObject() == null) {
                continue;
            }
            if (selectpaths == null) {
                if (treeNode.getUserObject().equals(startnode)) {
                    TreePath path = new TreePath(treeNode.getPath());
                    selectpaths = new ArrayList<TreePath>();
                    selectpaths.add(path);
                    if (startnode == endnode) {
                    	break;
                    }
                }
            }
            else {
                TreePath path = new TreePath(treeNode.getPath());
                if (this.isVisible(path)) {
                    selectpaths.add(path);
                }
                if (treeNode.getUserObject().equals(endnode)) {
                    break;
                }
            }
        }
        if (selectpaths != null && selectpaths.size() > 0) {
            if (addflag) {
                this.addSelectionPaths(selectpaths.toArray(new TreePath[0]));
            }
            else {
                this.setSelectionPath(selectpaths.get(0));
                this.scrollPathToVisibleForVertical(selectpaths.get(0));
                this.setSelectionPaths(selectpaths.toArray(new TreePath[0]));
            }
        }

        return;
    }

    /**
     * パスによって指定されるノードが表示されるようにスクロールします.<br/>
     * 水平方向のスクロールは行わなわず、垂直方向のスクロールのみ行う。
     * @param path		表示パス
     */
    public void scrollPathToVisibleForVertical(TreePath path) {

        Container cont = this.getParent();
        JViewport view = null;
        Point orgin = null;
        if (cont instanceof JViewport) {
            view = (JViewport)cont;
            // スクロール前のビュー位置
            orgin = view.getViewPosition();
        }

        // 選択パスを表示位置にスクロールする
        this.scrollPathToVisible(path);

        // 水平方向のスクロールは行わない
        if (view != null && orgin != null) {
            Point dest = view.getViewPosition();
            // 水平方向のスクロール位置を元に戻す。
            dest.x = orgin.x;
            view.setViewPosition(dest);
        }
    }

    /**
     * ツリーパスを取得する.<br/>
     * ノードが別オブジェクトノードであってもUserObjectが同一であれば同一パスとして検索する
     * @param path		ツリーパス
     * @return 			実ツリーパス
     */
    public TreePath getRealTreePath(TreePath path) {
        if (path == null) return null;
        DefaultMutableTreeNode root = (DefaultMutableTreeNode)this.getModel().getRoot();
        Object[] objs = path.getPath();
        if (objs == null || objs.length <= 0) return null;
        DefaultMutableTreeNode[] pathNodes = new DefaultMutableTreeNode[objs.length];
        for (int i=0; i<objs.length; i++) {
            pathNodes[i] = (DefaultMutableTreeNode) objs[i];
        }

        TreePath searchPath = searchTreePath(root, pathNodes);

        return searchPath;
    }

    /**
     * ツリーパスを検索する.<br/>
     * ノードが別オブジェクトノードであってもUserObjectが同一であれば同一パスとして検索する
     * @param parent			現在ノード
     * @param pathNodes			検索ツリーノードリスト
     * @return					ツリーパス
     */
    private TreePath searchTreePath(DefaultMutableTreeNode parent, DefaultMutableTreeNode[] pathNodes) {

        TreePath parentPath = getNodePath(parent, pathNodes);
        if (parentPath == null) return null;
        if (parentPath.getPathCount() == pathNodes.length) return parentPath;

        for (int i=0; i<parent.getChildCount(); i++) {
            DefaultMutableTreeNode child = (DefaultMutableTreeNode)parent.getChildAt(i);
            Object[] objs = child.getPath();
            if (objs == null || objs.length <= 0) return null;
            DefaultMutableTreeNode[] nodes = new DefaultMutableTreeNode[objs.length];
            for (int j=0; j<objs.length; j++) {
                nodes[j] = (DefaultMutableTreeNode) objs[j];
            }

            TreePath childPath = searchTreePath(child, pathNodes);
            if (childPath != null) {
                if (childPath.getPathCount() == pathNodes.length) return childPath;
            }
        }

        return null;
    }

    /**
     * ツリーパスを検索する.<br/>
     * ノードが別オブジェクトノードであってもUserObjectが同一であれば同一パスとして検索する
     * @param node			現在ノード
     * @param pathNodes			検索ツリーノードリスト
     * @return					ツリーパス
     */
    private TreePath getNodePath(DefaultMutableTreeNode node, DefaultMutableTreeNode[] pathNodes) {

        Object[] objs = node.getPath();
        if (objs == null || objs.length <= 0) return null;
        DefaultMutableTreeNode[] nodes = new DefaultMutableTreeNode[objs.length];
        for (int i=0; i<objs.length; i++) {
            nodes[i] = (DefaultMutableTreeNode) objs[i];
        }
        if (nodes.length > pathNodes.length) return null;

        for (int i=0; i<nodes.length; i++) {
            if (nodes[i].getUserObject() != pathNodes[i].getUserObject()) {
                return null;
            }
        }

        return SwingUtils.getTreePath(node);
    }


    /**
     * 現在のノード状態の待避を行う
     */
    public void storeTreeNode() {

        // 現在選択されているノードの取得
        TreePath[] selectedPath = this.getSelectionPaths();
        this.currentSelected = new ArrayList<TreePath>();
        if (selectedPath != null) {
            this.currentSelected.addAll(Arrays.asList(selectedPath));
        }
        // 現在の展開ノードの取得
        this.currentExpanded = new ArrayList<TreePath>();
        TreeModel model = this.getModel();
        if (model == null) return;

        DefaultMutableTreeNode root = (DefaultMutableTreeNode)model.getRoot();

        // ツリーノードを順方向で列挙
        Enumeration<?> depth = root.preorderEnumeration();
        while(depth.hasMoreElements()) {
            DefaultMutableTreeNode next = (DefaultMutableTreeNode)depth.nextElement();
            TreePath path = new TreePath(next.getPath());
            if (this.isExpanded(path)) {
                Object obj = next.getUserObject();
                this.currentExpanded.add(path);
            }
        }
        return;
    }

    /**
     * ノード状態の復元を行う
     */
    public void restoreTreeNode() {

        TreeModel model = this.getModel();
        if (model == null) return;

        DefaultMutableTreeNode root = (DefaultMutableTreeNode)model.getRoot();

        // ツリーノードを順方向で列挙
        Enumeration<?> depth = root.preorderEnumeration();
        while(depth.hasMoreElements()) {
            DefaultMutableTreeNode next = (DefaultMutableTreeNode)depth.nextElement();
            TreePath path = new TreePath(next.getPath());
            Object obj = next.getUserObject();

            if (this.currentSelected != null) {
                for (TreePath selected : currentSelected) {
                    if (path.equals(selected)) {
                        this.setSelectionPath(path);
                        this.scrollPathToVisible(path);
                    }
                }
            }

            if (this.currentExpanded != null) {
                for (TreePath expanded : currentExpanded) {
                    if (path.equals(expanded)) {
                        this.expandPath(path);
                    }
                }
            }
        }
        return;
    }


    /**
     * 最後のツリーノードを取得する.
     * @return  最後のツリーノード
     */
    public DefaultMutableTreeNode getLastTreeNode() {

        TreeModel model = this.getModel();
        DefaultMutableTreeNode root = (DefaultMutableTreeNode)model.getRoot();
        DefaultMutableTreeNode lastnode = null;

        // ツリーノードを順方向で列挙
        Enumeration<?> depth = root.preorderEnumeration();
        while(depth.hasMoreElements()) {
            lastnode = (DefaultMutableTreeNode)depth.nextElement();
        }
        return lastnode;
    }

    /**
     * ツリーパスが表示されているかチェックする.<br/>
     * フィルタノードで表示対象外であれば非表示とする
     * @param path		ツリーパス
     * @return			true=表示
     */
    @Override
    public boolean isVisible(TreePath path) {
        if (path == null) return false;
        Object obj = path.getLastPathComponent();
        if (obj == null) return false;
        if (obj instanceof FilterTreeNode) {
            FilterTreeNode node = (FilterTreeNode)obj;
            if (!node.isPassed()) {
                return false;
            }
        }
        return super.isVisible(path);
    }

    /**
     * ツリーを開く際に呼ばれるイベント
     */
    @Override
    public void treeWillExpand(TreeExpansionEvent event) throws ExpandVetoException {
        TreePath path = event.getPath();
        Object obj = path.getLastPathComponent(); //一番末端の(今回開こうとしている)ノード
        if (obj instanceof FilterTreeNode) {
            int depth = ((FilterTreeNode)obj).getDepth();
            if (depth > 0) {
                // ((FilterTreeNode)obj).removeAllChildren();
                // System.out.println("expand=" + obj + ", depth=" + depth);
                LanguageService service = new LanguageService();
                int writedepth = 0;
                if (((FilterTreeNode)obj).getUserObject() instanceof ProcedureUsage) {
                    ProcedureUsage call = (ProcedureUsage)((FilterTreeNode)obj).getUserObject();
                    service.writeProcedureUsage(call, ((FilterTreeNode)obj), true, writedepth);
                }
                else if (((FilterTreeNode)obj).getUserObject() instanceof Selection) {
                    Selection select = (Selection)((FilterTreeNode)obj).getUserObject();
                    service.writeSelection(select, ((FilterTreeNode)obj), true, writedepth);
                }
                else if (((FilterTreeNode)obj).getUserObject() instanceof Block) {
                    Block block = (Block)((FilterTreeNode)obj).getUserObject();
                    service.writeBlocks(block, ((FilterTreeNode)obj), true, writedepth);
                }
                else if (((FilterTreeNode)obj).getUserObject() instanceof Procedure) {
                    Procedure proc = (Procedure)((FilterTreeNode)obj).getUserObject();
                    service.writeProcedure(proc, ((FilterTreeNode)obj), true, writedepth);
                }
                ((FilterTreeNode)obj).setDepth(writedepth);
            }
        }

    }

    /**
     * ツリーを閉じる際に呼ばれるイベント
     */
    @Override
    public void treeWillCollapse(TreeExpansionEvent event) throws ExpandVetoException { }


    /**
     * 選択ノードを展開する.
     * 構造ツリーは未展開なノードがあるので事前に展開する.
     * @param selectnode		選択ノードユーザオブジェクト
     * @param parent			展開親ノード
     * @return             展開ノード
     */
    public DefaultMutableTreeNode expandObjectPath(Object selectnode, DefaultMutableTreeNode parent) {
    	if (selectnode == null) return null;
    	if (parent == null) return null;

        if (parent.getUserObject().equals(selectnode)) {
        	return parent;
        }
        DefaultMutableTreeNode parentnode = parent;
        if (parent.getUserObject() instanceof Selection) {
        	parentnode = (DefaultMutableTreeNode)parent.getParent();
        }
        TreePath parentpath = new TreePath(parentnode.getPath());
        // this.expandPath(parentpath);

        // ツリーノードを順方向で列挙
        Enumeration<?> depth = parentnode.preorderEnumeration();
        while(depth.hasMoreElements()) {
            DefaultMutableTreeNode treeNode = (DefaultMutableTreeNode)depth.nextElement();

            // ノード検索を行う
            if (treeNode == null || treeNode.getUserObject() == null) {
                continue;
            }
            if (treeNode.getUserObject().equals(selectnode)) {
            	this.expandPath(parentpath);
                TreePath path = new TreePath(treeNode.getPath());
                this.expandPath(path);
                return treeNode;
            }
        }

        // ノードが存在しなかったので閉じる
        //this.collapsePath(parentpath);

        return null;
    }
}


