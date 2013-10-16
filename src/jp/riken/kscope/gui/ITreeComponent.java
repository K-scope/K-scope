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
package jp.riken.kscope.gui;

import java.io.File;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;

import jp.riken.kscope.action.ExploreTreeChangeAction;
import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.IBlock;


/**
 * ツリーパネルコンポーネントインターフェイス.<br/>
 * ツリー構成を持つパネルコンポーネントのインターフェイス
 * @author riken
 */
public interface ITreeComponent {

    /**
     * 選択タブのツリーをすべて収納する。
     */
    public void collapseTreeAll();

    /**
     * 選択タブのツリーをすべて展開する。
     */
    public void expandTreeAll();

    /**
     * 選択タブの選択ツリー配下ノードを展開する。
     */
    public void expandTreeSelect();

    /**
     * 選択ファイルを取得する
     * @return		選択ファイル
     */
    public SourceFile[] getSelectedSourceFiles();

    /**
     * 選択ノードのフォルダ・ファイルを取得する
     * @return		選択フォルダ・ファイル
     */
    public File[] getSelectedNodeFiles();

    /**
     * 選択ソースコード行情報を取得する
     * @return		選択ソースコード行情報
     */
    public CodeLine[] getSelectedCodeLines();

    /**
     * 選択ブロックを取得する
     * @return		選択ブロック
     */
    public IBlock[] getSelectedBlocks();

    /**
     * エクスプローラツリーをエクスポートする
     * @param file		出力ファイル
     */
    public void export(File file);

    /**
     * エクスプローラパネル識別子を設定する
     * @return enumPanel		エクスプローラパネル識別子
     */
    public EXPLORE_PANEL getEnumPanel();

    /**
     * 現在選択されているノードを取得する。
     * @return		選択ノード
     */
    public DefaultMutableTreeNode getSelectedNode();

    /**
     * 現在選択されているノードリストを取得する。
     * @return		選択ノードリスト
     */
    public DefaultMutableTreeNode[] getSelectedNodes();


    /**
     * ツリーの変更リスナの登録を行う。
     * @param action		ツリーの変更リスナ
     */
    public void addTreeSelectionListener(ExploreTreeChangeAction action);

    /**
     * 選択ノードを設定する
     * @param node		選択ノード
     */
    public void setSelectedNode(Object node);

    /**
     * 選択ノードを設定する
     * @param nodes		選択ノード
     */
    public void setSelectedNodes(Object[] nodes);

    /**
     * ツリーモデルを取得する
     * @return		ツリーモデル
     */
    public TreeModel getTreeModel();

    /**
     * ツリーパスからノードを選択する
     * @param path		ツリーパス
     */
    public void setSelectionPath(TreePath path);

    /**
     * ノード範囲を選択する
     * @param startnode		選択開始ノード
     * @param endnode		選択終了ノード
     */
	public void setSelectedNodeArea(Object startnode, Object endnode);

    /**
     * ノード選択範囲を追加する
     * @param startnode		選択開始ノード
     * @param endnode		選択終了ノード
     */
    public void addSelectedNodeArea(Object startnode, Object endnode);

    /**
     * 選択ノードを追加する
     * @param nodes		選択ノード
     */
    public void addSelectedNodes(Object[] nodes);

    /**
     * 選択ノードの変更イベントを発生させる
     */
    public void fireSelectNodeChanged();
}


