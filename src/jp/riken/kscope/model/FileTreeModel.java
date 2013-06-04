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

package jp.riken.kscope.model;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Observable;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreePath;

import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.utils.FileUtils;
import jp.riken.kscope.utils.SwingUtils;


/**
 * ファイルツリーモデルクラス
 * @author riken
 *
 */
public class FileTreeModel extends Observable {

    /** ファイルツリーモデル */
    private DefaultTreeModel treeModel;

    /** プロジェクトフォルダ */
    private File projectFolder = null;

    /** 初期ツリータイトル */
    private String initTreeTitle;

    /**
     * ソースファイルを設定する。
     * @param sourceFiles          ソースファイルリスト
     */
    public void setSourceFile(SourceFile[] sourceFiles) {
        if (sourceFiles == null) {
            clearTreeModel();
            return;
        }
        // ソースファイルリストのソート
        java.util.Arrays.sort(sourceFiles,
                new java.util.Comparator<SourceFile>() {
                    public int compare(SourceFile o1, SourceFile o2) {
                    	if (o1 == null) return -1;
                    	if (o2 == null) return 1;
                        SourceFile src1 = (SourceFile) o1;
                        SourceFile src2 = (SourceFile) o2;
                        return src1.getPath().compareTo(src2.getPath());
                    }
                });

        DefaultMutableTreeNode root = null;
        File topFile = null;
        if (projectFolder != null) {
            root = new DefaultMutableTreeNode(projectFolder);
            topFile = projectFolder;
            // ルートフォルダ配下のファイルであるかチェックする。
            for (int i = 0; i < sourceFiles.length; i++) {
                File srcFile = null;
                if (sourceFiles[i] == null || sourceFiles[i].getFile() == null) {
                	continue;
                }
                if (sourceFiles[i].getFile().isAbsolute()) {
                    srcFile = sourceFiles[i].getFile();
                }
                else {
                    srcFile = new File(projectFolder.getAbsolutePath() + File.separator + sourceFiles[i].getFile().getPath());
                }
                if (!srcFile.exists()) continue;
                if (!FileUtils.isChildsFile(srcFile, projectFolder)) {
                    // ルートフォルダの子パスではないので、ルートフォルダを最上位のフォルダとする
                    root = null;
                    break;
                }
            }
        }

        if (root == null) {
            // ルートフォルダを最上位のフォルダとする
            if (sourceFiles[0] != null) {
                File srcFile = null;
                if (sourceFiles[0].getFile().isAbsolute()) {
                    srcFile = sourceFiles[0].getFile();
                }
                else {
                    srcFile = new File(projectFolder.getAbsolutePath() + File.separator + sourceFiles[0].getFile().getPath());
                }
                File path_list[] = FileUtils.getPathList(srcFile);
                root = new DefaultMutableTreeNode(path_list[1].getParentFile());
                topFile = path_list[0];
            }
        }

        // ツリーノードを作成する
        createNodes(root, topFile, sourceFiles);

        // ファイルツリーをソートする
        root = SwingUtils.sortTreeNode(root);

        // ツリーモデルの作成
        treeModel = new DefaultTreeModel(root);

        // モデルの変更を通知
        notifyModel();
    }

    /**
     * ツリーノードを作成する
     * @param tree			追加親ノード
     * @param folder			追加フォルダ要素
     * @param sourceFiles          ソースファイルリスト
     */
    private void createNodes(DefaultMutableTreeNode tree, File folder, SourceFile[] sourceFiles) {
        if (tree == null || folder == null) {
            clearTreeModel();
            return;
        }
        if(!folder.isDirectory()) return;

        for(int i=0; i<sourceFiles.length; i++) {
            File srcFile = null;
            if (sourceFiles[i] == null || sourceFiles[i].getFile() == null) {
            	continue;
            }
            if (sourceFiles[i].getFile().isAbsolute()) {
                srcFile = sourceFiles[i].getFile();
            }
            else {
                srcFile = new File(projectFolder.getAbsolutePath() + File.separator + sourceFiles[i].getFile().getPath());
            }
            try {
				if (folder.getCanonicalFile().equals(srcFile.getParentFile().getCanonicalFile())) {
				    DefaultMutableTreeNode subtree = new DefaultMutableTreeNode(new SourceFile(srcFile));
				    if (!containsChildNode(tree, subtree)) {
				    	tree.add(subtree);
				    }
				}
			} catch (IOException ex) {
				ex.printStackTrace();
			}
        }

        File[] files = folder.listFiles();
        if (files == null)   return;
        for(int i=0; i<files.length; i++) {
            try {
	        	if (files[i] == null) continue;
	        	if (files[i].isFile()) continue;
	        	if (FileUtils.isSymbolicLink(files[i])) continue;
	        	boolean ischild = false;
	            for(SourceFile srcFile : sourceFiles) {
	                if (srcFile == null || srcFile.getFile() == null) {
	                	continue;
	                }
	                File file = null;
	                if (srcFile.getFile().isAbsolute()) {
	                	file = srcFile.getFile();
	                }
	                else {
	                	file = new File(projectFolder.getAbsolutePath() + File.separator + srcFile.getFile().getPath());
	                }
		        	if (!FileUtils.isChildsFile(file, files[i])) continue;
		        	ischild = true;
	            }
	            if (!ischild) {
	            	continue;
	            }
	            DefaultMutableTreeNode subtree = new DefaultMutableTreeNode(files[i]);
	            if (!containsChildNode(tree, subtree)) {
	                createNodes(subtree, files[i], sourceFiles);
	                if (subtree.getChildCount() > 0) {
	                	tree.add(subtree);
	                }
	            }
			} catch (IOException ex) {
				ex.printStackTrace();
			}
        }
    }

    /**
     * ソースファイルの親パスであるかチェックする.<br/>
     * 同じファイルである場合は、trueを返す
     * @param parent			親ファイル
     * @param sourceFiles		ソースファイルリスト
     * @return		true=親パス
     */
    private boolean isParentPath(File parent, SourceFile[] sourceFiles) {
        for (int i = 0; i < sourceFiles.length; i++) {
            File srcFile = null;
            if (sourceFiles[i] == null || sourceFiles[i].getFile() == null) {
            	continue;
            }
            if (sourceFiles[i].getFile().isAbsolute()) {
                srcFile = sourceFiles[i].getFile();
            }
            else {
                srcFile = new File(projectFolder.getAbsolutePath() + File.separator + sourceFiles[i].getFile().getPath());
            }
            if (FileUtils.isEqualsFile(parent, srcFile)) {
                return true;
            }
            if (FileUtils.isEqualsFile(parent, srcFile.getParentFile())) {
                return true;
            }

            File[] path_list = FileUtils.getChildPathList( srcFile, parent);
            if (path_list != null) return true;
        }

        return false;
    }


    /**
     * すべてのソースファイルを取得する。
     *
     * @return ソースファイルリスト
     */
    public SourceFile[] getAllSourceFiles() {

        ArrayList<SourceFile> list = new ArrayList<SourceFile>();

        // ルートノード
        DefaultMutableTreeNode node = (DefaultMutableTreeNode) treeModel.getRoot();
        if (node == null)
            return null;

        SourceFile[] child_list = getSourceFiles(node);
        if (child_list != null) {
            // ソースファイルの追加
            list.addAll(Arrays.asList(child_list));
        }

        return list.toArray(new SourceFile[0]);
    }


    /**
     * 指定ノード配下の子要素のソースファイルを取得する.<br/>
     * 子、孫要素もすべて取得する。
     * @param node           子要素検索ノード
     * @return ソースファイルリスト
     */
    private SourceFile[] getSourceFiles(DefaultMutableTreeNode node) {

        if (node == null)
            return null;

        ArrayList<SourceFile> list = new ArrayList<SourceFile>();

        // 自身の追加
        Object obj = node.getUserObject();
        // ソースファイルオブジェクトであるか？
        if (obj instanceof SourceFile) {
            // ソースファイルの追加
            list.add((SourceFile) obj);
        }

        // 子要素の検索
        int count = node.getChildCount();
        for (int i = 0; i < count; i++) {
            DefaultMutableTreeNode child = (DefaultMutableTreeNode) node
                    .getChildAt(i);
            Object child_obj = child.getUserObject();
            if (child_obj == null)
                continue;

            // 子要素からソースファイルの取得
            SourceFile[] child_list = getSourceFiles(child);
            if (child_list != null) {
                // ソースファイルの追加
                list.addAll(Arrays.asList(child_list));
            }
        }

        if (list.size() <= 0)
            return null;

        return list.toArray(new SourceFile[0]);
    }

    /**
     * 選択されているソースファイルを削除する。
     * @param    paths			削除ツリーパス
     */
    public void deleteSelectedFiles(TreePath[] paths) {

        if (paths == null)
            return;
        for (int i = 0; i < paths.length; i++) {
            int count = paths[i].getPath().length;
            // ルート以外
            if (count >= 2) {
                // 子要素のソースファイルリストを取得する。
                DefaultMutableTreeNode node = (DefaultMutableTreeNode) paths[i].getPath()[count - 1];
                SourceFile[] del_files = getSourceFiles(node);

                // ツリー上の削除を行う。
                MutableTreeNode parent = (MutableTreeNode) paths[i].getPath()[count - 2];
                parent.remove((MutableTreeNode) paths[i].getPath()[count - 1]);
            } else if (count == 1) {

                // ルートの場合、子要素すべて削除
                DefaultMutableTreeNode node = (DefaultMutableTreeNode) treeModel.getRoot();
                node.removeAllChildren();
                break;
            }
        }

    }


    /**
     * ファイルツリーモデルを取得する
     * @return treeModel		ファイルツリーモデル
     */
    public DefaultTreeModel getTreeModel() {
        if (this.treeModel == null) {
            DefaultMutableTreeNode root = new DefaultMutableTreeNode(this.initTreeTitle);
            treeModel = new DefaultTreeModel(root);
        }
        return treeModel;
    }

    /**
     * ファイルツリーモデルを設定する
     * @param treeModel 	ファイルツリーモデル
     */
    public void setTreeModel(DefaultTreeModel treeModel) {
        this.treeModel = treeModel;
    }

    /**
     * プロジェクトフォルダを設定する
     * @param folder		プロジェクトフォルダ
     */
    public void setProjectFolder(File folder) {
        this.projectFolder = folder;
    }

    /**
     * モデルの変更を通知する
     */
    public void notifyModel() {
        this.setChanged();
        this.notifyObservers();
        this.clearChanged();
    }

    /**
     * ファイルツリーモデルをクリアする.
     */
    public void clearTreeModel() {
        DefaultMutableTreeNode root = new DefaultMutableTreeNode(this.initTreeTitle);
        treeModel = new DefaultTreeModel(root);

        // モデルの変更を通知
        notifyModel();
    }

    /**
     * 初期ツリータイトルを取得する
     * @return initTreeTitle		初期ツリータイトル
     */
    public String getInitTreeTitle() {
        return initTreeTitle;
    }

    /**
     * 初期ツリータイトルを設定する
     * @param initTreeTitle 	初期ツリータイトル
     */
    public void setInitTreeTitle(String initTreeTitle) {
        this.initTreeTitle = initTreeTitle;
    }

    /**
     * ツリー情報をファイル出力する。
     * @param   file   出力ファイル
     */
    public void writeFile(File file) {
        // ルートノード
        if (this.treeModel == null) return;
        DefaultMutableTreeNode root = (DefaultMutableTreeNode) this.treeModel.getRoot();
        if (root == null) return;
        if (root.getChildCount() <= 0) return;

        try {
            // ファイル出力
            PrintWriter pw = new PrintWriter(new BufferedWriter(new FileWriter(file)));

            // ツリーをCSV文字列にする。
            String buf = SwingUtils.toTreeText(root);
            // ファイル出力
            pw.print(buf);

            pw.close();
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }

    /**
     * ソースのルートを取得<br>
     * ツリーのルートではなく、ソースファイルが全て包含されるフォルダのこと
     * @return 	root
     */
    public String getRootFolder() {
    	if (this.treeModel == null) return "";
        DefaultMutableTreeNode root = (DefaultMutableTreeNode) this.treeModel.getRoot();
        if (root == null) return "";
        // rootから階層を降りていき、初めての分岐の直前か、分岐が無ければ終端の親フォルダ
        return scanSourceRoot(root);
    }

    private String scanSourceRoot(DefaultMutableTreeNode node) {
    	if (node == null) return "";
    	if (node.getChildCount() > 1) return node.toString();
    	DefaultMutableTreeNode child = (DefaultMutableTreeNode)node.getChildAt(0);
    	if (child.isLeaf()) return node.toString();
    	return scanSourceRoot((DefaultMutableTreeNode)node.getChildAt(0));
    }

    /**
     * 子ノードに同一ノードが存在するかチェックする.
     * @param parent		親ノード
     * @param child			子ノード
     * @return				true=同一ノードが存在する.
     */
    private boolean containsChildNode(DefaultMutableTreeNode parent, DefaultMutableTreeNode child) {
    	if (parent == null) return false;
    	if (child == null) return false;
    	if (parent.getChildCount() <= 0) return false;
    	Object childObj = child.getUserObject();
    	if (childObj == null) return false;

    	int count = parent.getChildCount();
    	if (childObj instanceof SourceFile) {
	    	SourceFile childFile = (SourceFile)childObj;
	    	for (int i=0; i<count; i++) {
	    		Object obj = ((DefaultMutableTreeNode)(parent.getChildAt(i))).getUserObject();
	    		SourceFile file = null;
	    		if (obj != null && obj instanceof SourceFile) {
	    			file = (SourceFile)obj;
	    		}
	    		if (childFile.equals(file)) {
	    			return true;
	    		}
	    	}
	    	return false;
    	}
    	else if (childObj instanceof File) {
	    	File childFile = (File)childObj;
	    	for (int i=0; i<count; i++) {
	    		Object obj = ((DefaultMutableTreeNode)(parent.getChildAt(i))).getUserObject();
	    		File file = null;
	    		if (obj != null && obj instanceof File) {
	    			file = (File)obj;
	    		}
	    		if (childFile.equals(file)) {
	    			return true;
	    		}
	    	}
	    	return false;
    	}
    	return false;
    }



}
