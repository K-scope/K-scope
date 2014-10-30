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

import java.awt.Component;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JOptionPane;
import javax.swing.event.ChangeEvent;

import jp.riken.kscope.Message;
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.common.KEYWORD_TYPE;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.Keyword;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.menu.SourcePanelPopupMenu;
import jp.riken.kscope.model.SourceCodeModel;
import jp.riken.kscope.properties.KeywordProperties;
import jp.riken.kscope.properties.RequiredBFProperties;
import jp.riken.kscope.properties.ProfilerProperties;
import jp.riken.kscope.properties.SourceProperties;
//import jp.riken.kscope.properties.SSHconnectProperties;
import jp.riken.kscope.properties.VariableMemoryProperties;
import jp.riken.kscope.utils.FileUtils;

/**
 * ソースコードビュークラス.<br/>
 * ソースコードタブを配置する。
 * @author RIKEN
 *
 */
public class SourceView extends ClosableTabbedPane implements  PropertyChangeListener  {

    /** シリアル番号  */
    private static final long serialVersionUID = 1L;

    /** ソースファイルパネルコンテキストメニュー */
    private SourcePanelPopupMenu menuSourcePanel;

    /** プロジェクトフォルダ */
    private File projectFolder;

    /**
     * コンストラクタ
     */
    public SourceView() {
        super(FRAME_VIEW.SOURCE_VIEW);
        initGUI();
    }

    /**
     * 初期化を行う.<br/>
     * ソースコードタブを配置する。
     */
    private void initGUI() {
        try {
            //(2012/4/17) changed by tomiyama and teraim
            //SourceCodePanel scrollPane = new SourceCodePanel();
            //this.addTab("blank:", scrollPane);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * ソースファイルをタブに表示する。
     * @param filename		ファイル名
     * @throws Exception		ソースファイル読込エラー
     */
    public void viewSource(String filename) throws Exception {
    	File file = new File(filename);
    	if (file.exists()) {
    		if (file.isFile()) {
    			viewSource(file);
    		}
    		else {
    			String message = filename + " " +
    					Message.getString("sourceview.errdialog.notfile"); //はファイルではありません。
    			JOptionPane.showMessageDialog(this.getParent(), message,
    					Message.getString("dialog.common.error"), //エラー
    					JOptionPane.ERROR_MESSAGE);
    			throw new Exception(message);
    		}
    	}
    	else {
    		String message = filename + " " +
    				Message.getString("sourceview.errdialog.filenotexist"); //が存在しません。
    		JOptionPane.showMessageDialog(this.getParent(), message,
    				Message.getString("dialog.common.error"), //エラー
    				JOptionPane.ERROR_MESSAGE);
            throw new Exception(message);
    	}
    }

    /**
     * ソースファイルをタブに表示する。
     * @param file		ファイル
     * @throws Exception		ソースファイル読込エラー
     */
    public void viewSource(File file) throws Exception {
        if (!file.exists()) {
        	String message = file.getPath() + " " +
        			Message.getString("sourceview.errdialog.filenotexist");//が存在しません。
        	JOptionPane.showMessageDialog(this.getParent(), message,
        			Message.getString("dialog.common.error"), //エラー
        			JOptionPane.ERROR_MESSAGE);
            throw new Exception(message);
        }
        if (!file.isFile()) {
        	String messgage = file.getPath() + " " +
        			Message.getString("sourceview.errdialog.notfile"); //はファイルではありません。
        	JOptionPane.showMessageDialog(this, messgage,
        			Message.getString("dialog.common.error"), //エラー
        			JOptionPane.ERROR_MESSAGE);
        	throw new Exception(messgage);
        }
        viewSource(new SourceFile(file));
    }

    /**
     * ソースファイルをタブに表示する。
     * @param source		ソースファイル
     * @throws Exception		ソースファイル読込エラー
     */
    public void viewSource(SourceFile source) throws Exception {
        CodeLine line = new CodeLine(source, source.getPath());
        viewSource(line);

        return;
    }


    /**
     * ソースファイルをタブに表示する。
     * @param line			コード行情報
     * @throws Exception 	ソースファイル取得エラー
     */
    public void viewSource(CodeLine line) throws Exception {

        if (line.getSourceFile() == null || line.getSourceFile().getFile() == null) {
        	JOptionPane.showMessageDialog(this.getParent(), Message.getString("sourceview.errdialog.notset", line.getStatement()),
        			Message.getString("dialog.common.error"), //エラー
        			JOptionPane.ERROR_MESSAGE);
            throw new Exception(Message.getString("sourceview.exception.missing")); //ソースファイルが取得できませんでした。
        }

        SourceFile source = line.getSourceFile();
        File file = source.getFile();
        if (!file.isAbsolute()) {
            // プロジェクトフォルダの相対パス
            file = new File(this.projectFolder.getAbsolutePath() + File.separator + file.getPath());
        }
        if (!file.isFile()) {
        	JOptionPane.showMessageDialog(this.getParent(), line.getStrSourceFile() + " " +
        			Message.getString("sourceview.errdialog.notfile"), //はファイルではありません。
        			Message.getString("dialog.common.error"), //エラー
        			JOptionPane.ERROR_MESSAGE);
        	throw new Exception(file.getPath() + Message.getString("sourceview.errdialog.notfile")); //はファイルではありません。
        }
        if (!file.exists()) {
        	JOptionPane.showMessageDialog(this.getParent(), line.getStrSourceFile() + " " +
        			Message.getString("sourceview.errdialog.filenotexist"), //が存在しません。
        			Message.getString("dialog.common.error"), //エラー
        			JOptionPane.ERROR_MESSAGE);
            throw new Exception(file.getPath() + Message.getString("sourceview.errdialog.filenotexist")); //が存在しません。
        }
        SourceFile readSource = new SourceFile(file);

        SourceCodePanel viewtab = null;

        // 表示ファイルが既に表示済みであるかチェックする。
        int index = getSourceTabIndex(file);
        if (index != -1) {
            // 表示済みであるので、アクティブにする。
            this.setSelectedIndex(index);
        }
        else {
            // 未表示であるので、空白タブの検索
            int newindex = -1;
            int count = this.getTabCount();
            for (int i=0; i<count; i++) {
                if (isBlankTab(i)) {
                    viewtab = getTab(i);
                    newindex = i;
                    break;
                }
            }

            // タブの新規作成
            if (viewtab == null) {
                viewtab = new SourceCodePanel();
                this.addTab("blank:", viewtab);
                newindex = getTabCount()-1;
            }

            // ファイルの読み込み
            viewtab.readFile(readSource);

            // タブタイトルの変更
            String name = file.getName();
            setTabTitle(newindex, name);

            // 開いたタブをアクティブにする
            this.setSelectedIndex(newindex);

            // ソースファイルパネルコンテキストメニューを設定する
            viewtab.setSourcePanelPopupMenu(menuSourcePanel);
        }

        // 表示ソースファイルをソースファイルツリーでアクティブにする
        fireChangedScrollCodePane();

        return;
    }


    /**
     * 指定ファイルの表示されているタブインデックスを取得する.<br/>
     * 存在しないときは、-1を返す。
     * @param file		ファイル
     * @return    タブインデックス（存在しないとき=-1)
     */
    private int getSourceTabIndex(File file) {
        if (file == null) return -1;

        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            Component tab = this.getComponentAt(i);
            if (!(tab instanceof SourceCodePanel)) continue;
            if (((SourceCodePanel)tab).getModel() == null) continue;
            SourceFile tabsource = ((SourceCodePanel)tab).getModel().getSourceFile();
            File tabfile = tabsource.getFile();
            if (!tabfile.isAbsolute()) {
                // プロジェクトフォルダの相対パス
                tabfile = new File(this.projectFolder.getAbsolutePath() + File.separator + tabfile.getPath());
            }
            File destfile = file;
            if (!destfile.isAbsolute()) {
                // プロジェクトフォルダの相対パス
                destfile = new File(this.projectFolder.getAbsolutePath() + File.separator + destfile.getPath());
            }

            // 同一ファイルを表示しているかチェックする。
            if (FileUtils.isEqualsFile(destfile, tabfile)) {
                return i;
            }
        }
        return -1;
    }


    /**
     * タブインデックスのタブが空白（ファイル表示なし）タブであるか判断する.
     * @param index		タブインデックス
     * @return    true=空白（ファイル表示なし）タブ
     */
    private boolean isBlankTab(int index) {

        Component tab = this.getComponentAt(index);
        if (!(tab instanceof SourceCodePanel)) return false;
        String tabpath = ((SourceCodePanel)tab).getFilePath();

        if (tabpath == null || tabpath.isEmpty()) {
            // 空白（ファイル表示なし）タブ
            return true;
        }
        return false;
    }

    /**
     * タブインデックスのソースパインを取得する.
     * @param index		タブインデックス
     * @return    true=空白（ファイル表示なし）タブ
     */
    private SourceCodePanel getTab(int index) {

        // (2012/5/25) added by Tomiyama
        if (index < 0)
            return null;
        Component tab = this.getComponentAt(index);
        if (!(tab instanceof SourceCodePanel)) return null;

        return (SourceCodePanel)tab;
    }


    /**
     * 現在選択されているテキストパインの取得を行う。
     * @return		選択テキストパイン
     */
    public CodePane getSelectedCodePane() {
        int index = this.getSelectedIndex();
        SourceCodePanel pane = getTab(index);
        return pane.getSourcePane();
    }

    /**
     * アクティブなタブを閉じる
     */
    @Override
    public void closeTabComponent() {
        int index = this.getSelectedIndex();
        // タブを閉じる
        closeTab(index);
    }

    /**
     * タブを閉じる
     * @param index		閉じるタブインデックス
     */
    @Override
    protected void closeTab(int index) {
        if (index >= 0) {
            this.remove(index);
        }

        //(2012/4/18) changed by tomiyama and teraim
        /*
        int count = this.getTabCount();
        if (count <= 0) {
            // ブランクタブを作成する。
            SourceCodePanel scrollPane = new SourceCodePanel();
            this.addTab("blank:", scrollPane);
        }
        */

    }

    /**
     * 全てのタブをクリアする
     */
    public void closeAllTabs() {
        this.removeAll();

        //(2012/4/18) changed by tomiyama and teraim
        // ブランクタブを作成する。
        /*
        SourceCodePanel scrollPane = new SourceCodePanel();
        this.addTab("blank:", scrollPane);
        */
    }


    /**
     * プロパティ変更イベント
     * @param event   イベント情報
     */
    @Override
    public void propertyChange(PropertyChangeEvent event) {
//        System.out.println(event.getPropertyName());

        // ソース表示フォント、フォント色等のソースビュープロパティの変更
        if (event.getNewValue() instanceof SourceProperties) {
            SourceProperties properties = (SourceProperties)event.getNewValue();

            int count = this.getTabCount();
            for (int i=0; i<count; i++) {
            	SourceCodePanel pane = getTab(i);
                pane.setSourceProperties(properties);
            }
        }
        // キーワードプロパティの変更
        else if (event.getNewValue() instanceof KeywordProperties) {
            KeywordProperties properties = (KeywordProperties)event.getNewValue();

            int count = this.getTabCount();
            for (int i=0; i<count; i++) {
            	SourceCodePanel pane = getTab(i);
                pane.setKeywordProperties(properties);
            }
        }
        // プロファイラプロパティの変更
        else if (event.getNewValue() instanceof ProfilerProperties) {
            ProfilerProperties properties = (ProfilerProperties)event.getNewValue();
            int count = this.getTabCount();
            for (int i=0; i<count; i++) {
            	SourceCodePanel pane = getTab(i);
                pane.setProfilerProperties(properties);
            }
        }
        // 変数アクセス先メモリプロパティの変更
        else if (event.getNewValue() instanceof VariableMemoryProperties) {
        	VariableMemoryProperties properties = (VariableMemoryProperties)event.getNewValue();
            int count = this.getTabCount();
            for (int i=0; i<count; i++) {
            	SourceCodePanel pane = getTab(i);
                pane.setVariableMemoryProperties(properties);
            }
        }
        // 要求B/F設定の変更
        else if (event.getNewValue() instanceof RequiredBFProperties) {
            int count = this.getTabCount();
            for (int i=0; i<count; i++) {
            	SourceCodePanel pane = getTab(i);
            	// 再描画を行うのみ
                pane.applyKeyword();
            }
        }
        
    }

    /**
     * 指定ファイルの表示されているタブを閉じる.<br/>
     * @param filename		ファイル名
     */
    public void closeSourceFile(String filename) {
        if (filename == null || filename.isEmpty()) return;


        int index = getSourceTabIndex(new File(filename));
        if (index < 0) return;

        // タブを閉じる
        closeTab(index);

    }

    /**
     * 選択ブロックをクリアする
     */
    public void clearSelectedBlock() {
        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
        	SourceCodePanel pane = getTab(i);
            pane.clearSelectedBlock();
        }
    }

    /**
     * 選択ソース行ブロックを設定する
     * @param line			選択ソース行ブロック
     */
    public void setSelectedBlock(CodeLine[] line) {

        if (line == null || line.length <= 0) return;

        // 選択範囲のクリア
        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            SourceCodePanel pane = getTab(i);
            // 選択範囲のクリア
            pane.clearSelectedBlock();
        }

        // 選択範囲の設定
        for (int i=0; i<line.length; i++) {
            if (line[i] == null) continue;
            if (line[i].getStartLine() <= 0) continue;
            if (line[i].getEndLine() <= 0) continue;
            if (line[i].getSourceFile() == null) continue;
            if (line[i].getSourceFile().getFile() == null) continue;

            // ソースコードと一致するテキストパネルに選択ブロックを設定する
            SourceFile source = line[i].getSourceFile();
            File file = source.getFile();
            if (!file.isAbsolute()) {
                // プロジェクトフォルダの相対パス
                file = new File(this.projectFolder.getAbsolutePath() + File.separator + file.getPath());
            }
            // ソースファイルと一致するソースパネルの検索
            int index = getSourceTabIndex(file);
            if (index == -1) continue;		// 一致するソースパネルなし
            SourceCodePanel pane = getTab(index);

            // 選択ソース行ブロック
            pane.addSelectedBlock(line[i]);
        }

    }

    /**
     * 選択ソース行範囲を設定する.
     * キャレット位置は変更しない
     * @param line			選択ソース行範囲
     */
    public void setSelectedBlockNoCaret(CodeLine line) {

        if (line == null) return;

        // 選択範囲のクリア
        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            SourceCodePanel pane = getTab(i);
            // 選択範囲のクリア
            pane.clearSelectedBlock();
        }

        // 選択範囲の設定
        if (line.getStartLine() <= 0) return;
        if (line.getEndLine() <= 0) return;
        if (line.getSourceFile() == null) return;
        if (line.getSourceFile().getFile() == null) return;

        // ソースコードと一致するテキストパネルに選択ブロックを設定する
        SourceFile source = line.getSourceFile();
        File file = source.getFile();
        if (!file.isAbsolute()) {
            // プロジェクトフォルダの相対パス
            file = new File(this.projectFolder.getAbsolutePath() + File.separator + file.getPath());
        }
        // ソースファイルと一致するソースパネルの検索
        int index = getSourceTabIndex(file);
        if (index == -1) return;		// 一致するソースパネルなし
        SourceCodePanel pane = getTab(index);

        // 選択ソース行ブロック
        pane.addSelectedBlockNoCaret(line);

    }

    /**
     * 指定ソース行を設定する.<br/>
     * 指定行番号の１行をアクティブにする.
     *
     * @param line			ソース行
     */
    public void setSelectedLine(CodeLine line) {
        if (line == null) return;

        // 選択範囲の設定
        if (line.getStartLine() <= 0) return;
        if (line.getEndLine() <= 0) return;
        if (line.getSourceFile() == null) return;
        if (line.getSourceFile().getFile() == null) return;

        // ソースコードと一致するテキストパネルに選択ブロックを設定する
        SourceFile source = line.getSourceFile();

        // ソースファイルと一致するソースパネルの検索
        int index = getSourceTabIndex(source.getFile());
        if (index == -1) return;		// 一致するソースパネルなし
        SourceCodePanel pane = getTab(index);
        if (pane == null) return;

        // 選択ソース行ブロック
        pane.setLinePosition(line);

    }


    /**
     * 開いているファイルの一覧を取得する.
     * @return    開いているファイル一覧
     */
    public SourceFile[] getOpenedSourceFile() {

        List<SourceFile> list = new ArrayList<SourceFile>();
        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            Component tab = this.getComponentAt(i);
            if (!(tab instanceof SourceCodePanel)) continue;
            SourceCodeModel model = ((SourceCodePanel)tab).getModel();
            if (model == null) continue;
            SourceFile file = model.getSourceFile();
            if (file == null) continue;
            list.add(file);
        }
        if (list.size() <= 0) return null;

        return list.toArray(new SourceFile[0]);
    }

    /**
     * ソースファイルパネルコンテキストメニューを設定する
     * @param menuSourcePanel		ソースファイルパネルコンテキストメニュー
     */
    public void setSourcePanelPopupMenu(SourcePanelPopupMenu menuSourcePanel) {
        this.menuSourcePanel = menuSourcePanel;
    }

    /**
     * 選択ソースコード行情報を取得する
     * @return		選択ソースコード行情報
     */
    public CodeLine getSelectedCodeLine() {
        int index = this.getSelectedIndex();
        SourceCodePanel pane = getTab(index);
        if (pane == null) {
            return null;
        }
        return pane.getSelectedCodeLine();
    }


    /**
     * 選択行範囲を取得する
     * @return		選択範囲行コード情報
     */
    public CodeLine getSelectedArea() {
        int index = this.getSelectedIndex();
        SourceCodePanel pane = getTab(index);
        if (pane == null) return null;
        return pane.getSelectedArea();
    }

    /**
     * ソースファイルを取得する
     * @return		ソースファイル
     */
    public SourceFile getSelectedSourceFile() {
        int index = this.getSelectedIndex();
        SourceCodePanel pane = getTab(index);
        return pane.getSelectedSourceFile();
    }

    /**
     * 検索・トレースキーワードを設定する
     * @param keywords		検索・トレースキーワード
     */
    public void setSearchWords(Keyword[] keywords) {
        if (keywords == null) {
            // 検索・トレースキーワードをクリアする
            clearSearchWords();
            return;
        }

        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            SourceCodePanel pane = getTab(i);
            if (pane == null) continue;
            pane.setSearchWords(keywords);
        }
    }

    /**
     * 検索・トレースキーワードをクリアする
     */
    public void clearSearchWords() {
        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            SourceCodePanel pane = getTab(i);
            if (pane == null) continue;
            pane.clearSearchWords();
        }
    }


    /**
     * 検索・トレースキーワードをクリアする
     * @param  type     クリアキーワードタイプ
     */
    public void clearSearchWords(KEYWORD_TYPE type) {
        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            SourceCodePanel pane = getTab(i);
            if (pane == null) continue;
            pane.clearSearchWords(type);
        }
    }


    /**
     * プロファイラバーグラフをクリアする
     */
    public void clearBargraphData() {
        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            SourceCodePanel pane = getTab(i);
            if (pane == null) continue;
            pane.clearBargraphData();
        }
    }


    /**
     * 現在選択されているテキストをクリップボードにコピーする
     */
    public void copyClipboard() {
        int index = this.getSelectedIndex();
        SourceCodePanel pane = getTab(index);
        if (pane == null) return;
        pane.copyClipboard();
    }

    /**
     * プロジェクトフォルダを設定する
     * @param folder		プロジェクトフォルダ
     */
    public void setProjectFolder(File folder) {
        this.projectFolder = folder;
    }

    /**
     * アクティブタブの変更イベント
     * @param event      イベント発生ソース
     */
    @Override
    public void stateChanged(ChangeEvent event) {
        super.stateChanged(event);

        // 表示ソースファイルをソースファイルツリーでアクティブにする
        fireChangedScrollCodePane();

    }

    /**
     * アクティブソースファイルの変更によるツリーとの同期を行う。
     */
    private void fireChangedScrollCodePane() {

        // 表示ソースファイル
        int index = this.getSelectedIndex();
        if (index < 0) return;
        SourceCodePanel pane = getTab(index);
        if (pane.getModel() == null) return;
        SourceFile file = pane.getModel().getSourceFile();
        if (file == null) return;

        // 表示ソースファイルをソースファイルツリーでアクティブにする
        MainFrame frame = (MainFrame)this.getParentComponent();
        if (frame == null) return;
        if (frame.getPanelExplorerView() == null) return;
        frame.getPanelExplorerView().setSelectedNode(file);

    }

    /**
     * プロファイル表示フッタの表示切替を行う
     * @param visible		true=プロファイル表示フッタ表示
     */
    public void setVisibleBargraph(boolean visible) {
        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            Component tab = this.getComponentAt(i);
            if (tab instanceof SourceCodePanel) {
                ((SourceCodePanel)tab).setVisibleBargraph(visible);
            }
        }

    }

    /**
     * プロファイル表示ルーラの表示切替を行う
     * @param visible		true=プロファイル表示ルーラ表示
     */
    public void setVisibleRuler(boolean visible) {
        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            Component tab = this.getComponentAt(i);
            if (tab instanceof SourceCodePanel) {
                ((SourceCodePanel)tab).setVisibleRuler(visible);
            }
        }

    }

    /**
     * 選択モデルを取得する
     * @return			ソースモデル
     */
    public SourceCodeModel getSelectedModel() {

        int index = this.getSelectedIndex();
        SourceCodePanel pane = getTab(index);
        if (pane == null) return null;
        return pane.getModel();
    }

    /**
     * プロファイラバーグラフをソースビューに設定する
     * @param bargraph		プロファイラバーグラフデータ
     */
    public void setProfilerBargraph(ISourceBargraph[] bargraph) {
        int count = this.getTabCount();
        for (int i=0; i<count; i++) {
            Component tab = this.getComponentAt(i);
            if (tab instanceof SourceCodePanel) {
                SourceCodeModel model = ((SourceCodePanel)tab).getModel();
                if (model == null) continue;
                // 同一のソースファイルのみバーグラフデータを設定する。
                SourceFile srcfile = model.getSourceFile();
                if (srcfile == null) continue;
                List<ISourceBargraph> list = new ArrayList<ISourceBargraph>();
                // 全体のデータの最大値、最小値
                float max = Float.MIN_VALUE;
                float min = Float.MAX_VALUE;
                if (bargraph != null) {
                    for (ISourceBargraph data : bargraph) {
                    	// 最大値
                    	if (max <= data.getBarValue()) max = data.getBarValue();
                    	// 最小値
                    	if (min >= data.getBarValue()) min = data.getBarValue();
                    	//  ソースファイルが一致しているか？
                        SourceFile barfile = data.getSourceFile();
                        if (srcfile.equals(barfile)) {
                            list.add(data);
                        }
                    }
                }
                if (list.size() <= 0) list = null;
                model.setListBarData(list, max, min);
            }

        }
    }

}


