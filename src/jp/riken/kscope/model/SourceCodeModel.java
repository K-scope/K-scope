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

import java.awt.Color;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Observable;

import javax.swing.text.SimpleAttributeSet;

import jp.riken.kscope.Message;
import jp.riken.kscope.common.KEYWORD_TYPE;
import jp.riken.kscope.data.BatchDocument;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.Keyword;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.data.VariableMemory;
import jp.riken.kscope.gui.ISourceBargraph;
import jp.riken.kscope.utils.TextFileReader;

/**
 * ソースコードモデルクラス
 * @author RIKEN
 *
 */
public class SourceCodeModel extends Observable {

    /** 表示ソースファイル */
    private SourceFile sourceFile;

    /** 検索キーワード */
    private List<Keyword> searchWords;

    /** 強調範囲:有効範囲等 */
    private List<CodeLine> highlightArea = new ArrayList<CodeLine>();

    /** 選択ブロック:構造ツリーの選択ブロック */
    private List<CodeLine> selectedBlock = new ArrayList<CodeLine>();

    /** ソースコードドキュメント */
    private BatchDocument document;

    /** 強調範囲:有効範囲等の背景色 */
    private Color colorHighlightArea;
    /** 選択ブロックの背景色 */
    private Color colorSelectedBlock;
    /** 検索文字列の文字色 */
    private Color colorSearchFont;
    /** 検索文字列の背景色 */
    private Color colorSearchBackground;

    /** プロファイラ：コストデータ */
    private List<ISourceBargraph> listBarData;
	/** プロファイラデータ:最大値 */
	private float maxValue;
	/** プロファイラデータ:最小値 */
	private float minValue;
    /** 変数アクセス先メモリ */
	private List<VariableMemory> variableMemories;

    /**
     * コンストラクタ
     * @param source		表示ソースファイル
     */
    public SourceCodeModel(SourceFile source) {
        this.sourceFile = source;
    }


    /**
     * 表示ソースファイルパス（絶対パス)を取得する
     * @return		表示ソースファイルパス（絶対パス)
     */
    public String getFilePath() {
        if (sourceFile == null) return null;
        return this.sourceFile.getPath();
    }

    /**
     * 表示ソースファイルパスを取得する
     * @return		表示ソースファイル
     */
    public SourceFile getSourceFile() {
        return this.sourceFile;
    }

    /**
     * 表示ソースファイルパスを設定する
     * @param file		表示ソースファイル
     */
    public void setSourceFile(SourceFile file) {
        this.sourceFile = file;
    }

    /**
     * 検索キーワードを取得する
     * @return searchWord		検索キーワード
     */
    public List<Keyword> getSearchWords() {
        return searchWords;
    }

    /**
     * 検索キーワードを設定する
     * @param list 	検索キーワード
     */
    public void setSearchWords(List<Keyword> list) {
        this.searchWords = list;

        // 検索キーワードにハイライト色設定を行う。
        setSearchWordColor();
    }

    /**
     * 検索キーワードを追加する
     * @param word 	検索キーワード
     */
    public void addSearchWords(Keyword word) {
        if (this.searchWords == null) {
            this.searchWords = new ArrayList<Keyword>();
        }
        this.searchWords.add(word);

        // 検索キーワードにハイライト色設定を行う。
        setSearchWordColor();
    }

    /**
     * 検索キーワードをクリアする
     */
    public void clearSearchWords() {
        if (this.searchWords == null) return;
        this.searchWords.clear();
    }

    /**
     * 強調範囲を取得する。
     * @return highlightArea		強調範囲
     */
    public List<CodeLine> getHighlightArea() {
        return highlightArea;
    }

    /**
     * 強調範囲を設定する
     * @param highlightArea 		強調範囲
     */
    public void setHighlightArea(List<CodeLine> highlightArea) {
        this.highlightArea = highlightArea;
    }

    /**
     * 強調範囲を追加する
     * @param area	 		強調範囲
     */
    public void addHighlightArea(CodeLine area) {
        this.highlightArea.add(area);
    }

    /**
     * 強調範囲をクリアする
     */
    public void clearHighlightArea() {
        this.highlightArea.clear();
    }


    /**
     * ファイルを読み込み、表示を行う。
     * @throws Exception		読込エラー
     */
    public void readFile() throws Exception {
        readFile(this.sourceFile);
    }


    /**
     * ファイルを読み込み、表示を行う。
     * @param filename		ファイル名
     * @throws Exception		読込エラー
     */
    public void readFile(String filename) throws Exception {
        File file = new File(filename);
        if (!file.exists()) {
            throw new Exception(filename + Message.getString("sourcecodemodel.exception.notexist")); //が存在しません。
        }

        readFile(new SourceFile(file));
    }

    /**
     * ファイルを読み込み、表示を行う。
     * @param source		ファイルオブジェクト
     * @throws Exception		読込エラー
     */
    public void readFile(SourceFile source) throws Exception {
        File file = source.getFile();
        if (!file.exists()) {
            throw new Exception(file.getName() + Message.getString("sourcecodemodel.exception.notexist")); //が存在しません。
        }

        SimpleAttributeSet attr = new SimpleAttributeSet();
//        StyleConstants.setForeground(attr, Color.RED);

        document = new BatchDocument();
        TextFileReader reader = new TextFileReader(file);
        String line;
        while ((line = reader.readLine()) != null) {
            document.appendBatchLineString(line, attr);
        }

        document.processBatchUpdates(0);
    }

    /**
     * ソースコードパインドキュメントを取得する
     * @return		ソースコードパインドキュメント
     */
    public BatchDocument getDocument() {
        return document;
    }

    /**
     * 選択ブロックを取得する。
     * @return selectedBlock		選択ブロック
     */
    public List<CodeLine> getSelectedBlock() {
        return selectedBlock;
    }

    /**
     * 選択ブロックを設定する
     * @param highlightArea 		選択ブロック
     */
    public void setSelectedBlock(List<CodeLine> highlightArea) {
        this.selectedBlock = highlightArea;
    }

    /**
     * 選択ブロックを追加する
     * @param line	 		選択ブロック
     */
    public void addSelectedBlock(CodeLine line) {
        this.selectedBlock.add(line);
    }

    /**
     * 選択ブロックをクリアする
     */
    public void clearSelectedBlock() {
        this.selectedBlock.clear();
    }


    /**
     * 強調範囲:有効範囲等の背景色の取得を行う
     * @return		強調範囲:有効範囲等の背景色
     */
    public Color getColorHighlightArea() {
        return colorHighlightArea;
    }

    /**
     * 強調範囲:有効範囲等の背景色を取得する
     * @param color		強調範囲:有効範囲等の背景色
     */
    public void setColorHighlightArea(Color color) {
        this.colorHighlightArea = color;
    }

    /**
     * 選択ブロックの背景色を取得する
     * @return		選択ブロックの背景色
     */
    public Color getColorSelectedBlock() {
        return colorSelectedBlock;
    }

    /**
     * 選択ブロックの背景色を設定する
     * @param color		選択ブロックの背景色
     */
    public void setColorSelectedBlock(Color color) {
        this.colorSelectedBlock = color;
    }

    /**
     * 検索文字列の文字色を取得する
     * @return		検索文字列の文字色
     */
    public Color getColorSearchFont() {
        return colorSearchFont;
    }

    /**
     * 検索文字列の文字色を設定する
     * @param colorSearchFont		検索文字列の文字色
     */
    public void setColorSearchFont(Color colorSearchFont) {
        this.colorSearchFont = colorSearchFont;

        // 検索キーワードにハイライト色設定を行う。
        setSearchWordColor();
    }

    /**
     * 検索文字列の背景色を取得する
     * @return		検索文字列の文字色
     */
    public Color getColorSearchBackground() {
        return colorSearchBackground;
    }

    /**
     * 検索文字列の背景色を設定する
     * @param colorSearchBackground		検索文字列の文字色
     */
    public void setColorSearchBackground(Color colorSearchBackground) {
        this.colorSearchBackground = colorSearchBackground;

        // 検索キーワードにハイライト色設定を行う。
        setSearchWordColor();
    }

    /**
     * 検索キーワードにハイライト色設定を行う。
     */
    private void setSearchWordColor() {
        if (this.searchWords == null) return;

        for (Keyword word : this.searchWords) {
            if (this.colorSearchFont != null) {
                word.setForecolor(this.colorSearchFont);
            }
            if (this.colorSearchBackground != null) {
                word.setBackgroundcolor(this.colorSearchBackground);
            }
        }
    }

    /**
     * 検索・トレースキーワードをクリアする
     * @param  type     クリアキーワードタイプ
     */
    public void clearSearchWords(KEYWORD_TYPE type) {
        if (this.searchWords == null) return;
        if (this.searchWords.size() <= 0) return;
        for (int i=this.searchWords.size()-1; i>=0; i--) {
            if (this.searchWords.get(i).getType() == type) {
                this.searchWords.remove(i);
            }
        }
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
     * ソースバーグラフデータを取得する
     * @return ソースバーグラフデータ
     */
    public List<ISourceBargraph> getListBarData() {
        return listBarData;
    }


    /**
     * ソースバーグラフデータを設定する
     * @param listBarData ソースバーグラフデータ
     * @param max		全体コストデータの最大値
     * @param min		全体コストデータの最小値
     */
    public void setListBarData(List<ISourceBargraph> listBarData, float max, float min) {
        this.listBarData = listBarData;
        this.maxValue = max;
        this.minValue = min;
        notifyModel();
    }

    /**
     * ソースバーグラフデータを追加する
     * @param list ソースバーグラフデータ
     */
    public void addListBarData(List<ISourceBargraph> list) {
        if (list == null) return;
        if (this.listBarData == null) {
            this.listBarData = new ArrayList<ISourceBargraph>();
        }
        this.listBarData.addAll(list);
        notifyModel();
    }

    /**
     * ソースバーグラフデータを追加する
     * @param list ソースバーグラフデータ
     */
    public void addListBarData(ISourceBargraph[] list) {
        if (list == null) return;
        addListBarData(Arrays.asList(list));
    }

    /**
     * 変数アクセス先メモリを設定する.
     * @param list		変数アクセス先メモリ
     */
	public void setVariableMemories(List<VariableMemory> list) {
        this.variableMemories = list;
	}


	/**
	 * 変数アクセス先メモリを取得する
	 * @return 変数アクセス先メモリ
	 */
	public List<VariableMemory> getVariableMemories() {
		return variableMemories;
	}


	/**
	 * プロファイラデータ:最大値を取得する
	 * @return 		プロファイラデータ:最大値
	 */
	public float getMaxValue() {
		return maxValue;
	}


	/**
	 * プロファイラデータ:最小値を取得する
	 * @return 		プロファイラデータ:最小値
	 */
	public float getMinValue() {
		return minValue;
	}
}


