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

import java.awt.Color;
import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.List;
import java.util.Observable;
import java.util.Observer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;
import javax.swing.text.Position;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleContext;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.KEYWORD_TYPE;
import jp.riken.kscope.component.FrameScrollPane;
import jp.riken.kscope.data.BatchDocument;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.Keyword;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.data.VariableMemory;
import jp.riken.kscope.menu.SourcePanelPopupMenu;
import jp.riken.kscope.model.SourceCodeModel;
import jp.riken.kscope.properties.KeywordProperties;
import jp.riken.kscope.properties.SourceProperties;
import jp.riken.kscope.properties.VariableMemoryProperties;
import jp.riken.kscope.utils.SwingUtils;


/**
 * ソースコード表示パイン.<br/>
 * ソースコードを表示するテキストペインを提供する。<br/>
 * @author riken
 */
public class ScrollCodePane extends FrameScrollPane implements ITabComponent, ChangeListener, CaretListener, Observer {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** 行番号最小桁数 */
    private final int MINIMUMDISPLAYDIGITS = 5;
    /** プロファイラコストデータ表示エリア最小桁数(=表示幅) */
    private final int PROFILER_BARGPRAPH_DIGITS = 7;

    /** コード表示テキストパイン */
    private CodePane sourcePane;

    /** 行番号表示ヘッダー */
    private TextLineNumber lineHeader;
    /** プロファイラー表示フッター */
    private ProfilerLineInfo profilerFooter;

    /** 表示ソースモデル */
    private SourceCodeModel model;

    /** 親コンポーネント */
    private ITabComponent parentCompornent = null;

    /** タブサイズ */
    private final int TAB_SIZE = 4;

    /**
     * キーワードプロパティ.<br/>
     * テキストパインの表示している領域のみキーワードのハイライトを適用する。<br/>
     * スクロールしたときに再適用する為に待避しておく。
     */
    private KeywordProperties propertiesKeyword;

    /** 選択コード行情報 */
    private CodeLine selectedline;

    /** 現在の表示開始行番号 */
    private int currentStartLine;
    /** 現在の表示終了行番号 */
    private int currentEndLine;

    /**
     * コンストラクタ
     */
    public ScrollCodePane() {
        super();
        initGUI();
    }

    /**
     * 初期化を行う.<br/>
     * ソースコードタブを配置する。
     */
    private void initGUI() {
        try {
            sourcePane = new CodePane();
            //sourcePane.setUI(new LineHighlightTextPaneUI(sourcePane));

            // 読み取り専用とする。
            sourcePane.setEditable(false);
            sourcePane.getCaret().setVisible(true);   // キャレットを表示する
            sourcePane.setParentComponent(this);

            // タブサイズを設定する。
            SwingUtils.setTabSize(sourcePane, TAB_SIZE);

            this.setViewportView(sourcePane);

            // 行番号表示
            this.lineHeader = new TextLineNumber(sourcePane, MINIMUMDISPLAYDIGITS);
            this.lineHeader.setBackground(new jp.riken.kscope.properties.SourceProperties().getLineNumberColor());
            this.setRowHeaderView(this.lineHeader);

            // プロファイラデータ表示フッター生成
            this.profilerFooter = new ProfilerLineInfo(sourcePane, PROFILER_BARGPRAPH_DIGITS);

            // スクロールイベント
            this.getViewport().addChangeListener(this);

            // CaretListener
            sourcePane.addCaretListener(this);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * プロファイラデータ表示フッターを表示する
     * @param visible		true=表示
     */
    public void setVisibleBargraph(boolean visible) {
        if (visible) {
            this.setRowFooterView(this.profilerFooter);
        }
        else {
            this.setRowFooterView(null);
        }
    }


    /**
     * 末尾に文字列を追加する。
     * @param str		追加文字列
     * @throws BadLocationException			文字列追加エラー
     */
    public void appendString(String str) throws BadLocationException {
        Document doc = this.sourcePane.getDocument();
        Position endPos = doc.getEndPosition();

        SimpleAttributeSet attr = new SimpleAttributeSet();
        doc.insertString(endPos.getOffset(), str, attr);

        // タブサイズを設定する。
        SwingUtils.setTabSize(sourcePane, TAB_SIZE);
    }


    /**
     * 表示ドキュメントをクリアする。
     */
    public void clearDocument() {
        StyleContext sc = new StyleContext();
        Document doc = new DefaultStyledDocument(sc);
        this.sourcePane.setDocument(doc);
    }


    /**
     * 表示ソースファイルパス（絶対パス)を取得する。
     * @return filePath		表示ソースファイルパス（絶対パス)
     */
    public String getFilePath() {
        if (this.model == null) return null;
        return this.model.getFilePath();
    }


    /**
     * テキストパインを取得する。
     * @return		テキストパイン
     */
    public CodePane getSourcePane() {
        return this.sourcePane;
    }

    /**
     * 親コンポーネントを取得する.
     * @return		親コンポーネント
     */
    @Override
    public ITabComponent getParentComponent() {
        return this.parentCompornent;
    }

    /**
     * 親コンポーネントを設定する.
     * @param component		親コンポーネント
     */
    @Override
    public void setParentComponent(ITabComponent component) {
        this.parentCompornent = component;
    }

    /**
     * フォーカスリスナを設定する
     * @param listener		フォーカスリスナ
     */
    @Override
    public void addTabFocusListener(TabFocusListener listener) {
        this.addFocusListener(listener);
        if (this.sourcePane != null) {
            this.sourcePane.addTabFocusListener(listener);
        }
    }

    /**
     * タブを閉じる
     */
    @Override
    public void closeTabComponent() {
        // 親のタブパインにてタブを閉じる。
        if (this.parentCompornent != null) {
            this.parentCompornent.closeTabComponent();
        }
    }

    /**
     * ソースビュープロパティを設定する
     * @param properties		ソースビュープロパティ
     */
    public void setSourceProperties(SourceProperties properties) {
        // フォント
        sourcePane.setFont(properties.getFont());
        // フォント色
        sourcePane.setForeground(properties.getFontColor());
        // 背景色
        sourcePane.setBackground(properties.getBackgroundColor());
        // 選択行背景色
        sourcePane.setActiveRowColor(properties.getActiverowColor());
        // 折り返し位置
        sourcePane.setWordwrap(properties.getWordwrap());
        if (this.model != null) {
            // 強調範囲背景色
            this.model.setColorHighlightArea(properties.getAreaColor());
            // 選択範囲背景色
            this.model.setColorSelectedBlock(properties.getBlockColor());

            // 背景色設定を更新する
            this.setLinesBackground();

            // 検索文字列の文字色
            this.model.setColorSearchFont(properties.getSearchFontColor());
            // 検索文字列の背景色
            this.model.setColorSearchBackground(properties.getSearchBackgroundColor());
        }

        // 行番号ヘッダーを再描画する。
        this.lineHeader.setBackground(properties.getLineNumberColor());
        this.lineHeader.update();

        // キーワードプロパティ、検索・トレースキーワードを適用する.
        applyKeyword();
    }

    /**
     * キーワードプロパティを設定する
     * @param properties		キーワードプロパティ
     */
    public void setKeywordProperties(KeywordProperties properties) {
        this.propertiesKeyword = properties;

        // キーワードプロパティ、検索・トレースキーワードを適用する.
        applyKeyword();
    }


    /**
     * 検索・トレースキーワードを設定する
     * @param keywords		検索・トレースキーワード
     */
    public void setSearchWords(Keyword[] keywords) {
        if (this.model == null) return;
        if (keywords == null) return;

        // キーワードがこのソースに適用するか判断する
        List<Keyword> list = new ArrayList<Keyword>();
        for (Keyword word : keywords) {
            CodeLine searchline = word.getSearchLine();
            if (searchline == null || searchline.getSourceFile() == null) {
                list.add(word);
            }
            else {
                SourceFile searchFile = searchline.getSourceFile();
                SourceFile srcFile = this.model.getSourceFile();
                if (srcFile.equals(searchFile)) {
                    list.add(word);
                }
            }
        }
        this.model.setSearchWords(list);

        // キーワードプロパティ、検索・トレースキーワードを適用する.
        applyKeyword();
    }

    /**
     * 検索・トレースキーワードをクリアする.
     */
    public void clearSearchWords() {
        if (this.model == null) return;
        this.model.clearSearchWords();

        // キーワードプロパティ、検索・トレースキーワードを適用する.
        applyKeyword();
    }


    /**
     * キーワードプロパティ、検索・トレースキーワードを適用する.
     */
    public void applyKeyword() {
        if (this.model == null) return;

        if (!(sourcePane.getDocument() instanceof BatchDocument)) {
            return;
        }
        BatchDocument doc = (BatchDocument)sourcePane.getDocument();

        // 表示始点、終点キャレット位置から表示開始、終了行番号の取得
        int startline = this.sourcePane.getViewStartLine();
        int endline = this.sourcePane.getViewEndLine();
        if (startline <= 0 || endline <= 0) return;

        // キーワードハイライトのクリア
        doc.clearKeywordAttributes(startline, endline);

        // 表示位置の予約後のハイライト設定
        doc.applyHighlighting(this.propertiesKeyword, startline, endline);

        int startOffset   = this.sourcePane.getLineStartOffset(startline);
        int endOffset   = this.sourcePane.getLineEndOffset(endline);
        // 検索・トレースキーワードのハイライト設定
        List<Keyword> list = this.model.getSearchWords();
        if (list != null) {
            for (Keyword word : list) {
                try {
                	// 検索文字のコード行情報
                    CodeLine searchline = word.getSearchLine();
                    if (searchline == null) {
                        // ハイライト設定の適用
                        doc.applyKeyword(word, startOffset, endOffset);
                    }
                    else {
                        // 適用対象行を含むかチェックする
                        int startLineOffset   = this.sourcePane.getLineStartOffset(searchline.getStartLine());
                        int endLineOffset   = this.sourcePane.getLineEndOffset(searchline.getEndLine());
                        if (endOffset < startLineOffset) continue;
                        if (endLineOffset < startOffset) continue;

                        // ハイライト設定の適用
                        doc.applyKeyword(word, startLineOffset, endLineOffset);
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }

        // 変数アクセス先メモリキーワードのハイライト設定
        List<VariableMemory> listVar = this.model.getVariableMemories();
        if (listVar != null) {
            for (Keyword word : listVar) {
                try {
                    CodeLine searchline = word.getSearchLine();
                    if (searchline == null) continue;
                    // 適用対象行を含むかチェックする
                    int startLineOffset   = this.sourcePane.getLineStartOffset(searchline.getStartLine());
                    int endLineOffset   = this.sourcePane.getLineEndOffset(searchline.getEndLine());
                    if (endOffset < startLineOffset) continue;
                    if (endLineOffset < startOffset) continue;

                    // ハイライト設定の適用
                    doc.applyKeyword(word, startLineOffset, endLineOffset);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }
    }

    /**
     * スクロール変更イベント
     * @param event		イベント情報
     */
    @Override
    public void stateChanged(ChangeEvent event) {

        // 表示開始行番号
        int startLine = this.sourcePane.getViewStartLine();
        // 現在の表示終了行番号
        int endLine = this.sourcePane.getViewEndLine();

        // ビューの変更により表示行の変更がなければ、適用は行わない。
        // イベントが頻発する為の適用呼出の軽減対策
        if (this.currentStartLine == startLine && this.currentEndLine == endLine) {
            return;
        }

        // キーワードプロパティ、検索・トレースキーワードを適用する.
        applyKeyword();

        this.currentStartLine = startLine;
        this.currentEndLine = endLine;
    }

    /**
     * ソースファイルを読み込む
     * @param source		ソースファイル
     * @throws Exception		ソースファイル読込エラー
     */
    public void readFile(SourceFile source) throws Exception {
        this.model = new SourceCodeModel(source);
        this.model.readFile();

        sourcePane.setDocument(this.model.getDocument());

        // タブサイズを設定する。
        SwingUtils.setTabSize(sourcePane, TAB_SIZE);

        // キャレット位置を先頭にする
        this.sourcePane.setCaretPosition(0);
        this.sourcePane.getCaret().setVisible(true);   // キャレットを表示する

        // 選択行情報
        SourceFile f = model.getSourceFile();
        String fn = null;
        if (f != null) {
        	fn = f.getPath();
        }
        this.selectedline = new CodeLine(f, null, 1, fn);

        // オブザーバを設定する。
        model.addObserver(this);
    }

    /**
     * キャレット位置更新イベント
     * @param event		イベント情報
     */
    @Override
    public void caretUpdate(CaretEvent event) {
        if (this.model == null) return;
        // 選択行情報
        this.selectedline = null;

        if (sourcePane.getDocument() == null) return;
        if (sourcePane.getDocument().getLength() <= 0) {
            Application.status.setMessageLocation(1, 1, null);
            return;
        }

        // キャレットの位置
        int dot = event.getDot();
        // 論理選択範囲の反対側の位置を返します。選択範囲がない場合は、ドットと同じ
        int mark = event.getMark();

        // キャレット位置の行番号
        //int row = SwingUtils.getRow((JTextComponent)event.getSource(), event.getDot());
        int row = sourcePane.getRow(event.getDot());
        // キャレット位置の列番号
        int col = SwingUtils.getColumn((JTextComponent)event.getSource(), event.getDot());

        // 選択文字列クリア
        String selectword = null;
        try{
            BatchDocument document = (BatchDocument)sourcePane.getDocument();

            // 選択文字列数
            int len = dot < mark ? mark-dot : dot-mark;

            if (len > 0) {
                // 選択文字列
                selectword = document.getText(dot < mark ? dot : mark, len);
                if (selectword != null) {
                    selectword = selectword.trim();
                    // 選択範囲の場合、改行位置まで
                    int crpos = selectword.indexOf('\n');
                    if (crpos > 0) {
                        selectword = selectword.substring(0, crpos);
                    }
                }
            }
            else if (row > 0 && col > 0) {
                selectword = document.getCaretWord(row-1, col-1);
//                System.out.println(document.getCaretWord(dot));
            }
            if (selectword != null) selectword = selectword.trim();

            // 選択文字列が変数として認識可能であるかチェックする。
            if (!isVariableWord(selectword)) {
                // 変数名ではない。
                selectword = null;
            }

            // 選択行情報
            SourceFile f = model.getSourceFile();
            String fn = null;
            if (f != null) {
            	fn = f.getPath();
            }
            selectedline = new CodeLine(f, selectword, row, fn);
            // ステータスバーに表示
            Application.status.setMessageLocation(row, col, selectword);

        }catch (BadLocationException ble){
            System.err.println(Message.getString("scrollcodepane.errout.processfaild")); //文書の読み込みに失敗しました。
        }

    }

    /**
     * 変数文字列として認識可能であるかチェックする.<br/>
     * a-zの英字を含む文字列であるかチェックする.
     * @param word		変数文字列
     * @return			true=変数文字列として認識可能
     */
    private boolean isVariableWord(String word) {
        if (word == null) return false;

        String regex = Message.getString("scrollcodepane.variableword"); //[a-zA-Z]
        Pattern p = Pattern.compile(regex);
        Matcher m = p.matcher(word);

        return m.find();
    }


    /**
     * コード行情報の選択範囲を追加する
     * @param lines		コード行情報
     */
    public void setSelectedBlock(CodeLine[] lines) {
        if (this.model == null) return;
        if (lines == null || lines.length <= 0) {
            clearSelectedBlock();
            return;
        }

        // ソースファイル情報が一致しているかチェックする
        List<CodeLine> list = new ArrayList<CodeLine>();
        for (int i=0; i<lines.length; i++) {
            if (lines[i].getSourceFile() != null) {
                 if (lines[i].getSourceFile().equals(this.model.getSourceFile())) {
                     // ソースファイル一致
                     list.add(lines[i]);
                 }
            }
            else {
                // ソースファイルの情報がないので、そのまま追加する
                list.add(lines[i]);
            }
        }

        // コード行情報の選択範囲を追加する
        this.model.setSelectedBlock(list);

        // 背景色設定を更新する
        this.setLinesBackground();
    }


    /**
     * コード行情報の選択範囲を追加する
     * @param line		コード行情報
     */
    public void addSelectedBlock(CodeLine line) {
        if (line == null) return;
        if (this.model == null) return;

        // ソースファイル情報が一致しているかチェックする
         // ソースファイルの情報がない場合は、そのまま追加する
//        if (line.getSourceFile() != null) {
//             if (!line.getSourceFile().equals(this.model.getSourceFile())) {
//                 // ソースファイル不一致
//                 return;
//             }
//        }

        // コード行情報の選択範囲を追加する
        this.model.addSelectedBlock(line);

        // 背景色設定を更新する
        this.setLinesBackground();

        setLinePosition(line);
    }

    /**
     * コード行情報の選択範囲をクリアする。
     */
    public void clearSelectedBlock() {
        if (this.model == null) return;
        this.model.clearSelectedBlock();

        // 背景色設定を更新する
        this.setLinesBackground();

        this.repaint();
        this.updateUI();
    }

    /**
     * ソースコードパインに行ハイライト情報を設定する
     */
    private void setLinesBackground() {
        if (this.model == null) return;
        // 行ハイライト設定のクリア
        this.sourcePane.clearListLinesColor();

        // 強調範囲
        if (this.model.getColorHighlightArea() != null && this.model.getHighlightArea() != null) {
            List<CodeLine> lines = this.model.getHighlightArea();
            Color background = this.model.getColorHighlightArea();
            for (CodeLine line : lines) {
                this.sourcePane.addLinesBackground(line.getStartLine(), line.getEndLine(), background);
            }

        }
        // 選択ブロック
        if (this.model.getColorSelectedBlock() != null && this.model.getSelectedBlock() != null) {
            List<CodeLine> lines = this.model.getSelectedBlock();
            Color background = this.model.getColorSelectedBlock();
            for (CodeLine line : lines) {
                this.sourcePane.addLinesBackground(line.getStartLine(), line.getEndLine(), background);
            }
        }

        return;
    }

    /**
     * 指定行番号位置を表示領域に表示する。
     * @param line		表示行番号
     */
    public void setLinePosition(CodeLine line) {
        if (line == null) return;

        // 開始行番号
        int start = line.getStartLine();
        // 指定行番号位置を表示領域に表示する。
        setLinePosition(start);

    }

    /**
     * 指定行番号位置を表示領域に表示する。
     * @param line		表示行番号
     */
    public void setLinePosition(int start) {

        if (start <= 0) start = 1;

        // 上部に2行の余白を設ける
        int viewLine = start - 2;
        if (viewLine <= 0) viewLine = 1;

        // 行番号のキャレットインデックスの取得を行う
        int pos = this.sourcePane.getLineStartOffset(start);
        this.sourcePane.setCaretPosition(pos);

        try{
            Document doc = this.sourcePane.getDocument();
            Element root = doc.getDefaultRootElement();
            Element elem = root.getElement(viewLine-1);
            if (elem == null) return;
            Rectangle rect = this.sourcePane.modelToView(elem.getStartOffset());
            if (rect == null) return;
            Rectangle viewRect = this.getViewport().getViewRect();
            rect.setSize(10, viewRect.height);
            this.sourcePane.scrollRectToVisible(rect);
          }catch(BadLocationException ble) {
            java.awt.Toolkit.getDefaultToolkit().beep();
          }
    }

    /**
     * ソースコードモデルを取得する
     * @return			ソースコードモデル
     */
    public SourceCodeModel getModel() {
        return this.model;
    }

    /**
     * ソースファイルパネルコンテキストメニューを設定する
     * @param menuSourcePanel		ソースファイルパネルコンテキストメニュー
     */
    public void setSourcePanelPopupMenu(SourcePanelPopupMenu menuSourcePanel) {
        this.sourcePane.setComponentPopupMenu(menuSourcePanel);
    }

    /**
     * 選択行、選択文字情報を取得する
     * @return		選択行情報
     */
    public CodeLine getSelectedCodeLine() {
        // 選択されている文字列を取得する
        String selectText = sourcePane.getSelectedText();
        if (selectedline == null) return null;

        // 実際に選択されている文字列を設定する
        selectedline.setStatement(selectText);
        return selectedline;
    }

    /**
     * 選択行範囲を取得する
     * @return		選択範囲行コード情報
     */
    public CodeLine getSelectedArea() {
        // 選択されている文字列を取得する
        String selectText = sourcePane.getSelectedText();
        int startpos = sourcePane.getSelectionStart();
        int endpos = sourcePane.getSelectionEnd();
        int startrow = sourcePane.getRow(startpos);
        int endrow = sourcePane.getRow(endpos);
        SourceFile f = model.getSourceFile();
        String fn = null;
        if (f != null) {
        	fn = f.getPath();
        }
        CodeLine code = new CodeLine(f, selectText, startrow, endrow, fn);
        return code;
    }


    /**
     * ソースファイルを取得する
     * @return		ソースファイル
     */
    public SourceFile getSelectedSourceFile() {
        if (this.model == null) return null;
        SourceFile  file = this.model.getSourceFile();
        return file;
    }

    /**
     * クリップボードへコピーを行う
     */
    public void copyClipboard() {
        this.sourcePane.copy();
    }

    /**
     * 検索・トレースキーワードをクリアする
     * @param  type     クリアキーワードタイプ
     */
    public void clearSearchWords(KEYWORD_TYPE type) {
        if (this.model == null) return;
        this.model.clearSearchWords(type);

        // キーワードプロパティ、検索・トレースキーワードを適用する.
        applyKeyword();
    }


    /**
     * バーグラフデータを設定する
     * @param bardata			バーグラフデータ
     */
    public void setBargraphData(List<ISourceBargraph> bardata) {
        this.profilerFooter.setBargraphData(bardata);
    }

    /**
     * バーグラフデータをクリアする。
     */
    public void clearBargraphData() {
        this.profilerFooter.clearBargraphData();

        this.repaint();
        this.updateUI();
    }


    /**
     * ソースビューモデルの変更通知イベント
     * @param o			通知元
     * @param arg		通知項目
     */
    @Override
    public void update(Observable o, Object arg) {
        clearBargraphData();
        List<ISourceBargraph> list = this.model.getListBarData();
        if (list == null) {
            return;
        }

        // バーグラフデータをセットする
        setBargraphData(list);

        this.repaint();
        this.updateUI();
    }

    /**
     * 変数アクセス先メモリプロパティを設定する
     * @param properties	変数アクセス先メモリプロパティ
     */
	public void setVariableMemoryProperties(VariableMemoryProperties properties) {

        if (this.model == null) return;
        if (properties == null) return;

        // 変数アクセス先メモリがこのソースに適用するか判断する
        List<VariableMemory> varmems = properties.getListVariableMemory();
        List<VariableMemory> list = new ArrayList<VariableMemory>();
        for (VariableMemory var : varmems) {
            CodeLine varline = var.getSearchLine();
            if (varline == null || varline.getSourceFile() == null) {
                continue;
            }
            SourceFile searchFile = varline.getSourceFile();
            SourceFile srcFile = this.model.getSourceFile();
            if (srcFile.equals(searchFile)) {
                list.add(var);
            }
        }
        this.model.setVariableMemories(list);

        // キーワードプロパティ、検索・トレースキーワードを適用する.
        applyKeyword();
	}

	public void addSelectedBlockNoCaret(CodeLine line) {

        if (line == null) return;
        if (this.model == null) return;
        // コード行情報の選択範囲を追加する
        this.model.addSelectedBlock(line);

        // 背景色設定を更新する
        this.setLinesBackground();

        clearSelectionPos();
	}


    /**
     * 選択範囲をクリアする。
     */
    public void clearSelectionPos() {
        int start = this.sourcePane.getSelectionStart();
        int end = this.sourcePane.getSelectionEnd();
        this.sourcePane.setSelectionEnd(start);
    }
}



