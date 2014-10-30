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
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.util.ArrayList;
import java.util.List;
import java.awt.Font;

import javax.swing.JTextPane;
import javax.swing.SizeRequirements;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.plaf.basic.BasicTextPaneUI;
import javax.swing.text.AbstractDocument;
import javax.swing.text.BadLocationException;
import javax.swing.text.BoxView;
import javax.swing.text.ComponentView;
import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.IconView;
import javax.swing.text.LabelView;
import javax.swing.text.ParagraphView;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;
import javax.swing.text.StyledEditorKit;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;

import jp.riken.kscope.utils.SwingUtils;

/**
 * ソースコード表示用テキストパイン
 * @author RIKEN
 */
public class CodePane extends JTextPane implements ITabComponent, FocusListener {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** 自動改行文字位置（0=自動改行しない） */
    private int wordwrap;

    //(2012/4/18) changed by teraim
    private Font sourceFont;       //ソース・フォント
    private Color activeRowColor;  //選択行背景色

    /** 親コンポーネント */
    private ITabComponent parentComponent = null;

    /** 行背景色リスト */
    private List<LinesBackground> listLinesColor;

    /**
     * 複数行範囲の背景色の情報クラス
     * @author RIKEN
     */
    public class LinesBackground {
        /** 開始行番号(1〜) */
        public int start;
        /** 終了行番号(1〜) */
        public int end;
        /** 背景色 */
        public Color color;

        /**
         * コンストラクタ
         * @param start		開始行
         * @param end		終了行
         * @param color		背景色
         */
        public LinesBackground(int start, int end, Color color) {
            this.start = start;
            this.end = end;
            this.color = color;
        }
    }


    /**
     * コンストラクタ
     */
    public CodePane() {
        super();
        initGUI();

    }

    /**
     * コンストラクタ
     * @param doc		スタイル付きドキュメント
     */
    public CodePane(StyledDocument doc) {
        super(doc);
        initGUI();
    }


    /**
     * GUIの初期化を行う
     */
    private void initGUI() {

        //(2012/4/18) added by teraim
        try{
            //アクティブ行の背景色を設定
            this.activeRowColor = new jp.riken.kscope.properties.SourceProperties().getActiverowColor();
            //ソースビューのフォントの設定
            this.sourceFont = new jp.riken.kscope.properties.SourceProperties().getFont();
            this.setFont(sourceFont);
        }catch(Exception e){
            e.printStackTrace();
        }

        this.setEditorKit(new WordWrapEditorKit());
        this.setUI(new LineHighlightTextPaneUI());

        this.addFocusListener(this);
    }

    /**
     * 折り返し文字列数から折り返し位置（ピクセル）を取得する。
     * @return		折り返し位置（ピクセル）
     */
    private int getWordWrapWidth() {
        if (wordwrap <= 0) return 0;
        FontMetrics fontMetrics = getFontMetrics(getFont());
        int width = fontMetrics.charWidth('0') * wordwrap;
        return width;
    }

    /**
     * 自動改行文字位置を取得する。
     * @return wordwrap		自動改行文字位置
     */
    public int getWordwrap() {
        return wordwrap;
    }

    /**
     * 自動改行文字位置を設定する。
     * @param wordwrap 自動改行文字位置
     */
    public void setWordwrap(int wordwrap) {
        this.wordwrap = wordwrap;
        this.repaint();
    }


    /**
     * 行折り返し段落のビュークラス
     * @author RIKEN
     */
    private class WordWrapParagraphView extends ParagraphView {
        /**
         * コンストラクタ
         * @param elem		このビューが扱う要素
         */
        public WordWrapParagraphView(Element elem) {
            super(elem);
        }

        /**
         * 行の幅のサイズ要件を計算します.<br/>
         * １行の折り返しサイズを設定する。
         * @param axis			行位置
         * @param r				コンポーネントのサイズと位置オブジェクト
         * @return				コンポーネントのサイズと位置オブジェクト
         */
        @Override
        protected SizeRequirements calculateMinorAxisRequirements(int axis, SizeRequirements r) {
            SizeRequirements req = super.calculateMinorAxisRequirements(axis, r);
            int wrapPixel = getWordWrapWidth();
            if (wrapPixel <= 0) {
                req.minimum = req.preferred;
            }
            else {
                req.minimum = wrapPixel;
                req.preferred = wrapPixel;
            }
            return req;
        }

        /**
         * 指定された子のインデックスに反してフローする制約スパンを取り出します。
         * @param index		照会されるビューのインデックス
         * @return			ビューの制約スパン
         */
        @Override
        public int getFlowSpan(int index) {
            int wrapPixel = getWordWrapWidth();
            if (wrapPixel <= 0) {
                return Integer.MAX_VALUE;
            }
            else {
                return wrapPixel;
            }
        }
        /*
      public void layout(int width, int height) {
        super.layout(500, height);
      }
         */

    }

    /**
     * ビューの作成クラス
     * @author RIKEN
     */
    private class WordWrapViewFactory implements ViewFactory {
        /**
         * 要素に基づいてビューを作成します。
         * @param elem		作成対象要素
         * @return			ビュー
         */
        @Override
        public View create(Element elem) {
            String kind = elem.getName();
            if(kind != null) {
                if(kind.equals(AbstractDocument.ContentElementName)) {
                    return new LabelView(elem);
                }else if(kind.equals(AbstractDocument.ParagraphElementName)) {
                    return new WordWrapParagraphView(elem);
                }else if(kind.equals(AbstractDocument.SectionElementName)) {
                    return new BoxView(elem, View.Y_AXIS);
                }else if(kind.equals(StyleConstants.ComponentElementName)) {
                    return new ComponentView(elem);
                }else if(kind.equals(StyleConstants.IconElementName)) {
                    return new IconView(elem);
                }
            }
            return new LabelView(elem);
        }
    }

    /**
     * 現在行に背景色を設定する。
     * @author RIKEN
     */
    public class LineHighlightTextPaneUI extends BasicTextPaneUI {
        /**
         * コンストラクタ
         */
        public LineHighlightTextPaneUI() {
            CodePane.this.addCaretListener(new CaretListener() {
                @Override
				public void caretUpdate(CaretEvent e) {
                    CodePane.this.repaint();
                }
            });
        }

        /**
         * 背景色を描画する.
         * @param g		描画グラフィック
         */
        @Override
        public void paintBackground(Graphics g) {
            super.paintBackground(g);
            try {

                // 行の背景色を設定する
                if (CodePane.this.listLinesColor != null) {

                    int startline = CodePane.this.getViewStartLine();
                    int endline = CodePane.this.getViewEndLine();
                    for (LinesBackground line : CodePane.this.listLinesColor) {
                        // 表示範囲内であるか
                        if (startline > line.end) continue;
                        if (endline < line.start) continue;

                        // 範囲行番号のオフセット位置
                        int start = CodePane.this.getLineStartOffset(line.start);
                        int end = CodePane.this.getLineEndOffset(line.end);

                        // 開始、終了行の描画範囲の取得
                        Rectangle startRect = modelToView(CodePane.this, start);
                        Rectangle endRect = modelToView(CodePane.this, end);

                        // 描画を行う範囲の取得
                        Rectangle drawRect = new Rectangle(startRect);
                        drawRect.height = endRect.y + endRect.height - startRect.y;
                        if (startRect.x > endRect.x) {
                            drawRect.x = endRect.x;
                        }
                        int startEndPos = startRect.x+startRect.width;
                        int endEndPos = endRect.x+endRect.width;
                        drawRect.width = (startEndPos > endEndPos) ?  startEndPos - drawRect.x : endEndPos - drawRect.x;

                        // 背景色の描画
                        g.setColor(line.color);
                        g.fillRect(0, drawRect.y, CodePane.this.getWidth(), drawRect.height);
                    }
                }

                // 現在キャレット位置のハイライト表示
                if (activeRowColor != null) {
                    Rectangle rect = modelToView(CodePane.this, CodePane.this.getCaretPosition());
                    int y = rect.y;
                    int h = rect.height;
                    g.setColor(activeRowColor);
                    g.fillRect(0, y, CodePane.this.getWidth(), h);
                }

            }catch(BadLocationException ex) {
                ex.printStackTrace();
            }
        }
    }

    /**
     * 書式付きテキストスタイル
     * @author RIKEN
     */
    private class WordWrapEditorKit extends StyledEditorKit {
        /** シリアル番号 */
        private static final long serialVersionUID = 1L;

        /**
         * ビュー作成クラスを取得する。
         * @return		ビュー作成クラス
         */
        @Override
        public ViewFactory getViewFactory() {
            return new WordWrapViewFactory();
        }
    }


    /**
     * 親コンポーネントを取得する.
     * @return		親コンポーネント
     */
    @Override
    public ITabComponent getParentComponent() {
        return this.parentComponent;
    }

    /**
     * 親コンポーネントを設定する.
     * @param component		親コンポーネント
     */
    @Override
    public void setParentComponent(ITabComponent component) {
        this.parentComponent = component;
    }

    /**
     * フォーカスリスナを設定する
     * @param listener		フォーカスリスナ
     */
    @Override
    public void addTabFocusListener(TabFocusListener listener) {
        this.addFocusListener(listener);
    }

    /**
     * タブを閉じる
     */
    @Override
    public void closeTabComponent() {
        // 親のタブパインにてタブを閉じる。
        if (this.parentComponent != null) {
            this.parentComponent.closeTabComponent();
        }
    }

    /**
     * フォーカス取得イベント.<br/>
     * フォーカス移動時にキャレットが非表示となるので、フォーカス取得時にキャレット表示を再設定する。
     * @param event		イベント情報
     */
    @Override
    public void focusGained(FocusEvent event) {
        this.getCaret().setVisible(true);
    }

    /**
     * フォーカス喪失イベント
     * @param event		イベント情報
     */
    @Override
    public void focusLost(FocusEvent event) { }

    /**
     * 選択行背景色を設定する。
     * @param color		選択行背景色
     */
    public void setActiveRowColor(Color color) {
        this.activeRowColor = color;
        this.repaint();
    }


    /**
     * 選択行背景色を取得する。
     * @return		選択行背景色
     */
    public Color getActiveRowColor() {
        return this.activeRowColor;
    }

    /**
     * 表示先頭位置のドキュメントの先頭からのオフセット (0 以上)を取得する。
     * @return		ドキュメントの先頭からのオフセット (0 以上)
     */
    public int getViewStartOffset() {
        if (this.parentComponent == null) return 0;
        if (!(this.parentComponent instanceof ScrollCodePane)) return 0;

        ScrollCodePane scroll = (ScrollCodePane)this.parentComponent;
        int start = this.viewToModel(scroll.getViewport().getViewPosition());

        return start;
    }

    /**
     * 表示末尾位置のドキュメントの先頭からのオフセット (0 以上)を取得する。
     * @return		ドキュメントの先頭からのオフセット (0 以上)
     */
    public int getViewEndOffset() {
        if (this.parentComponent == null) return 0;
        if (!(this.parentComponent instanceof ScrollCodePane)) return 0;

        ScrollCodePane scroll = (ScrollCodePane)this.parentComponent;
        Rectangle rect = scroll.getViewport().getVisibleRect();
        int end = this.viewToModel(
                new Point(scroll.getViewport().getViewPosition().x + this.getWidth(),
                        scroll.getViewport().getViewPosition().y + rect.height));

        return end;
    }

    /**
     * 表示先頭位置の行番号を取得する。
     * @return		先頭行番号 (1〜)
     */
    public int getViewStartLine() {
        // 表示開始位置
        int start = getViewStartOffset();
        if (start < 0) return 0;

        Document doc = this.getDocument();
        int startline = doc.getDefaultRootElement().getElementIndex(start);

        return startline + 1;
    }

    /**
     * 表示末尾位置の行番号を取得する。
     * @return		末尾行番号 (1〜)
     */
    public int getViewEndLine() {
        // 表示終了位置
        int end = getViewEndOffset();
        if (end <= 0) return 0;

        Document doc = this.getDocument();
        int endline = doc.getDefaultRootElement().getElementIndex(end);

        return endline + 1;
    }

    /**
     * 表示ドキュメントの行数を取得する。
     * @return		ドキュメント行数
     */
    public int getEndLine() {
        Document doc = this.getDocument();
        int endline = doc.getDefaultRootElement().getElementCount();
        return endline;
    }

    /**
     * 指定行番号の先頭のドキュメントの先頭からのオフセット (0 以上)を取得する。
     * @param line		指定行番号(１〜)
     * @return		ドキュメントの先頭からのオフセット (0 以上)
     */
    public int getLineStartOffset(int line) {
        if (line <= 0) return 0;

        // 開始、終了キャレット位置
        Document doc = this.getDocument();
        if (doc == null) return 0;
        Element root = doc.getDefaultRootElement();
        if (root == null) return 0;
        if (root.getElement( line-1 ) == null) return 0;
        int startOffset   = root.getElement( line-1 ).getStartOffset();

        return startOffset;
    }

    /**
     * 指定行番号の末尾のドキュメントの先頭からのオフセット (0 以上)を取得する。
     * @param line		指定行番号(１〜)
     * @return		ドキュメントの先頭からのオフセット (0 以上)
     */
    public int getLineEndOffset(int line) {
        if (line <= 0) return 0;
        // 開始、終了キャレット位置
        Document doc = this.getDocument();
        if (doc == null) return 0;
        Element root = doc.getDefaultRootElement();
        if (root == null) return 0;
        if (root.getElement( line-1 ) == null) return 0;
        int endOffset   = root.getElement( line-1 ).getEndOffset() - 1;

        return endOffset;
    }

    /**
     * ドキュメントの先頭からのオフセット (0 以上)から行番号を取得する
     * @param  pos		ドキュメントの先頭からのオフセット (0 以上)
     * @return		    行番号
     */
    public int getRow(int pos) {

        if (this.parentComponent == null) {
            // ドキュメント先頭から探索する
            return SwingUtils.getRow(this, pos);
        }
        if (!(this.parentComponent instanceof ScrollCodePane)) {
            // ドキュメント先頭から探索する
            return SwingUtils.getRow(this, pos);
        }
        int startOffset = getViewStartOffset();
        int endOffset =  getViewEndOffset();
        int startLine = getViewStartLine();
        int endLine = getViewEndLine();

        if (pos < startOffset) {
            // ドキュメント先頭から探索する
            return SwingUtils.getRow(this, pos);
        }
        if (pos >= endOffset) {
            return endLine;
        }

        // 行ライン内に存在するかチェックする
        for (int line = startLine; line<=endLine; line++) {
            int start = getLineStartOffset(line);
            int end = getLineEndOffset(line);
            if (start <= pos && pos <= end) {
                return line;
            }
        }

        return 0;
    }

    /**
     * 列番号を取得する
     * @param  pos		ドキュメントの先頭からのオフセット (0 以上)
     * @return		    列番号
     */
    public int getColumn(int pos) {
        return 0;
    }


    /**
     * 行背景色リストを取得する
     * @return		行背景色リスト
     */
    public List<LinesBackground> getListLinesColor() {
        return listLinesColor;
    }

    /**
     * 行背景色リストを設定する
     * @param list		行背景色リスト
     */
    public void setListLinesColor(List<LinesBackground> list) {
        this.listLinesColor = list;
    }

    /**
     * 行背景色を追加する
     * @param line		行背景色
     */
    public void addListLinesColor(LinesBackground line) {
        if (this.listLinesColor == null) {
            this.listLinesColor = new ArrayList<LinesBackground>();
        }
        this.listLinesColor.add(line);
    }

    /**
     * 行背景色リストをクリアする
     */
    public void clearListLinesColor() {
        this.listLinesColor = null;
    }

    /**
     * 行背景色を追加する
     * @param start		開始行
     * @param end		終了行
     * @param color		背景色
     */
    public void addLinesBackground(int start, int end, Color color) {
        LinesBackground line = new LinesBackground(start, end, color);
        addListLinesColor(line);
    }


}
