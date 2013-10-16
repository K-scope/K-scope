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
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.HashMap;

import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.MatteBorder;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;
import javax.swing.text.StyleConstants;
import javax.swing.text.Utilities;

/**
 * ソースコード行番号表示クラス
 * @author riken
 */
public class TextLineNumber extends JPanel implements CaretListener, DocumentListener, PropertyChangeListener {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** 行番号の配置:左 */
    public final static float LEFT = 0.0f;
    /** 行番号の配置:中央 */
    public final static float CENTER = 0.5f;
    /** 行番号の配置:右 */
    public final static float RIGHT = 1.0f;
    /** 行番号表示エリアのボーダ設定 */
    private final static Border OUTER = new MatteBorder(0, 0, 0, 2, Color.GRAY);
    /** 行番号表示エリア高さ */
    private final static int HEIGHT = Integer.MAX_VALUE - 1000000;

    /** 行番号表示対象のテキストコンポーネント */
    private JTextComponent component;

    /** フォントの変更フラグ */
    private boolean updateFont;
    /** 行番号の左と右の余白 */
    private int borderGap;
    /** 行番号の現在行のフォントからー */
    private Color currentLineForeground;
    /** 行番号の配置 */
    private float digitAlignment;
    /** 行番号の最小桁数 */
    private int minimumDisplayDigits;

    /** 現在行番号桁数 */
    private int lastDigits;
    /** 現在行番号エリア高さ */
    private int lastHeight;
    /** 現在行番号 */
    private int lastLine;

    /** フォントテーブル */
    private HashMap<String, FontMetrics> fonts;

    /**
     * コンストラクタ
     * @param component		行番号表示対象のテキストコンポーネント
     */
    public TextLineNumber(JTextComponent component) {
        this(component, 3);
    }


    /**
     * コンストラクタ
     * @param component		行番号表示対象のテキストコンポーネント
     * @param minimumDisplayDigits		行番号桁数
     */
    public TextLineNumber(JTextComponent component, int minimumDisplayDigits) {
        this.component = component;

        setFont(component.getFont());

        setBorderGap(5);
        setCurrentLineForeground(Color.RED);
        setDigitAlignment(RIGHT);
        setMinimumDisplayDigits(minimumDisplayDigits);
        setBackground(Color.WHITE);

        component.getDocument().addDocumentListener(this);
        component.addCaretListener(this);
        component.addPropertyChangeListener("font", this);
    }

    /**
     * フォントの更新フラグを取得する。
     * @return 		フォント更新フラグ
     */
    public boolean getUpdateFont() {
        return updateFont;
    }

    /**
     * フォントの更新フラグを設定する。
     * @param updateFont		フォント更新フラグ（true=フォント更新)
     */
    public void setUpdateFont(boolean updateFont) {
        this.updateFont = updateFont;
    }

    /**
     * 行番号の左と右の余白を取得する。
     * @return  行番号の左と右の余白
     */
    public int getBorderGap() {
        return borderGap;
    }

    /**
     * 行番号の左と右の余白を設定する.<br/>
     * デフォルト余白=5px
     * @param borderGap			行番号の左と右の余白
     */
    public void setBorderGap(int borderGap) {
        this.borderGap = borderGap;
        Border inner = new EmptyBorder(0, borderGap, 0, borderGap);
        setBorder(new CompoundBorder(OUTER, inner));
        lastDigits = 0;
        setPreferredWidth();
    }

    /**
     * 行番号の現在行のフォントカラーを取得する
     * @return   行番号の現在行のフォントカラー
     */
    public Color getCurrentLineForeground() {
        return currentLineForeground == null ? getForeground()
                : currentLineForeground;
    }

    /**
     * 行番号の現在行のフォントカラーを設定する.<br/>
     * デフォルトカラー=RED
     * @param currentLineForeground		行番号の現在行のフォントカラー
     */
    public void setCurrentLineForeground(Color currentLineForeground) {
        this.currentLineForeground = currentLineForeground;
    }

    /**
     * 行番号の配置（LEFT、CENTER、RIGHT）を取得する。
     * @return   行番号の配置（LEFT、CENTER、RIGHT）
     */
    public float getDigitAlignment() {
        return digitAlignment;
    }

    /**
     * 行番号の配置（LEFT、CENTER、RIGHT）を設定する.<br/>
     * <ul>
     * <li>TextLineNumber.LEFT
     * <li>TextLineNumber.CENTER
     * <li>TextLineNumber.RIGHT (default)
     * </ul>
     * @param digitAlignment		行番号の配置（LEFT、CENTER、RIGHT）
     */
    public void setDigitAlignment(float digitAlignment) {
        this.digitAlignment = digitAlignment > 1.0f ? 1.0f
                : digitAlignment < 0.0f ? -1.0f : digitAlignment;
    }

    /**
     * 行番号の最小桁数を取得する.
     * @return   行番号の最小桁数
     */
    public int getMinimumDisplayDigits() {
        return minimumDisplayDigits;
    }

    /**
     * 行番号の最小桁数を設定する.
     * @param minimumDisplayDigits   行番号の最小桁数
     */
    public void setMinimumDisplayDigits(int minimumDisplayDigits) {
        this.minimumDisplayDigits = minimumDisplayDigits;
        setPreferredWidth();
    }

    /**
     * 行番号表示幅を設定する.
     */
    private void setPreferredWidth() {
        Element root = component.getDocument().getDefaultRootElement();
        int lines = root.getElementCount();
        int digits = Math.max(String.valueOf(lines).length(),
                minimumDisplayDigits);

        // Update sizes when number of digits in the line number changes

        if (lastDigits != digits) {
            lastDigits = digits;
            FontMetrics fontMetrics = getFontMetrics(getFont());
            int width = fontMetrics.charWidth('0') * digits;
            Insets insets = getInsets();
            int preferredWidth = insets.left + insets.right + width;

            Dimension d = getPreferredSize();
            d.setSize(preferredWidth, HEIGHT);
            setPreferredSize(d);
            setSize(d);
        }
    }

    /**
     * 行番号を描画する.
     */
    @Override
    public void paintComponent(Graphics g) {
        super.paintComponent(g);

        // Determine the width of the space available to draw the line number

        FontMetrics fontMetrics = component.getFontMetrics(component.getFont());
        Insets insets = getInsets();
        int availableWidth = getSize().width - insets.left - insets.right;

        // Determine the rows to draw within the clipped bounds.

        Rectangle clip = g.getClipBounds();
        int rowStartOffset = component.viewToModel(new Point(0, clip.y));
        int endOffset = component
                .viewToModel(new Point(0, clip.y + clip.height));

        try {
            while (rowStartOffset <= endOffset) {
                if (isCurrentLine(rowStartOffset))
                    g.setColor(getCurrentLineForeground());
                else
                    g.setColor(getForeground());

                // Get the line number as a string and then determine the
                // "X" and "Y" offsets for drawing the string.

                String lineNumber = getTextLineNumber(rowStartOffset);
                int stringWidth = fontMetrics.stringWidth(lineNumber);
                int x = getOffsetX(availableWidth, stringWidth) + insets.left;
                int y = getOffsetY(rowStartOffset, fontMetrics);
                g.drawString(lineNumber, x, y);

                // Move to the next row

                rowStartOffset = Utilities.getRowEnd(component, rowStartOffset) + 1;

            }
        } catch (Exception e) {
        }
    }

    /**
     * 現在行であるか判断する。
     * @return    true=現在行
     */
    private boolean isCurrentLine(int rowStartOffset) {
        int caretPosition = component.getCaretPosition();
        Element root = component.getDocument().getDefaultRootElement();

        if (root.getElementIndex(rowStartOffset) == root
                .getElementIndex(caretPosition))
            return true;
        else
            return false;
    }

    /**
     * 現在行番号を取得する
     * @param rowStartOffset		行オフセット
     * @return			行番号文字列
     */
    protected String getTextLineNumber(int rowStartOffset) {
        Element root = component.getDocument().getDefaultRootElement();
        int index = root.getElementIndex(rowStartOffset);
        Element line = root.getElement(index);

        if (line.getStartOffset() == rowStartOffset)
            return String.valueOf(index + 1);
        else
            return "";
    }

    /**
     * 行番号描画X位置を取得する
     * @param   availableWidth		表示幅
     * @param   stringWidth			行番号文字列幅
     */
    private int getOffsetX(int availableWidth, int stringWidth) {
        return (int) ((availableWidth - stringWidth) * digitAlignment);
    }


    /**
     * 行番号描画Y位置を取得する
     * @param   rowStartOffset		表示始点高さ
     * @param   fontMetrics			行番号文字フォント
     */
    private int getOffsetY(int rowStartOffset, FontMetrics fontMetrics)
            throws BadLocationException {
        // Get the bounding rectangle of the row

        Rectangle r = component.modelToView(rowStartOffset);
        int lineHeight = fontMetrics.getHeight();
        int y = r.y + r.height;
        int descent = 0;

        // The text needs to be positioned above the bottom of the bounding
        // rectangle based on the descent of the font(s) contained on the row.

        if (r.height == lineHeight) // default font is being used
        {
            descent = fontMetrics.getDescent();
        } else // We need to check all the attributes for font changes
        {
            if (fonts == null)
                fonts = new HashMap<String, FontMetrics>();

            Element root = component.getDocument().getDefaultRootElement();
            int index = root.getElementIndex(rowStartOffset);
            Element line = root.getElement(index);

            for (int i = 0; i < line.getElementCount(); i++) {
                Element child = line.getElement(i);
                AttributeSet as = child.getAttributes();
                String fontFamily = (String) as
                        .getAttribute(StyleConstants.FontFamily);
                Integer fontSize = (Integer) as
                        .getAttribute(StyleConstants.FontSize);
                String key = fontFamily + fontSize;

                FontMetrics fm = fonts.get(key);

                if (fm == null) {
                    Font font = new Font(fontFamily, Font.PLAIN, fontSize);
                    fm = component.getFontMetrics(font);
                    fonts.put(key, fm);
                }

                descent = Math.max(descent, fm.getDescent());
            }
        }

        return y - descent;
    }

    /**
     * テキストキャレット位置の更新イベント
     * @param e			テキストキャレット位置の更新イベント
     */
    @Override
    public void caretUpdate(CaretEvent e) {
        // Get the line the caret is positioned on

        int caretPosition = component.getCaretPosition();
        Element root = component.getDocument().getDefaultRootElement();
        int currentLine = root.getElementIndex(caretPosition);

        // Need to repaint so the correct line number can be highlighted

        if (lastLine != currentLine) {
            repaint();
            lastLine = currentLine;
        }
    }

    /**
     * ドキュメントの変更イベント
     * @param e		ドキュメントの変更イベント
     */
    @Override
    public void changedUpdate(DocumentEvent e) {
        documentChanged();
    }

    /**
     * ドキュメントの追加イベント
     * @param e		ドキュメントの変更イベント
     */
    @Override
    public void insertUpdate(DocumentEvent e) {
        documentChanged();
    }

    /**
     * ドキュメントの削除イベント
     * @param e		ドキュメントの変更イベント
     */
    @Override
    public void removeUpdate(DocumentEvent e) {
        documentChanged();
    }

    /**
     * ドキュメントの変更イベント
     */
    private void documentChanged() {
        // Preferred size of the component has not been updated at the time
        // the DocumentEvent is fired

        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                int preferredHeight = component.getPreferredSize().height;

                // Document change has caused a change in the number of lines.
                // Repaint to reflect the new line numbers

                if (lastHeight != preferredHeight) {
                    setPreferredWidth();
                    repaint();
                    lastHeight = preferredHeight;
                }
            }
        });
    }

    /**
     * プロパティ変更イベント
     * @param evt		プロパティ変更イベント
     */
    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        if (evt.getNewValue() instanceof Font) {
            if (updateFont) {
                Font newFont = (Font) evt.getNewValue();
                setFont(newFont);
                lastDigits = 0;
                setPreferredWidth();
            } else {
                repaint();
            }
        }
    }

    /**
     * 再描画する。
     */
    public void update() {
        documentChanged();
    }
}

