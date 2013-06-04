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
package jp.riken.kscope.data;

import java.awt.Font;
import java.text.BreakIterator;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.SwingUtilities;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Element;
import javax.swing.text.MutableAttributeSet;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;

import jp.riken.kscope.properties.KscopeProperties;
import jp.riken.kscope.properties.KeywordProperties;
import jp.riken.kscope.utils.StringUtils;

/**
 * スタイル付テキストモデル
 * @author riken
 *
 */
public class BatchDocument extends DefaultStyledDocument {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /** 行末キャラクタ */
    private static final char[] EOL_ARRAY = { '\n' };

    /** 行文字列リスト */
    private ArrayList<ElementSpec> batch = null;

    private SimpleAttributeSet defualtAttributeSet = new SimpleAttributeSet();

    /**
     * コンストラクタ
     */
    public BatchDocument() {
        batch = new ArrayList<ElementSpec>();
    }

    /**
     * 文字列を追加する。
     * @param   str		追加文字列
     * @param   attr    スタイル情報
     */
    public void appendBatchString(String str, AttributeSet attr) {
        attr = attr.copyAttributes();
        char[] chars = str.toCharArray();
        // batch.add(new ElementSpec(attr, ElementSpec.ContentType, chars, 0, str.length()));
        addBachList(attr, ElementSpec.ContentType, chars, str.length());
    }

    /**
     * 行を追加する。
     * @param   str		追加行
     * @param   attr    スタイル情報
     */
    public void appendBatchLineString(String str, AttributeSet attr) {
        appendBatchString(str, attr);
        appendBatchLineFeed(attr);
    }

    /**
     * 行末文字列を追加する。
     * @param   attr    スタイル情報
     */
    public void appendBatchLineFeed(AttributeSet attr) {
        // 行末キャラクタを追加する。
        // batch.add(new ElementSpec(attr, ElementSpec.ContentType, EOL_ARRAY, 0, 1));
        addBachList(attr, ElementSpec.ContentType, EOL_ARRAY, 1);

        Element paragraph = getParagraphElement(0);
        AttributeSet pattr = paragraph.getAttributes();
        //batch.add(new ElementSpec(null, ElementSpec.EndTagType));
        //batch.add(new ElementSpec(pattr, ElementSpec.StartTagType));
        addBachList(null, ElementSpec.EndTagType, null, 0);
        addBachList(pattr, ElementSpec.StartTagType, null, 0);
    }

    /**
     * 追加した要素(文字列+スタイル)をDefaultStyledDocumentにまとめて追加する。
     * @param offs			追加オフセット
     * @throws BadLocationException			ドキュメントモデル中の不正な位置エラー
     */
    public void processBatchUpdates(int offs) throws BadLocationException {
        // 追加済みの要素リストを配列に変換する。
        ElementSpec[] inserts = new ElementSpec[batch.size()];
        batch.toArray(inserts);

        // 要素リストを配列の追加
        super.insert(offs, inserts);
    }

    /**
     * ドキュメントの要素リストに追加
     * @param attr		スタイル属性
     * @param type		タイプ
     * @param txt		ドキュメントテキスト
     * @param len		テキスト長
     */
    private void addBachList(AttributeSet attr, short type, char[] txt, int len) {
        if (len > 0) {
            batch.add(new ElementSpec(attr, type, txt, 0, len));
        }
        else {
            batch.add(new ElementSpec(attr, type));
        }
    }

    /**
     * キーワードのハイライト設定をクリアする
     * @param startline			ハイライト開始行インデックス
     * @param endline			ハイライト終了行インデックス
     */
    public void clearKeywordAttributes(final int startline, final int endline)  {

        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                // 開始、終了キャレット位置
                Element root = getDefaultRootElement();
                int startOffset   = root.getElement( startline-1 ).getStartOffset();
                int endOffset   = root.getElement( endline-1 ).getEndOffset();
                int lineLength  = endOffset - startOffset;

                // 設定したスタイル属性をクリアする。
                setCharacterAttributes(startOffset, lineLength, defualtAttributeSet, true);
            }
        });
    }


    /**
     * キーワードのハイライトを行う
     * @param properties		キーワード設定
     * @param startline			ハイライト開始行インデックス
     * @param endline			ハイライト終了行インデックス
     */
    public void applyHighlighting(final KeywordProperties properties, final int startline, final int endline)  {
        if (properties == null) return;

        // 開始、終了キャレット位置
        Element root = getDefaultRootElement();
        int startOffset   = root.getElement( startline-1 ).getStartOffset();
        int endOffset   = root.getElement( endline-1 ).getEndOffset();

        int count = properties.getKeywordCount();
        for (int i=0; i<count; i++ ) {
            try {
                Keyword keyword = properties.getKeyword(i);
                if (!keyword.isEnabled()) continue;
                // キーワード検索を行う。
                applyKeyword(keyword, startOffset, endOffset);

            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * 単語の開始位置の探索を行う
     * @param properties		キーワード設定情報
     * @param startOffset		開始ドキュメントキャレット位置
     * @param endOffset			終了ドキュメントキャレット位置
     * @throws Exception
     */
    private void checkForTokens(KeywordProperties properties, int startOffset, int endOffset) throws Exception {
        int length = endOffset - startOffset + 1;
        String content = this.getText(startOffset, length);
        int startElement = startOffset;
        int endElement = endOffset;
        int start = 0;
        int end = endOffset-startOffset;
        // 単語の検索
        while (start <= end) {
            // 区切り文字の探索
            while (isDelimiter(content.substring(start, start+1))) {
                if (start < end) {
                    start++;
                } else {
                    break;
                }
            }
            start = getOtherToken(properties, content, start, end, startElement);
        }

        // 正規表現による探索
        int count = properties.getKeywordCount();
        for (int i=0; i<count; i++ ) {
            Keyword keyword = properties.getKeyword(i);
            if (!keyword.isEnabled()) continue;
            if (!keyword.isRegex()) continue;
            if (keyword.getKeyword() == null || keyword.getKeyword().isEmpty()) continue;
            String regex = keyword.getKeyword();
            int flags = keyword.isSensitivecase() ? 0 : Pattern.CASE_INSENSITIVE;
            flags += Pattern.MULTILINE;

            // キーワード情報からスタイル属性の作成を行う。
            MutableAttributeSet attr = createStyleAttributeSet(keyword);

            // 正規表現検索
            Matcher m = Pattern.compile(regex, flags).matcher(content);
            while (m.find()) {
                if (m.groupCount() == 0) {
                    // スタイルの適用
                    setCharacterAttributes(m.start() + startElement, m.end() - m.start(), attr, false);
                }
                else if (m.groupCount() >= 1) {
                    // グループ化されている場合、最初(全体)は除外する。
                    for (int j = 1; j <= m.groupCount(); j++){
                        // スタイルの適用
                        setCharacterAttributes(m.start(j) + startElement, m.end(j) - m.start(j), attr, false);
                    }
                }
            }
        }
    }

    /**
     * キーワード
     * @param keyword		キーワード設定
     * @param startOffset			開始オフセット値
     * @param endOffset			終了オフセット値
     * @throws Exception		キーワード設定エラー
     */
    public void applyKeyword(final Keyword keyword, final int startOffset, final int endOffset) throws Exception {

        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                try {
                    // 開始、終了キャレット位置
                    int length = endOffset - startOffset;
                    String content = BatchDocument.this.getText(startOffset, length);

                    if (!keyword.isEnabled()) return;
                    if (keyword.getKeyword() == null || keyword.getKeyword().isEmpty()) return;

                    // キーワード情報からスタイル属性の作成を行う。
                    MutableAttributeSet attr = createStyleAttributeSet(keyword);

                    if (keyword.isRegex()) {
                        // 正規表現
                        setRegexAttributes(content, keyword.getKeyword(), startOffset, attr, keyword.isSensitivecase());
                    }
                    else if (keyword.isSearchVariable()) {
                        // 変数・トレース検索
                        setVariableAttributes(content, keyword.getKeyword(), startOffset, attr, keyword.isSensitivecase());
                    }
                    else {
//                        System.out.println("keyword=" + keyword.getKeyword() + ": word=" + keyword.isSearchWord()
//                                           + ": bold=" + keyword.getStyle() + "color=" + keyword.getForecolor());
                        // 文字列検索
                        setFindAttributes(content, keyword.getKeyword(), startOffset, attr, keyword.isSensitivecase(), keyword.isSearchWord());
                    }
                } catch (Exception ex) {
                    ex.printStackTrace();
                }
            }
        });
    }

    /**
     * 正規表現による一致文字列に対して、ハイライト設定を行う.
     * @param content		検索対象文字列
     * @param regex			正規表現
     * @param startOffset		開始オフセット値
     * @param attr				適用ハイライト設定
     * @param sensitivecase		true=大文字・小文字を区別する.
     */
    private void setRegexAttributes(String content, String regex, int startOffset, MutableAttributeSet attr, boolean sensitivecase) {
        if (content == null || content.isEmpty()) return;
        if (regex == null || regex.isEmpty()) return;
        if (attr == null) return;

        int flags = sensitivecase ? 0 : Pattern.CASE_INSENSITIVE;
        flags += Pattern.MULTILINE;

        // 正規表現検索
        Matcher m = Pattern.compile(regex, flags).matcher(content);
        while (m.find()) {
            if (m.groupCount() == 0) {
                // スタイルの適用
                setCharacterAttributes(m.start() + startOffset, m.end() - m.start(), attr, true);
            }
            else if (m.groupCount() >= 1) {
                // グループ化されている場合、最初(全体)は除外する。
                for (int j = 1; j <= m.groupCount(); j++){
                    // スタイルの適用
                    setCharacterAttributes(m.start(j) + startOffset, m.end(j) - m.start(j), attr, true);
                }
            }
        }
    }

    /**
     * 文字検索による一致文字列に対して、ハイライト設定を行う.
     * @param content		検索対象文字列
     * @param find			検索文字列
     * @param startOffset		開始オフセット値
     * @param attr				適用ハイライト設定
     * @param sensitivecase		true=大文字・小文字を区別する.
     * @param word		    true=単語検索

     */
    private void setFindAttributes(String content, String find, int startOffset, MutableAttributeSet attr,  boolean sensitivecase, boolean word) {
        if (content == null || content.isEmpty()) return;
        if (find == null || find.isEmpty()) return;
        if (attr == null) return;

        String contentText = content;
        String findText = find;
        if (!sensitivecase) {
            // 大文字・小文字を区別しないので、すべて小文字に変換する
            contentText = contentText.toLowerCase();
            findText = findText.toLowerCase();
        }
        // 文字列検索を行う
        int fromIndex = 0;
        int start = -1;
        while ((start = contentText.indexOf(findText, fromIndex)) != -1) {
            int end = start + findText.length();
            fromIndex = end;
            // 単語検索
            if (word) {
                // 検索結果の前後の文字がデリミタであるかチェックする
                if (start > 0) {
                    if (!isDelimiter(contentText.substring(start-1, start))) {
                        continue;
                    }
                }
                if (end < contentText.length()) {
                    String endChar = contentText.substring(end, end+1);
                    if (endChar != null && !endChar.isEmpty()) {
                        if (!isDelimiter(endChar)) {
                            continue;
                        }
                    }
                }
            }

            // スタイルの適用
            setCharacterAttributes(start + startOffset, findText.length(), attr, true);
        }

    }

    /**
     * 変数・トレース検索による一致文字列に対して、ハイライト設定を行う.
     * @param content		検索対象文字列
     * @param name			変数名
     * @param startOffset		開始オフセット値
     * @param attr				適用ハイライト設定
     * @param sensitivecase		true=大文字・小文字を区別する.
     *
     */
    private void setVariableAttributes(String content, String name, int startOffset, MutableAttributeSet attr,  boolean sensitivecase) {
        if (content == null || content.isEmpty()) return;
        if (name == null || name.isEmpty()) return;
        if (attr == null) return;

        // 文字列をデリミタで分解する
        String[] unuseddelimiters = {"%"}; 
        // 変数名に%が含まれていれば、デリミタから%を除外する。
        if (name.indexOf("%") < 0) {
        	unuseddelimiters = null;
        }
        List<String> list = StringUtils.tokenizer(content, unuseddelimiters);
        if (list == null || list.size() <= 0) return;

        int start = 0;
        for (String word : list) {
            if (word == null) {
                // \r, \nはnullでセットされる。
                start += 1;
                continue;
            }
            int len = word.length();
            if (len == 0) continue;
            if (word.trim().isEmpty()) {
                start += len;
                continue;
            }

            // コメントチェック
            if ("!".equals(word)) {
                // 行末までコメントであるので、以後適用無し
                break;
            }

            boolean match = false;
            if (!sensitivecase) {
                // 大文字。小文字を区別しない
                match = word.equalsIgnoreCase(name);
            }
            else {
                // 大文字。小文字を区別する
                match = word.equals(name);
            }

            if (match) {
                // スタイルの適用
                setCharacterAttributes(start + startOffset, len, attr, true);
            }

            // 開始インデックス
            start += len;
        }
    }


    /**
     * 単語の開始位置からキーワード設定を行う単語の探索を行う
     *
     * @param properties		キーワード設定情報
     * @param content			探索文字列
     * @param start				探索文字列開始位置
     * @param end				探索文字列終了位置
     * @param startElement		開始ドキュメントキャレット位置
     * @return					次回探索文字列開始位置
     */
    private int getOtherToken(KeywordProperties properties, String content, int start, int end, int startElement) {
        int endOfToken = start + 1;
        while ( endOfToken <= end ) {
            if ( isDelimiter( content.substring(endOfToken, endOfToken + 1) ) ) {
                break;
            }
            endOfToken++;
        }
        String token = content.substring(start, endOfToken);
        Keyword keyword = properties.getKeyword(token);

        // キーワードが有効 AND 正規表現ではないこと
        if (keyword != null && keyword.isEnabled() && !keyword.isRegex()) {
            // キーワード情報からスタイル属性の作成を行う。
            MutableAttributeSet attr = createStyleAttributeSet(keyword);
            // スタイルの適用
            setCharacterAttributes(start + startElement, endOfToken - start, attr, false);
        }
        return endOfToken + 1;
    }

    /**
     * 文字が区切り文字であるかチェックする。
     * @param character		チェック文字
     * @return			true=区切り文字
     */
    private boolean isDelimiter(String character) {
        if (character == null || character.isEmpty()) {
            return false;
        }
        return Character.isWhitespace(character.charAt(0)) || KscopeProperties.DELIMITER_CHARS.indexOf(character)!=-1;
    }

    /**
     * キーワード情報からスタイル属性の作成を行う。
     * @param keyword		キーワード情報
     * @return			スタイル属性
     */
    private MutableAttributeSet createStyleAttributeSet(Keyword keyword) {
        if (keyword == null) return null;
        MutableAttributeSet attr = new SimpleAttributeSet();
        if (keyword.getForecolor() != null) {
            StyleConstants.setForeground(attr, keyword.getForecolor());
        }
        if (keyword.getBackgroundcolor() != null) {
            StyleConstants.setBackground(attr, keyword.getBackgroundcolor());
        }
        int style = keyword.getStyle();
        if ((style & Font.BOLD) != 0) {
            StyleConstants.setBold(attr, true);
        }
        if ((style & Font.ITALIC) != 0) {
            StyleConstants.setItalic(attr, true);
        }

        return attr;
    }

    /**
     * 行、列位置の単語を取得する
     * @param rowIndex			キャレット行インデックス
     * @param columnIndex		列インデックス
     * @return		キャレット位置単語
     */
    public String getCaretWord(int rowIndex, int columnIndex) {

        try {
            // 選択行の文字列の取得
            Element root = getDefaultRootElement();
            int startOffset   = root.getElement( rowIndex ).getStartOffset();
            int endOffset   = root.getElement( rowIndex ).getEndOffset();
            String line = this.getText(startOffset, endOffset - startOffset);

            //BreakIterator
            BreakIterator boundary = BreakIterator.getWordInstance(Locale.ENGLISH);
            boundary.setText(line);
            int start = boundary.first();
            String word = null;
            for (int end = boundary.next(); end != BreakIterator.DONE;start = end, end = boundary.next()) {

                if (start <= columnIndex && columnIndex <= end) {
                    String str = line.substring(start, end);
                    if (str != null && !str.trim().isEmpty()) {
                        word = str.trim();
                    }
                }
            }
            return word;

        } catch (BadLocationException ex) {
            ex.printStackTrace();
        }

        return null;
    }


    /**
     * キャレット位置の単語を取得する
     * @param index			キャレットインデックス
     * @return		キャレット位置単語
     */
    public String getCaretWord(int index) {

        // 選択行の文字列の取得
        Element root = getDefaultRootElement();

        /* キャレットの位置にある行番号を取得 */
        int rowIndex = root.getElementIndex(index);

        int startOffset   = root.getElement( rowIndex ).getStartOffset();
        int columnIndex = index - startOffset;

        return getCaretWord(rowIndex, columnIndex);
    }

}



