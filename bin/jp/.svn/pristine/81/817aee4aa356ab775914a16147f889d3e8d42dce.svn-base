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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Observable;
import java.util.StringTokenizer;
import java.util.regex.Matcher;

import jp.riken.kscope.Message;
import jp.riken.kscope.data.Program;
import jp.riken.kscope.information.TextInfo;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.properties.ProgramProperties;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.utils.SwingUtils;

/**
 * 付加情報モデルクラス
 * @author riken
 */
public class InformationModel extends Observable implements PropertyChangeListener {

    /** 付加情報リスト */
    List<InformationNode> listInformation;

    /** 外部ツール設定プロパティ */
    private ProgramProperties propertiesProgram;

    /** プロジェクトフォルダ */
    private File projectFolder;

    /** タイトル */
    private String title;

    /**
     * 付加情報ノードクラス.
     * @author riken
     */
    private class InformationNode {
        /** 付加情報設定開始ノード */
        private IInformation startNode;
        /** 付加情報設定終了ノード */
        private IInformation endNode;
        /** 付加情報 */
        private TextInfo info;

        /**
         * コンストラクタ
         * @param node 付加情報設定ステートメント
         * @param info 付加情報
         */
        public InformationNode(IInformation node, TextInfo info) {
            super();
            this.startNode = node;
            this.endNode = node;
            this.info = info;
        }
        /**
         * コンストラクタ
         * @param snode 付加情報設定開始ステートメント
         * @param enode 付加情報設定終了ステートメント
         * @param info 付加情報
         */
        public InformationNode(IInformation snode, IInformation enode, TextInfo info) {
            super();
            this.startNode = snode;
            this.endNode = enode;
            this.info = info;
        }

        /**
         * 付加情報設定ノードを取得する
         * @return   付加情報設定ノード
         */
        public IInformation getNode() {
            return this.startNode;
        }
        /**
         * 付加情報設定開始ノードを取得する
         * @return   付加情報設定開始ノード
         */
        public IInformation getStartNode() {
            return this.startNode;
        }
        /**
         * 付加情報設定終了ノードを取得する
         * @return   付加情報設定終了ノード
         */
        public IInformation getendNode() {
            return this.endNode;
        }

        /**
         * 付加情報を取得する
         * @return		付加情報
         */
        public TextInfo getInfo() {
            return info;
        }

        /**
         * 付加情報を設定する
         * @param	info	付加情報
         */
        public void setInfo(TextInfo info) {
            this.info = info;
        }
    }

    /**
     * モデルの変更を通知する
     */
    private void notifyModel() {
        this.setChanged();
        this.notifyObservers();
        this.clearChanged();
    }

    /**
     * タイトルを取得する.
     * @return		付加情報タイトル
     */
    public String getTitle() {
        return this.title;
    }

    /**
     * タイトルを設定する.
     * @param title		付加情報タイトル
     */
    public void setTitle(String title) {
        this.title = title;
    }

    /**
     * 付加情報リスト数を取得する
     * @return		付加情報リスト数
     */
    public int getInformationListCount() {
        if (listInformation == null) return 0;
        return listInformation.size();
    }

    /**
     * 付加情報リストから指定インデックスの付加情報設定ノードを取得する
     * @param   index    インデックス
     * @return		付加情報設定ノード
     */
    public IInformation getInformationNode(int index) {
        if (this.listInformation == null) return null;
        if (this.listInformation.size() <= index) return null;
        return listInformation.get(index).getNode();
    }

    /**
     * 付加情報リストから指定インデックスの付加情報を取得する
     * @param   index    インデックス
     * @return		付加情報
     */
    public TextInfo getInformationInfo(int index) {
        if (this.listInformation == null) return null;
        if (this.listInformation.size() <= index) return null;
        return listInformation.get(index).getInfo();
    }


    /**
     * 付加情報のコンテンツを取得する
     * @param index		付加情報リストインデックス
     * @return		付加情報:コンテンツ
     */
    public String getInformationContent(int index) {
        if (listInformation == null) return null;
        if (listInformation.size() <= index) return null;
        if (listInformation.get(index) == null) return null;
        if (listInformation.get(index).getInfo() == null) return null;
        return listInformation.get(index).getInfo().getContent();
    }

    /**
     * 付加情報のHTML形式のコンテンツを取得する
     * @param index		付加情報リストインデックス
     * @return		付加情報:HTML形式のコンテンツ
     */
    public String getInformationHtmlContent(int index) {
        if (listInformation == null) return null;
        if (listInformation.size() <= index) return null;
        if (listInformation.get(index) == null) return null;
        if (listInformation.get(index).getInfo() == null) return null;

        if (propertiesProgram == null) {
            return listInformation.get(index).getInfo().getContent();
        }

        // 外部ツール設定リストの取得
        List<Program> list = this.propertiesProgram.getListProgram();
        if (list == null || list.size() <= 0) {
            return listInformation.get(index).getInfo().getContent();
        }

        // 付加情報文字列
        String content = listInformation.get(index).getInfo().getContent();

        // 外部ツールファイルをアンカータグに変換する
        content = createHtmlContent(content);

        return content;
    }

    /**
     * 文字列をHTMLアンカー文字列に変換する
     * @param content		付加情報
     * @return			HTML文字列
     */
    public String createHtmlContent(String content) {

        // 外部ツールファイルをアンカータグに変換する
        content = replaceAnchorTag(content);

        // 改行文字、空白、タブをHTMLコードに置換する
        content = StringUtils.textTohtml(content);

        content = "<html><body>\n" + content;
        content = content + "\n</body></html>";

        return content;
    }


    /**
     * 外部ツール設定によりアンカータグに置換する
     * @param content		付加情報
     * @return				アンカータグ置換付加情報
     */
    private String replaceAnchorTag(String content) {

        StringTokenizer st = new StringTokenizer(content, " \t\n\r\f\"　", true);
        List<String> list = new ArrayList<String>();
        boolean openquot = false;
        StringBuffer quotbuf = new StringBuffer();
        while (st.hasMoreTokens()) {
            String token = st.nextToken();
            if ("\"".equals(token)) {
                if (openquot) {
                    list.add(quotbuf.toString());
                    quotbuf = new StringBuffer();
                }
                list.add(token);
                openquot = !openquot;
            }
            else {
                if (openquot) {
                    quotbuf.append(token);
                }
                else {
                    list.add(token);
                }
            }
        }

        // 外部ツール設定によりアンカータグに置換する
        StringBuffer buf = new StringBuffer();
        for (String word : list) {
            // 外部ツール設定リストの取得
            List<Program> programs = this.propertiesProgram.getListProgram();
            if (programs == null || programs.size() <= 0) break;

            String atag = null;
            for (Program prog : programs) {
                // 外部ツールプログラム
                String program = null;
                String option = null;
                if (!prog.isRelation()) {
                    // 関連付けではないので、外部プログラム名の設定
                    program = prog.getExename();
                    if (prog.getOption() != null && !prog.getOption().isEmpty()) {
                        option = prog.getOption();
                    }
                }

                // 正規表現
                if (prog.isRegex()) {
                    atag = createAnchorTag(word, prog.getPattern(), program, option);
                }
                else {
                    // 拡張子
                    int count = prog.getPatternExtsCount();
                    for (int i=0; i<count; i++) {
                        String ext = prog.getPatternExt(i);
                        // 拡張子ファイルの検索正規表現
                        String pattern = "^.+\\." + ext + "$";
                        atag = createAnchorTag(word, pattern, program, option);
                        if (atag != null) break;
                    }
                }
                if (atag != null) break;
            }
            if (atag != null) buf.append(atag);
            else buf.append(word);
        }

        return buf.toString();
    }


    /**
     * 外部ツール設定ファイルをアンカータグで囲む.<br/>
     * 外部ツール設定ファイルではない場合は、nullを返す。
     * @param content		付加情報
     * @param regex			起動ファイル正規表現
     * @param program		外部プログラム
     * @param option		起動オプション
     * @return				アンカータグ変換付加情報
     */
    private String createAnchorTag(String content, String regex, String program, String option) {

        if (content == null || content.isEmpty() || content.trim().isEmpty()) {
            return null;
        }

        // 正規表現によるマッチング
        java.util.regex.Pattern pattern = java.util.regex.Pattern.compile(
                                                regex,
                                                java.util.regex.Pattern.CASE_INSENSITIVE+java.util.regex.Pattern.MULTILINE);
        Matcher m = pattern.matcher(content);
        if (!m.find()) return null;

        // アンカータグの挿入
        String file = null;
        int start = 0;
        int end = 0;
        if (m.groupCount() == 0) {
            file = m.group();
            start = m.start();
            end = m.end();
        }
        else if (m.groupCount() >= 1) {
            // グループ化されている場合、最初(全体)は除外する。
            file = m.group(m.groupCount());
            start = m.start(m.groupCount());
            end = m.end(m.groupCount());
        }
        file = file.trim();

        // URL,ファイル名をURL文字列に変換する
        String href = toURL(file);

        // hrefの組み立て
        String anchor = "<a href='" + href + "' ";

        // 起動プログラム名はclass属性にセットする
        if (program != null) {
            anchor += "class='" + program + "' ";
        }
        // 起動オプションはcomment属性にセットする
        if (option != null) {
            anchor += "comment='" + option + "' ";
        }
        anchor += ">" + file + "</a>";

        // アンカータグの置換
        content = content.substring(0, start) + anchor + content.substring(end);

        return content;
    }

    /**
     * URL, ファイル名をURL書式文字列に変換する
     * @param name		url又はファイル名
     * @return			URL書式文字列
     */
    private String toURL(String name) {
        if (name == null || name.isEmpty()) return null;

        URL url = null;
        try {
            URI uri = new URI(name);

            url = uri.toURL();

        } catch (Exception e) {
            try {
                File file = new  File(name);
                if (!file.isAbsolute() && projectFolder != null) {
                    file = new File(projectFolder, name);
                }
                URI uri = file.toURI();
                url = uri.toURL();
            } catch (MalformedURLException e1) {
            }
        }

        if (url == null) return null;

        return url.toString();
    }



    /**
     * モデル情報のエクスポートを行う
     * @param file			出力ファイル
     */
    public void writeFile(File file) {

        if (this.listInformation == null || this.listInformation.size() <= 0) return;

        try {
            // ファイル出力
            PrintWriter pw = new PrintWriter(new BufferedWriter(new FileWriter(file)));
            // ヘッダー出力
            pw.println(Message.getString("informationmodel.file.header")); //コード, 付加情報

            // 付加情報を出力する
            for (InformationNode node : this.listInformation) {
                // 名前
                String name = node.getNode().toString();
                // 付加情報
                String content = node.getInfo().getContent();
                content = content.trim();

                // 出力
                pw.print(SwingUtils.escapeCsv(name));
                pw.print(",");
                pw.print(SwingUtils.escapeCsv(content));

                // 改行追加
                pw.println();
            }

            pw.close();

        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }

    /**
     * 外部ツール設定プロパティの変更イベント
     * @param event		イベント情報
     */
    @Override
    public void propertyChange(PropertyChangeEvent event) {

        // 外部ツール設定プロパティの変更
        if (event.getNewValue() instanceof ProgramProperties) {
            propertiesProgram = (ProgramProperties)event.getNewValue();

            // 付加情報リストの設定を通知する
            notifyModel();
        }
    }

    /**
     * 外部ツール設定プロパティを設定する
     * @param properties		外部ツール設定プロパティ
     */
    public void setPropertiesExtension(ProgramProperties properties) {
        this.propertiesProgram = properties;
    }

    /**
     * プロジェクトフォルダを設定する
     * @param folder		プロジェクトフォルダ
     */
    public void setProjectFolder(File folder) {
        this.projectFolder = folder;
    }

    /**
     * 付加情報を設定する
     * @param node		付加情報設定ノード
     * @param info			付加情報
     */
    public void setInformation(IInformation node, TextInfo info) {
        // モデルのクリア
        clearInformation();

        if (node == null) return;
        if (info == null) return;

        // 付加情報が存在するか
        if (info == null || info.getContent() == null || info.getContent().isEmpty()) return;

        // 付加情報を追加する
        addInformation(node, info);
    }


    /**
     * 付加情報を設定する
     * @param node		付加情報設定ノード
     */
    public void setInformation(IInformation node) {
        // モデルのクリア
        clearInformation();

        if (node == null) return;
        setInformation(node, node.getInformation());
    }

    /**
     * 付加情報を追加する
     * @param node		付加情報設定ノード
     */
    public void addInformation(IInformation node) {
        if (node == null) return;

        // 付加情報が存在するか
        TextInfo info = node.getInformation();
        if (info == null || info.getContent() == null || info.getContent().isEmpty()) return;

        // 付加情報を追加する
        addInformation(node, info);
    }


    /**
     * 付加情報を追加する
     * @param node		付加情報設定ノード
     * @param info			付加情報
     */
    public void addInformation(IInformation node, TextInfo info) {
        this.addInformation(node, node, info);
    }
    /**
     * 付加情報を追加する
     * @param snode		付加情報設定開始ノード
     * @param enode		付加情報設定終了ノード
     * @param info			付加情報
     */
    public void addInformation(IInformation snode, IInformation enode, TextInfo info) {
        if (snode == null) return;

        // 既存の付加情報リストから検索する
        InformationNode infonode = getInformationNode(snode, enode);
        if (infonode != null) {
            // 付加情報を設定する
            infonode.setInfo(info);
        }
        else {

            // 存在しないので、追加する。
            if (this.listInformation == null) {
                this.listInformation = new ArrayList<InformationNode>();
            }
            infonode = new InformationNode(snode, enode, info);
            this.listInformation.add(infonode);
        }

        // 付加情報リストの設定を通知する
        notifyModel();
    }

    /**
     *  付加情報を取得する
     * @param snode		設定開始ノード
     * @param enode		設定終了ノード
     * @return		付加情報
     */
    private InformationNode getInformationNode(IInformation snode, IInformation enode) {
        if (this.listInformation == null) return null;

        for (InformationNode infonode : this.listInformation) {
            if (infonode.getStartNode() == snode) {
                if (infonode.getendNode() == null) {
                    return infonode;
                } else if (infonode.getendNode() == enode) {
                    return infonode;
                }
            }
        }

        return null;
    }

    /**
     * テーブルモデルをクリアする。
     */
    public void clearInformation() {
        this.listInformation = new ArrayList<InformationNode>();
        this.title = null;

        // 付加情報リストの設定を通知する
        notifyModel();
    }

    /**
     * モデルが空か否か
     * @return	空か否か（ture: 空，false: データあり）
     */
    public boolean isEmpty() {
    	if (this.listInformation == null) return true;
    	return (this.listInformation.size() < 1);
    }
}


