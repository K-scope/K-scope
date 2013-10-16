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

import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.util.Date;

import jp.riken.kscope.utils.FileUtils;

/**
 * ソースファイルクラス ソースファイル名、ファイルタイプを持つ。
 *
 * @author riken
 *
 */
public class SourceFile implements Serializable {

	/** シリアル番号 */
    private static final long serialVersionUID = -1930551091523251203L;
    /** ソースファイル */
    private File m_file;
    /** 言語タイプ */
    private FILE_TYPE m_fileType;
    /** 更新日付 */
    private Date modifyDate;
    /** 関連ファイル:ソースファイルの場合は、XMLファイル, XMLファイルの場合はソースファイル */
    private SourceFile relationFile = null;

    /**
     * コンストラクタ
     * @param  file			ソースファイル
     * @param  fileType		言語タイプ
     */
    public SourceFile(File file, FILE_TYPE fileType) {
    	this.m_file = file;
    	this.m_fileType = fileType;
		try {
			setModifyDate(new Date(m_file.lastModified()));
		} catch (Exception e) {}

    }

    /**
     * コンストラクタ
     * @param  filename		ファイル名
     */
    public SourceFile(String filename) {
    	this.m_file = new File(filename);
    	this.m_fileType = FILE_TYPE.FILE_AUTO;
		try {
			setModifyDate(new Date(m_file.lastModified()));
		} catch (Exception e) {}
    }

    /**
     * コンストラクタ
     * @param  file			ソースファイル
     */
    public SourceFile(File file) {
    	this.m_file = file;
    	this.m_fileType = FILE_TYPE.FILE_AUTO;
		try {
			setModifyDate(new Date(m_file.lastModified()));
		} catch (Exception e) {}
    }

    /**
     * コピーコンストラクタ
     * @param source		コピー元ソースファイル
     */
    public SourceFile(SourceFile source) {
        if (source.m_file != null) {
            m_file = new File(source.m_file.getPath());
        }
        m_fileType = source.m_fileType;
        setModifyDate(source.modifyDate);
    }

    /**
     * ソースファイルを取得する。
     *
     * @return ソースファイル
     */
    public File getFile() {
        return m_file;
    }

    /**
     * ファイルタイプ
     * @return ファイルタイプ
     */
    public FILE_TYPE getFileType() {
        return m_fileType;
    }

    /**
     * ファイルタイプ
     * @param  type ファイルタイプ
     */
    public void setFileType(FILE_TYPE type) {
        m_fileType = type;
    }

    /**
     * ソースファイルのファイル名のみを返す。
     */
    @Override
    public String toString() {
        if (m_file == null)
            return "";
        return m_file.getName();
    }

    /**
     * ソースファイルのパス名を返す。
     * @return   ソースファイルパス名
     */
    public String getPath() {
        if (m_file == null) return null;
        return m_file.getPath();
    }

    /**
     * ソースファイルが等しいかチェックする。 m_fileが等しければ等しいとする。
     *
     * @param obj
     *            ソースファイル
     * @return true:等しい/false:等しくない
     */
    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof SourceFile))
            return false;
        SourceFile src = (SourceFile) obj;
        if (src == null || src.m_file == null)
            return false;
        if (this.m_file == null) return false;

        try {
            if (this.m_file.isAbsolute() && src.m_file.isAbsolute()) {
                return FileUtils.isEqualsFile(m_file, src.m_file);
            }
            else if (!this.m_file.isAbsolute() && !src.m_file.isAbsolute()) {
                return FileUtils.isEqualsFile(m_file, src.m_file);
            }
            else if (this.m_file.isAbsolute() && !src.m_file.isAbsolute()) {
                String thispath = trimPath(this.m_file.getPath());
                String srcpath = trimPath(src.m_file.getPath());
                return thispath.endsWith(srcpath);
            }
            else if (!this.m_file.isAbsolute() && src.m_file.isAbsolute()) {
                String thispath = trimPath(this.m_file.getPath());
                // String srcpath = src.m_file.getCanonicalPath();
                String srcpath = trimPath(src.m_file.getPath());
                return srcpath.endsWith(thispath);
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        return false;
    }

    /**
     * パス文字列の先頭から./を削除する
     * @param path		パス文字列
     * @return			'./'削除パス文字列
     */
    private String trimPath(String path) {
        if (path == null) return null;
        if (path.startsWith("./")) {
            return path.substring(2);
        }
        if (path.startsWith("." + File.separator)) {
            return path.substring(2);
        }
        return path;
    }


    /**
     * ソースファイル情報を出力する
     * @return		ソースファイル情報
     */
    public String toStringInfo() {
    	if (m_file != null)
    		return m_file.getName() + " [" + m_fileType.toString() + "]";
    	else
    		return null;
    }

    /**
     * ソースファイルを設定する
     * @param file		ソースファイル
     */
    public void setFile(File file) {
        this.m_file = file;
        this.m_fileType = FILE_TYPE.FILE_AUTO;
		try {
			setModifyDate(new Date(m_file.lastModified()));
		} catch (Exception e) {}
    }

	/**
	 * ファイルパス文字列のハッシュコードを返す。
	 * @return		ハッシュコード
	 */
	@Override
	public int hashCode() {
		if (this.m_file == null) return 0;
		int hash = this.m_file.getPath().hashCode();
		return hash;
	}

	/**
	 * 更新日付を取得する
	 * @return		更新日付
	 */
	public Date getModifyDate() {
		return modifyDate;
	}

	/**
	 * 更新日付を設定する.
	 * @param modifyDate		更新日付
	 */
	public void setModifyDate(Date modifyDate) {
		this.modifyDate = modifyDate;
	}

	/**
	 * 更新日付を設定する.
	 * @param file		更新ファイル
	 */
	public void setModifyDate(File file) {
		try {
			setModifyDate(new Date(file.lastModified()));
		} catch (Exception e) {}
	}

	/**
	 * 関連ファイルを取得する.
	 * ソースファイルの場合は、XMLファイル, XMLファイルの場合はソースファイルを取得する.
	 * @return		関連ファイル
	 */
	public SourceFile getRelationFile() {
		return this.relationFile;
	}

	/**
	 * 関連ファイルを設定する.
	 * ソースファイルの場合は、XMLファイル, XMLファイルの場合はソースファイルを設定する.
	 * @param file		関連ファイル
	 */
	public void setRelationFile(SourceFile file) {
		this.relationFile = file;
	}

	/**
	 * 更新日付が有効であるかチェックする.
	 * @return		true=更新日付が有効
	 */
	public boolean validateModifyDate() {
		if (this.modifyDate == null) return false;
		long time = this.modifyDate.getTime();
		return (time != 0);
	}

	/**
	 * 更新日付を更新する.
	 */
	public void updateModifyDate() {
		if (m_file == null) return;
		try {
			setModifyDate(new Date(m_file.lastModified()));
		} catch (Exception e) {}
	}
}

