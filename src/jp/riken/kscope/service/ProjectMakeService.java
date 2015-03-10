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
package jp.riken.kscope.service;

import java.io.File;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JOptionPane;

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.data.FILE_TYPE;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.exception.LanguageException;
import jp.riken.kscope.information.InformationBlock;
import jp.riken.kscope.information.InformationBlocks;
import jp.riken.kscope.language.BlockType;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.IBlock;
import jp.riken.kscope.language.IInformation;
import jp.riken.kscope.language.Module;
import jp.riken.kscope.language.ProgramUnit;
import jp.riken.kscope.language.utils.LanguageVisitor;
import jp.riken.kscope.language.utils.ValidateLanguage;
import jp.riken.kscope.model.ProjectModel;
import jp.riken.kscope.properties.ProjectProperties;
import jp.riken.kscope.properties.RemoteBuildProperties;
import jp.riken.kscope.utils.Logger;
import jp.riken.kscope.utils.SwingUtils;
import jp.riken.kscope.xcodeml.XcodeMLParserStax;


/**
 * Makefileの実行クラス.
 * Makefileを実行し、データベースの再構築を行う.
 * @author RIKEN
 */
public class ProjectMakeService  extends BaseService {
	/** makeコマンド実行フォルダ */
	private File workdirectory;
	/** makeコマンド出力ストリーム */
	private OutputStream outStream;
	/** プロジェクトモデル */
	private ProjectModel projectModel;
    /** フォートラン構文解析結果格納データベース:現在のデータベース. */
    private Fortran currentDb = null;
    /** XMLファイル検索パスリスト */
    private List<File> listSearchPath;
    /** スレッド実行フラグ true:実行継続/false:中止. */
    private boolean m_running = true;
    
    private AppController controller;

    /**
     * コンストラクタ.
     * @param BUILD_COMMAND            Makeコマンド
     * @param work            Makeコマンド実行フォルダ
     * @param controller	AddController
     */
    public ProjectMakeService(File work, AppController controller) {
    	this.workdirectory = work;
        this.controller = controller;
    }

    /**
     * 構造情報再実行を行う.
     * @return 		成否
     */
    public boolean rebuild()  {
        Application.status.setProgressStart(true);
        if (this.listSearchPath == null || this.listSearchPath.size() <= 0) {
        	this.addErrorInfo("can not set search path list.");
        	return false;
        }

		try {
			// XMLファイルリスト取得
			ProjectService service = new ProjectService();
			SourceFile[] listBuildXml = service.getSourceFiles(this.listSearchPath.toArray(new File[0]),
														FILE_TYPE.XCODEML_XML,
														true);
			// ソースファイルが存在するかチェックする.
			listBuildXml = validateXmlFiles(listBuildXml);
			if (listBuildXml == null || listBuildXml.length <= 0) {
				// 中間コード(XML)がありませんでした。
				String msg = Message.getString("projectmakeservice.rebuild.notexists.xml");
				this.addErrorInfo(msg);
				return false;
			}

			// 現在のファイルリスト
			List<SourceFile> listCurrentSrc = this.currentDb.getSourceFileList();
			// 現在のXMLファイルリスト
			List<SourceFile> listCurrentXml = new ArrayList<SourceFile>();
			for (SourceFile src : listCurrentSrc) {
				SourceFile xml = src.getRelationFile();
				listCurrentXml.add(xml);
			}
			// 更新ファイルリスト
			SourceFile[] listUpdate = getUpdateFiles(listCurrentXml.toArray(new SourceFile[0]), listBuildXml);
			SourceFile[] listDelete = getDeleteFiles(listCurrentXml.toArray(new SourceFile[0]), listBuildXml);
			if ( (listUpdate == null || listUpdate.length <= 0)
				&& (listDelete == null || listDelete.length <= 0) ) {
				// 更新ファイルがありませんでした。
				String msg = Message.getString("projectmakeservice.rebuild.notexists.updatefile");
				this.addErrorInfo(msg);
				return false;
			}

			// 更新ファイルのパースを行う.
			Fortran buildDb = parseSourceFile(listUpdate);

			// データベースのモジュールコピーを行う
			copyModules(buildDb, this.currentDb, listDelete);

			// 変数定義の関連付けを行う.
			buildDb.analyseDB();

            // データベースの検証を行う.
            ValidateLanguage validate = new ValidateLanguage(buildDb);
            LanguageVisitor visitor = new LanguageVisitor(validate);
            visitor.entry();
            int error = validate.analyseTypes();
            if (error > 0) {
            	this.getErrorInfoModel().addErrorInfos(validate.getErrorList());
            	String msg = Message.getString("validatelanguage.final.error", error);
            	this.addErrorInfo(msg);
            }

			// 作成データベースを元のデータベースにコピーする
			this.currentDb.copyShallow(buildDb);

			// XMLファイルリストを設定する
			this.projectModel.setListXmlFile(listBuildXml);

	        return true;

        } catch (LanguageException lang_ex) {
            Logger.error(lang_ex);
            Logger.error(lang_ex.getCodeInfo());
            lang_ex.printStackTrace();
            // エラー箇所の情報をセットする
            this.addErrorInfo(lang_ex);
	        return false;
        } catch (InterruptedException ex) {
            // キャンセルによる終了 : projectmakeservice.rebuild.cancel=キャンセルにより中断しました。
        	String msg = Message.getString("projectmakeservice.rebuild.cancel");
            this.addErrorInfo(msg);
            Application.status.setMessageStatus(msg);
            return false;
        } catch (Exception ex) {
            Logger.error(ex);
            ex.printStackTrace();

            String error_message = ex.getMessage();
            if (error_message == null) {
                error_message = ex.toString();
            }
            // エラー箇所の情報をセットする
            this.addErrorInfo(error_message);
            return false;
        } finally {
        	Application.status.setProgressStart(false);
        }

    }

    /**
     * XMLファイルを検証する.
     * ソースファイルが存在するかチェックする.
     * @param xmlfiles
     * @return		フォートランソースファイルリスト
     */
	private SourceFile[] validateXmlFiles(SourceFile[] xmlfiles) {
		if (xmlfiles == null) return null;

        // XMLパーサの作成
        XcodeMLParserStax fortranParser = new XcodeMLParserStax();
		List<SourceFile> list = new ArrayList<SourceFile>();
		for (SourceFile xml : xmlfiles) {
            try {
				if (xml.getRelationFile() != null && xml.getRelationFile().getFile() != null) {
					SourceFile file = xml.getRelationFile();
					if (file.getFile().exists()) {
						list.add(xml);
					}
					continue;
				}
				// XMLファイルをパースする.
	            fortranParser.readFile(xml);
	            fortranParser.parseSourceFile();
	            SourceFile source = fortranParser.getLanguageFile();
	            if (source != null && source.getFile() != null) {
	            	if (source.getFile().exists()) {
	            		list.add(xml);
	            	}
				}

            } catch (LanguageException lang_ex) { }
		}

		if (list.size() <= 0) return null;
		return list.toArray(new SourceFile[0]);
	}

	/**
     * 削除ファイルリストを取得する.
     * @param currents		現在のファイルリスト
     * @param builds			再構築のファイルリスト
     * @return					削除ファイルリスト
     */
    private SourceFile[] getDeleteFiles(SourceFile[] currents, SourceFile[] builds) {
    	if (currents == null || currents.length <= 0) return null;
    	if (builds == null || builds.length <= 0) return null;

    	// 削除ファイルリスト
    	List<SourceFile> listDelete = new ArrayList<SourceFile>();
    	List<SourceFile> listCurrent = Arrays.asList(currents);
    	List<SourceFile> listBuild = Arrays.asList(builds);
    	for (SourceFile file : listCurrent) {
    		if (!listBuild.contains(file)) {
    			listDelete.add(file);
    		}
    	}
    	if (listDelete.size() <= 0) {
    		return null;
    	}

		return listDelete.toArray(new SourceFile[0]);
	}

    /**
     * 更新ファイルリストを取得する.
     * 更新日付が変更されているファイル、追加されたファイルを取得する
     * @param currents		現在のファイルリスト
     * @param builds			再構築のファイルリスト
     * @return					更新・追加ファイルリスト
     */
	private SourceFile[] getUpdateFiles(SourceFile[] currents, SourceFile[] builds) {
    	if (builds == null || builds.length <= 0) return null;
    	if (currents == null || currents.length <= 0) return builds;

    	// 更新ファイルリスト
    	List<SourceFile> listUpdate = new ArrayList<SourceFile>();
    	List<SourceFile> listCurrent = Arrays.asList(currents);
    	List<SourceFile> listBuild = Arrays.asList(builds);
    	for (SourceFile file : listBuild) {
    		// 追加ファイル
    		if (!listCurrent.contains(file)) {
    			listUpdate.add(file);
    			continue;
    		}
    		// 更新日付チェック
    		Date currentDate = null;
    		Date buildDate = file.getModifyDate();
    		int idx = listCurrent.indexOf(file);
    		if (idx >= 0) {
    			SourceFile currentFile = listCurrent.get(idx);
        		if (currentFile != null) {
        			currentDate = currentFile.getModifyDate();
        		}
    		}
    		if (currentDate == null) {
    			listUpdate.add(file);
    		}
    		else if (buildDate == null) {
    			listUpdate.add(file);
    		}
    		else if (!currentDate.equals(buildDate)) {
    			listUpdate.add(file);
    		}
    	}
    	if (listUpdate.size() <= 0) {
    		return null;
    	}

		return listUpdate.toArray(new SourceFile[0]);
	}

    /**
     * スレッドの実行をキャンセルする。
     */
    public void cancelRunning() {
        m_running = false;
    }

    /**
     * スレッドの実行がキャンセルであるかチェックする
     * @return    true=キャンセル
     */
    public boolean isCancel() {
        return !this.m_running;
    }

    /**
     * makeコマンド出力ストリームを設定する.
     * @param out		makeコマンド出力ストリーム
     */
	public void setOutputStream(OutputStream out) {
		this.outStream = out;
	}

	/**
	 * フォートランデータベースを設定する
	 * @param fortran		フォートランデータベース
	 */
	public void setFortranLanguage(Fortran fortran) {
		this.currentDb = fortran;
	}

	/**
	 * makeコマンドを実行する.
	 * @return		true = 正常終了、又は継続実行
	 * @throws Exception
	 */
	public boolean executeMakeCommand() throws Exception {
		// ステータスメッセージ
        Application.status.setProgressStart(true);
        ProjectProperties pproperties = this.controller.getPropertiesProject();
        RemoteBuildProperties rb_properties = this.controller.getRBproperties();
        String build_command = pproperties.getBuildCommand(); 
        if (build_command == null || build_command.length() <= 0) return false;
        String[] exec_commands = null;
        
        
        if (useServer(pproperties, rb_properties)) {
        	if (this.controller.haveDIAAS()) {
	        	// inject remote build command
	        	String[] diaas_cl = rb_properties.getCommandLineOptions();
	        	int formal_commands = 1;
	        	exec_commands = new String [formal_commands + diaas_cl.length];
	        	exec_commands[0] = "./makeRemote.sh";
	        	for (int i = 0; i < diaas_cl.length; i++) {
	        		exec_commands[i + formal_commands] = diaas_cl[i];
	        	}
        	}
        	else if (this.controller.haveSSHconnect()) {
        		// inject remote build command
	        	String[] sshc_cl = rb_properties.getCommandLineOptions();
	            int formal_commands = 3;
	            exec_commands = new String [formal_commands + sshc_cl.length];
	            exec_commands[0] = "java";
	            exec_commands[1] = "-jar";
	            exec_commands[2] = "SSHconnect.jar";
	            for (int i = 0; i < sshc_cl.length; i++) {
	                exec_commands[i + formal_commands] = sshc_cl[i];
	            }	        	
        	}
        	else {
        		/*
        		 * This case should never happen.
        		 * useServer() and remote_build should only be set to TRUE
        		 * in case either makeRemote or SSHconnect are present.
        		 */
        		Exception ex = new Exception("Incosistent settings:\nProjectProperties.useServer() " + pproperties.useServer()
        				+ "\nRemoteBuildProperties.remote_build "+rb_properties.remote_build
        				+ "\nAppController.haveDIAAS() "+ this.controller.haveDIAAS()
        				+ "\nAppController.haveSSHconnect() "+ this.controller.haveSSHconnect());
        		throw ex;
        	}
        } 
        Application.status.setMessageStatus(build_command);

        // makeコマンド実行
    	int result = -1;
		try {
			if (useServer(pproperties, rb_properties)) result = SwingUtils.processRun(exec_commands, this.workdirectory, this.outStream);
			else result = SwingUtils.processRun(build_command.split(" "), this.workdirectory, this.outStream);
			if (result != 0) { // 中間コードの生成に失敗した場合は継続するか確認
				if (JOptionPane.showConfirmDialog(null,
						Message.getString("projectmakeservice.executemakecommand.continue.message"),
						Message.getString("projectmakeservice.executemakecommand.error"),
						JOptionPane.YES_NO_OPTION) != JOptionPane.YES_OPTION) {
					// ステータスメッセージ
			        Application.status.setProgressStart(false);
					return false;
				}
			}
			Application.status.setProgressStart(false);
			return true;
		} catch (Exception ex) {
			ex.printStackTrace();
			throw ex;
		}
	}

	/**
	 * @param pproperties
	 * @param rb_properties
	 * @return
	 */
	private boolean useServer(ProjectProperties pproperties, RemoteBuildProperties rb_properties) {
		if (rb_properties == null || pproperties == null) return false;
		return rb_properties.remote_build && pproperties.useServer();
	}

	/**
	 * XMLファイル検索パスリストを設定する.
	 * @param list    XMLファイル検索パスリスト
	 */
	public void setListSearchPath(List<File> list) {
		this.listSearchPath = list;
	}

	/**
	 * XMLファイルをパースする.
	 * @param updateFiles		更新XMLファイルリスト
	 * @return  フォートランデータベース
	 * @throws InterruptedException		割り込み例外
	 */
	private Fortran parseSourceFile(SourceFile[] updateFiles) throws InterruptedException {

        // フォートランデータベース
        Fortran fortranDb = new Fortran();
        if (updateFiles == null) return fortranDb;

        // XMLパーサの作成
        XcodeMLParserStax fortranParser = new XcodeMLParserStax();
        ArrayList<SourceFile> sourceFileList = new ArrayList<SourceFile>();
        for (SourceFile file : updateFiles) {
            try {
                String filename = file.toString();
                Application.status.setMessageStatus(filename);

                // ソースファイルからファイルを読み込む
                fortranParser.readFile(file);

                // 読込コード行を構文解析する。
                fortranParser.parseFile(fortranDb);

                // オリジナルフォートランソースファイルの取得
                sourceFileList.add(fortranParser.getLanguageFile());

                // パースエラーの取得
                if (fortranParser.getErrorInfos() != null) {
                	this.addErrorInfos(fortranParser.getErrorInfos());
                }
            } catch (Exception lang_ex) {
                // エラー箇所の情報をセットする
                this.addErrorInfo(lang_ex);
            }

            // キャンセルチェック
            if (this.isCancel()) {
                return null;
            }
        }

        // ソースファイルリストの設定
        fortranDb.setSourceFileList(sourceFileList);

        return fortranDb;
	}

	/**
	 * モジュールコピーを行う.
	 * @param buildDb		ビルドデータベース
	 * @param originalDb		元データベース
	 * @param deletes	削除XMLリスト
	 * @return				true=success
	 */
    private boolean copyModules(Fortran buildDb, Fortran originalDb, SourceFile[] deletes) {
    	if (buildDb == null) return false;
    	if (originalDb == null) return false;
    	Map<String, Module> buildModules = buildDb.getModules();
    	Map<String, Module> currentModules = originalDb.getModules();
    	if (currentModules == null || currentModules.size() <= 0) {
    		return true;
    	}
    	// 削除対象XMLファイルリスト
    	List<SourceFile> listDelete = null;
    	if (deletes != null && deletes.length > 0) {
    		listDelete = Arrays.asList(deletes);
    	}

		// 削除モジュールリスト
		List<Module> deleteModules = new ArrayList<Module>();
		// 更新モジュールマップ <旧モジュール, 新モジュール>
		HashMap<Module, Module> mapUpdate = new HashMap<Module,Module>();

		// 現在のデータベースからビルドデータベースへモジュールのコピーを行う.
    	CURRENT_LOOP : for ( String key : currentModules.keySet() ) {
    		Module module = currentModules.get( key );
    		if (module == null) continue;
    		// モジュールのコンパイルXMLファイルを取得する.
    		SourceFile xml = getXmlFile(module, originalDb.getSourceFileList());
    		// 削除対象のモジュールであるか
    		if (xml != null && listDelete != null && listDelete.contains(xml)) {
		    	deleteModules.add(module);
		    	continue;
    		}
    		// 更新対象のモジュールであるか
    		if (xml != null) {
    	    	for ( String buildKey : buildModules.keySet() ) {
    	    		Module buildModule = buildModules.get( buildKey );
    	    		if (buildModule == null) continue;
    	    		// モジュールのコンパイルXMLファイルを取得する.
    	    		SourceFile buildXml = getXmlFile(buildModule, buildDb.getSourceFileList());
    	    		if (xml.equals(buildXml)) {
    	    			// 更新モジュール
    	    			mapUpdate.put(module, buildModule);
    	    			continue CURRENT_LOOP;
    	    		}
    	    	}
    		}
    		// 同一モジュール名が存在するか？
    		Module updateModule = buildDb.module(module.get_name());
    		if (updateModule != null) {
                if ("NO_MODULE".equalsIgnoreCase(updateModule.get_name()) ) {
                	if (updateModule.isEmpty()) {
                		updateModule = null;
                	}
                }
    		}
    		if (updateModule != null) {
    			// 更新モジュール
    			mapUpdate.put(module, updateModule);
    			continue;
    		}
    		// モジュールコピー
    		buildDb.addModule(module);
    	}

		// メインプログラム名のコピー
		if (buildDb.getMainName() == null || buildDb.getMainName().isEmpty()) {
			buildDb.setMainName(originalDb.getMainName());
		}

	    // ソースファイルリストのコピー
	    ArrayList<SourceFile> listOrgFile = originalDb.getSourceFileList();
	    ArrayList<SourceFile> listBuildFile = buildDb.getSourceFileList();
	    if (listOrgFile != null) {
		    if (listBuildFile == null) {
		    	listBuildFile = new ArrayList<SourceFile>();
		    	buildDb.setSourceFileList(listBuildFile);
		    }
	    	for (SourceFile file : listOrgFile) {
	    		SourceFile xml = file.getRelationFile();
	    		if (listDelete != null) {
	    			if (listDelete.contains(xml)) {
	    				continue;
	    			}
	    		}
	    		if (!listBuildFile.contains(file)) {
	    			listBuildFile.add(file);
	    		}
	    	}
	    }

	    // COMMONマップ
	    Map<String, List<ProgramUnit>> mapCommon = originalDb.getCommonMap();
	    if (mapCommon != null) {
	    	CURRENT_LOOP : for ( String key : mapCommon.keySet() ) {
	    		List<ProgramUnit> list = mapCommon.get( key );
		    	for ( ProgramUnit unit : list ) {
		    		// モジュール、サブルーチンが削除モジュールリストに含まれているか？
		    		for (Module module : deleteModules) {
		    			if (module.containsChildren(unit)) {
		    				continue CURRENT_LOOP;
		    			}
		    		}
		    		// モジュール、サブルーチンが更新モジュールリストに含まれているか？
		    		for ( Module module : mapUpdate.keySet() ) {
			    		if (module.containsChildren(unit)) {
			    			// 更新モジュールに含まれているので、パースでcommonMapに追加されているはずである。
		    				continue CURRENT_LOOP;
			    		}
		    		}
		    		// 削除、更新モジュール、サブルーチンでないので、モジュールコピーで存在しているはずである。
		    		boolean existsModule = false;
	    	    	for ( String buildKey : buildModules.keySet() ) {
	    	    		Module buildModule = buildModules.get( buildKey );
	    	    		if (buildModule == null) continue;
			    		if (buildModule.containsChildren(unit)) {
			    			existsModule = true;
			    			break;
			    		}
	    	    	}
	    	    	if (!existsModule) {
	    	    		// COMMON文の宣言モジュールが見つからない
	    	    		throw new LanguageException("not found ProgramUnit of COMMON[name=" + key + "].", unit.getStartCodeLine());
	    	    	}

	    	    	// COMMON文追加
	    	    	buildDb.addCommonMap(key, unit);
		    	}
	    	}
	    }

	    // 変数定義、サブルーチン参照をクリアする
	    // ClearDefinitions validate = new ClearDefinitions(buildDb);
	    // validate.setListClearModule(mapUpdate);
        // LanguageVisitor visitor = new LanguageVisitor(validate);
        // visitor.entry();

		return true;
	}

    /**
     * モジュールのコンパイルXMLファイルを取得する.
     * @param module		モジュール
     * @param list		ソースファイルリスト
     * @return		XMLファイル
     */
    private SourceFile getXmlFile(Module module, List<SourceFile> list) {
    	if (module == null) return null;
		if (module.getStartCodeLine() == null) return null;
		SourceFile src = module.getStartCodeLine().getSourceFile();
    	if (src == null) return null;
	    SourceFile xml = src.getRelationFile();
	    if (xml == null && list != null) {
	    	for (SourceFile file : list) {
	    		if (src.equals(file)) {
	    			xml = file.getRelationFile();
	    			break;
	    		}
	    	}
	    }
    	return xml;
    }

	/**
	 * 付加情報のProgramUnitを取得する.
	 * @param info		コピー付加情報
	 * @param destDb		コピー先モジュール
	 * @param srcDb		    コピー元モジュール
	 * @return				付加情報のProgramUnit[programDest, programSrc]
	 */
    @SuppressWarnings("unused")
	private ProgramUnit[] getInformationProgramUnits(IInformation info, Fortran destDb, Fortran srcDb) {
    	if (info == null) return null;
    	if (destDb == null) return null;
    	if (srcDb == null) return null;

    	Map<String, Module> srcModules = srcDb.getModules();
    	if (srcModules == null || srcModules.size() <= 0) {
    		return null;
    	}
    	ProgramUnit[] programInfos = null;
    	for ( String key : srcModules.keySet() ) {
    		Module srcModule = srcModules.get( key );
    		if (srcModule == null) continue;
    		Module destModule = destDb.module( key );
    		if (destModule == null) continue;

			// (1) 更新対象のモジュールであるか = [programDest, programSrc]
			programInfos = getInformationProgramUnits(info, destModule, srcModule);
			if (programInfos != null) {
				break;
			}
    	}
    	return programInfos;
    }

	/**
	 * 付加情報のProgramUnitを取得する.
	 * @param info		コピー付加情報
	 * @param destModule		コピー先モジュール
	 * @param srcModule		    コピー元モジュール
	 * @return				付加情報のProgramUnit[programDest, programSrc]
	 */
    private ProgramUnit[] getInformationProgramUnits(IInformation info, Module destModule, Module srcModule) {
    	if (info == null) return null;
    	if (destModule == null) return null;
    	if (srcModule == null) return null;

        // ネームスペース（モジュール＋サブルーチン）のProgramUnitを取得する
    	String namespace = info.getNamespace();
    	if (namespace == null || namespace.isEmpty()) {
    		return null;
    	}
        ProgramUnit programDest = getProgramUnitByNamespace(destModule, namespace);
        ProgramUnit programSrc = getProgramUnitByNamespace(srcModule, namespace);

        if (programDest == null || programSrc == null) {
        	return null;
        }
        return new ProgramUnit[]{programDest, programSrc};
    }

	/**
     * ネームスペース（モジュール＋サブルーチン）のProgramUnitを取得する
     * @param destDb		検索対象データベース
     * @param namespace		検索ネームスペース
     * @return				ProgramUnit
     */
    @SuppressWarnings("unused")
    private ProgramUnit getProgramUnitByNamespace(Fortran destDb, String namespace) {
    	if (destDb == null) return null;
    	Map<String, Module> modules = destDb.getModules();
    	if (modules == null || modules.size() <= 0) {
    		return null;
    	}

    	ProgramUnit unit = null;
    	for ( String key : modules.keySet() ) {
    		Module module = modules.get( key );
    		unit = module.findProgramUnitBy(namespace);
    		break;
    	}
    	return unit;
    }

    /**
     * ネームスペース（モジュール＋サブルーチン）のProgramUnitを取得する
     * @param destModule		検索対象モジュール
     * @param namespace		検索ネームスペース
     * @return				ProgramUnit
     */
    private ProgramUnit getProgramUnitByNamespace(Module destModule, String namespace) {
    	if (destModule == null) return null;
    	ProgramUnit unit = destModule.findProgramUnitBy(namespace);
    	return unit;
    }


	/**
	 * InformationBlocksにInformationBlockが含まれているかチェックする.
	 * @param infos		InformationBlocks(=InformationBlockリスト)
	 * @param src		検索InformationBlock
	 * @return			true=含まれる
	 */    
	@SuppressWarnings("unused")
	private boolean containsInformationBlock(InformationBlocks infos, InformationBlock src) {
		if (infos == null) return false;
		if (src == null) return false;

		for (InformationBlock info : infos) {
			if (src.getStartBlock() != info.getStartBlock()) continue;
			if (src.getEndBlock() != info.getEndBlock()) continue;
			return true;
		}

		return false;
	}

	/**
	 * DO,IF,SELECT文であるかチェックする.
	 * @param  info   付加情報ブロック
	 * @return    DO,IF,SELECT文
	 */
	@SuppressWarnings("unused")
	private boolean isBlock(IInformation info) {
		if (!(info instanceof IBlock)) return false;
		if (((IBlock)info).getBlockType() == BlockType.SELECTION
			|| ((IBlock)info).getBlockType() == BlockType.REPETITION
			|| ((IBlock)info).getBlockType() == BlockType.CONDITION) {
			return true;
		}

		return false;

	}

	/**
	 * 同じブロック階層であるかチェックする.
	 * 親ブロック階層が同じであるかチェックする.
	 * 親ブロックの種別のみチェックする.
	 * @param destinfo		ブロック１
	 * @param srcinfo		ブロック２
	 * @return			true=同一ブロック構造
	 */
	@SuppressWarnings("unused")
	private boolean equalsParentLayout(IInformation destinfo, IInformation srcinfo) {
		if (destinfo == null) return false;
		if (srcinfo == null) return false;
		if (!(destinfo instanceof IBlock)) return false;
		if (!(srcinfo instanceof IBlock)) return false;

		if (((IBlock)srcinfo).getBlockType() != ((IBlock)destinfo).getBlockType()) {
			return false;
		}

		IBlock srcparent = ((IBlock)srcinfo).getMotherBlock();
		IBlock destparent = ((IBlock)destinfo).getMotherBlock();

		// ブロック構造をチェックする
		while (srcparent != null && destparent != null) {
			if (srcparent.getBlockType() != destparent.getBlockType()) {
				return false;
			}
			srcparent = srcparent.getMotherBlock();
			destparent = destparent.getMotherBlock();
			if (srcparent == null && destparent == null) {
				return true;
			}
			else if (srcparent == null || destparent == null) {
				return false;
			}
		}
		return true;
	}

	/**
	 * 同じブロック構造であるかチェックする.
	 * 親ブロック構造が同じであるかチェックする.
	 * 親ブロックの構文も同じであること.
	 * @param destinfo		ブロック１
	 * @param srcinfo		ブロック２
	 * @return			true=同一ブロック構造
	 */
	@SuppressWarnings("unused")
	private boolean equalsParentBlock(IInformation destinfo, IInformation srcinfo) {
		if (destinfo == null) return false;
		if (srcinfo == null) return false;
		if (!(destinfo instanceof IBlock)) return false;
		if (!(srcinfo instanceof IBlock)) return false;

		if (!srcinfo.toString().equalsIgnoreCase(destinfo.toString())) {
			return false;
		}

		IBlock srcparent = ((IBlock)srcinfo).getMotherBlock();
		IBlock destparent = ((IBlock)destinfo).getMotherBlock();

		// ブロック構造をチェックする
		while (srcparent != null && destparent != null) {
			if (!srcparent.toString().equalsIgnoreCase(destparent.toString())) {
				return false;
			}
			srcparent = srcparent.getMotherBlock();
			destparent = destparent.getMotherBlock();
			if (srcparent == null && destparent == null) {
				return true;
			}
			else if (srcparent == null || destparent == null) {
				return false;
			}
		}
		return true;
	}

	/**
	 * プロジェクトモデルを設定する.
	 * @param model 		プロジェクトモデル
	 */
	public void setProjectModel(ProjectModel model) {
		this.projectModel = model;
	}
}


