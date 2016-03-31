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

package jp.riken.kscope.utils;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URLClassLoader;
import java.nio.file.DirectoryStream;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import javax.swing.Icon;
import javax.swing.ImageIcon;

import jp.riken.kscope.properties.KscopeProperties;

/**
 * リソースユーティリティクラス
 *
 * @author RIKEN
 */
public class ResourceUtils {

    /** java.net.URLクラスオブジェクト */
    private static final Class<?>[] parameters = new Class[] { java.net.URL.class };

    /** アプリケーションルートクラス */
    private static Class<?> rootAppClass;

    // Name of the last file used in getPropertiesFile
    public static String PROPERTIES_FILE_USED = "";

    private static List<File> add_classpaths = new ArrayList<File>();

    /**
     * アプリケーションクラスを設定する。
     *
     * @param rootAppClass
     *            アプリケーションクラス
     */
    public static void setRootAppClass(Class<?> rootAppClass) {
        ResourceUtils.rootAppClass = rootAppClass;
    }

    /**
     * アイコンリソースを取得する.<br/>
     * クラスローダから取得できない場合は、'./resouces/icons'パスから取得する。
     *
     * @param name
     *            アイコン名
     * @return アイコンオブジェクト
     */
    public static Icon getIcon(String name) {
        try {
            // クラスローダ
            // java.net.URL url =
            // ResourceUtils.class.getClassLoader().getResource(name);
            java.net.URL url = ResourceUtils.class.getClassLoader()
                    .getResource("icons/" + name);

            if (url != null) {
                return new ImageIcon(url);
            }

            // 開発環境用（デフォルト）
            url = new File("./resources/icons/" + name).toURI().toURL();
            File file = new File(url.toURI());
            if (file != null && file.exists()) {
                return new ImageIcon(url);
            }

            file = ResourceUtils.getResourceFile(name);
            if (file != null && file.exists()) {
                return new ImageIcon(file.getPath());
            }
        } catch (Exception ex) {
            ex.printStackTrace();
            return null;
        }

        return null;
    }

    /**
     * リソースファイルを取得する.
     *
     * @param name
     *            ファイル名
     * @return リソース入力ストリーム
     */
    public static InputStream getPropertiesFile(String name) {
        boolean debug = (System.getenv("DEBUG")!= null);
        try {
            java.net.URL url = ResourceUtils.class.getClassLoader()
                    .getResource(name);
            if (url == null) {
                url = rootAppClass.getResource(name);
            }
            if (url != null) {
                InputStream is = url.openStream();
                PROPERTIES_FILE_USED = url.getPath();
                return is;
            }

            // クラスパスからのプロパティフォルダから検索する
            url = rootAppClass.getResource(rootAppClass.getSimpleName()
                    + ".class");
            java.net.URI uri = url.toURI();
            if (uri.toString().startsWith("jar:")) {
                url = new java.net.URL(uri.toString().substring(4));
                uri = url.toURI();
            }

            File file = new File(uri);
            // if (file == null || !file.exists()) return null;

            file = file.getParentFile().getParentFile();
            File propatiesFolder = null;
            while (file != null) {
                File folder = new File(file.getAbsolutePath() + File.separator
                        + KscopeProperties.PROPERTIES_FOLDER);
                if (folder.exists()) {
                    propatiesFolder = folder;
                    break;
                }
                file = file.getParentFile();
            }

            if (propatiesFolder != null) {
                PROPERTIES_FILE_USED = propatiesFolder + File.separator + name;
                InputStream is = new FileInputStream(PROPERTIES_FILE_USED);
                return is;
            }

        } catch (Exception e) {
            System.err.println("Error opening recource file "+name);
            e.printStackTrace();
        }

        return null;
    }

    /**
     * クラスパスを追加する。
     *
     * @param s
     *            追加パス名
     * @throws IOException
     *             不正パスエラー
     */
    public static void addFile(String s) throws IOException {
        File f = new File(s);
        addFile(f);
    }

    /**
     * クラスパスを追加する。
     *
     * @param f
     *            追加パス
     * @throws IOException
     *             不正パスエラー
     */
    public static void addFile(File f) throws IOException {
        // クラスパスの追加
        ResourceUtils.add_classpaths.add(f);
        addURL(f.toURI().toURL());
    }

    /**
     * クラスパスを追加する.<br/>
     * 使用方法 ： addFile("./resources/icons");<br/>
     * クラスローダからリソースファイルを取得する場合に、事前にクラスパスを追加しておく。<br/>
     * クラスパスを追加するとgetIconの'new File("./resources/icons/" +
     * name).toURI().toURL();'はしなくてもよい。<br/>
     *
     * @param u
     *            追加パスURL
     * @throws IOException
     *             不正パスエラー
     */
    public static void addURL(java.net.URL u) throws IOException {
        URLClassLoader sysloader = (URLClassLoader) ClassLoader
                .getSystemClassLoader();

        Class<?> sysclass = URLClassLoader.class;

        try {
            Method method = sysclass.getDeclaredMethod("addURL", parameters);
            method.setAccessible(true);
            method.invoke(sysloader, new Object[] { u });
        } catch (Throwable t) {
            throw new IOException("could not add " + u + " to classpath");
        }
    }

    /**
     * リソースパスからファイル一覧を取得する.<br/>
     * クラスローダから取得できない場合は、./templates/" + dir_nameパスから取得する。
     *
     * @param name         リソースフォルダ
     * @param ext_name       拡張子名
     * @return   ファイル一覧
     */
    public static Map<String, InputStream> getInputStreams(String dir_name) {
        try {
            // クラスローダ
            // java.net.URL url =
            // ResourceUtils.class.getClassLoader().getResource(name);
            java.net.URL url = ResourceUtils.class.getClassLoader().getResource(dir_name);

            // 開発環境用（デフォルト）
            if (url == null) {
                url = new File("./templates/" + dir_name).toURI().toURL();
            }

            Map<String, InputStream> map = new HashMap<String, InputStream>();
            if (url.toString().startsWith("jar:")) {
                FileSystem fileSystem = null;
                try {
                    fileSystem = FileSystems.newFileSystem(url.toURI(), Collections.<String, Object>emptyMap());
                } catch (Exception e) {
                    fileSystem = FileSystems.getFileSystem(url.toURI());
                }
                Path path = fileSystem.getPath(dir_name);
                Stream<Path> walk = Files.walk(path, 1);
                for (Iterator<Path> it = walk.iterator(); it.hasNext();){
                    Path find_file = it.next();
                    if (!Files.isDirectory(find_file)) {
                        String name = find_file.toString();
                        name = name.substring(1);
                        InputStream is = ResourceUtils.class.getClassLoader().getSystemResourceAsStream(name);
                        if (is != null) {
                            map.put(name, is);
                        }
                    }
                }
                walk.close();
            }
            else {
                String path = url.getPath();
                if (path == null) return null;
                File dir = new File(path);
                if (dir == null || !dir.exists()) {
                    dir = ResourceUtils.getResourceFile(dir_name);
                }
                if (dir == null || !dir.exists()) {
                    return null;
                }
                File[] files = dir.listFiles();
                if (files == null) return null;
                for (File file : files) {
                    InputStream is = new FileInputStream(file);
                    if (is != null) {
                        map.put(file.getName(), is);
                    }
                }
            }
            if (map.size() <= 0) return null;

            return map;
        } catch (Exception ex) {
            ex.printStackTrace();
            return null;
        }
    }


    /**
     * リソースパスからファイル一覧を取得する.<br/>
     * クラスローダから取得できない場合は、./templates/" + dir_nameパスから取得する。
     *
     * @param name         リソースフォルダ
     * @param ext_name       拡張子名
     * @return   ファイル一覧
     */
    public static List<String> getTemplateFileList(String dir_name) {
        try {
            // クラスローダ
            // java.net.URL url =
            // ResourceUtils.class.getClassLoader().getResource(name);
            java.net.URL url = ResourceUtils.getTemplateResourceUrl(dir_name);
            if (url == null) {
                return null;
            }

            List<String> list = new ArrayList<String>();
            if (url.toString().startsWith("jar:")) {
                FileSystem fileSystem = null;
                try {
                    fileSystem = FileSystems.newFileSystem(url.toURI(), Collections.<String, Object>emptyMap());
                } catch (Exception e) {
                    fileSystem = FileSystems.getFileSystem(url.toURI());
                }
                Path path = fileSystem.getPath(dir_name);

                try {
                    DirectoryStream<Path> directoryStream = Files.newDirectoryStream(path);
                    for (Path find_file : directoryStream) {
                        if (!Files.isDirectory(find_file)) {
                            String name = find_file.toString();
                            name = name.substring(1);   // 先頭の'/'を削除する
                            list.add(name);
                        }
                    }
                    directoryStream.close();
                }
                catch (Exception ex) {}

                /****   for java8
                Stream<Path> walk = Files.walk(path, 1);
                for (Iterator<Path> it = walk.iterator(); it.hasNext();){
                    Path find_file = it.next();
                    if (!Files.isDirectory(find_file)) {
                        String name = find_file.toString();
                        name = name.substring(1);   // 先頭の'/'を削除する
                        list.add(name);
                    }
                }
                walk.close();
                */
            }
            else {
                String path = url.getPath();
                if (path == null) return null;
                File dir = new File(path);
                if (dir == null || !dir.exists()) {
                    dir = ResourceUtils.getResourceFile(dir_name);
                }
                if (dir == null || !dir.exists()) {
                    return null;
                }
                File[] files = dir.listFiles();
                if (files == null) return null;
                for (File file : files) {
                    if (file.isDirectory()) continue;
                    list.add(file.getName());
                }
            }
            if (list.size() <= 0) return null;
            return list;
        } catch (Exception ex) {
            ex.printStackTrace();
            return null;
        }
    }

    /**
     * リソースファイルを取得する.
     * @param path            パス
     * @return   リソースファイル
     */
    public static File getResourceFile(String path) {
        try {
            java.net.URL url = ResourceUtils.class.getClassLoader().getResource(path);

            if (url == null) {
                url = rootAppClass.getResource(path);
            }
            if (url != null) {
                File resource_file = new File(url.toURI());
                if (resource_file.exists()) {
                    return resource_file;
                }
            }

            // 起動JARファイルのクラスパスからのプロパティフォルダから検索する
            url = rootAppClass.getResource(rootAppClass.getSimpleName() + ".class");
            java.net.URI uri = url.toURI();
            if (uri.toString().startsWith("jar:")) {
                url = new java.net.URL(uri.toString().substring(4));
                uri = url.toURI();
            }

            File file = new File(uri);
            if (file == null || !file.exists()) return null;
            if (ResourceUtils.add_classpaths.size() <= 0) return null;

            file = file.getParentFile().getParentFile();
            while (file != null) {
                for (File add_file : ResourceUtils.add_classpaths) {
                    File find_file = new File(file.getAbsolutePath()
                                        + File.separator
                                        + add_file.getPath()
                                        + File.separator
                                        + path);
                    if (find_file.exists()) {
                        return find_file;
                    }
                }
                file = file.getParentFile();
            }

        } catch (Exception e) {
            // System.err.println("Error opening recource file " + path);
            // e.printStackTrace();
        }

        return null;
    }

    /**
     * リソースパスからファイルパスを取得する.<br/>
     * クラスローダから取得できない場合は、./templates/" + dir_nameパスから取得する。
     *
     * @param name         リソースフォルダ
     * @return   リソースパス
     */
    public static java.net.URL getTemplateResourceUrl(String dir_name) {
        try {
            // クラスローダ
            // java.net.URL url =
            // ResourceUtils.class.getClassLoader().getResource(name);
            dir_name = dir_name.replace('\\', '/');
            java.net.URL url = ResourceUtils.class.getClassLoader().getResource(dir_name);

            // 開発環境用（デフォルト）
            if (url == null) {
                url = new File("./templates/" + dir_name).toURI().toURL();
            }
            return url;

        } catch (MalformedURLException ex) {
            ex.printStackTrace();
            return null;
        }
    }

    /**
     * kscope.jarのパスを取得する.
     * @return        kscope.jarパス
     */
    public static java.net.URL getKscopeJarUrl() {
        try {
            java.net.URL url = ResourceUtils.class.getProtectionDomain().getCodeSource().getLocation();
            if (url == null || url.toURI() == null) return null;

            File jar_path = new File(url.toURI().getPath());
            if (!jar_path.exists()) return null;
            return url;
        } catch (URISyntaxException ex) {
            ex.printStackTrace();
            return null;
        }

    }


}
