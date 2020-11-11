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
import java.net.URLClassLoader;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import jp.riken.kscope.properties.KscopeProperties;

/**
 * Resource utility class
 *
 * @author RIKEN
 */
public class ResourceUtils {

  /** java.net.URL class object */
  private static final Class<?>[] parameters = new Class[] {java.net.URL.class};

  /** Application root class */
  private static Class<?> rootAppClass;

  // Name of the last file used in getPropertiesFile
  public static String PROPERTIES_FILE_USED = "";

  /**
   * Set the application class.
   *
   * @param rootAppClass Application class
   */
  public static void setRootAppClass(Class<?> rootAppClass) {
    ResourceUtils.rootAppClass = rootAppClass;
  }

  /**
   * Get the icon resource. <br>
   * If it cannot be obtained from the class loader, obtain it from the'./resouces/icons' path.
   *
   * @param name Icon name
   * @return icon object
   */
  public static Icon getIcon(String name) {
    try {
      // class loader
      // java.net.URL url =
      // ResourceUtils.class.getClassLoader().getResource(name);
      java.net.URL url = ResourceUtils.class.getClassLoader().getResource("icons/" + name);

      // For development environment (default)
      if (url == null) {
        url = new File("./resources/icons/" + name).toURI().toURL();
      }
      return new ImageIcon(url);
    } catch (MalformedURLException ex) {
      ex.printStackTrace();
      return null;
    }
  }

  /**
   * Get the resource file.
   *
   * @param name file name
   * @return Resource input stream
   */
  public static InputStream getPropertiesFile(String name) {
    boolean debug = (System.getenv("DEBUG") != null);
    try {
      java.net.URL url = ResourceUtils.class.getClassLoader().getResource(name);
      if (url == null) {
        url = rootAppClass.getResource(name);
      }
      if (url != null) {
        InputStream is = url.openStream();
        PROPERTIES_FILE_USED = url.getPath();
        return is;
      }

      // Search from the properties folder from the classpath
      url = rootAppClass.getResource(rootAppClass.getSimpleName() + ".class");
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
        File folder =
            new File(file.getAbsolutePath() + File.separator + KscopeProperties.PROPERTIES_FOLDER);
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
      System.err.println("Error opening recource file " + name);
      e.printStackTrace();
    }

    return null;
  }

  /**
   * Add a classpath.
   *
   * @param s Additional path name
   * @throws IOException Illegal path error
   */
  public static void addFile(String s) throws IOException {
    File f = new File(s);
    addFile(f);
  }

  /**
   * Add a classpath.
   *
   * @param f Additional path
   * @throws IOException Illegal path error
   */
  public static void addFile(File f) throws IOException {
    addURL(f.toURI().toURL());
  }

  /**
   * Add classpath. <br>
   * Usage: addFile ("./resources/icons"); <br>
   * Add the classpath in advance when retrieving the resource file from the class loader. <br>
   * Add the classpath and getIcon's'new File ("./resources/icons/" + name) .toURI (). toURL ();'You
   * do not have to. <br>
   *
   * @param u Additional path URL
   * @throws IOException Illegal path error
   */
  public static void addURL(java.net.URL u) throws IOException {
    URLClassLoader sysloader = (URLClassLoader) ClassLoader.getSystemClassLoader();

    Class<?> sysclass = URLClassLoader.class;

    try {
      Method method = sysclass.getDeclaredMethod("addURL", parameters);
      method.setAccessible(true);
      method.invoke(sysloader, new Object[] {u});
    } catch (Throwable t) {
      throw new IOException("could not add " + u + " to classpath");
    }
  }
}
