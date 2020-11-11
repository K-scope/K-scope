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
package jp.riken.kscope;

import java.util.Observable;
import java.util.Observer;
import javax.swing.SwingUtilities;

/**
 * Global settings class for application progress messages, progress, etc.
 *
 * @author RIKEN
 */
public class Application {

  /** Progress information */
  public static StatusPrint status = new StatusPrint();

  /** Constructor */
  private Application() {}

  /**
   * Add status information observer
   *
   * @param o Observer
   */
  public static void addStatus(Observer o) {
    status.addObserver(o);
  }

  /**
   * Progress information class
   *
   * @author RIKEN
   */
  public static class StatusPrint extends Observable {
    /** Main message */
    private StringBuffer messageMain = new StringBuffer();
    /** Location message */
    private StringBuffer messageLocation = new StringBuffer();
    /** Status message */
    private StringBuffer messageStatus = new StringBuffer();
    /** Progress bar: Minimum value */
    private Integer progressMin = null;
    /** Progress bar: Maximum value */
    private Integer progressMax = null;
    /** Progress bar: Value */
    private Integer progressValue = null;
    /** Progress bar: Start */
    private boolean progressStart = false;

    /** Notify status change */
    private void notifyStatus() {
      SwingUtilities.invokeLater(
          new Runnable() {
            @Override
            public void run() {
              if (countObservers() > 0) {
                setChanged();
                notifyObservers();
                clearChanged();
              } else {
                String msg = "main message=" + messageMain.toString() + "\n";
                msg += "location message=" + messageLocation.toString() + "\n";
                msg += "status message=" + messageStatus.toString() + "\n";
                msg +=
                    "progress:value="
                        + progressValue
                        + ", min="
                        + progressMin
                        + ", max="
                        + progressMax;
                msg += "progress:start=" + (progressStart ? "start" : "stop");
                System.out.println(msg);
              }
            }
          });
    }

    /**
     * Set the main message
     *
     * @param message Main message
     */
    public void setMessageMain(String message) {
      this.messageMain = new StringBuffer();
      if (message != null) {
        this.messageMain.append(message);
      }
      notifyStatus();
    }

    /**
     * Get the main message
     *
     * @return main message
     */
    public String getMessageMain() {
      return this.messageMain.toString();
    }

    /**
     * Set status message
     *
     * @param message Status message
     */
    public void setMessageStatus(String message) {
      this.messageStatus = new StringBuffer();
      if (message != null) {
        this.messageStatus.append(message);
      }
      notifyStatus();
    }

    /**
     * Get status message
     *
     * @return status message
     */
    public String getMessageStatus() {
      try {
        if (this.messageStatus == null) return null;
        if (this.messageStatus.length() <= 0) return null;
        return this.messageStatus.toString();
      } catch (Exception e) {
        return null;
      }
    }

    /**
     * Set location message
     *
     * @param message Location message
     */
    public void setMessageLocation(String message) {
      this.messageLocation = new StringBuffer();
      if (message != null) {
        this.messageLocation.append(message);
      }
      notifyStatus();
    }

    /**
     * Set location message
     *
     * @param row line number
     * @param col Column index
     * @param word Location message
     */
    public void setMessageLocation(int row, int col, String word) {
      this.messageLocation = new StringBuffer();
      this.messageLocation.append(row);
      this.messageLocation.append(" : ");
      this.messageLocation.append(col);
      if (word != null) {
        this.messageLocation.append("  [");
        this.messageLocation.append(word);
        this.messageLocation.append("]");
      }
      notifyStatus();
    }

    /**
     * Get location message
     *
     * @return Location message
     */
    public String getMessageLocation() {
      return this.messageLocation.toString();
    }

    /**
     * Set the progress bar
     *
     * @param orient value
     * @param min minimum value
     * @param max Maximum value
     */
    public void setProgress(int orient, int min, int max) {
      this.progressValue = orient;
      this.progressMin = min;
      this.progressMax = max;
      this.progressStart = true;
      notifyStatus();
    }

    /**
     * Set the progress bar.
     *
     * @param min minimum value
     * @param max Maximum value
     */
    public void setProgress(int min, int max) {
      this.progressValue = null;
      this.progressMin = min;
      this.progressMax = max;
      notifyStatus();
    }

    /**
     * Set the progress bar.
     *
     * @param value value
     */
    public void setProgressValue(int value) {
      this.progressValue = value;
      this.progressStart = true;
      notifyStatus();
    }

    /**
     * Set the start state of the progress bar
     *
     * @param start true = start
     */
    public void setProgressStart(boolean start) {
      this.progressStart = start;
      if (!start) {
        // Clear progress bar settings
        this.progressValue = null;
        this.progressMin = null;
        this.progressMax = null;
      }
      notifyStatus();
    }

    /**
     * Progress bar: Get the value.
     *
     * @return progress bar: value
     */
    public Integer getProgressValue() {
      return this.progressValue;
    }

    /**
     * Progress bar: Get the minimum value.
     *
     * @return Progress bar: Minimum
     */
    public Integer getProgressMin() {
      return this.progressMin;
    }

    /**
     * Progress bar: Get the maximum value.
     *
     * @return Progress bar: Maximum
     */
    public Integer getProgressMax() {
      return this.progressMax;
    }

    /**
     * Get the start status of the progress bar
     *
     * @return true = start
     */
    public boolean isProgressStart() {
      return this.progressStart;
    }
  }
}
