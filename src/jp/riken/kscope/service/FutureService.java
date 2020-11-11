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

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.concurrent.Callable;
import java.util.concurrent.FutureTask;
import jp.riken.kscope.common.Constant;

/**
 * Thread task service class
 *
 * @author RIKEN
 * @param <Integer> Thread exit code
 */
@SuppressWarnings("hiding")
public class FutureService<Integer> extends FutureTask<Integer> {

  /** Thread end notification */
  private PropertyChangeSupport threadEvent;
  /** Thread end message */
  private String message;

  /**
   * Constructor
   *
   * @param callable Thread call class
   */
  public FutureService(Callable<Integer> callable) {
    super(callable);
  }

  /**
   * Constructor
   *
   * @param runnable execution thread
   * @param result parameter
   */
  public FutureService(Runnable runnable, Integer result) {
    super(runnable, result);
  }

  /**
   * Constructor
   *
   * @param runnable execution thread
   */
  public FutureService(Runnable runnable) {
    super(runnable, null);
  }

  /**
   * Register the thread end notification listener
   *
   * @param listener Property change listener
   */
  public void addPropertyChangeListener(PropertyChangeListener listener) {
    if (threadEvent == null) {
      threadEvent = new PropertyChangeSupport(this);
    }
    threadEvent.addPropertyChangeListener(listener);
  }

  @Override
  public boolean cancel(boolean mayInterruptIfRunning) {
    return super.cancel(mayInterruptIfRunning);
  }

  /** End thread execution */
  @Override
  @SuppressWarnings("unchecked")
  protected void done() {
    super.done();

    Integer result = null;
    if (this.isCancelled()) {
      result = (Integer) new java.lang.Integer(Constant.CANCEL_RESULT);
    } else if (this.isDone()) {
      try {
        result = this.get();
      } catch (Exception ex) {
        ex.printStackTrace();
      }
    }

    // Use PropertyChangeSupport to notify the end of the thread
    if (threadEvent != null) {
      this.threadEvent.firePropertyChange(Constant.PROPERTYNAME_THREADDONE, null, result);
    }
  }

  /**
   * Get thread end message
   *
   * @return Thread end message
   */
  public String getMessage() {
    return message;
  }

  /**
   * Set the thread end message.
   *
   * @param message Thread end message
   */
  public void setMessage(String message) {
    this.message = message;
  }
}
