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
package jp.riken.kscope.action;

import java.awt.event.ActionEvent;
import java.io.File;

import jp.riken.kscope.common.ANALYSIS_PANEL;
import jp.riken.kscope.model.PropertiesTableModel;
import jp.riken.kscope.service.AppController;
import jp.riken.kscope.service.FileService;

/**
 * File property action class
 * @author RIKEN
 */
public class FilePropertiesAction extends ActionBase {

    /**
     * Constructor
     * @param controller Application controller
     */
    public FilePropertiesAction(AppController controller) {
        super(controller);
    }

    /**
     * Action occurrence event
     * @param event Event information
     */
    @Override
    public void actionPerformed(ActionEvent event) {

        // Set the property.
        setProperties();

        // Activate the Properties tab
        this.controller.getMainframe().getPanelAnalysisView().setSelectedPanel(ANALYSIS_PANEL.PROPARTIES);
    }

    /**
     * Set properties.
     */
    public void setProperties() {

        // Get the selected file
        File[] files = this.controller.getMainframe().getPanelExplorerView().getSelectedNodeFiles();
        if (files == null || files.length <= 0) return;

        // Get the file property setting model
        PropertiesTableModel model = this.controller.getPropertiesTableModel();

        // Get file properties
        FileService service = new FileService();
        service.setFileProperties(files[0], model);

    }

}
