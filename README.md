# Flight Reference Scenario for the ABAP RESTful Programming Model
The ABAP RESTful programming model defines the architecture for efficient end-to-end development of intrinsically SAP HANA-optimized Fiori apps in SAP Cloud Platform ABAP Environment. It supports the development of all types of Fiori applications as well as A2X services. It is based on technologies and frameworks such as Core Data Services (CDS) for defining semantically rich data models and a service model infrastructure for creating OData services with bindings to an OData protocol and ABAP-based application services for custom logic and SAPUI5-based user interfaces.

## Prerequisites
Please make sure to fulfill the following requirements:
* You have access to an SAP Cloud Platform ABAP Environment instance (see [here](https://blogs.sap.com/2018/09/04/sap-cloud-platform-abap-environment) for additional information).
* You have downloaded and installed ABAP Development Tools (ADT). Make sure to use the most recent version as indicated on the [installation page](https://tools.hana.ondemand.com/#abap). 
* You have created an ABAP Cloud Project in ADT that allows you to access your SAP Cloud Platform ABAP Environment instance (see [here](https://help.sap.com/viewer/5371047f1273405bb46725a417f95433/Cloud/en-US/99cc54393e4c4e77a5b7f05567d4d14c.html) for additional information).
* You have installed the [abapGit](https://github.com/abapGit/eclipse.abapgit.org) plug-in for ADT from the update site `http://eclipse.abapgit.org/updatesite/`.

## Download
Use the abapGit plug-in to install the <em>Flight Reference Scenario</em> by executing the following steps:
1. In your ABAP cloud project, create the ABAP package `/DMO/FLIGHT` (using the superpackage `/DMO/SAP`) as the target package for the demo content to be downloaded (leave the suggested values unchanged when following the steps in the package creation wizard).
2. To add the <em>abapGit Repositories</em> view to the <em>ABAP</em> perspective, click `Window` > `Show View` > `Other...` from the menu bar and choose `abapGit Repositories`.
3. In the <em>abapGit Repositories</em> view, click the `+` icon to clone an abapGit repository.
4. Enter the following URL of this repository: `https://github.com/SAP/abap-platform-refscen-flight.git` and choose <em>Next</em>.
5. Select the master branch and enter the newly created package `/DMO/FLIGHT` as the target package.
6. Create a new transport request that you only use for this demo content installation (recommendation) and choose <em>Finish</em> to start the cloning of the repository contents. Note that this procedure may take a few minutes.
7. Once the cloning has finished, refresh your project tree.

As a result of the installation procedure above, the ABAP system creates an inactive version of all artifacts from the demo content and adds the following sub packages to the target package:
* `/DMO/FLIGHT_LEGACY`
* `/DMO/FLIGHT_REUSE`
* `/DMO/FLIGHT_READONLY` - represents a read-only list reporting app (see also: corresponding [end-to-end guide](https://help.sap.com/viewer/c0d02c4330c34b3abca88bdd57eaccfc/Cloud/en-US/a1243bff462b4ee3a03e2bb6fc30e015.html)).
* `/DMO/FLIGHT_UNMANAGED` - represents the transactional app with implementation type <em>unmanaged</em> (see also: corresponding [end-to-end guide](https://help.sap.com/viewer/c0d02c4330c34b3abca88bdd57eaccfc/Cloud/en-US/971e03cd952a47458e57f87fc566a8f3.html)).

Please note:  
The demo packages do not include <em>service bindings</em>. They must be created in your own namespace to complete the service. 
Then you can run the e.g. UI services with the <em>Fiori Elements</em> preview in the <em>service binding</em>.

## Configuration
To activate all development objects from the `/DMO/FLIGHT` package: 
1. Click the mass-activation icon (<em>Activate Inactive ABAP Development Objects</em>) in the toolbar.  
2. In the dialog that appears, select all development objects in the transport request (that you created for the demo content installation) and choose `Activate`.

To create a <em>service binding</em> for the read-only list reporting app (package `/DMO/FLIGHT_READONLY`):
1. Right-click the service definition `/DMO/FLIGHT_R` and choose `New Service Binding` (see [here](https://help.sap.com/viewer/c0d02c4330c34b3abca88bdd57eaccfc/Cloud/en-US/6709cab6cb5b4c01b28463d760429a9a.html) for additional information). 
2. Specify your own package and your own namespace when following the steps in the creation wizard.

NOTE: The namespace /DMO/ is reserved for the demo content. Apart from the downloaded demo content, do not use the namespace /DMO/ and do not create any development objects in the downloaded packages. You can access the development objects in /DMO/ from your own namespace.

To create a <em>service binding</em> for the transactional app with implementation type unmanaged (package `/DMO/FLIGHT_UNMANAGED`): 
1. Right-click the service definition `/DMO/TRAVEL_U` and choose `New Service Binding` (see [here](https://help.sap.com/viewer/c0d02c4330c34b3abca88bdd57eaccfc/Cloud/en-US/9cda72a7bd9e476b8696f625da404605.html) for additional information). 
2. Create the service binding in your own package and your own namespace. 

To fill the demo database tables with sample business data:
1. Expand the package structure in the Project Explorer `/DMO/FLIGHT_LEGACY` > `Source Code Library` > `Classes`.
2. Select the data generator class `/DMO/CL_FLIGHT_DATA_GENERATOR` and press `F9` (Run as Console Application). 

## Limitations
The abapGit plug-in currently only supports to download content. It's not possible to upload any changes.

## How to obtain support
This project is provided "as-is": there is no guarantee that raised issues will be answered or addressed in future releases.

## License
Copyright (c) 2018 SAP SE or an SAP affiliate company. All rights reserved.
This project is licensed under the SAP Sample Code License except as noted otherwise in the [LICENSE](LICENSE) file.
