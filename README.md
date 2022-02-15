[![REUSE status](https://api.reuse.software/badge/github.com/SAP-samples/abap-platform-refscen-flight)](https://api.reuse.software/info/github.com/SAP-samples/abap-platform-refscen-flight)

# ABAP Flight Reference Scenario for the ABAP RESTful Application Programming Model for SAP BTP ABAP Environment
The ABAP RESTful programming model defines the architecture for efficient end-to-end development of intrinsically SAP HANA-optimized Fiori apps in SAP BTP ABAP Environment. It supports the development of all types of Fiori applications as well as Web APIs. It is based on technologies and frameworks such as Core Data Services (CDS) for defining semantically rich data models and a service model infrastructure for creating OData services with bindings to an OData protocol and ABAP-based application services for custom logic and SAPUI5-based user interfaces.

The ABAP Flight Reference Scenario provides sample data and services as well as legacy business logic to get familiar with the ABAP RESTful Application Programming Model. You can check out the end-to-end scenarios or build your own app based on the sample data.

For more information, see [Downloading the ABAP Flight Reference Scenario](https://help.sap.com/viewer/923180ddb98240829d935862025004d6/Cloud/en-US/def316685ad14033b051fc4b88db07c8.html).

## Prerequisites
Make sure to fulfill the following requirements:
* You have access to an SAP BTP ABAP Environment instance (see [here](https://blogs.sap.com/2018/09/04/sap-cloud-platform-abap-environment) for additional information).
* You have downloaded and installed ABAP Development Tools (ADT). Make sure to use the most recent version as indicated on the [installation page](https://tools.hana.ondemand.com/#abap). 
* You have created an ABAP Cloud Project in ADT that allows you to access your SAP BTP ABAP Environment instance (see [here](https://help.sap.com/viewer/5371047f1273405bb46725a417f95433/Cloud/en-US/99cc54393e4c4e77a5b7f05567d4d14c.html) for additional information). Your log-on language is English.
* You have installed the [abapGit](https://github.com/abapGit/eclipse.abapgit.org) plug-in for ADT from the update site `http://eclipse.abapgit.org/updatesite/`.

## Download
Use the abapGit plug-in to install the <em>ABAP Flight Reference Scenario</em> by executing the following steps:
1. In your ABAP cloud project, create the ABAP package `/DMO/FLIGHT` (using the superpackage `/DMO/SAP`) as the target package for the demo content to be downloaded (leave the suggested values unchanged when following the steps in the package creation wizard).
2. To add the <em>abapGit Repositories</em> view to the <em>ABAP</em> perspective, click `Window` > `Show View` > `Other...` from the menu bar and choose `abapGit Repositories`.
3. In the <em>abapGit Repositories</em> view, click the `+` icon to clone an abapGit repository.
4. Enter the following URL of this repository: `https://github.com/SAP-samples/abap-platform-refscen-flight.git` and choose <em>Next</em>.
5. Select the branch <em>BTP-ABAP</em> and enter the newly created package `/DMO/FLIGHT` as the target package and choose <em>Next</em>.
6. Create a new transport request that you only use for this demo content installation (recommendation) and choose <em>Finish</em> to link the Git repository to your ABAP cloud project. The repository appears in the abapGit Repositories View with status <em>Linked</em>.
7. Right-click on the new ABAP repository and choose `pull` to start the cloning of the repository contents. Note that this procedure may take a few minutes. 
8. Once the cloning has finished, the status is set to `Pulled Successfully`. (Refresh the `abapGit Repositories` view to see the progress of the import). Then refresh your project tree.

As a result of the installation procedure above, the ABAP system creates an inactive version of all artifacts from the demo content and adds the following sub packages to the target package: 
* `/DMO/FLIGHT_LEGACY`
* `/DMO/FLIGHT_REUSE` The reuse package contains a package for the supplement business object `/DMO/FLIGHT_REUSE_SUPPLEMENT`, which is reused in the other development scenarios. The reuse package also contains the package `/DMO/FLIGHT_REUSE_CARRIER`, which contains a mulit-inline-edit scenario for maintaining carrier data (see also: corresponding [end-to-end guide](https://help.sap.com/viewer/923180ddb98240829d935862025004d6/Cloud/en-US/f713ec52bcb8405ca9262918cffa5d25.html)).   
* `/DMO/FLIGHT_READONLY` - represents a read-only list reporting app (see also: corresponding [end-to-end guide](https://help.sap.com/viewer/923180ddb98240829d935862025004d6/Cloud/en-US/504035c0850f44f787f5b81e35791d10.html)).
* `/DMO/FLIGHT_MANAGED` - represents the transactional app with implementation type <em>managed</em> (see also: corresponding [end-to-end guide](https://help.sap.com/viewer/923180ddb98240829d935862025004d6/Cloud/en-US/b5bba99612cf4637a8b72a3fc82c22d9.html)).
* `/DMO/FLIGHT_UNMANAGED` - represents the transactional app with implementation type <em>unmanaged</em> (see also: corresponding [end-to-end guide](https://help.sap.com/viewer/923180ddb98240829d935862025004d6/Cloud/en-US/f6cb3e3402694f5585068e5e5161a7c1.html)).
* `/DMO/FLIGHT_DRAFT` - represents the transactional app with <em>draft</em> (see also: corresponding [end-to-end guide](https://help.sap.com/viewer/923180ddb98240829d935862025004d6/Cloud/en-US/71ba2bec1d0d4f22bc344bba6b569f2e.html)).

NOTE: The service bindings of the develop scenarios are imported with the warning: `To enable activation of local service endpoint, generate service artifacts`. 

## Configuration

To activate all development objects from the `/DMO/FLIGHT` package: 
1. Click the mass-activation icon (<em>Activate Inactive ABAP Development Objects</em>) in the toolbar.  
2. In the dialog that appears, select all development objects except for all service binding artifacts (`/DMO/API_TRAVEL_U_V2`, `/DMO/UI_CARRIERS_S_O4`, `/DMO/UI_FLIGHT_R_V2`, `/DMO/UI_SUPPLEMENT_04`, `/DMO/UI_TRAVEL_A_D_O2`, `/DMO/UI_TRAVEL_APPR_M_O2`, `/DMO/UI_TRAVEL_D_D_O4`, `/DMO/UI_TRAVEL_PROC_M_O2`, and `/DMO/UI_TRAVEL_U_V2`) in the transport request (that you created for the demo content installation) and choose `Activate`. 
3. Service bindings can only be activated once the underlying service definition is activated. To activate the service bindings click the mass-activation icon again and select all service bindings and choose `Activate`.

To generate service artifacts for the service bindings:
1. In each service binding, choose the button `Publish` or choose `Publish local service endpoint` in the top right corner of the editor.

To fill the demo database tables for develop scenarios with sample business data: 
1. Expand the package structure in the Project Explorer `/DMO/FLIGHT_LEGACY` > `Source Code Library` > `Classes`.
2. Select the data generator class `/DMO/CL_FLIGHT_DATA_GENERATOR` and press `F9` (Run as Console Application). 

NOTE: The namespace /DMO/ is reserved for the demo content. Apart from the downloaded demo content, do not use the namespace /DMO/ and do not create any development objects in the downloaded packages. You can access the development objects in /DMO/ from your own namespace.

## How to obtain support
This project is provided "as-is": there is no guarantee that raised issues will be answered or addressed in future releases.

## License
Copyright (c) 2018-2022 SAP SE or an SAP affiliate company. All rights reserved.
This project is licensed under the SAP Sample Code License except as noted otherwise in the [LICENSE](LICENSES/Apache-2.0.txt) file.

