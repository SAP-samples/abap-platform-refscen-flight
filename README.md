[![REUSE status](https://api.reuse.software/badge/github.com/SAP-samples/abap-platform-refscen-flight)](https://api.reuse.software/info/github.com/SAP-samples/abap-platform-refscen-flight)

# ABAP Flight Reference Scenario for RAP on ABAP Platform Cloud
The ABAP RESTful Application Programming Model (RAP) defines the architecture for efficient end-to-end development of intrinsically SAP HANA-optimized Fiori apps. It supports the development of all types of Fiori applications as well as Web APIs. It is based on technologies and frameworks such as Core Data Services (CDS) for defining semantically rich data models and a service model infrastructure for creating OData services with bindings to an OData protocol and ABAP-based application services for custom logic and SAPUI5-based user interfaces.

The ABAP Flight Reference Scenario provides sample data and services as well as legacy business logic to get familiar with RAP. You can check out the end-to-end scenarios or build your own app based on the sample data.

For more information, see [Downloading the ABAP Flight Reference Scenario](https://help.sap.com/docs/abap-cloud/abap-rap/downloading-abap-flight-reference-scenario?locale=en-US).

**Note:**  
The branches of this repository were renamed recently. If you have already linked an ABAP Package to a branch with an outdated name, unlink the repository first and then pull the link to the branch with the new name, as described in step 3 of the <em>Download</em> section. 

## Prerequisites
Make sure to fulfill the following requirements:
* You have access to an ABAP Environment instance on SAP BTP or S/4HANA Cloud, public edition (see [SAP BTP-ABAP Environment](https://help.sap.com/docs/BTP/65de2977205c403bbc107264b8eccf4b/11d62652aa2b4600a0fa136de0789648.html) or [SAP S/4HANA Cloud-Developer Extensibility](https://help.sap.com/docs/SAP_S4HANA_CLOUD/6aa39f1ac05441e5a23f484f31e477e7/e1059ff581854a699f15734049f14293.html)).
* You have downloaded and installed ABAP Development Tools (ADT). Make sure to use the most recent version as indicated on the [installation page](https://tools.hana.ondemand.com/#abap). 
* You have created an ABAP Cloud Project in ADT that allows you to access your ABAP Environment instance (see Creating an ABAP Cloud Project ([SAP BTP](https://help.sap.com/docs/BTP/5371047f1273405bb46725a417f95433/99cc54393e4c4e77a5b7f05567d4d14c.html) / [SAP S/4HANA Cloud](https://help.sap.com/docs/SAP_S4HANA_CLOUD/25cf71e63940453397a32dc2b7676947/99cc54393e4c4e77a5b7f05567d4d14c.html))). Your log-on language is English.
* You have installed the [abapGit](https://github.com/abapGit/eclipse.abapgit.org) plug-in for ADT from the update site `http://eclipse.abapgit.org/updatesite/`.

## Download
Use the abapGit plug-in to import the <em>ABAP Flight Reference Scenario</em> by executing the following steps:
1. In your ABAP cloud project, create the ABAP package `/DMO/FLIGHT` (using the superpackage `/DMO/SAP`) as the target package for the demo content to be downloaded (leave the suggested values unchanged when following the steps in the package creation wizard).
2. To add the <em>abapGit Repositories</em> view to the <em>ABAP</em> perspective, click `Window` > `Show View` > `Other...` from the menu bar and choose `abapGit Repositories`.
3. In the <em>abapGit Repositories</em> view, click the `+` icon to clone an abapGit repository.
4. Enter the following URL of this repository: `https://github.com/SAP-samples/abap-platform-refscen-flight.git` and choose <em>Next</em>.
5. Select the branch <em>ABAP-platform-cloud</em> and enter the newly created package `/DMO/FLIGHT` as the target package and choose <em>Next</em>.
6. Create a new transport request that you only use for this demo content installation (recommendation) and choose <em>Finish</em> to link the Git repository to your ABAP cloud project. The repository appears in the abapGit Repositories View with status <em>Linked</em>.
7. Right-click on the new ABAP repository and choose `pull` to start the cloning of the repository contents. Note that this procedure may take a few minutes. 
8. Once the cloning has finished, the status is set to `Pulled Successfully`. (Refresh the `abapGit Repositories` view to see the progress of the import). Refresh the project tree. In case the status is set to ‘Pulled with errors’ and the authorization object /DMO/CNTRY reports the error ‘No authorization to change authorization field &/DMO/CNTRY&’, please raise a ticket on component BC-SEC-AUT-PFC.

As a result of the installation procedure above, the ABAP system creates an inactive version of all artifacts from the demo content and adds the following sub packages to the target package: 
* `/DMO/FLIGHT_LEGACY`
* `/DMO/FLIGHT_REUSE` The reuse package contains a package for the supplement business object `/DMO/FLIGHT_REUSE_SUPPLEMENT`, which is reused in the other development scenarios. The reuse package also contains the package `/DMO/FLIGHT_REUSE_CARRIER`, which contains a mulit-inline-edit scenario for maintaining carrier data (see [Developing Transactional Apps with Multi-Inline-Edit Capabilities](https://help.sap.com/docs/abap-cloud/abap-rap/developing-transactional-apps-with-multi-inline-edit-capabilities?locale=en-US)).
Lastly, the reuse package contains the package `/DMO/FLIGHT_REUSE_AGENCY` which incorporates a business object for administering agency master data, including the possibility of maintaining Large Objects. The business object is extensibility-enabled as described in the RAP extensibility guide (see [Extend](https://help.sap.com/docs/abap-cloud/abap-rap/extend?locale=en-US). This extensibility guide also contains examples on how to develop extensions for the business object. These code examples are contained in sub packages of the `/DMO/FLIGHT_REUSE_AGENCY` package.
* `/DMO/FLIGHT_READONLY` - represents a read-only list reporting app (see [Developing Read-Only List Reporting Apps]([https://help.sap.com/docs/BTP/923180ddb98240829d935862025004d6/504035c0850f44f787f5b81e35791d10.html](https://help.sap.com/docs/abap-cloud/abap-rap/developing-read-only-list-reporting-apps?locale=en-US)).
* `/DMO/FLIGHT_MANAGED` - represents the transactional app with implementation type <em>managed</em> (see [Developing Managed Transactional Apps](https://help.sap.com/docs/abap-cloud/abap-rap/developing-managed-transactional-apps?locale=en-US).
* `/DMO/FLIGHT_UNMANAGED` - represents the transactional app with implementation type <em>unmanaged</em> (see [Developing Unmanaged Transactional Apps](https://help.sap.com/docs/abap-cloud/abap-rap/developing-unmanaged-transactional-apps?locale=en-US).
* `/DMO/FLIGHT_DRAFT` - represents the transactional app with <em>draft</em> (see [Developing Transactional Apps with Draft Capabilities](https://help.sap.com/docs/abap-cloud/abap-rap/developing-transactional-apps-with-draft-capabilities?locale=en-US).
* `/DMO/FLIGHT_HIERARCHY` - contains the development objects showcasing hierarchies in a read-only and an editable treeview scenario (see [Developing Apps with Hierarchical Data Structures](https://help.sap.com/docs/abap-cloud/abap-rap/implementing-hierarchical-view?locale=en-US)).

NOTE: The service bindings of the develop scenarios are imported with the warning: `To enable activation of local service endpoint, generate service artifacts`. 

## Configuration
To activate all development objects from the `/DMO/FLIGHT` package: 
1. Click the mass-activation icon (<em>Activate Inactive ABAP Development Objects</em>) in the toolbar.  
2. In the dialog that appears, select all development objects in the transport request (that you created for the demo content installation) and choose `Activate`. (The activation may take a few minutes.) 
3. Service definitions need a provider contract before they can be released for the release contract <em>Extend (C0)</em>. The service definition /DMO/UI_AGENCY from the package /DMO/FLIGHT_REUSE_AGENCY is shipped without this release contract for maintenance reasons and does not contain a provider contract. If you want to release the service definition /DMO/UI_AGENCY for the release contract <em>Extend (C0)</em>, you need to define a suitable provider contract first. You can also directly copy the source code from [service_definition_agency](service_definition_agency). Activate the service definition after.

In case the mass-activation or the Service Bindings report the error ‘Failed to read the runtime table SRVD_RT_EXTENDS for service …’ and/or the error ‘An active version of the Service Definition … does not exist’, the respective Service Definition has not been imported properly. In this case, please proceed as follows:
1. Delete the Service Bindings that are based on this Service Definition.
2. Reactivate the Service Definition (make sure to edit / touch it before).
3. Recreate the Service Bindings deleted in step 1.

To generate service artifacts for the service bindings:
1. In each service binding, choose the button `Publish` or choose `Publish local service endpoint` in the top right corner of the editor.

To fill the demo database tables for develop scenarios with sample business data: 
1. Expand the package structure in the Project Explorer `/DMO/FLIGHT_LEGACY` > `Source Code Library` > `Classes`.
2. Select the data generator class `/DMO/CL_FLIGHT_DATA_GENERATOR` and press `F9` (Run as Console Application). 

NOTE: The namespace /DMO/ is reserved for the demo content. Apart from the downloaded demo content, do not use the namespace /DMO/ and do not create any development objects in the downloaded packages. You can access the development objects in /DMO/ from your own namespace.

## How to obtain support
This project is provided "as-is": there is no guarantee that raised issues will be answered or addressed in future releases.

## License
Copyright (c) 2018-2023 SAP SE or an SAP affiliate company. All rights reserved.
This project is licensed under the SAP Sample Code License except as noted otherwise in the [LICENSE](LICENSES/Apache-2.0.txt) file.

