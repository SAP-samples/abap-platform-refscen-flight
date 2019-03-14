# Flight Reference Scenario for the ABAP RESTful Programming Model
The ABAP RESTful programming model defines the architecture for efficient end-to-end development of intrinsically SAP HANA-optimized Fiori apps in SAP Cloud Platform ABAP Environment. It supports the development of all types of Fiori applications as well as A2X services. It is based on technologies and frameworks such as Core Data Services (CDS) for defining semantically rich data models and a service model infrastructure for creating OData services with bindings to an OData protocol and ABAP-based application services for custom logic and SAPUI5-based user interfaces.

## Prerequisites
Please make sure to fulfill the following requirements:
* You have access to an SAP Cloud Platform ABAP Environment instance (see [here](https://blogs.sap.com/2018/09/04/sap-cloud-platform-abap-environment) for additional information)
* You have downloaded and installed the front end components of [ABAP Development Tools](https://tools.hana.ondemand.com/#abap) (ADT)
* You have created an ABAP Cloud Project in ADT that allows you to access your SAP Cloud Platform ABAP Environment instance (see [here](https://help.sap.com/viewer/5371047f1273405bb46725a417f95433/Cloud/en-US/99cc54393e4c4e77a5b7f05567d4d14c.html) for additional information)
* You have installed the [abapGit](https://github.com/abapGit/eclipse.abapgit.org) plug-in for ADT from the update site `http://eclipse.abapgit.org/updatesite/`

## Download and Installation
Use the abapGit plug-in to install the Flight Reference Scenario by executing the following steps:
* In your ABAP cloud project, create the ABAP package `/DMO/FLIGHT` (using the superpackage `/DMO/SAP`) as the target package for the demo content to be downloaded (Leave the suggested values unchanged when following the steps in the package creation wizard.)
* To add the <em>abapGit Repositories</em> view to the <em>ABAP</em> perspective, click `Window` > `Show View` > `Other...` from the menu bar and choose `abapGit Repositories`
* In the <em>abapGit Repositories</em> view, click the `+` icon to clone a abapGit repository
* Enter the following URL of this repository: `https://github.com/SAP/abap-platform-refscen-flight.git` and choose <em>Next</em>
* On the next page choose the master branch and provide the package `/DMO/FLIGHT`
* Provide a valid transport request and choose `Finish`. This starts the cloning of the repository - which might take a few minutes
* Once the cloning has finished, refresh your project tree

## Configuration
The cloned content contains "legacy" logic only. This includes e.g. DDIC artifacts and Function Modules. The legacy content can be used together with the end-2end development guides (see [here](https://help.sap.com/viewer/c0d02c4330c34b3abca88bdd57eaccfc/Cloud/en-US/3b77569ca8ee4226bdab4fcebd6f6ea6.html) > Develop) to build e.g. a Fiori List Reporting and a Fiori Transactional Application.

Once cloned, run ABAP class `/DMO/CL_FLIGHT_DATA_GENERATOR` in ADT via `F9` (Run as Console Application) to generate some sample data.

## Known Issues
After cloning a abapGit repository some objects might not be active. Use the mass activation feature in ADT to activate those artifacts.  

## Limitations
The abapGit plug-in currently only supports a one-time download of the content. You can neither download any delta nor upload any changes.

## How to obtain support
This project is provided "as-is": there is no guarantee that raised issues will be answered or addressed in future releases.

## License
Copyright (c) 2018 SAP SE or an SAP affiliate company. All rights reserved.
This project is licensed under the SAP Sample Code License except as noted otherwise in the [LICENSE](LICENSE) file.
