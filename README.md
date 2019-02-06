# Flight Reference Scenario for the ABAP RESTful Programming Model
The ABAP RESTful programming model defines the architecture for efficient end-to-end development of intrinsically SAP HANA-optimized Fiori apps in SAP Cloud Platform ABAP Environment. It supports the development of all types of Fiori applications as well as A2X services. It is based on technologies and frameworks such as Core Data Services (CDS) for defining semantically rich data models and a service model infrastructure for creating OData services with bindings to an OData protocol and ABAP-based application services for custom logic and SAPUI5-based user interfaces.

## Prerequisites
Please make sure to fulfill the following requirements:
* You have access to an SAP Cloud Platform ABAP Environment instance (see [here](https://blogs.sap.com/2018/09/04/sap-cloud-platform-abap-environment) for additional information)
* You have downloaded and installed [ABAP Development Tools for SAP NetWeaver](https://tools.hana.ondemand.com/#abap) (ADT)
* You have created an ABAP Cloud Project in ADT that allows you to access your SAP Cloud Platform ABAP Environment instance
* You have installed the [abapGit](https://github.com/abapGit/eclipse.abapgit.org) plug-in for Eclipse via the updatesite `http://eclipse.abapgit.org/updatesite/`

## Download and Installation
Use the abapGit plug-in to install the Flight Reference Scenario by executing the following steps:
* In ADT create the package `/DMO/FLIGHT` as a subpackage under `/DMO/SAP` (keep the defaulted values)
* In ADT click on `Window` > `Show View` > `Other...` and choose the entry `abapGit Repositories` to open the abapGit view
* Make sure to have the right ABAP Cloud Project marked (See the little headline in the abapGit view for the current project)
* Click on the `+` icon to clone a abapGit repository
* Provide the URL of this repository: `https://github.com/SAP/abap-platform-refscen-flight.git`
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
