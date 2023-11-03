[![REUSE status](https://api.reuse.software/badge/github.com/SAP-samples/abap-platform-refscen-flight)](https://api.reuse.software/info/github.com/SAP-samples/abap-platform-refscen-flight)

# ABAP Flight Reference Scenario for the ABAP RESTful Application Programming Model on ABAP Platform 
The ABAP RESTful Application Programming Model (RAP) defines the architecture for efficient end-to-end development of intrinsically SAP HANA-optimized Fiori apps on the Application Server ABAP. It supports the development of all types of Fiori applications as well as Web APIs. It is based on technologies and frameworks such as Core Data Services (CDS) for defining semantically rich data models and a service model infrastructure for creating OData services with bindings to an OData protocol and ABAP-based application services for custom logic and SAPUI5-based user interfaces.

The ABAP Flight Reference Scenario provides sample data and services as well as legacy business logic to get familiar with RAP. You can check out the end-to-end scenarios or build your own app based on the sample data.

For more information, see [Downloading the ABAP Flight Reference Scenario](https://help.sap.com/docs/ABAP_PLATFORM_NEW/fc4c71aa50014fd1b43721701471913d/def316685ad14033b051fc4b88db07c8.html?version=202210.latest).

Note:
The branches of this repository were renamed recently. If you have already linked an ABAP Package to a branch with an outdated name, unlink the repository first and then pull the link to the branch with the new name, as described in step 3 of the Download section.

## Prerequisites
Make sure to fulfill the following requirements:
* You are working on Application Server ABAP 7.58 or higher. 
* You have downloaded and installed ABAP Development Tools (ADT). Make sure to use the most recent version as indicated on the [installation page](https://tools.hana.ondemand.com/#abap). 
* You have created an ABAP Project in ADT that allows you to access your Application Server as mentioned above. Your log-on language is English.
* You have downloaded and installed the `zabapgit_standalone` report. Make sure to use the most recent version as indicated on the [installation page](https://docs.abapgit.org/). 
* You have installed the certificate files for github.com, see [abapGit Documentation](https://docs.abapgit.org/guide-ssl-setup.html).  

## Set Up Namespace
SAP uses a reserved namespace for the demo objects. 

To enable the namespace in your customer system, follow the steps described in [Setting Up a Namespace for Development](https://help.sap.com/docs/SAP_NETWEAVER_750/4a368c163b08418890a406d413933ba7/bdddbe08ac5c11d2850e0000e8a57770.html). For step 8, enter the following values: 
* Namespace: `/DMO/`
* Namespace Role : `C`
* Repair license: `32869948212895959389`
* Short Text: Enter a suitable description for the namespace  , for example `SAP Demo Scenarios`.
* Owner: `SAP` 

Choose `save` and write the changes to a transport. 

To be able to import /DMO/ objects into your system, set the system change option. Proceed as follows: 
1.	Go to  <em>Transport Organizer Tools</em> (transaction `SE03`) 
2.	Go to <em>Administration</em> and start the program `Set System Change Option`.
3.	In the table <em>Namespace/Name Range</em> table search for the <em>/DMO/</em> namespace. 
4.	In the column <em>Modifiable</em> change the entry to `Modifiable`. 
5.	Save the settings.

For more information, see [Setting the System Change Option](https://help.sap.com/docs/SAP_NETWEAVER_750/4a368c163b08418890a406d413933ba7/5738de9b4eb711d182bf0000e829fbfe.html). 

## Download
Use the <em>zabapgit_standalone</em> program to install the <em>ABAP Flight Reference Scenario</em> by executing the following steps:
1.	In your ABAP project, create the package `/DMO/FLIGHT` as target package for the demo content. Use `HOME` as software component. Assign it to a new transport request that you only use for the demo content import. 
2.	In your ABAP project, run the program `zabapgit_standalone`.  
3.	Choose `New Online` and enter the following URL of this repository  `https://github.com/SAP/abap-platform-refscen-flight.git`. 
4.	In the package field, enter the newly created package `/DMO/FLIGHT`. In the branch field, select the branch `ABAP-platform-2023`.
5.	Leave the other fields unchanged and choose `Create Online Repo`.
6. Enter your credentials for abapgit. You will see the available artifacts to import into your ABAP system. 
7.	Choose `Pull` and confirm every subpackage on your transport request. 
8.	Select the package `/DMO/FLIGHT` to be overwritten with the demo content. In some cases, the shown ZABAPGIT dialogue offers you to delete the /DMO/ namespace locally. Do not delete the /DMO/ namespace locally because the pull operation will fail if no suiting namespace exists in your package.
9. You will get an information screen telling you to only make repairs when they are urgent, which you can confirm. You can also confirm the dialogue telling you that objects can only be created in the package of the namespace /DMO/.  
10. In the following screen, select all inactive objects and confirm the activation.
11.	Once the cloning has finished, refresh your project tree.


As a result of the installation procedure above, the ABAP system creates an inactive version of all artifacts from the demo content and adds the following sub packages to the target package: 
* `/DMO/FLIGHT_LEGACY`
* `/DMO/FLIGHT_REUSE` The reuse package contains a package for the supplement business object `/DMO/FLIGHT_REUSE_SUPPLEMENT`, which is reused in the other development scenarios. The reuse package also contains the package `/DMO/FLIGHT_REUSE_CARRIER`, which contains a mulit-inline-edit scenario for maintaining carrier data (see [Developing Transactional Apps with Multi-Inline-Edit Capabilities](https://help.sap.com/docs/ABAP_PLATFORM_NEW/fc4c71aa50014fd1b43721701471913d/f713ec52bcb8405ca9262918cffa5d25.html?version=202210.000)).
Lastly, the reuse package contains the package `/DMO/FLIGHT_REUSE_AGENCY` which incorporates a business object for administering agency master data, including the possibility of maintaining Large Objects. The business object is extensibility-enabled as described in the RAP extensibility guide (see [Extend](https://help.sap.com/docs/ABAP_PLATFORM_NEW/fc4c71aa50014fd1b43721701471913d/492d88ed89f640e5b18dd1c57f6817b1.html?version=202210.000)). This extensibility guide also contains examples on how to develop extensions for the business object. These code examples are contained in sub packages of the `/DMO/FLIGHT_REUSE_AGENCY` package.
* `/DMO/FLIGHT_READONLY` - represents a read-only list reporting app (see [Developing Read-Only List Reporting Apps](https://help.sap.com/docs/ABAP_PLATFORM_NEW/fc4c71aa50014fd1b43721701471913d/504035c0850f44f787f5b81e35791d10.html?version=202210.000)).
* `/DMO/FLIGHT_MANAGED` - represents the transactional app with implementation type <em>managed</em> (see [Developing Managed Transactional Apps](https://help.sap.com/docs/ABAP_PLATFORM_NEW/fc4c71aa50014fd1b43721701471913d/b5bba99612cf4637a8b72a3fc82c22d9.html?version=202210.000)).
* `/DMO/FLIGHT_UNMANAGED` - represents the transactional app with implementation type <em>unmanaged</em> (see [Developing Unmanaged Transactional Apps](https://help.sap.com/docs/ABAP_PLATFORM_NEW/fc4c71aa50014fd1b43721701471913d/f6cb3e3402694f5585068e5e5161a7c1.html?version=202210.000)).
* `/DMO/FLIGHT_DRAFT` - represents the transactional app with <em>draft</em> (see [Developing Transactional Apps with Draft Capabilities](https://help.sap.com/docs/ABAP_PLATFORM_NEW/fc4c71aa50014fd1b43721701471913d/71ba2bec1d0d4f22bc344bba6b569f2e.html?version=202210.000)).

NOTE: The service bindings of the development scenarios are imported with the warning: `To enable activation of local service endpoint, generate service artifacts`. 

NOTE: If you pull the repository again after a successfull import, make sure that you do not delete the local objects `G4BA`, `SUSH` and `NSPC`.

## Configuration
Service definitions need a provider contract before they can be released for the release contract Extend (C0). The service definition /DMO/UI_AGENCY from the package /DMO/FLIGHT_REUSE_AGENCY is shipped without this release contract for maintenance reasons and does not contain a provider contract. If you want to release the service definition /DMO/UI_AGENCY for the release contract Extend (C0), you need to define a suitable provider contract first. You can also directly copy the source code from [service_definition_agency](service_definition_agency). Activate the service definition after.

To generate service artifacts for the service bindings:
1. In each service binding, choose the button `Publish`.

To fill the demo database tables with sample business data: 
1. Expand the package structure in the Project Explorer `/DMO/FLIGHT_LEGACY` > `Source Code Library` > `Classes`.
2. Select the data generator class `/DMO/CL_FLIGHT_DATA_GENERATOR` and press `F9` (Run as Console Application). 

NOTE: In case the activation via the button in the service bindings is not possible, you can use Gateway tools to activate the service, see [here](https://help.sap.com/docs/ABAP_PLATFORM_NEW/fc4c71aa50014fd1b43721701471913d/b58a3c27df4e406f9335d4b346f6be04.html?version=202210.LATEST#%EE%81%B0-service-transport2).  

NOTE: The namespace /DMO/ is reserved for the demo content. Apart from the downloaded demo content, do not use the namespace /DMO/ and do not create any development objects in the downloaded packages. You can access the development objects in /DMO/ from your own namespace.


## How to obtain support
This project is provided "as-is": there is no guarantee that raised issues will be answered or addressed in future releases.

## License
Copyright (c) 2018-2023 SAP SE or an SAP affiliate company. All rights reserved.
This project is licensed under the SAP Sample Code License except as noted otherwise in the [LICENSE](LICENSES/Apache-2.0.txt) file.
