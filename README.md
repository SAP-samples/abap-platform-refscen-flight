# ABAP Flight Reference Scenario for the ABAP RESTful Application Programming Model 
The ABAP RESTful Application Programming Model defines the architecture for efficient end-to-end development of intrinsically SAP HANA-optimized Fiori apps on Application Server ABAP. It supports the development of all types of Fiori applications as well as Web APIs. It is based on technologies and frameworks such as Core Data Services (CDS) for defining semantically rich data models and a service model infrastructure for creating OData services with bindings to an OData protocol and ABAP-based application services for custom logic and SAPUI5-based user interfaces.

The ABAP Flight Reference Scenario provides sample data and services as well as legacy business logic to get familiar with the ABAP RESTful Application Programming Model. You can check out the end-to-end scenarios or build your own app based on the sample data.

For more information, see [Downloading the ABAP Flight Reference Scenario](https://help.sap.com/viewer/fc4c71aa50014fd1b43721701471913d/201909.000/en-US/def316685ad14033b051fc4b88db07c8.html).

## Prerequisites
Make sure to fulfill the following requirements:
* You are working on Application Server ABAP 7.54 or higher. 
* You have downloaded and installed ABAP Development Tools (ADT). Make sure to use the most recent version as indicated on the [installation page](https://tools.hana.ondemand.com/#abap). 
* You have created an ABAP Project in ADT that allows you to access your Application Server as mentioned above. Your log-on language is English.
* You have downloaded and installed the `zabapgit` report. Make sure to use the most recent version as indicated on the [installation page](https://docs.abapgit.org/). 
* You have installed the certificate files for github.com, see [abapGit Documentation](https://docs.abapgit.org/guide-ssl-setup.html).  

## Set Up Namespace
SAP uses a reserved namespace for the demo objects. 

To enable the namespace in your customer system, follow the steps described in [Setting Up a Namespace for Development](https://help.sap.com/viewer/4a368c163b08418890a406d413933ba7/LATEST/en-US/bdddbe08ac5c11d2850e0000e8a57770.html). For step 8, enter the following values: 
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

For more information, see [Setting the System Change Option](https://help.sap.com/viewer/4a368c163b08418890a406d413933ba7/LATEST/en-US/5738de9b4eb711d182bf0000e829fbfe.html). 


## Download
Use the <em>zabapgit</em> to install the <em>ABAP Flight Reference Scenario</em> by executing the following steps:
1.	In your ABAP project, create the package `/DMO/FLIGHT` as target package for the demo content. Use `HOME` as software component. Assign it to a new transport request that you only use for the demo content import. 
2.	In your ABAP project, run the program `zabapgit`.  
3.	Choose `+Online` and enter the following URL of this repository  `https://github.com/SAP/abap-platform-refscen-flight.git`. 
4.	In the package field, enter the newly created package `/DMO/FLIGHT`.
5.	Leave the other fields unchanged and choose `OK`. You will see the available artifacts to import into your ABAP system. 
6.	To select the on-premise branch, choose `Branch` > `Switch` and select `On-Premise-1909`. 
7.	Choose `Pull` and confirm every subpackage on your transport request. 
8.	Select the package `/DMO/FLIGHT` to be overwritten with the demo content and again confirm the transport request. You will get an information screen telling you to only make repairs when they are urgent, which you can confirm.  
9.	Once the cloning has finished, refresh your project tree.


As a result of the installation procedure above, the ABAP system creates an inactive version of all artifacts from the demo content and adds the following sub packages to the target package: 
* `/DMO/FLIGHT_LEGACY`
* `/DMO/FLIGHT_REUSE`
* `/DMO/FLIGHT_READONLY` - represents a read-only list reporting app (see also: corresponding [end-to-end guide](https://help.sap.com/viewer/fc4c71aa50014fd1b43721701471913d/201909.000/en-US/504035c0850f44f787f5b81e35791d10.html).
* `/DMO/FLIGHT_UNMANAGED` - represents the transactional app with implementation type <em>unmanaged</em> (see also: corresponding [end-to-end guide](https://help.sap.com/viewer/fc4c71aa50014fd1b43721701471913d/201909.000/en-US/f6cb3e3402694f5585068e5e5161a7c1.html).


NOTE: The demo packages do not include <em>behavior definitions</em>, <em>service definitions</em> or <em>service bindings</em>. They must be created to complete the service (see configuration section). Then you can run, for example, the UI services with the <em>Fiori Elements</em> preview in the <em>service binding</em>.

## Configuration
To activate all development objects from the `/DMO/FLIGHT` package: 
1. Click the mass-activation icon (<em>Activate Inactive ABAP Development Objects</em>) in the toolbar.  
2. In the dialog that appears, select all development objects in the transport request (that you created for the demo content installation) and choose `Activate`.

To fill the demo database tables for the read-only and the unmanaged scenario with sample business data: 
1. Expand the package structure in the Project Explorer `/DMO/FLIGHT_LEGACY` > `Source Code Library` > `Classes`.
2. Select the data generator class `/DMO/CL_FLIGHT_DATA_GENERATOR` and press `F9` (Run as Console Application). 

To create the missing development objects (<em>service definition</em> and <em>service binding</em>) for the read-only list reporting app (package `/DMO/FLIGHT_READONLY`):
##### Service Definititon
1. Right-click the CDS entity `/DMO/I_CONNECTION_R` and choose `New Service Definition` (see [here](https://help.sap.com/viewer/fc4c71aa50014fd1b43721701471913d/201909.000/en-US/ade0637f7c554c229cbfd4dc02c7fcaa.html) for additional information). Use the name `/DMO/FLIGHT_R`.
2. Include the CDS views that are relevant for the read-only scenario to be exposed in the service definition (see [here](https://help.sap.com/viewer/fc4c71aa50014fd1b43721701471913d/201909.000/en-US/fb9cb12aebf94852bac4665c2db2a25a.html) for additional information). You can also directly copy the source code from [Service Definition Read-Only /DMO/FLIGHT_R](service_definition_read_only)
3. Activate the service definition.


##### Service Binding
4.	Right-click the newly created service definition `/DMO/FLIGHT_R` and choose `New Service Binding` (see [here](https://help.sap.com/viewer/fc4c71aa50014fd1b43721701471913d/2019.000/en-US/777e027f61c3490dba0433443d9143a6.html) for additional information).  
5.	Specify your own package and your own namespace when following the steps in the creation wizard. A service binding cannot be created in the namespace <em>/DMO/</em>. 
6.	Choose the  `Activate` button in the service binding editor.  


To create the missing development objects (<em>behavior definition</em>, <em>service definition</em> and <em>service binding</em> ) for the transactional app with implementation type <em>unmanaged</em> (package `/DMO/FLIGHT_UNMANAGED`):
##### Behavior Definition
1.	Right-click the CDS root entity `/DMO/I_TRAVEL_U` and choose `New Behavior Definition`. In the creation wizard, choose implementation type `unmanaged`. (see [here](https://help.sap.com/viewer/fc4c71aa50014fd1b43721701471913d/201909.000/en-US/453ff778fd9c4bb0a0b1cd4b19afb8f3.html) for additional information).
2.	Copy the source code for the behavior definition from [Behavior Definition Unmanaged /DMO/I_TRAVEL_U](behavior_definition_unmanaged). 
3.	Activate the behavior definition. 


##### Service Definition
4.	Right-click the CDS root entity `/DMO/I_TRAVEL_U` and choose `New Service Definition` (see [here](https://help.sap.com/viewer/fc4c71aa50014fd1b43721701471913d/201909.000/en-US/453ff778fd9c4bb0a0b1cd4b19afb8f3.html) for additional information). Use the name `/DMO/TRAVEL_U`.
5.	Include the CDS views that are relevant for the unmanaged scenario to be exposed in the service definition. You can also directly copy the source code from [Service Definition Unmanaged /DMO/TRAVEL_U](service_definition_unmanaged).
6.	Activate the service definition. 


##### Service Binding
7. Right-click the service definition `/DMO/TRAVEL_U` and choose `New Service Binding` (see [here](https://help.sap.com/viewer/fc4c71aa50014fd1b43721701471913d/201909.000/en-US/ade0637f7c554c229cbfd4dc02c7fcaa.html). 
8. Specify your own package and your own namespace when following the steps in the creation wizard. A service binding cannot be created in the namespace <em>/DMO/</em>. 
9.	Choose the  `Activate` button in the service binding editor.  

 
NOTE: The namespace /DMO/ is reserved for the demo content. Apart from the downloaded demo content and the development objects that need to be created to complete the scenario, do not use the namespace /DMO/ and do not create any development objects in the downloaded packages. You can access the development objects in /DMO/ from your own namespace.


## How to obtain support
This project is provided "as-is": there is no guarantee that raised issues will be answered or addressed in future releases.

## License
Copyright (c) 2018-2020 SAP SE or an SAP affiliate company. All rights reserved.
This project is licensed under the SAP Sample Code License except as noted otherwise in the [LICENSE](LICENSES/Apache-2.0.txt) file.

[![REUSE status](https://api.reuse.software/badge/github.com/SAP-samples/abap-platform-refscen-flight)](https://api.reuse.software/info/github.com/SAP-samples/abap-platform-refscen-flight)
