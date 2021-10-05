[![REUSE status](https://api.reuse.software/badge/github.com/SAP-samples/abap-platform-refscen-flight)](https://api.reuse.software/info/github.com/SAP-samples/abap-platform-refscen-flight)

# ABAP Flight Reference Scenario for the ABAP RESTful Application Programming Model 
The ABAP RESTful Application Programming Model defines the architecture for efficient end-to-end development of intrinsically SAP HANA-optimized Fiori apps on Application Server ABAP. It supports the development of all types of Fiori applications as well as Web APIs. It is based on technologies and frameworks such as Core Data Services (CDS) for defining semantically rich data models and a service model infrastructure for creating OData services with bindings to an OData protocol and ABAP-based application services for custom logic and SAPUI5-based user interfaces.

The ABAP Flight Reference Scenario provides sample data and services as well as legacy business logic to get familiar with the ABAP RESTful Application Programming Model. You can check out the end-to-end scenarios or build your own app based on the sample data.

For more information, see [Downloading the ABAP Flight Reference Scenario](https://help.sap.com/viewer/fc4c71aa50014fd1b43721701471913d/202110.000/en-US/def316685ad14033b051fc4b88db07c8.html).

## Prerequisites
Make sure to fulfill the following requirements:
* You are working on Application Server ABAP 7.56 or higher. 
* You have downloaded and installed ABAP Development Tools (ADT). Make sure to use the most recent version as indicated on the [installation page](https://tools.hana.ondemand.com/#abap). 
* You have created an ABAP Project in ADT that allows you to access your Application Server as mentioned above. Your log-on language is English.
* You have downloaded and installed the `zabapgit_standalone` report. Make sure to use the most recent version as indicated on the [installation page](https://docs.abapgit.org/). 
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
Use the <em>zabapgit_standalone</em> program to install the <em>ABAP Flight Reference Scenario</em> by executing the following steps:
1.	In your ABAP project, create the package `/DMO/FLIGHT` as target package for the demo content. Use `HOME` as software component. Assign it to a new transport request that you only use for the demo content import. 
2.	In your ABAP project, run the program `zabapgit_standalone`.  
3.	Choose `New Online` and enter the following URL of this repository  `https://github.com/SAP/abap-platform-refscen-flight.git`. 
4.	In the package field, enter the newly created package `/DMO/FLIGHT`. In the branch field, select the branch `On-Premise-2021`.
5.	Leave the other fields unchanged and choose `Create Online Repo`.
6. Enter your credentials for abapgit. You will see the available artifacts to import into your ABAP system. 
7.	Choose `Pull` and confirm every subpackage on your transport request. 
8.	Select the package `/DMO/FLIGHT` to be overwritten with the demo content and again confirm the transport request. You will get an information screen telling you to only make repairs when they are urgent, which you can confirm. You can also confirm the dialogue telling you that objects can only be created in the package of the namespace /DMO/.
9. In the following screen, select all inactive objects and confirm the activation. 
10.	Once the cloning has finished, refresh your project tree.


As a result of the installation procedure above, the ABAP system creates all development objects of the demo content and adds the following sub packages to the target package: 
* `/DMO/FLIGHT_LEGACY`
* `/DMO/FLIGHT_REUSE`
* `/DMO/FLIGHT_REUSE_CARRIER` - represents a transactional app with multi-inline-edit capabilities (see also: corresponding [end-to-end guide](https://help.sap.com/viewer/fc4c71aa50014fd1b43721701471913d/202110.000/en-US/f713ec52bcb8405ca9262918cffa5d25.html)).
* `/DMO/FLIGHT_REUSE_SUPPLEMENT` - represents a transactional app for editing language-dependent fields by means of an augmentation implementation (see also: corresponding [end-to-end guide](https://help.sap.com/viewer/fc4c71aa50014fd1b43721701471913d/202110.000/en-US/fc374ac9b02e4dbcba356afc77432dc2.html)).
* `/DMO/FLIGHT_READONLY` - represents a read-only list reporting app (see also: corresponding [end-to-end guide](https://help.sap.com/viewer/fc4c71aa50014fd1b43721701471913d/202110.000/en-US/504035c0850f44f787f5b81e35791d10.html)).
* `/DMO/FLIGHT_MANAGED` - represents the transactional app with implementation type <em>managed</em> (see also: corresponding [end-to-end guide](https://help.sap.com/viewer/fc4c71aa50014fd1b43721701471913d/202110.000/en-US/b5bba99612cf4637a8b72a3fc82c22d9.html)).
* `/DMO/FLIGHT_UNMANAGED` - represents the transactional app with implementation type <em>unmanaged</em> (see also: corresponding [end-to-end guide](https://help.sap.com/viewer/fc4c71aa50014fd1b43721701471913d/202110.000/en-US/f6cb3e3402694f5585068e5e5161a7c1.html)).
* `/DMO/FLIGHT_DRAFT` - represents the transactional app with <em>draft</em> (see also: corresponding [end-to-end guide](https://help.sap.com/viewer/fc4c71aa50014fd1b43721701471913d/202110.000/en-US/71ba2bec1d0d4f22bc344bba6b569f2e.html)).


NOTE: The demo packages do not include <em>service bindings</em>. They must be created to complete the services (see configuration section). Then you can run, for example, the UI services with the <em>Fiori Elements</em> preview in the <em>service binding</em>.

## Configuration
To fill the demo database tables with sample business data: 
1. Expand the package structure in the Project Explorer `/DMO/FLIGHT_LEGACY` > `Source Code Library` > `Classes`.
2. Select the data generator class `/DMO/CL_FLIGHT_DATA_GENERATOR` and press `F9` (Run as Console Application). 

#### Read-Only Scenario
To create the missing <em>service bindings</em> for the read-only list reporting app (package `/DMO/FLIGHT_READONLY`):

1.	Right-click the service definition `/DMO/FLIGHT_R` and choose `New Service Binding` (see [here](https://help.sap.com/viewer/fc4c71aa50014fd1b43721701471913d/202110.000/en-US/777e027f61c3490dba0433443d9143a6.html) for additional information).  
2.	Specify your own package and your own namespace when following the steps in the creation wizard. A service binding cannot be created in the namespace <em>/DMO/</em>. 
3.	Choose the  `Publish` button in the service binding editor.  

#### Unmanaged Scenario
To create the missing <em>service bindings</em> for the transactional app with implementation type <em>unmanaged</em> (package `/DMO/FLIGHT_UNMANAGED`):

1. Right-click the service definition `/DMO/TRAVEL_U` and choose `New Service Binding` (see [here](https://help.sap.com/viewer/fc4c71aa50014fd1b43721701471913d/202110.000/en-US/ade0637f7c554c229cbfd4dc02c7fcaa.html) for additional information).  
2. Specify your own package and your own namespace when following the steps in the creation wizard. A service binding cannot be created in the namespace <em>/DMO/</em>. 
3.	Choose the  `Publish` button in the service binding editor.  

#### Managed Scenario
To create the missing <em>service bindings</em> for the transactional app with implementation type <em>managed</em> (package `/DMO/FLIGHT_MANAGED`):

1. Right-click the service definition `/DMO/UI_TRAVEL_PROCESSOR_M` and choose `New Service Binding` (see [here](https://help.sap.com/viewer/fc4c71aa50014fd1b43721701471913d/202110.000/en-US/3dd07cd970d7433584f1b86588176bf1.html) for additional information). 
2. Specify your own package and your own namespace when following the steps in the creation wizard. A service binding cannot be created in the namespace <em>/DMO/</em>. 
3. Use `ODATA V2 UI` binding type.
4.	Choose the  `Publish` button in the service binding editor.   


5. Right-click the service definition `/DMO/UI_TRAVEL_APPROVER_M` and choose `New Service Binding` (see [here](https://help.sap.com/viewer/fc4c71aa50014fd1b43721701471913d/202110.000/en-US/3dd07cd970d7433584f1b86588176bf1.html) for additional information). 
6. Specify your own package and your own namespace when following the steps in the creation wizard. A service binding cannot be created in the namespace <em>/DMO/</em>. 
7. Use `ODATA V2 UI` binding type.
8.	Choose the  `Publish` button in the service binding editor. 

#### Draft Scenario
To create the missing <em>service bindings</em> for the transactional app with <em>draft</em> (package `/DMO/FLIGHT_DRAFT`):

1. Right-click the service definition `/DMO/UI_TRAVEL_A_D` and choose `New Service Binding` (see [here](https://help.sap.com/viewer/fc4c71aa50014fd1b43721701471913d/202110.000/en-US/ba62670e882e4b12aa7f7e545dfebe31.html) for additional information). 
2. Specify your own package and your own namespace when following the steps in the creation wizard. A service binding cannot be created in the namespace <em>/DMO/</em>. 
3. Use `ODATA V2 UI` binding type.
4.	Choose the  `Publish` button in the service binding editor.   


5. Right-click the service definition `/DMO/UI_TRAVEL_D_D` and choose `New Service Binding` (see [here](https://help.sap.com/viewer/fc4c71aa50014fd1b43721701471913d/202110.000/en-US/b10ea0d6cae24a20910bf337c1e5c1cb.html) for additional information). 
6. Specify your own package and your own namespace when following the steps in the creation wizard. A service binding cannot be created in the namespace <em>/DMO/</em>. 
7. Use `ODATA V2 UI` binding type.
8.	Choose the  `Publish` button in the service binding editor. 

#### Multi-inline-edit Scenario
To create the missing <em>service bindings</em> for the multi-inline-edit sceanrio (package `/DMO/FLIGHT_REUSE_CARRIER`):

1. Right-click the service definition `/DMO/UI_Carriers_S` and choose `New Service Binding` (see [here](https://help.sap.com/viewer/fc4c71aa50014fd1b43721701471913d/202110.000/en-US/ade0637f7c554c229cbfd4dc02c7fcaa.html) for additional information).  
2. Specify your own package and your own namespace when following the steps in the creation wizard. A service binding cannot be created in the namespace <em>/DMO/</em>. 
3.	Choose the  `Publish` button in the service binding editor. 

#### Augmentation Scenario
To create the missing <em>service bindings</em> for the augmentation sceanrio (package `/DMO/FLIGHT_REUSE_SUPPLEMENT`):

1. Right-click the service definition `/DMO/UI_Supplement` and choose `New Service Binding` (see [here](https://help.sap.com/viewer/fc4c71aa50014fd1b43721701471913d/202110.000/en-US/ade0637f7c554c229cbfd4dc02c7fcaa.html) for additional information).  
2. Specify your own package and your own namespace when following the steps in the creation wizard. A service binding cannot be created in the namespace <em>/DMO/</em>. 
3.	Choose the  `Publish` button in the service binding editor. 
 
NOTE: The namespace /DMO/ is reserved for the demo content. Apart from the downloaded demo content and the development objects that need to be created to complete the scenario, do not use the namespace /DMO/ and do not create any development objects in the downloaded packages. You can access the development objects in /DMO/ from your own namespace.


## How to obtain support
This project is provided "as-is": there is no guarantee that raised issues will be answered or addressed in future releases.

## License
Copyright (c) 2018-2021 SAP SE or an SAP affiliate company. All rights reserved.
This project is licensed under the SAP Sample Code License except as noted otherwise in the [LICENSE](LICENSES/Apache-2.0.txt) file.

