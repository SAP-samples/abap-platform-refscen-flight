"! <p class="shorttext synchronized" lang="en">Demos for RAP BO TDF, with /DMO/R_TRAVEL_D BO as dependency</p>
"! Demos for RAP BO Test double framework with /DMO/R_TRAVEL_D BO (a draft enabled BO) as dependency.<br/>
"! Covers isolating dependency on EML with operations on draft instances.<br/><br/>
"! Uses {@link /dmo/tc_travel_d_bo_consumer} as a dummy consumer of /DMO/R_TRAVEL_D i.e. it has code under test methods
"! with EML dependency on /DMO/R_TRAVEL_D BO.
CLASS /dmo/tc_botd_travel_d_demos DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /dmo/tc_botd_travel_d_demos IMPLEMENTATION.
ENDCLASS.
