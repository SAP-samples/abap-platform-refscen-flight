"! <p class="shorttext synchronized" lang="en">Demos for RAP BO TDF, with /DMO/I_TRAVEL_M BO as dependency</p>
"! Demos for RAP BO Test double framework to isolate dependency on /DMO/I_TRAVEL_M BO.<br/><br/>
"! Uses {@link /dmo/tc_travel_m_bo_consumer} as a dummy consumer of /DMO/I_TRAVEL_M i.e. it has code under test methods
"! with EML dependency on /DMO/I_TRAVEL_M BO.
CLASS /dmo/tc_botd_travel_m_demos DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /dmo/tc_botd_travel_m_demos IMPLEMENTATION.
ENDCLASS.
