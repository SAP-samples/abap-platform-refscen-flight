extend view entity /DMO/R_AgencyTP with
association [0..1] to /DMO/ZZ_P_Agency_Review_Avg as /DMO/ZZ_ReviewAvgZAG on /DMO/ZZ_ReviewAvgZAG.AgencyId = Agency.AgencyID
{
  /DMO/ZZ_ReviewAvgZAG
}
