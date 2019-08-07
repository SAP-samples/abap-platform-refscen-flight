@AbapCatalog.sqlViewName: '/DMO/ITRAVEL_M'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'Travel view - CDS data model'

define root view /DMO/I_Travel_M
  as select from /dmo/travel_m           as Travel -- the travel table is the data source for this view

  composition [0..*] of /DMO/I_Booking_M as _Booking

  association [0..1] to /DMO/I_Agency    as _Agency   on $projection.agency_id     = _Agency.AgencyID
  association [0..1] to /DMO/I_Customer  as _Customer on $projection.customer_id   = _Customer.CustomerID
  association [0..1] to I_Currency       as _Currency on $projection.currency_code = _Currency.Currency
  
 
{   
  key travel_id,      
    agency_id,         
    customer_id,  
    begin_date,  
    end_date,   
    @Semantics.amount.currencyCode: 'currency_code'
    booking_fee,
    @Semantics.amount.currencyCode: 'currency_code'
    total_price,  
    @Semantics.currencyCode: true
    currency_code,  
    overall_status,
    description,   
    @Semantics.user.createdBy: true 
    created_by,
    @Semantics.systemDateTime.createdAt: true       
    created_at,
    @Semantics.user.lastChangedBy: true    
    last_changed_by,
    @Semantics.systemDateTime.lastChangedAt: true
    last_changed_at,                -- used as etag field
    
    /* Associations */
    _Booking,
    _Agency,
    _Customer, 
    _Currency
}

