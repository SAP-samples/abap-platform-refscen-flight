@AbapCatalog.sqlViewName: '/DMO/CURRHLP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Help View for Currency Conversion'
define view /DMO/CURRENCY_HELPER
  with parameters
    amount             : /dmo/total_price,
    source_currency    : /dmo/currency_code,
    target_currency    : /dmo/currency_code,
    exchange_rate_date : /dmo/booking_date

  as select from /dmo/agency

{
  key currency_conversion( amount             => $parameters.amount,
                           source_currency    => $parameters.source_currency,
                           target_currency    => $parameters.target_currency,
                           exchange_rate_date => $parameters.exchange_rate_date,
                           error_handling     => 'SET_TO_NULL' ) as ConvertedAmount
}
