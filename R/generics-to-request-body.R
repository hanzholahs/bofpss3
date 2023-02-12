pss_to_request_body <- function(object) {
  UseMethod("pss_to_request_body")
}

pss_to_request_body.pss_system_dataset <- function(object) {
  system_id <- pss_search_system(object$proj_name, object$system_name)$id

  list(
    systemId = system_id,
    datasetId = object$system_data_id,
    name = object$system_name,
    description = object$dataset_description,
    type = object$system_type,
    creditAvailability = object$credit_availability,
    transferBalances = object$transfer_balances,
    bilateralLimitUse = object$bilateral_limit_use,
    transferTransactions = object$transfer_transactions,
    openingTime = object$opening_time,
    closingTime = object$closing_time,
    systemAlgorithms = object$system_algorithms,
    editable = object$editable
  )
}
