#' Get Eventbrite object
#'
#' This function compiles extracts the Eventbrite's event data from its original JSON form.
#' function relies on "jsonlite" package and will install if it doesn't exist
#' @param url the Eventbrite API url to call. This URL can be generated with call_eventbrite
#' @import "jsonlite"
#' @export
#' @examples
#'
#' event = eb_query(event_id = event_id, sub_query = "orders", token = token) %>%
#' get_eb_orders(event)

eb_query <- function(query = "events", query_id = NA, sub_query = NA, sub_id = NA, token = NA){
  url <- call_eventbrite(query = query, query_id = query_id, sub_query = sub_query, sub_id = sub_id,
                         token = token)
  result <- jsonlite::fromJSON(url)
  object <-  names(result)[-grepl("pagination", names(result))]
  message(paste("Your connection has returned:", object))
  return(result)
}

