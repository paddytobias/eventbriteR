#' Get Eventbrite object
#'
#' This function is a wrapper for call_eventbrite to simplify getting data after the url has been tested to work. It extracts the Eventbrite's event data from its original JSON format.
#' The function relies on "jsonlite" package and will install if it doesn't exist
#' @param query_id the unique id that every Eventbrite event has
#' @param query the top-level query. Defaults to "events". Other query entries could be "users" or "orders".
#' @param sub_id the unique id for the sub-query
#' @param sub_query the particular information wanted from the query. If query = "events", then sub_query could be "attendees", "users", "orders", "display_settings", "ticket_classes", "canned_questions", "questions", "discounts", "public_discounts", "access_codes", "transfers", "teams", "ticket_groups", "ticket_classes", "ticket_buyer_settings"
#' @param token the API token needed to call the API. Go here to create your own app and get an API token: http://www.eventbrite.com/myaccount/apps/
#' @import "jsonlite"
#' @export
#' @examples
#'
#' event = eb_query(event_id = event_id, sub_query = "orders", token = .Options$eventbrite_token) %>%
#' get_eb_orders(event)

eb_query <- function(query = "events", query_id = NA, sub_query = NA, sub_id = NA, token = eventbrite.token){
  url <- call_eventbrite(query = query, query_id = query_id, sub_query = sub_query, sub_id = sub_id,
                         token = token)
  result <- jsonlite::fromJSON(url)
  object <-  names(result)[-grepl("pagination", names(result))]
  if (length(object)==0){
    object = query
  }
  message(paste("Your query has returned the object:", object))
  result$type <- object
  return(result)
}

