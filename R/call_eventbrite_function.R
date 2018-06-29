#' Call Eventbrite
#'
#' This function compiles the url required to call the Eventbrite API based on a few parameters.
#' call_eventbrite returns the URL as a string, which should be used with get_eb_event()
#' function relies on "httr" package and will install if it doesn't exist
#' @param query_id the unique id that every Eventbrite event has
#' @param query the top-level query. Defaults to "events". Other query entries could be "users" or "orders".
#' @param sub_query the particular information wanted from the query. If query = "events", then sub_query could be "attendees", "users", "orders", "display_settings", "ticket_classes", "canned_questions", "questions", "discounts", "public_discounts", "access_codes", "transfers", "teams", "ticket_groups", "ticket_classes", "ticket_buyer_settings"
#' @param token the API token needed to call the API. Go here to create your own app and get an API token: http://www.eventbrite.com/myaccount/apps/
#' @import "httr"
#' @export
#' @examples
#' url = call_eventbrite(query_id = query_id, sub_query = "orders", token = token) # will return the url for a given event's orders/registrations (using query_id)
#' event = eb_query(url)
#' orders = get_eb_orders(event)




call_eventbrite = function(query="events", query_id=NA, sub_query=NA, sub_id=NA, token =NA){
  #tryCatch(library(httr), error = function(e){install.packages("httr")})
  baseURL = "https://www.eventbriteapi.com/v3/"
  if (is.na(token)) {
    warning("Please enter API token")
  }
  params = paste0("?token=",token)

  possible_first = c("events", "users", "orders")
  if (query %in% possible_first){
    first = paste0(query, "/")
  } else {
    stop("Not a valid first API call: ", query)
  }

  possible_second= c("attendees", "users", "orders", "display_settings",
                     "ticket_classes", "canned_questions", "questions",
                     "discounts", "public_discounts", "access_codes",
                     "transfers", "teams", "ticket_groups",
                     "ticket_classes", "ticket_buyer_settings", "search")
  if (is.na(sub_query)){
    sub_query = ""
  } else  if (query=="events" & sub_query %in% possible_second){
    second = paste0(sub_query, "/")
  } else {
    stop("Not a valid second API call: ", sub_query)
  }

  if (query != "events" & is.na(query_id)){
    stop("No id inserted for: ", query)
#  } else if (query == "events" & sub_query == "search"){
 #   query_id = "/"
  } else if (is.na(query_id)){
    query_id = ""
  } else {
    query_id = paste0(query_id, "/")
  }



  if (!is.na(sub_id)){
    sub_id = paste0(sub_id, "/")
  } else {
    sub_id =""
  }

  url = paste0(baseURL, first, query_id, sub_query, sub_id, params)

  message("Calling ", url)
  req <- GET(url)

  if(req$status_code != 200){
    stop("Problem with calling the API - response: ", "\n", content(req)[3], ": ",content(req)[2])
  } else {
    message("Calling API with URL was successful")
  }

  return(url)

}





