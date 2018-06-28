#' Call Eventbrite
#' 
#' This function compiles the url required to call the Eventbrite API based on a few parameters.
#' call_eventbrite returns the URL as a string, which should be used with get_eb_event()
#' function relies on "httr" package and will install if it doesn't exist
#' @param event_id the unique id that every Eventbrite event has
#' @param query the top-level query. Defaults to "events". Other query entries could be "users" or "orders".
#' @param sub_query the particular information wanted from the query. If query = "events", then sub_query could be "attendees", "users", "orders", "display_settings", "ticket_classes", "canned_questions", "questions", "discounts", "public_discounts", "access_codes", "transfers", "teams", "ticket_groups", "ticket_classes", "ticket_buyer_settings"
#' @param token the API token needed to call the API. Go here to create your own app and get an API token: http://www.eventbrite.com/myaccount/apps/
#' @examples 
#' url = call_eventbrite(event_id = event_id, sub_query = "orders", token = token) # will return the url for a given event's orders/registrations (using event_id)
#' event = get_eb_event(url)
#' orders = get_eb_orders(event)




call_eventbrite = function(event_id=NA, query="events", sub_query=NA, second_id=NA, token =NA){
  tryCatch(library(httr), error = function(e){install.packages("httr")})
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
                     "ticket_classes", "ticket_buyer_settings")
  if (query=="events" & sub_query %in% possible_second){
    second = paste0(sub_query, "/")
  } else {
    stop("Not a valid second API call: ", sub_query)
  }
  
  if (!is.na(first) & is.na(event_id)){
    stop("No id inserted for: ", first)
  } else {
    event_id = paste0(event_id, "/")
  }
  
  if (!is.na(second_id)){
    second_id = paste0(second_id, "/")
  } else {
    second_id =""
  }
  
  url = paste0(baseURL, first, event_id, second, second_id, params)
  
  message("Calling ", url)
  req <- GET(url)
  
  if(req$status_code != 200){
    stop("Problem with calling the API - response: ", "\n", content(req)[3], ": ",content(req)[2])
  } 
  
  
  return(url)
  
}





