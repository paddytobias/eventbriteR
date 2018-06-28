#' Get Eventbrite event
#' 
#' This function compiles extracts the Eventbrite's event data from its original JSON form.
#' function relies on "jsonlite" package and will install if it doesn't exist
#' @param url the Eventbrite API url to call. This URL can be generated with call_eventbrite
#' @import "jsonlite"
#' @examples 
#' url = call_eventbrite(event_id = event_id, sub_query = "orders", token = token) # will return the url for a given event's orders/registrations (using event_id)
#' event = get_eb_event(url)
#' orders = get_eb_orders(event)

get_eb_event = function(url){
  #tryCatch(library(jsonlite), error = function(e){install.packages("jsonlite")})
  return(fromJSON(url))
}

