#' Get Eventbrite object
#'
#' This function compiles extracts the Eventbrite's event data from its original JSON form.
#' function relies on "jsonlite" package and will install if it doesn't exist
#' @param url the Eventbrite API url to call. This URL can be generated with call_eventbrite
#' @import "jsonlite"
#' @export
#' @examples
#' url = call_eventbrite(event_id = event_id, sub_query = "orders", token = token) # will return the url for a given event's orders/registrations (using event_id)
#' event = eb_query(url)
#' orders = get_eb_orders(event)

eb_query <- function(url){
  #tryCatch(library(jsonlite), error = function(e){install.packages("jsonlite")})
  result <- fromJSON(url)
  object <-  names(result)[-grepl("pagination", names(result))]
  message(paste("Your query has returned a sub-query for:", object))
  return(result)
}

