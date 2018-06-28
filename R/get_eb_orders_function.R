#' Get Eventbrite orders
#'
#' This function returns the Eventbrite orders of an event. The function only works on Events that hold order data. As such, it is designed to be used on the output of call_eventbrite(sub_query="orders") >> get_eb_event()
#' @param event the event object returned from get_eb_event(url)
#' @param names.only Default set TRUE. To return a clean data.frame of orders, which only contains names of registrations. If user wants all data in output, they can set names.only=FALSE
#' @export
#' @examples
#' url = call_eventbrite(event_id = event_id, sub_query = "orders", token = token) # will return the url for a given event's orders/registrations (using event_id)
#' event = get_eb_event(url)
#' orders = get_eb_orders(event)


get_eb_orders = function(event, names.only = TRUE){
  if (!("orders" %in% names(event))){
    stop("Event doesn't contain orders information")
  }

  if (names.only == TRUE){
    orders = event$orders[,grepl("name|email", names(event$orders))]
    message("You are only getting names of registered people. Use names.only=FALSE if you want all results")
  } else {
    orders = event$orders
  }
  return(orders)
}
