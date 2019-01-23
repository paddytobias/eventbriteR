#' Get Eventbrite attendees
#'
#' This function returns the Eventbrite attendees of an event. The function only works on Events that hold attendees data. As such, it is designed to be used on the output of
#' Or call_eventbrite(sub_query="attendees") >> get_eb_attendees()
#' It will return a dataframe of attendees of the course including a column giving a status on if they were "checked-in"
#' @param event the event object returned from get_eb_attendees(url)
#' @param names.only Default set TRUE. To return a clean data.frame of attendees, which only contains names of registrations. If user wants all data in output, they can set names.only=FALSE
#' @export
#' @examples
#' url = connect_eb(event_id = event_id, sub_query = "attendees", token = token) # will return the url for a given event's orders/registrations (using event_id)
#' attendees = query_eb(url)
#' attendees_names = get_eb_orders(attendees)

get_eb_attendees = function(event, names.only = TRUE){
  if (!("attendees" %in% names(event))){
    stop("Event doesn't contain orders information")
  }


  if (names.only == TRUE){
    attendees = event$attendees$profile[,grepl("name|email", names(event$attendees$profile))]

    message("You are only getting names of registered people. Use names.only=FALSE if you want all results")
  } else {
    attendees = event$attendees$profile
  }
  checked_in_status = attendees$attendees$checked_in
  attendee_names$checked_in = checked_in_status
  return(attendees)
}
