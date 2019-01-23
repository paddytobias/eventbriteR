% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_eb_attendees_function.R
\name{get_eb_attendees}
\alias{get_eb_attendees}
\title{Get Eventbrite attendees}
\usage{
get_eb_attendees(event, names.only = TRUE)
}
\arguments{
\item{event}{the event object returned from get_eb_event(url)}

\item{names.only}{Default set TRUE. To return a clean data.frame of orders, which only contains names of registrations. If user wants all data in output, they can set names.only=FALSE}
}
\description{
This function returns the Eventbrite orders of an event. The function only works on Events that hold order data. As such, it is designed to be used on the output of 
Or call_eventbrite(sub_query="attendees") >> get_eb_attendees()
}
\examples{
url = connect_eb(event_id = event_id, sub_query = "orders", token = token) # will return the url for a given event's orders/registrations (using event_id)
event = query_eb(url)
orders = get_eb_orders(event)
}