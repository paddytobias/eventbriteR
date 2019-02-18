#' Search Eventbrite with various parameters
#'
#' Retrieving events based on a search query. There are numerous parameters that can be used in the query. Possible parameters can be found here: https://www.eventbrite.com/platform/api#/reference/event-search/search-events.
#' In order to give you more control, after running the command, it will display how many pages of data (each a maximum of 50 rows) and ask if you want to continue with the query.
#'
#' A dataframe is returned of all events the meet the query parameters.
#'
#' @param query the search term to query on. The Eventbrite API looks for matches in an event's title.
#' @param ... see possible parameters here: \link{https://www.eventbrite.com/platform/api#/reference/event-search/search-events}. Use the same parameter names as found in the API documentation.
#' @param token the API token needed to call the API. Go here to create your own app and get an API token: http://www.eventbrite.com/myaccount/apps/
#' @import dplyr
#' @export
#'
#' @examples
#' python_courses = search_eventbrite(query = "python", location.address="Melbourne", location.within="50km")


search_eventbrite = function(query, ..., token = .Options$eventbrite.token){
  response = get_search(query = query, ... , token = token)

  n_pages = response$pagination$page_count
  message("There are ", n_pages, " pages to collect data from\n")
  dat = data.frame(list.flatten(response$events), stringsAsFactors = F)
  if(n_pages > 1){
  	## prompt to continue?
  	answer=FALSE
  	while(answer == FALSE){
    	to_continue = readline("Do you want to continue? Y / n")
    	answer = to_continue %in% c("Y", "n")
  	}

  	if (to_continue == "Y"){
    	#dat = data.frame(list.flatten(response$events), stringsAsFactors = F)
    	message("Collected from page 1\n")
    	for (i in 2:n_pages){
      		response = get_search(query = query, ... , token = token, page = i)
      		message("Collected from page ", i, "\n")
      		response = data.frame(list.flatten(response$events), stringsAsFactors = F)

      		dat = suppressMessages(dplyr::full_join(dat, response))
    		}
  		}
  	}

  return(dat)
}


