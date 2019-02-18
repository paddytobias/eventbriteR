#' Utilities
#' @import jsonlite
#' @export

get_search = function(query, ..., token = eventbrite.token){
  baseURL = "https://www.eventbriteapi.com/v3/events/search?"
  call = GET(baseURL,
             query = list(q = query, ...),
             add_headers("Authorization" = paste("Bearer", token), "Content-Type" = "application/json")
  )

  url = paste0(call$url, "&token=", token)
  print(url)
  response = jsonlite::fromJSON(url)
  return(response)
}

# borrowed from https://github.com/renkun-ken/rlist/blob/master/R/list.flatten.R. For some reason, can't "import" with roxygen2 so saving it locally
list.flatten <- function(x, use.names = TRUE, classes = "ANY") {
  len <- sum(rapply(x, function(x) 1L, classes = classes))
  y <- vector("list", len)
  i <- 0L
  items <- rapply(x, function(x) {
    i <<- i + 1L
    y[[i]] <<- x
    TRUE
  }, classes = classes)
  if (use.names && !is.null(nm <- names(items)))
    names(y) <- nm
  y
}
