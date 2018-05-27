CreateNeosComm <- function(curlopts = list(httpheader = c(`Content-Type` = "text/xml", 'User-Agent' = "R"), port = 3333), curlhandle = getCurlHandle()){
  url <- "https://www.neos-server.org"
  if(!("httpheader" %in% names(curlopts))){
    stop("\nNo 'httpheader' list element has been specified in 'curlopts'.\n")
  }
  if(!("port" %in% names(curlopts))){
    stop("\nNo 'port' list element has been specified in 'curlopts'.\n")
  }
  if(!(class(curlhandle) == "curl_handle")){
    stop("\nObject for 'curlhandle' must be of class 'curl_handle'.\n")
  }
  result <- new("NeosComm", url = url, curlopts = curlopts, curlhandle = curlhandle)
  return(result)
}
