
xml.rpc <- function(url, method, .convert = TRUE, .opts = list(), .curl = NULL, .args = list()) {
    if ( length(.opts) ) {
        ## remove the header since in curl it is set via handle_setheaders
        .opts <- .opts[!grep("header", names(.opts))]
    }

    xmlrpc(url = url, method = method, params = .args, 
           opts = .opts , convert = .convert, handle  = .curl)
}

getCurlHandle <- function() {
    handle <- new_handle()
    handle_setopt(handle, port = 3333)
    handle
}
    