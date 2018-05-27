##
## XML-RPC method: getIntermediateResults
##
NgetIntermediateResults <- function (obj, offset = NULL, convert = TRUE){
    if (!(class(obj) == "NeosJob")) {
        stop("\nObject 'obj' is not of class 'NeosJob'.\n")
    }
    call <- match.call()
    jobnumber <- obj@jobnumber
    password <- obj@password
    nc <- obj@nc
    if(is.null(offset)){
        offset <- as.integer(0)
    } else {
        offset <- as.integer(offset)
    }
    ans <- xml.rpc(url = nc@url, method = "getIntermediateResults",
                   .args = list(jobnumber = jobnumber, password = password, offset = offset),
                   .convert = FALSE, .opts = nc@curlopts, .curl = nc@curlhandle)

    tmp <- rawToChar(ans$content)
    if ( convert ) {
        ans <- xmlrpc_to_r(tmp)
        offset <- as.integer(ans[[2]])
        ans <- if ( nchar(ans[[1]]) ) ans[[1]] else "\nNothing left to return from NEOS.\n"
    } else {
        offset <- as.integer(unlist(as_list(read_xml(tmp)), use.names = FALSE))
    }

    res <- new("NeosOff", ans = ans, offset = offset, jobnumber = jobnumber,
               password = password, method = "getIntermediateResults", call = call, nc = nc)
    return(res)
}

