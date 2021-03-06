##
## XML-RPC method: submitJob()
##
NsubmitJob <- function(xmlstring, user = "rneos", interface = "", id = 0, nc = CreateNeosComm()){
    if(!(class(nc) == "NeosComm")) {
        stop("\nObject provided for 'nc' must be of class 'NeosComm'.\n")
    }
    call <- match.call()
    ans <- xml.rpc(url = nc@url, method = "submitJob",
                   .args = list(xmlstring = xmlstring, user = user, interface = interface, id = id),
                   .convert = TRUE, .opts = nc@curlopts, .curl = nc@curlhandle)
    res <- new("NeosJob", jobnumber = ans[[1]], password = ans[[2]], method = "submitJob", call = call, nc = nc)
    return(res)
}
