##
## XML-RPC method: getFinalResults()
##
NgetFinalResults <- function(obj, convert = TRUE){
    if (!(class(obj) == "NeosJob")) {
        stop("\nObject 'obj' is not of class 'NeosJob'.\n")
    }
    call <- match.call()
    jobnumber <- obj@jobnumber
    password <- obj@password
    nc <- obj@nc
    ans <- xml.rpc(url = nc@url, method = "getFinalResults",
                   .args = list(jobnumber = jobnumber, password = password),
                   .convert = FALSE, .opts = nc@curlopts, .curl = nc@curlhandle)
    if ( convert ) {
        ans <- decode_content(ans$content)
    }
    res <- new("NeosAns", ans = ans, method = "getFinalResults", call = call, nc = nc)
    return(res)
}

decode_content <- function(x) {
    base64_val <- xml_text(xml_find_all(read_xml(rawToChar(x)), "//param/value"))
    rawToChar(base64decode(base64_val))
}
