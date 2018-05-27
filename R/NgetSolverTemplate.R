##
## NEOS: getSolverTemplate 
##
NgetSolverTemplate <- function(category, solvername, inputMethod, nc = CreateNeosComm()){
    if (!(class(nc) == "NeosComm")) {
        stop("\nObject provided for 'nc' must be of class 'NeosComm'.\n")
    }
    call <- match.call()
    ans <- xml.rpc(url = nc@url, method = "getSolverTemplate",
                   .args = list(category = category, solvername = solvername, inputMethod = inputMethod),
                   .convert = TRUE, .opts = nc@curlopts, .curl = nc@curlhandle)
    xml <- read_xml(ans)
    res <- new("NeosXml", xml = xml, method = "getSolverTemplate", call = call, nc = nc)
    return(res)
}
