##
## Function: CreateXmlString
##
CreateXmlString <- function(neosxml, cdatalist){
  if(!(class(neosxml) == "NeosXml")){
    stop("\nPlease provide an object of class 'NeosXml' for argument neosxml.\n")
  }
  xmlstr <-  xml_children(neosxml@xml)
  xmlstr_names <- as.character(lapply(xmlstr, xml_name))
  if (!(all(names(cdatalist) %in% xmlstr_names))) {
    stop("\nNamed list object 'cdatalist' does contain entries that are not node names of object 'xmlstr'.\n")
  }

  for (i in seq_along(xmlstr)) {
    name <- xml_name(xmlstr[[i]])
    if ( name %in%  names(cdatalist) ) {
      xml_replace(xmlstr[[i]], new_xml_node(name, cdatalist[[name]]))
    }
  }
  ans <- as.character(neosxml@xml)
  return(ans)
}

new_xml_node <- function(name, data) {
  node <- xml_new_root(name)
  xml_add_child(node, xml_cdata(data))
  xml_child(xml_parent(node))
}

