df_to_xml <- function(df, root_name){
  library(xml2)
  
  root <- xml_new_root(root_name)
  node_name <- substr(root_name, 1, nchar(root_name)-1)
  
  for (i in 1:nrow(df)){
    node <- xml_add_child(root, node_name)
    for (col in colnames(df)){
      xml_add_child(node, col, as.character(df[[i,col]]))
    }
  }
  
  return (root)
}

xml_to_df <- function(nodeset){
  library(xml2)
  library(dplyr)
  
  
  # get all children of document
  entries <- xml_children(nodeset)
  
  if (length(entries) == 0) {
    stop("Error! Check XML!")
  }
  
  # Get all unique names 
  all_fields <- unique( # collect unique values from vector
                  unlist( #from list
                    lapply(entries, function(entry) { # anonym func return list
                      sapply(xml_children(entry), xml_name) #get name of children of entries
                      }
                    )
                  )
  )
  
  # Get list with vectors with values
  rows <- lapply(entries, function(entry) { #anonym func return list
    vals <- setNames(rep(NA, length(all_fields)), all_fields) #empty vector with names
    children <- xml_children(entry) # get children
    for (child in children) {
      vals[[xml_name(child)]] <- xml_text(child) #set values from children
    }
    vals
  })
  
  df <- bind_rows(rows)
  return(df)
  
}