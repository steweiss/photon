.check_node_works <- function(node_top_dir,
                              expected_version) {
  
  message("Looking for nodejs...")
  
  expected_version <- .check_node_version_format(expected_version)
  
  os <- electricShine::get_os()
  
  node_path <- normalizePath(node_top_dir,
                             "/",
                             mustWork = FALSE)
  
  if (os == "win") {
    
    node_path <- file.path(node_path,
                           "node.exe")
    
  } else {
    
    node_path <- file.path(node_path,
                           "node")
  }
  
  node_exists <- file.exists(node_path)
  
  if (!node_exists) {
    
    message("...nodejs executable not found")
    node_path <- FALSE
    return(node_path)
    
  } else {
    
    message("...found nodejs executable.")
    message("Checking if nodejs executable is functional...")
    
    # Check that the node version is the same as what we expect and nodejs is functional
    
    quoted_node_path <- shQuote(node_path)
    
    command <- paste0(quoted_node_path,
                      " -v")
    
    nodejs_response <- tryCatch(system(command,
                                       intern = T),
                                error = function(e) FALSE,
                                warning = function(e) FALSE)
    
    if (nodejs_response == FALSE) {
      
      message(glue::glue("...nodejs at {node_path} seems not to be functional."))
      return(FALSE)
      
    } else if (nodejs_response != expected_version) {
      
      message(glue::glue("...found nodejs {node_path} \n but it is version {nodejs_response}, expected {expected_version}."))
      return(FALSE)
      
    } else if (nodejs_response == expected_version) {
      
      message(glue::glue("...found a functional nodejs {nodejs_response} at: {node_path}."))
      return(node_path)
      
    } else {
      
      stop("Unexpected error when looking for nodejs")
      
    }
  }
}
