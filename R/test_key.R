test_key <- function() {
  # Check if the key exists in the current environment, and request / load a key if it isn't found. 


  if (exists("scimicro_key") && !is.null(scimicro_key) && scimicro_key != "") {
    return(scimicro_key)
  } 
  
  # Check if the key exists in the environment variables
  scimicro_key <- Sys.getenv("scimicro_key")
  
  if (scimicro_key == "") {
    message("No key called 'scimicro_key' found.")
    message("Opening browser to request a key...")
    
    # Open the website for the user
    utils::browseURL("https://scientificmicroservices.com/request_key.html")
    
    new_key <- readline(prompt = "Please copy and paste your key here and press Enter: ")
    new_key <- trimws(new_key)
    
    if (new_key == "") {
      stop("No key provided. Operation cancelled.")
    }
    
    # Ask the user how they want to store the key
    save_choice <- utils::menu(
      choices = c("Save permanently to .Renviron (recommended)", 
                  "Load into memory for this session only"),
      title = "\nWould you like to save this key for future sessions?"
    )
    
    # Handle the user's choice
    if (save_choice == 1) {
      renviron_path <- file.path(Sys.getenv("HOME"), ".Renviron")
      cat(paste0("\nscimicro_key=", new_key, "\n"), file = renviron_path, append = TRUE)
      message("Your key will be saved to the .Renviron file and loaded into memory.")
      
    } else if (save_choice == 2) {
      message("Your key will be loaded into memory for this session only.")
      
    } else {
      stop("Key setup cancelled.")
    }
    
    # Load the key into the current active session regardless of the choice above
    Sys.setenv(scimicro_key = new_key)
    
    return(new_key)
    
  } else {
    return(scimicro_key)
  }
}
