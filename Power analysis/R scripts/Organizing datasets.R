
organize_v1 = function(parameter){
  
  if(parameter == "alpha"){
    
    alpha_effect_sizes = c(0,0.2,0.4,0.6,0.8,1,1.2,1.4,1.6,1.8,2,2.5,3)
    trials = c(10, 50, 100, 150)
    subjects = c(5, 10, 15, 20, 40, 100)
    
    for(alpha in alpha_effect_sizes){
      for (trial in trials){
        for (sub in subjects){
          effect_size_alpha_value <- alpha
          trials_value <- trial
          subjects_value <- sub
          
          new_dir_name = paste0("effect_size_alpha = ",effect_size_alpha_value, " trials = ", trials_value," subjects = ", subjects_value)
          
          # Regular expression pattern with the placeholders
          pattern <- paste0(
            "effect_size_alpha\\s*=\\s*", effect_size_alpha_value, "\\s+.*",
            "trials\\s*=\\s*", trials_value, "\\s+.*",
            "subjects\\s*=\\s*", subjects_value
          )
          # Check if each string matches the pattern
          matches <- grepl(pattern, list.files(here::here("datasets")))
          
          # Filter out the matching strings
          matching_texts <- list.files(here::here("datasets"))[matches]
          
          
          directory <- here::here("Data sets", "Alpha", new_dir_name)
          
          # Create the directory if it doesn't exist
          if (!file.exists(directory)) {
            dir.create(directory)
          }
          
          
          
          # Move files to the directory
          file_paths <- file.path(directory, matching_texts)
          
          file.copy(here::here("datasets",matching_texts), file_paths)
          
          
        }
        
      }
    }
  }
  if(parameter == "beta"){
    
    beta_effect_sizes = seq(0,3,by = 0.2)
    trials = c(10, 50, 100, 150)
    subjects = c(5, 10, 15, 20, 40, 100)
    
    for(beta in beta_effect_sizes){
      for (trial in trials){
        for (sub in subjects){
          effect_size_beta_value <- beta
          trials_value <- trial
          subjects_value <- sub
          
          new_dir_name = paste0("effect_size_beta = ",effect_size_beta_value, " trials = ", trials_value," subjects = ", subjects_value)
          
          
          
          pattern <- paste0(
            "\\s+.*",  # Match any characters with optional whitespace at the start
            "effect_size_beta\\s*=\\s*", effect_size_beta_value, "\\s+",  # Match effect_size_beta
            "trials\\s*=\\s*", trials_value, "\\s+",  # Match trials
            "subjects\\s*=\\s*", subjects_value, "\\s+.*"  # Match subjects
          )
          # Check if each string matches the pattern
          matches <- grepl(pattern, list.files(here::here("datasets")))
          
          # Filter out the matching strings
          matching_texts <- list.files(here::here("datasets"))[matches]
          
          
          directory <- here::here("Data sets","Beta", new_dir_name)
          
          # Create the directory if it doesn't exist
          if (!file.exists(directory)) {
            dir.create(directory)
          }
          
          
          
          # Move files to the directory
          file_paths <- file.path(directory, matching_texts)
          
          file.copy(here::here("datasets",matching_texts), file_paths)
          
          
        }
        
      }
    }
  }
  if(parameter == "both"){
    print("IDK ")
  }
  
  delete_empty_directories <- function(directory) {
    # List all files and directories recursively
    files <- list.files(directory, full.names = TRUE)
    
    # Get information about each file and directory
    file_info <- lapply(files, file.info)
    
    # Identify empty directories
    empty_dirs <- files[sapply(file_info, function(x) x$size == 4096)]
    
    # Delete empty directories
    if (length(empty_dirs) > 0) {
      unlink(empty_dirs, recursive = TRUE)
      print(paste(length(empty_dirs), "empty directories deleted."))
    } else {
      print("No empty directories found.")
    }
  }
  
  # Example usage:
  delete_empty_directories(here::here("Power analysis", "Data sets", parameter))
}


