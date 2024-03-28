#' Save data to .rds format and meta data to .md format.
#'
#' @param df A data frame to be saved.
#' @param path_data The file path to save the .rds file to.
#' @param path_meta The file path to save the .md file to. Defaults to `path_data`.
#' @param hide A character vector with the names of the columns that should be omitted from the meta data
#' @param include If TRUE will print the data summary.

# Helper functions -------------------------------------------------------------

require(tidyverse)
require(here)
require(skimr)
require(kableExtra)
require(stringr)
require(haven)

round_size <- function(bytes, size, unit) {
  (bytes/size) %>%
    round(0) %>%
    paste(unit)
}

clean_size <- function(df) {
  bytes <- object.size(df)
  
  if (bytes < 1e3) {
    return(
      round_size(bytes, 1, "B")
    )
  }
  else if (bytes >= 1e3 & bytes < 1e6) {
    return(
      return(
        round_size(bytes, 1e3, "KB")
      )
    )
  }
  else if (bytes >= 1e6 & bytes < 1e9) {
    return(
      return(
        round_size(bytes, 1e6, "MB")
      )
    )
  }
  else if (bytes >= 1e9 & bytes < 1e12) {
    return(
      return(
        round_size(bytes, 1e9, "GB")
      )
    )
  }
  else if (bytes >= 1e12 & bytes < 1e15) {
    return(
      return(
        round_size(bytes, 1e12, "TB")
      )
    )
  }
}

# Main function ----------------------------------------------------------------

write_meta <-
  function(
    df, 
    path_data = NULL,
    path_meta = path_data,
    hide = NULL,
    include = TRUE,
    meta_only = FALSE
  ) {
    
    # Prepare file paths ----------------------------
    
    path_meta <-
      path_meta %>%
      paste0(".md")
    
    path_data <- 
      path_data %>%
      paste0(".rds")
    
    # Save data ------------------------------------
    
    if (!meta_only) {
      df %>%
        write_rds(path_data)
    }
    
    # Remove omitted columns from data -------------
    if (!is.null(hide)) {
      df <-
        df %>%
        select(-all_of(hide))
    }
    
    # Get meta data --------------------------------
    
    skimmed <-
      df %>%
      skim %>%
      partition()
    
    # Write meta data ------------------------------
    
    write("", file = path_meta, append = FALSE)
    
    paste0(
      "- Number of observations: ", nrow(df) %>% format(big.mark = ","), "\n",
      "- Number of variables: ", ncol(df) %>% format(big.mark = ","), "\n",
      "- Disk size: ", clean_size(df), "\n",
      "- Last saved: ", Sys.time(), "\n"
    ) %>%
      write(file = path_meta, append = TRUE)
    
    for (type in names(skimmed)) {
      paste("\n## Variable type:", type) %>%
        write(file = path_meta, append = TRUE)
      
      skimmed %>% 
        pluck(type) %>%
        kable(format = "markdown") %>%
        str_replace_all(
          pattern = "\\|NA ",
          replace = "\\|   "
        ) %>%
        str_replace_all(
          pattern = "\\|NaN ",
          replace = "\\|    "
        ) %>%
        str_replace_all(
          pattern = "NA\\|",
          replace = "  \\|"
        ) %>%
        str_replace_all(
          pattern = "NaN\\|",
          replace = "   \\|"
        ) %>%
        write(file = path_meta, append = TRUE)
    }
    
    # Print summary ---------------------------------
    
    if (include) {
      
      df %>%
        skim %>% 
        return
      
    }
  }