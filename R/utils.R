iupac_to_glycoct <- function(x, java_app_path = "/Users/fubin/GlycanFormatConverter-cli") {
  checkmate::assert_character(x)
  checkmate::assert_string(java_app_path)
  
  if (!dir.exists(java_app_path)) {
    cli::cli_abort("Java application directory not found: {.path {java_app_path}}")
  }
  
  jar_path <- file.path(java_app_path, "target", "GlycanFormatConverter-cli.jar")
  if (!file.exists(jar_path)) {
    cli::cli_abort("Java application JAR file not found: {.path {jar_path}}")
  }
  
  # Convert each IUPAC string to GlycoCT
  results <- purrr::map_chr(x, function(iupac_str) {
    tryCatch({
      # Step 1: Convert IUPAC to WURCS
      wurcs_result <- convert_iupac_to_wurcs(iupac_str, jar_path)
      
      # Step 2: Convert WURCS to GlycoCT
      glycoct_result <- convert_wurcs_to_glycoct(wurcs_result, jar_path)
      
      return(glycoct_result)
    }, error = function(e) {
      cli::cli_warn("Failed to convert IUPAC string: {.str {iupac_str}}")
      cli::cli_warn("Error: {.str {e$message}}")
      return(NA_character_)
    })
  })
  
  return(results)
}

convert_iupac_to_wurcs <- function(iupac_str, jar_path) {
  # Prepare the command with proper escaping
  cmd <- sprintf(
    'java -jar "%s" -i IUPAC-Condensed -e WURCS -seq "%s"',
    jar_path,
    gsub('"', '\\"', iupac_str)
  )
  
  # Execute the command
  result <- system(cmd, intern = TRUE, ignore.stderr = FALSE)
  
  # Check if command was successful
  if (length(result) == 0) {
    cli::cli_abort("Failed to convert IUPAC to WURCS: No output from Java application")
  }
  
  # Extract WURCS from result
  wurcs_line <- result[stringr::str_detect(result, "^WURCS=")]
  
  if (length(wurcs_line) == 0) {
    cli::cli_abort("Failed to convert IUPAC to WURCS: No WURCS output found")
  }
  
  return(wurcs_line[1])
}

convert_wurcs_to_glycoct <- function(wurcs_str, jar_path) {
  # Prepare the command with proper escaping
  cmd <- sprintf(
    'java -jar "%s" -i WURCS -e GlycoCT -seq "%s"',
    jar_path,
    gsub('"', '\\"', wurcs_str)
  )
  
  # Execute the command
  result <- system(cmd, intern = TRUE, ignore.stderr = FALSE)
  
  # Check if command was successful
  if (length(result) == 0) {
    cli::cli_abort("Failed to convert WURCS to GlycoCT: No output from Java application")
  }
  
  # Extract GlycoCT from result (should be multi-line)
  # Find the start of GlycoCT content
  start_idx <- which(stringr::str_detect(result, "^RES"))
  
  if (length(start_idx) == 0) {
    cli::cli_abort("Failed to convert WURCS to GlycoCT: No GlycoCT output found")
  }
  
  # Return the GlycoCT content (from RES line to end)
  glycoct_content <- result[start_idx[1]:length(result)]
  
  # Join multi-line GlycoCT content
  glycoct_str <- paste(glycoct_content, collapse = "\n")
  
  return(glycoct_str)
}

# The anomer positions are fix for known monosaccharides.
# Except for the monosaccharides with C2 anomer, all others are on C1.
decide_anomer_pos <- function(mono) {
  anomer_on_pos2 <- c(
    "Neu5Ac", "Neu5Gc", "Neu", "Kdn", "Pse", "Leg", "Aci",
    "4eLeg", "Kdo", "Dha", "Fru", "Tag", "Sor", "Psi"
  )
  dplyr::if_else(mono %in% anomer_on_pos2, "2", "1")
}
