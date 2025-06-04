# api_config.R (Modified for env_uris in JSON)

library(jsonlite)

.config_file_path <- file.path("my_api_settings.json")

# Default values if JSON file doesn't exist or is incomplete
.api_base_uri <- "http://appbridge.cims-global.tw"
.env_uris_map <- list(
  "localhost" = "http://localhost",
  "cims" = "http://appbridge.cims-global.tw"
  # Add other defaults here if you want a fallback even without the JSON file
)

# Internal function: Read configuration from JSON
.read_api_config <- function() {
  if (file.exists(.config_file_path)) {
    tryCatch({
      config_data <- fromJSON(.config_file_path, simplifyVector = FALSE) # simplifyVector=FALSE to preserve list structure
      
      # Read current_base_uri
      if ("api" %in% names(config_data) && "current_base_uri" %in% names(config_data$api)) {
        .api_base_uri <<- config_data$api$current_base_uri
      }

      # Read environments map
      if ("api" %in% names(config_data) && "environments" %in% names(config_data$api)) {
        .env_uris_map <<- config_data$api$environments
      }
      return(TRUE) # Indicate that config was read
    }, error = function(e) {
      warning("Failed to read or parse JSON config file: ", e$message)
      return(FALSE)
    })
  }
  return(FALSE) # Indicate that config file doesn't exist
}

# Internal function: Write configuration to JSON
.write_api_config <- function(current_uri, environments_map) {
  dir.create(dirname(.config_file_path), showWarnings = FALSE)

  config_to_write <- list(
    api = list(
      current_base_uri = current_uri,
      environments = environments_map
    )
  )
  write_json(config_to_write, .config_file_path, pretty = TRUE, auto_unbox = TRUE)
  message("API settings saved to: ", .config_file_path)
}

# --- Initialization Logic ---
# Attempt to load configuration at script/package load time
if (!.read_api_config()) {
  message("No API config file found or failed to load. Using default settings.")
  # Optionally, write the initial default config to file if it doesn't exist
  .write_api_config(.api_base_uri, .env_uris_map)
}


#' Configure the API base URI
#'
#' Sets the base URI for subsequent API calls made by functions like `push()`.
#' The setting will be saved to a custom .json file for persistence.
#'
#' @param env A string specifying the environment or alias for the base URI.
#'   This value should correspond to a key in the 'environments' section of your JSON config.
#' @export
config <- function(env = "cims") {
  # Use the dynamically loaded .env_uris_map
  selected_uri <- .env_uris_map[[env]]

  if (is.null(selected_uri)) {
    warning(paste0("Unknown environment '", env, "'. Using default 'cims' environment (", .env_uris_map[["cims"]], ")."))
    selected_uri <- .env_uris_map[["cims"]] # Fallback to 'cims' from the loaded map
  }

  .api_base_uri <<- selected_uri # Update session variable
  .write_api_config(selected_uri, .env_uris_map) # Save both current URI and the full map

  message("API base URI set to: ", .api_base_uri)
  invisible(NULL)
}

# The push function remains unchanged as it only depends on .api_base_uri
# (Ensure push.R is sourced after this config setup, or part of the same package)