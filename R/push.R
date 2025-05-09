
#' Push current working directory to remote server
#'
#' Compresses all files in the current working directory into a zip file
#' named <labelName>.zip and uploads it to the remote API.
#'
#' @param labelName A string used as both the folder name and the zip file name.
#' @return API response from the upload call
#' @export
push <- function(labelName) {
  if (missing(labelName) || !nzchar(labelName)) stop("labelName cannot be empty")

    # Create zip file path
  zipfile <- paste0(labelName, ".zip")
  
  # Get current working dir (e.g., /home/ubuntu/source/myDemo1)
  current_dir <- getwd()
  
  # Get just last folder name and its parent path
  parent_dir <- dirname(current_dir)
  folder_name <- basename(current_dir)
  
  # Temporarily switch to target folder to zip its content only
  old_wd <- setwd(current_dir)
  on.exit(setwd(old_wd), add = TRUE)  # Ensure we switch back even if error
  
  # List all files/folders inside current_dir (no hidden files)
  files_to_zip <- list.files(all.files = FALSE, recursive = FALSE)
  
  # Compress (only contents, no full path)
  utils::zip(zipfile = file.path(parent_dir, zipfile),
             files = files_to_zip,
             extras = "-r9Xq")

  api_url <- "http://appbridge.cims-global.tw/upload"
  res <- httr::POST(
    url = api_url, 
    body = list(
      labelName = labelName, 
      file = httr::upload_file(file.path(parent_dir, zipfile))
    ), 
    encode = "multipart"
  )
  
  # Remove zip file
  unlink(file.path(parent_dir, zipfile))

  # Parse JSON response
  res_content <- httr::content(res, as = "parsed", type = "application/json")
  
  # Check for success conditions
  success <- isTRUE(res_content$dockerfile_found) &&
             isTRUE(res_content$docker_build$executed) &&
             isTRUE(res_content$docker_build$success)
  
  # Return success/failure message
  if (success) {
    message("✅ Upload and Docker build succeeded: ", res_content$message)
    return(invisible(TRUE))
  } else {
    warning_msg <- paste(
      "❌ Upload or Docker build failed.",
      if (!isTRUE(res_content$dockerfile_found)) "Dockerfile not found." else NULL,
      if (!isTRUE(res_content$docker_build$executed)) "Docker build not executed." else NULL,
      if (isTRUE(res_content$docker_build$executed) && !isTRUE(res_content$docker_build$success)) "Docker build failed." else NULL,
      sep = " "
    )
    warning(warning_msg)
    return(invisible(FALSE))
  }
}
