# push.R (保持不變)

# ... (確保 .api_base_uri 和 config 函數已被定義或載入)

#' Push current working directory to remote server with real-time streaming build output
#'
#' Compresses all files in the current working directory into a zip file
#' named <labelName>.zip and uploads it to the remote API's streaming endpoint.
#' Docker build output will be streamed directly to the R console in real-time.
#'
#' @param labelName A string used as both the folder name and the zip file name.
#' @return A logical value indicating the overall success of the operation.
#' @export
push <- function(labelName) {
  if (missing(labelName) || !nzchar(labelName)) {
    stop("labelName cannot be empty")
  }

  # --- 1. Prepare the ZIP file ---
  zipfile <- paste0(labelName, ".zip")
  current_dir <- getwd()
  parent_dir <- dirname(current_dir)
  full_zip_path <- file.path(parent_dir, zipfile)

  # Temporarily switch to target folder to zip its content only
  old_wd <- setwd(current_dir)
  on.exit(setwd(old_wd), add = TRUE)

  files_to_zip <- list.files(all.files = FALSE, recursive = FALSE, include.dirs = TRUE)

  message("Compressing files into ", full_zip_path, "...")
  tryCatch({
    utils::zip(zipfile = full_zip_path,
               files = files_to_zip,
               extras = "-r9Xq")
  }, error = function(e) {
    stop("Failed to create zip file: ", e$message)
  })


  # --- 2. Construct and Execute the curl command for streaming upload ---
  endpoint_path <- "/upload-with-stream"
  api_url <- paste0(.api_base_uri, endpoint_path) # 這裡會使用從 .json 或預設值載入的 URI

  quoted_labelName <- shQuote(labelName)

  message("Starting upload and Docker build stream to ", api_url, "...")
  message("------------------- STREAMING OUTPUT START -------------------")

  curl_cmd_args <- c(
    "-X", "POST",
    api_url,
    "-F", paste0("labelName=", quoted_labelName),
    "-F", paste0("file=@", shQuote(full_zip_path))
  )

  curl_result <- system2(
    command = "curl",
    args = curl_cmd_args,
    stdout = "",
    stderr = "",
    wait = TRUE
  )

  message("-------------------- STREAMING OUTPUT END --------------------")


  # --- 3. Clean up and Report Status ---
  unlink(full_zip_path)
  message("Local zip file removed.")

  if (curl_result == 0) {
    message("✅ Overall upload and Docker build process succeeded.")
    return(TRUE)
  } else {
    warning("❌ Overall upload or Docker build process failed with curl exit code: ", curl_result, ". Please review the streamed output for details.")
    return(FALSE)
  }
}