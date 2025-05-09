
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
  zip_filename <- paste0(labelName, ".zip")
  temp_zip_path <- file.path(tempdir(), zip_filename)
  all_files <- list.files(getwd(), full.names = TRUE, recursive = TRUE, all.files = TRUE)
  utils::zip(zipfile = temp_zip_path, files = all_files, flags = "-r9Xq")
  api_url <- "http://appbridge.cims-global.tw/upload"
  res <- httr::POST(url = api_url, body = list(labelName = labelName, file = httr::upload_file(temp_zip_path)), encode = "multipart")
  unlink(temp_zip_path)
  if (httr::status_code(res) == 200) message("Upload successful") else warning("Upload failed: ", httr::content(res, as = "text"))
  return(res)
}
