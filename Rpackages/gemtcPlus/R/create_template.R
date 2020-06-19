#' Creates a minimal project template for selected model type
#'
#' @param type model type
#' @param file file name (optional); if not provided, default filename will be <type>_model.Rmd. The .Rmd extension will be added.
#' @examples
#'   create_template("binary")
#' @export
create_template <- function(type = c("binary", "gsd", "fp", "pwe"), file = NULL) {
  
  conn <- system.file("templates", "template.Rmd",package = "gemtcPlus")
  suppressWarnings( lines <- readLines(conn) )
  
  lines[2] <- paste0("title: '", toupper(type), " model - ** update title **'")
  lines[3] <- paste0("author: '", Sys.getenv("USERNAME"), "'")
  lines[31] <- paste0("model_plan <- ", switch(type, 
                                              "binary" = "plan_binary()", 
                                              "gsd"    = "plan_gsd()", 
                                              "fp"     = "plan_fp()",
                                              "pwe"    = "plan_pwe()"))
  
  if (is.null(file)){
    file_name <- paste0(type, "_model.Rmd")
  } else{
    file_name <- paste0(file, ".Rmd")
  }
  writeLines(lines, file_name)
  
  cat("Template created- ", paste0(getwd(), "/", file_name))
}

