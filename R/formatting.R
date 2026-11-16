#' Formatting scaffold folder yaml files
#' @export
format_jamovi <- function() {

  doit<-function(root) {
  say("formatting files in ",root)  
  files <- list.files(root, ".yaml")
  for (file in files) {
    say("formatting ",file)
    yaml <- yaml::read_yaml(paste0(root,file))
    say("Writing file", file)
    yaml::write_yaml(yaml,paste0(root,file),indent=4,indent.mapping.sequence=TRUE,handlers=list(logical=yaml::verbatim_logical))
  }
  say("...done")
  }
    
  say("Checking folders...")
  if (!dir.exists("jamovi")) {
    stop("You need to be in the root of the projecs, where the `jamovi` folder is")
  }
  say("Checking if scaffold folder is present...")
  if (dir.exists("jamovi/scaffold")) {
      doit("./jamovi/scaffold/")
  } else
    say("No `jamovi/scaffolding` folder: moving on")
  root<-"./jamovi/"
  doit(root)
}

#' Formatting R files
#' @export
format_r <- function() {
  
 mc_style <- function(...) {
  transformers <- styler::tidyverse_style(indent_by=4,...)
  transformers$set_line_break_after_assignment<-NULL
  transformers
}

  say("Formatting R folder")
  if (!dir.exists("R")) {
    stop("You need to be in the root of the projecs, where the `R` folder is")
  }
  styler::style_pkg(filetype="R",style=mc_style)  
  say("...done")
}
