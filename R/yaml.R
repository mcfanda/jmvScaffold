preprocess_yalm <- function() {
  
  cat("\nPreprocessing yaml in jamovi folder\n")
  if (!dir.exists("jamovi")) {
    stop("You need to be in the root of the projecs, where the `jamovi` folder is")
  }
  if (!dir.exists("jamovi/scaffold")) {
    warning("No `jamovi/scaffolding` folder. Nothing to do in preprocessing yaml")
    return()
  }

  files <- list.files("./jamovi/", "r.yaml")
  for (file in files) {
    mark<-FALSE
    message("check for preprocessing ",file)
    root<-gsub("r.yaml","",file,fixed = T)
    prepfile<-paste0(paste0("jamovi/scaffold/", root, "preprocess.yaml"))
    cwfile<-paste0(paste0("jamovi/scaffold/", root, "options.yaml"))
    cat("file ",prepfile," found:",file.exists(prepfile),"\n")
    cat("file ",cwfile," found:",file.exists(cwfile),"\n")
    
    if (file.exists(prepfile) & file.exists(cwfile)) {
      
      options <- yaml::read_yaml(cwfile)
      
      lines <- readr::read_lines(prepfile)
      newlines <- c()
      for (line in lines) {
        
        test <- grep("clearWith:", line)
        if (length(test) > 0) {
          mark <- TRUE
        }
        test <- grep(": ", line)
        if (length(test) > 0) {
          mark <- FALSE
        }

        if (mark) {
          test <- grep("__.+__", line)
          if (length(test) > 0) {
            nspace<-stringr::str_count(stringr::str_split_fixed(line,"_",2)[[1]]," ")
            oline <- gsub("__", "", line, fixed = T)
            oline <- trimws(gsub("-", "", oline, fixed = T))
            opts <- options[[oline]]
            newlines[length(newlines) + 1] <- paste(paste0(rep(" ",nspace),collapse = ""),"###", oline, " begins")
            for (opt in opts) newlines[length(newlines) + 1] <- gsub("__.+__", opt, line)
            line <- paste(paste0(rep(" ",nspace),collapse = ""),"###", oline, " ends")
          }
        }
        newlines[length(newlines) + 1] <- line
      }
       readr::write_lines(newlines, paste0("jamovi/", file))
       message("File ",file," preprocessed")
       
    } else
       message("No preprocessing file ",file,". Nothing to do")
    
  }
  cat("\n...done\n")
}
