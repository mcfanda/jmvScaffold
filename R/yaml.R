preprocess_yaml<-function() {
  
  preprocess_a_yaml()
  preprocess_u_yaml()
  preprocess_r_yaml()
  

}

postprocess_yaml<-function() {

  cat("\nPpostprocessing u.yaml in jamovi folder\n")
  
  files <- list.files("./jamovi/", "u.yaml")
  for (file in files) {
    file<-paste0("./jamovi/",file)
    fix_u(file)
  }
  
}


preprocess_r_yaml <- function() {
  
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
    prepfile<-paste0(paste0("jamovi/scaffold/", root, "r.preprocess.yaml"))
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


preprocess_a_yaml <- function() {
  
  cat("\nPreprocessing a.yaml in jamovi folder\n")
  if (!dir.exists("jamovi")) {
    stop("You need to be in the root of the projecs, where the `jamovi` folder is")
  }
  if (!dir.exists("jamovi/scaffold")) {
    warning("No `jamovi/scaffolding` folder. Nothing to do in preprocessing yaml")
    return()
  }

  files <- list.files("./jamovi/", "a.yaml")
  for (file in files) {
    message("check for preprocessing ",file)
    root<-gsub("a.yaml","",file,fixed = T)
    prefile<-paste0(paste0("jamovi/scaffold/", root, "a.preprocess.yaml"))
    cat("file ",prefile," found:",file.exists(prefile),"\n")
    if (file.exists(prefile)) {
      yaml <- yaml::read_yaml(prefile)
      options <- yaml$options
      soptions<-rlist::list.find(options, type == "scaffold",n=Inf)
      for (opt in soptions) {
         ofile<-paste0("jamovi/scaffold/",opt$file)
         if (file.exists(ofile)) {
           coptions<-yaml::read_yaml(ofile) 
           copt<-rlist::list.find(coptions, name == opt$name,n=1)
           if (length(copt)>0) {
              message("found option ", opt$name, " in file ", ofile)
              i <- rlist::list.findi(yaml$options,name == opt$name,n=1)
              yaml$options[[i]]<-copt[[1]]
           }
           else
              warning("Option ",opt$name," not found in file ",opt$file)
         } else
           warning("file ",opt$file," defined in option ",opt$name," does not exist")

      }
      file <- paste0("jamovi/",file)
      message("Writing file", file)
      yaml::write_yaml(fix_some(yaml),file,indent=8,handlers=list(logical=yaml::verbatim_logical))

    } else
       message("No preprocessing file ",file,". Nothing to do")
    
  }
  cat("\n...done\n")
}



preprocess_u_yaml <- function() {
  
  cat("\nPreprocessing u.yaml in jamovi folder\n")
  if (!dir.exists("jamovi")) {
    stop("You need to be in the root of the projecs, where the `jamovi` folder is")
  }
  if (!dir.exists("jamovi/scaffold")) {
    warning("No `jamovi/scaffolding` folder. Nothing to do in preprocessing yaml")
    return()
  }

    
  files <- list.files("./jamovi/", "u.yaml")
  for (file in files) {
    message("check for preprocessing ",file)
    root<-gsub("u.yaml","",file,fixed = T)
    prefile<-paste0(paste0("jamovi/scaffold/", root, "u.preprocess.yaml"))

    cat("file ",prefile," found:",file.exists(prefile),"\n")
    if (file.exists(prefile)) {
      yaml <- yaml::read_yaml(prefile)
      options <- yaml$children
      soptions<-rlist::list.find(options, type == "scaffold",n=Inf)
      for (opt in soptions) {
         ofile<-paste0("jamovi/scaffold/",opt$file)
         if (file.exists(ofile)) {
           coptions<-yaml::read_yaml(ofile) 
           copt<-rlist::list.find(coptions, name == opt$name,n=1)
           if (length(copt)>0) {
              message("found option ", opt$name, " in file ", ofile)
              i <- try(rlist::list.findi(yaml$children, name == opt$name,n=1))
              if (class(i)=="try-error") {
                 for (a in yaml$children)
                      print(paste("type=",a$type,"name=",a$name))
                stop("name not found")
              }
              yaml$children[[i]]<-copt[[1]]
           }
           else
              warning("Option ",opt$name," not found in file ",opt$file)
         } else
           warning("file ",opt$file," defined in option ",opt$name," does not exist")

      }
      file <- paste0("jamovi/",file)
      message("Writing file", file)
      yaml::write_yaml(yaml,file,indent=8,handlers=list(logical=yaml::verbatim_logical))

    } else
       message("No preprocessing file ",file,". Nothing to do")
    
  }
  cat("\n...done\n")
}



fix_some<-function(yaml) {

  fix<-c("suggested","permitted")
  for (i in seq_along(yaml$options)) {
    for (f in fix) {
    if (f %in% names(yaml$options[[i]])) {
     if (length(yaml$options[[i]][[f]])==1)
         yaml$options[[i]][[f]]<-list(yaml$options[[i]][[f]])
    }
    }
    if (yaml$options[[i]]["type"] == "NMXList") {
      message("found NML")

       if ("default" %in% names(yaml$options[[i]]) && length(yaml$options[[i]][["default"]])==1)
         yaml$options[[i]][["default"]]<-list(yaml$options[[i]][["default"]])
    }  
  }
  return(yaml)
}


read_option<-function(option,file) {
  
      file<-paste0("jamovi/scaffold/",file)
      if (!file.exists(file)) {
        warning("file ",file, "defined for option ",option," does not exist")
        return()
      }
       options <- yaml::read_yaml(file)
       q<-rlist::list.first(options,name==option)
      if (length(q) == 0) {
        warning("Option ",option, " not found in ",file)
        return()
      }
       return(q)
       
}     



rec_u<-function(alist) {
  
      if (!is.list(alist)) 
          return(alist)
      if (hasName(alist,"type") && alist$type=="Label") {
         if (length(alist$children)==0) {
            message("Removing empty label")
            return(NULL)
         }
      }
  
      if ("children" %in% names(alist)) {
        j<-1
        for (i in seq_along(alist[["children"]])) {
           if (length(alist$children)<2)
             next
           item<-rec_u(alist$children[[j]])
           if (is.null(item)) {
               alist$children[[j]]<-NULL
           } else {
              alist$children[[j]]<-item
              j<-j+1
           }
        }
      }
       alist
}


fix_u<-function(file) {
  
  yaml <- yaml::read_yaml(file)
  yaml::write_yaml(rec_u(yaml),file,indent=8,handlers=list(logical=yaml::verbatim_logical))
  
}
