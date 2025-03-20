preprocess_yaml<-function() {
  
  preprocess_a_yaml()
  preprocess_u_yaml()
  preprocess_r_yaml()
  

}

postprocess_yaml<-function() {

  say("postprocessing u.yaml in jamovi folder")
  
  files <- list.files("./jamovi/", "u.yaml")
  for (file in files) {
    file<-paste0("./jamovi/",file)
    fix_u(file)
  }
  
}


preprocess_r_yaml <- function() {
  
  say("preprocessing r.yaml in jamovi folder")
  if (!dir.exists("jamovi")) {
    stop("You need to be in the root of the projecs, where the `jamovi` folder is")
  }
  if (!dir.exists("jamovi/scaffold")) {
    warn("No `jamovi/scaffolding` folder. Nothing to do in preprocessing yaml")
    return()
  }

  files <- list.files("./jamovi/", "r.yaml")
  for (file in files) {
    mark<-FALSE
    say("check for preprocessing ",file)
    root<-gsub("r.yaml","",file,fixed = T)
    prepfile<-paste0(paste0("jamovi/scaffold/", root, "r.preprocess.yaml"))
    say("file ",prepfile," found:",file.exists(prepfile))

    if (file.exists(prepfile)) {
      yaml <- yaml::read_yaml(prepfile)
      pre<-yaml[["items"]]

      for (p in seq_along(pre)) {
         opt<-pre[[p]]
         opt<-replace_tag_list(opt,"clearWith")
         
        if (utils::hasName(opt,"items") && (length(opt$items)>1) ) {
         for (k in seq_along(opt$items)) {
           opt$items[[k]]<-replace_tag_list( opt$items[[k]],"clearWith")
         }
        }
          
        if (utils::hasName(opt,"template")) {
          
           opt$template<-replace_tag_list( opt$template,"clearWith")
           #  message("processing template for", opt$name)
           
           if (utils::hasName(opt$template,"items") && (is.list(opt$template$items)) ) {
              for (k in seq_along(opt$template$items)) {
               opt$template$items[[k]]<-replace_tag_list( opt$template$items[[k]],"clearWith")
           }
        
         }
        } ## end template
        # done
        pre[[p]]<-opt
      }
    }

      yaml[["items"]]<-pre
      say("Writing file ", file)
      root<-"jamovi/"
      yaml::write_yaml(yaml,paste0(root,file),indent=4,indent.mapping.sequence=TRUE,handlers=list(logical=yaml::verbatim_logical))

    } #end of loop file
  say("\n...done\n")
  
}

replace_tag_list<-function(opt,tag, newtag="children") {
  
      if (utils::hasName(opt,tag)) {
          results<-list()
          for (j in seq_along(opt[[tag]])) {
            cw<-opt[[tag]][[j]]
            if (utils::hasName(cw,"type") && cw$type=="scaffold") {
              results<-c(results,get_scaff_option(cw)[[newtag]])
            } else {
              results<-c(results,cw)
            }
          }
          opt[[tag]]<-as.list(unique(unlist(results)))
      }
    return(opt)
}

get_scaff_option<-function(opt) {

               ofile<-paste0("jamovi/scaffold/",opt$file)
               if (file.exists(ofile)) {
                    coptions<-yaml::read_yaml(ofile) 
                    copt<-rlist::list.find(coptions, name == opt$name,n=1)
                    if (length(copt)>0) {
                           # message("found option ", opt$name, " in file ", ofile)
                            return(copt[[1]])
                     }
                     else
                        warn("Option ",opt$name," not found in file ",opt$file)
         } else
           warn("file ",opt$file," defined in option ",opt$name," does not exist")
         
         return(NULL)

}



preprocess_a_yaml <- function() {
  
  say("Preprocessing a.yaml in jamovi folder")
  if (!dir.exists("jamovi")) {
    stop("You need to be in the root of the projecs, where the `jamovi` folder is")
  }
  if (!dir.exists("jamovi/scaffold")) {
    warn("No `jamovi/scaffolding` folder. Nothing to do in preprocessing yaml")
    return()
  }

  files <- list.files("./jamovi/", "a.yaml")
  for (file in files) {
    say("check for preprocessing ",file)
    root<-gsub("a.yaml","",file,fixed = T)
    prefile<-paste0(paste0("jamovi/scaffold/", root, "a.preprocess.yaml"))
    say("file ",prefile," found:",file.exists(prefile))
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
              i <- rlist::list.findi(yaml$options,name == opt$name,n=1)
              yaml$options[[i]]<-copt[[1]]
           }
           else
              warn("Option ",opt$name," not found in file ",opt$file)
         } else
           warn("file ",opt$file," defined in option ",opt$name," does not exist")

      }
      file <- paste0("jamovi/",file)
      message("Writing file", file)
      yaml::write_yaml(fix_some(yaml),file,indent=4,indent.mapping.sequence=TRUE,handlers=list(logical=yaml::verbatim_logical))

    } else
       say("No preprocessing file ",file,". Nothing to do")
    
  }
  say("...done")
}



preprocess_u_yaml <- function() {
  
  say("Preprocessing u.yaml in jamovi folder")
  if (!dir.exists("jamovi")) {
    stop("You need to be in the root of the projecs, where the `jamovi` folder is")
  }
  if (!dir.exists("jamovi/scaffold")) {
    warn("No `jamovi/scaffolding` folder. Nothing to do in preprocessing yaml")
    return()
  }

    
  files <- list.files("./jamovi/", "u.yaml")
  for (file in files) {
    say("check for preprocessing ",file)
    root<-gsub("u.yaml","",file,fixed = T)
    prefile<-paste0(paste0("jamovi/scaffold/", root, "u.preprocess.yaml"))

    say("file ",prefile," found:",file.exists(prefile))
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
              i <- try(rlist::list.findi(yaml$children, name == opt$name,n=1))
              if (class(i)=="try-error") {
                 for (a in yaml$children)
                      print(paste("type=",a$type,"name=",a$name))
                stop("name not found")
              }
              yaml$children[[i]]<-copt[[1]]
           }
           else
              warn("Option ",opt$name," not found in file ",opt$file)
         } else
           warn("file ",opt$file," defined in option ",opt$name," does not exist")
      }
      file <- paste0("jamovi/",file)
      say("Writing file", file)
      yaml::write_yaml(yaml,file,indent=4,indent.mapping.sequence=TRUE,handlers=list(logical=yaml::verbatim_logical))

    } else
       say("No preprocessing file ",file,". Nothing to do")
    
  }
  say("...done")
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
      say("found NML")

       if ("default" %in% names(yaml$options[[i]]) && length(yaml$options[[i]][["default"]])==1)
         yaml$options[[i]][["default"]]<-list(yaml$options[[i]][["default"]])
    }  
  }
  return(yaml)
}


read_option<-function(option,file) {
  
      file<-paste0("jamovi/scaffold/",file)
      if (!file.exists(file)) {
        warn("file ",file, "defined for option ",option," does not exist")
        return()
      }
       options <- yaml::read_yaml(file)
       q<-rlist::list.first(options,name==option)
      if (length(q) == 0) {
        warn("Option ",option, " not found in ",file)
        return()
      }
       return(q)
       
}     



rec_u<-function(alist) {
  
      if (!is.list(alist)) 
          return(alist)
      if (hasName(alist,"type") && alist$type=="Label") {
         if (length(alist$children)==0) {
            say("Removing empty label")
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
  yaml::write_yaml(rec_u(yaml),file,indent=4,indent.mapping.sequence=TRUE,handlers=list(logical=yaml::verbatim_logical))
  
}

#' Formatting scaffold folder yaml files
#' @export
format_scaffold <- function() {
  
  say("Formatting scaffold folder")
  if (!dir.exists("jamovi")) {
    stop("You need to be in the root of the projecs, where the `jamovi` folder is")
  }
  if (!dir.exists("jamovi/scaffold")) {
    warn("No `jamovi/scaffolding` folder. Nothing to do in preprocessing yaml")
    return()
  }
  root<-"./jamovi/scaffold/"
  files <- list.files(root, ".yaml")
  for (file in files) {
    say("formatting ",file)
    yaml <- yaml::read_yaml(paste0(root,file))
    say("Writing file", file)
    yaml::write_yaml(yaml,paste0(root,file),indent=4,indent.mapping.sequence=TRUE,handlers=list(logical=yaml::verbatim_logical))
  }
  say("...done")
}
