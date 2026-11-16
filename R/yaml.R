#' Formatting scaffold folder yaml files
#' @export
postprocess_yaml<-function() {

  say("postprocessing u.yaml in jamovi folder")
  
  files <- list.files("./jamovi/", "u.yaml")
  for (file in files) {
    file<-paste0("./jamovi/",file)
    #\fix_u(file)
  }
  
}

#' Formatting scaffold folder yaml files
#' @export
preprocess_yaml <- function() {
  cat("\n\n")
  say("preprocessing .yaml in jamovi folder")
  if (!dir.exists("jamovi")) {
    stop("You need to be in the root of the projecs, where the `jamovi` folder is")
  }
  if (!dir.exists("jamovi/scaffold")) {
    warn("No `jamovi/scaffolding` folder. Nothing to do in preprocessing yaml")
    return()
  }

    files <- list.files("./jamovi/", ".yaml")
  
    for (file in files) {
    say("check for preprocessing ",file)
    root<-gsub(".yaml","",file,fixed = T)
    prefile<-paste0(paste0("jamovi/scaffold/", root, ".preprocess.yaml"))
    say("file ",prefile," found:",file.exists(prefile))
    if (file.exists(prefile)) {
      yaml <- yaml::read_yaml(prefile)
      yaml<-process_one(yaml)
      yaml<-fix_some(yaml)
      file <- paste0("jamovi/",file)
      say("Writing file", file)
      yaml::write_yaml(yaml,file,indent=4,indent.mapping.sequence=TRUE,handlers=list(logical=yaml::verbatim_logical))
      
    } else
       say("No preprocessing file ",file,". Nothing to do")
    
  }

  
  
  
    say("\n...done\n")
  
}






process_one <- function(node) {
  if (is.list(node)) {
    # Check if the list is named
    if (!is.null(names(node))) {
      # Named list: recurse on each element by name
      for (n in names(node)) {
        node[[n]] <- process_one(node[[n]])
      }
    } else {
      # Unnamed list: treat like array
      for (i in seq_along(node)) {
        item <- node[[i]]
        if (is.list(item) && !is.null(item$type) && item$type == "scaffold") {
          item<-read_option(item$name,item$file)
        }
        node[[i]] <- process_one(item)
      }
    }
  }
  return(node)
}

find_one <- function(node, tag,fun) {
  
  if (is.list(node)) {
    # Check if the list is named
    if (!is.null(names(node))) {
      # Named list: recurse on each element by name
      for (n in names(node)) {
        node[[n]] <- find_one(node[[n]], tag, fun)
      }
    } else {
      # Unnamed list: treat like array
      for (i in seq_along(node)) {
        item <- node[[i]]
        if (tag %in% names(item) ) {
          item[[tag]]<-fun(item[[tag]])
        }
        node[[i]] <- find_one(item,tag,fun)
      }
    }
  }
  return(node)
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
       if (hasName(q,"type") && q$type=="scaffold") {
           if (hasName(q,"children")) {
              return(q$children)
           }
       }
       return(q)
       
}     


fix_some<-function(yaml) {

  fun<-function(alist) {
    
    if (length(unlist(alist))==1) {
      alist<-list(alist)
    }
   alist
  }
  
  yaml<-find_one(yaml,"permitted",fun)
  yaml<-find_one(yaml,"suggested",fun)
  yaml<-find_one(yaml,"valueKey",fun)

  for (i in seq_along(yaml$options)) {
  if (yaml$options[[i]]["type"] == "NMXList") {
      say("found NML")

       if ("default" %in% names(yaml$options[[i]]) && length(yaml$options[[i]][["default"]])==1)
         yaml$options[[i]][["default"]]<-list(yaml$options[[i]][["default"]])
    }  
    
  }
  
  yaml <- fix_clearWith(yaml)
  yaml <- remove_empty_labels(yaml)
  return(yaml)
}


fix_clearWith <- function(node) {
  if (is.list(node)) {
    if (!is.null(names(node))) {
      for (n in names(node)) {
        if (n == "clearWith") {
          val <- node[[n]]

          # Convert to list if it's just a single string
          if (is.character(val) && length(val) == 1) {
            node[[n]] <- list(val)

          # If it's a list of character vectors (e.g., list of lists of strings), flatten
          } else if (is.list(val) && all(sapply(val, is.character))) {
            node[[n]] <- as.list(unlist(val))

          # If it's a single character vector (not wrapped), wrap as list
          } else if (is.character(val)) {
            node[[n]] <- as.list(val)
          }

        } else {
          # Recurse
          node[[n]] <- fix_clearWith(node[[n]])
        }
      }
    } else {
      # Recurse for unnamed lists
      for (i in seq_along(node)) {
        node[[i]] <- fix_clearWith(node[[i]])
      }
    }
  }
  return(node)
}

remove_empty_labels <- function(node) {
  if (is.list(node)) {
    # If unnamed list (e.g., list of items), clean it
    if (is.null(names(node))) {
      new_list <- list()
      for (item in node) {
        # Check for Label type with empty children
        if (is.list(item) && 
            !is.null(item$type) && item$type == "Label" &&
            (is.null(item$children) || length(item$children) == 0)) {
          next  # Skip this item
        } else {
          # Recurse into the item and keep it
          new_list <- append(new_list, list(remove_empty_labels(item)))
        }
      }
      return(new_list)
    } else {
      # For named lists, recurse into each element
      for (n in names(node)) {
        node[[n]] <- remove_empty_labels(node[[n]])
      }
    }
  }
  return(node)
}


