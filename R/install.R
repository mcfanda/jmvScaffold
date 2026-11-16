#' Installing a jamovi module
#' @export
install_module_full<-function(what, debug=TRUE) {
  library(what,character.only=TRUE)
  say("Full installation of", what)
  s<-utils::sessionInfo()
  pkg<-s$otherPkgs[[what]]
  pv<-pkg$Version
  zf<-yaml::read_yaml("jamovi/0000.yaml")
  zv<-zf$version
  h<-git2r::repository_head()
  gv<-gsub("Version.","",h$name,fixed = T)
  gv<-gsub("version.","",gv,fixed = T)
  say("yaml version:",zv,"\n")
  say("pack version:",pv,"\n")
  say("git version:",gv,"\n")
  
  if (all(c(pv,zv)==gv)) {
    #processReadme()
    out<-preprocess_yaml()
    jmvtools::install(home = "flatpak")
    out<-postprocess_yaml()
    if (debug) jdebug(TRUE)
    jmvtools::install(home = "flatpak")
    if (debug) jdebug(FALSE)    

  }   else
    warning("versions mismatch")
  
}

#' Installing a jamovi module
#' @export
install_module<-function(what, ref=NULL, debug=TRUE) {
  library(what,character.only=TRUE)
  say("Installing", what)
  s<-utils::sessionInfo()
  pkg<-s$otherPkgs[[what]]
  pv<-pkg$Version
  zf<-yaml::read_yaml("jamovi/0000.yaml")
  zv<-zf$version
  h<-git2r::repository_head()
  gv<-gsub("Version.","",h$name,fixed = T)
  gv<-gsub("version.","",gv,fixed = T)
  say("yaml version:",zv)
  say("pack version:",pv)
  say("git version:",gv)
  
  if (all(c(pv,zv)==gv)) {
    if (is.null(ref)) ref<-what
    versionize(ref,gv)
    processReadme()
    if (debug) jdebug(TRUE)
    jmvtools::install(home = "flatpak")
    if (debug) jdebug(FALSE)
  }   else
    warn("versions mismatch")
  
}


#' change debug setting
#' @export

jdebug<-function(value) {
    say("Setting debug to ", value)
    file_path <- "R/jScafFunctions.R"  
    if (!file.exists(file_path))
      return()
    text_lines <- readLines(file_path)
    i  <- grep("j_DEBUG\\s*(=|<-)", text_lines)
    if (length(i)>0)
        text_lines[i] <- paste0("j_DEBUG = ",value)
    i  <- grep("j_INFO\\s*(=|<-)", text_lines)
    if (length(i)>0)
        text_lines[i] <- paste0("j_INFO = ",value)
    writeLines(text_lines, file_path)
}

#' @export

processReadme<-function() {
  
    zf<-yaml::read_yaml("jamovi/0000.yaml")
    zv<-zf$version
    file_path <- "README.md"  
    text_lines <- readLines(file_path)
    i  <- grep('<em id="version">', text_lines)
    if (length(i)>0) {
        say("Setting version in README.md to",zv)
        text_lines[i] <- paste('<em id="version">Version ',zv,'</em>')
    }
    writeLines(text_lines, file_path)
}

versionize<-function(ref,version) {
  if (!file.exists("jamovi/00refs.yaml")) {
    warn("no version updating. 00refs.yaml not found")
    return()
  }
  yaml<-yaml::read_yaml("jamovi/00refs.yaml")
  if (!hasName(yaml$refs,ref)) {
    warn("no version updating", ref, "not found in references")
    return()
  }
  if (!hasName(yaml$refs[[ref]],"versioning")) {
    warn("no version updating.",ref, "reference has not versioning: key")
    return()
  }
  say("updating reference",ref,version)
  yaml$refs[[ref]]$title<-paste0(yaml$refs[[ref]]$versioning," (Version ",version,")")
  yaml::write_yaml(yaml,"jamovi/00refs.yaml",indent=4,indent.mapping.sequence=TRUE,handlers=list(logical=yaml::verbatim_logical))
  
}