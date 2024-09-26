#' Installing a jamovi module
#' @export
install_module_full<-function(what) {
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
    jmvtools::install(home = "flatpak")

  }   else
    warning("versions mismatch")
  
}

#' Installing a jamovi module
#' @export
install_module<-function(what) {
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
    processReadme()
    jdebug(TRUE)
    jmvtools::install(home = "flatpak")
    jdebug(FALSE)
  }   else
    warn("versions mismatch")
  
}


#' change debug setting
#' @export

jdebug<-function(value) {
    say("Setting debug to ", value)
    file_path <- "R/jScafFunctions.R"  # specify your file path
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
        text_lines[i] <- paste('<em id="version">Version ',zv,'</em>\n')
    }
    writeLines(text_lines, file_path)
}
