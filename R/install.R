#' Installing a jamovi module
#' @export
install_module_full<-function(what) {
  library(what,character.only=TRUE)
  s<-utils::sessionInfo()
  pkg<-s$otherPkgs[[what]]
  pv<-pkg$Version
  zf<-yaml::read_yaml("jamovi/0000.yaml")
  zv<-zf$version
  h<-git2r::repository_head()
  gv<-gsub("Version.","",h$name,fixed = T)
  gv<-gsub("version.","",gv,fixed = T)
  cat("yaml version:",zv,"\n")
  cat("pack version:",pv,"\n")
  cat("git version:",gv,"\n")
  
  if (all(c(pv,zv)==gv)) {
    processReadme()
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
  s<-utils::sessionInfo()
  pkg<-s$otherPkgs[[what]]
  pv<-pkg$Version
  zf<-yaml::read_yaml("jamovi/0000.yaml")
  zv<-zf$version
  h<-git2r::repository_head()
  gv<-gsub("Version.","",h$name,fixed = T)
  gv<-gsub("version.","",gv,fixed = T)
  cat("yaml version:",zv,"\n")
  cat("pack version:",pv,"\n")
  cat("git version:",gv,"\n")
  
  if (all(c(pv,zv)==gv)) {
    processReadme()
    jmvtools::install(home = "flatpak")

  }   else
    warning("versions mismatch")
  
}


processReadme = function() {
  cat("Fixing README.md\n")
  zf<-yaml::read_yaml("jamovi/0000.yaml")
  zv<-zf$version
  conin = file("README.md", "r")
  conout = file(".readme.", "w")

  while ( TRUE ) {
    line = readLines(conin, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    check<-grep('id="version"',line,fixed=T)
    if (length(check)>0) {
      line<-paste('<em id="version">Version',zv,'</em>\n')
    }
    writeLines(line,con=conout)

  }
  close(conin)
  close(conout)
  file.copy(".readme.","README.md",overwrite=T)
  file.remove(".readme.")
}

