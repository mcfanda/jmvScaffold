
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
    preprocess_yalm()
    jmvtools::install(home = "flatpak")
  }   else
    warning("versions mismatch")
  
}
