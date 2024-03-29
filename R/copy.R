
#' Copy essential files to the jamovi module R 
#' @export
copy_files<-function() {
    
    fname<-"jScaffolding.R"
    f<-system.file(fname, package="jmvScaffold")
    file.copy(f, file.path("R",fname), overwrite = TRUE)
    cat("File ",fname," copied into R folder.\n")
    fname<-"jScafFunctions.R"
    f<-system.file(fname, package="jmvScaffold")
    file.copy(f, file.path("R",fname), overwrite = TRUE)
    cat("File ",fname," copied into R folder.\n")
    fname<-"SmartTabs.R"
    f<-system.file(fname, package="jmvScaffold")
    file.copy(f, file.path("R",fname), overwrite = TRUE)
    cat("File ",fname," copied into R folder.\n")
    fname<-"Dispatch.R"
    f<-system.file(fname, package="jmvScaffold")
    file.copy(f, file.path("R",fname), overwrite = TRUE)
    cat("File ",fname," copied into R folder.\n")
    
    
}

