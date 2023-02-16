

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
    
    
}

copy_files()
