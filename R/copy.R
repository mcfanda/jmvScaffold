
#' Copy essential files to the jamovi module R folder 
#' @export
copy_files<-function(exclude=NULL) {
    
    fnames<-c("jScaffolding.R","jScafFunctions.R","SmartTabs.R","SmartInfo.R","Dispatch.R","PlotState.R", "b64.R")
    
    for (fname in fnames) {
      if (!(tolower(gsub(".R","",fname,fixed = T)) %in% tolower(exclude))) {
        f<-system.file(fname, package="jmvScaffold")
        file.copy(f, file.path("R",fname), overwrite = TRUE)
        cat("File ",fname," copied into R folder.\n")
      }
    }

}

