
jmvScaffoldClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jmvScaffoldClass",
    inherit = jmvScaffoldBase,
    private = list(
        .smartTabs=list(),
        .estimator=NULL,
        .init=function() {
            ginfo("Module phase: init")
            
            if (!is.something(self$options$dep))
                return()
            
            if (!is.something(self$options$somevars))
                return()
            
            dispatcher<-Dispatch$new(self$results)
            private$.estimator<-Estimate$new(self$options,dispatcher)
            
            aSmartTab<-SmartTable$new(self$results$info,private$.estimator)
            private$.smartTabs<-append_list(private$.smartTabs,aSmartTab)
            
            aSmartTab<-SmartTable$new(self$results$main$means,private$.estimator)
            private$.smartTabs<-append_list(private$.smartTabs,aSmartTab)

            aSmartTab<-SmartTable$new(self$results$main$correlations,private$.estimator)
            aSmartTab$expandable<-TRUE
            private$.smartTabs<-append_list(private$.smartTabs,aSmartTab)
            
            aSmartArray<-SmartArray$new(self$results$mediansarray,private$.estimator)
            private$.smartTabs<-append_list(private$.smartTabs,aSmartArray)

            aSmartArray<-SmartArray$new(self$results$regressionsarray,private$.estimator)
            aSmartArray$activated<-self$options$regression
            private$.smartTabs<-append_list(private$.smartTabs,aSmartArray)
            
            aSmartArray<-SmartArray$new(self$results$randomarray,private$.estimator)
            aSmartArray$activated<-TRUE
            aSmartArray$keys_sep<-' \u2229 '
            private$.smartTabs<-append_list(private$.smartTabs,aSmartArray)
            
            for (i in seq_along(private$.smartTabs)) 
                private$.smartTabs[[i]]$initTable()
            

            table<-self$results$pure
            table$addRow("1",list(info="ciao1"))
            table$addRow("2",list(info="ciao2"))
            
        },
        .run = function() {
            
            ginfo("Module phase: run")
            if (!is.something(self$options$dep))
                return()
            if (!is.something(self$options$somevars))
                return()
            
            private$.estimator$update(self$data)
            
            for (i in seq_along(private$.smartTabs)) 
                private$.smartTabs[[i]]$runTable()
            
            

            ### a pure 
            table<-self$results$pure
            table$setRow(rowKey="1",list(specs="cccccc"))
            table$setRow(rowKey="2",list(specs="ciao"))
            
            if (self$options$residuals && self$results$residuals$isNotFilled()) {
                ginfo("Saving residuals")
                p<-self$data[[dep]]-mean(self$data[[dep]])
                # we need the rownames in case there are missing in the datasheet
                pdf <- data.frame(residuals=p, row.names=rownames(self$data))
                self$results$residuals$setValues(pdf)
            }
            

        })
)
