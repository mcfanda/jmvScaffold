
Dispatch <- R6::R6Class(
  "Dispatch",
  class=FALSE, ## this and the next 
  cloneable=FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
  public=list(
    vars=NULL,
    options=NULL,
    initialize=function(options,vars) {
      self$vars<-vars
      self$options<-options
    },
    ####### eval option that may not exist ####
    
    option=function(val,spec=NULL) {
      
        res<-hasName(self$options,val)
        if (res) {
          if (is.logical(self$options[[val]]))
              res<-self$options[[val]]
          else
              res<-is.something(self$options[[val]])

          if (!is.null(spec))
              res<-(spec %in% self$options[[val]])
        }
        res      
      
    },
    
    ### this requires that sooner or later something is a Dispatch object ###
    absorbe_issues=function(obj) {

      if (!is.something(obj))
         return()
      if (inherits(obj,"Dispatch")) {
              wars<-obj$warnings
              errs<-obj$errors
              for (tablename in names(wars)) {
                      table<-wars[[tablename]]
                      for (t in table) 
                              self$warnings<-list(table=tablename,message=t)
              }
              for (tablename in names(errs)) {
                table<-errs[[tablename]]
                for (t in table) 
                  self$errors<-list(table=tablename,message=t)
              }

       } else
            for (aobj in obj) 
                 self$absorbe_issues(aobj)
    }
    
  ),
  active=list(
        warnings=function(obj) {

              if (missing(obj))
                           return(private$.warnings)
              if (is.null(obj$message))
                           return()
              if (obj$message==FALSE)
                           return()
              obj<-fromb64(obj,self$vars)
              table<-private$.warnings[[obj$table]]
              msg<-private$.translate(obj$message)
              table[[length(table)+1]]<-msg
              table<-unique(table)
              private$.warnings[[obj$table]]<-table
          },
        errors=function(obj) {
          
          if (missing(obj))
            return(private$.errors)

          if (!is.list(obj))
             stop("Errors require a named list with table and message")
          
          if (!hasName(obj,"table"))
              stop("Errors require a named list with table and message")
  
          if (!hasName(obj,"message"))
            stop("Errors require a named list with table and message")
          
          if (is.null(obj$message))
            return()
          
          if (obj$message==FALSE)
            return()
          
          obj<-fromb64(obj,self$vars)
          table<-private$.errors[[obj$table]]
          table[[length(table)+1]]<-obj$message
          table<-unique(table)
          private$.errors[[obj$table]]<-table

        }
        
  ),
  
  private = list(
     .warnings=list(),
     .errors=list(),
     .translate=function(msg) {
         
       for (w in TRANSWARNS) {
         if (length(grep(w$original,msg,fixed = T))>0)
             return(w$new)
       }
       return(msg)

     }
  )
)



