#' Dispatch R6 Class
#' @description 
#'  Send messages to jamovi  tables from anywhere in the code.
#'  Warnings can be sent to tables or arrays. For tables in Array, they do not need to exist at 
#'  the moment the message is broadcast, because the message is stored in the parent object
#'  and then passed to the table at run time.
#'  
#'  Warnings can be transient (get removed after .init()) when `init=TRUE` is passed.
#'  
#'  Errors must be sent to existing (already defined) objects. Errors are passed directly
#'  to the jamovi object. If option `final=TRUE`, a `stop()` is issued
#'  @export
Dispatch <- R6::R6Class(
            "Dispatch",
            class=TRUE, 
            cloneable=FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
            public=list(
                        #' @description Initialize the Dispatch object
                        #' @param  results A jamovi results object  \code{\link[jmvcore]{jmvcore::Results}}
                        initialize=function(results) { 
                          
                                  private$.tables<-results
                                  
                        },
                        #' @description Translate a message
                        #' @param  msgobj a list containing the field `message`. The function looks in the environment
                        #'                for a global list named `TRANS_WARNS`. The list must contain lists with the field `original` and
                        #'                one of the following: `new`: a new message to replace `original`;
                        #'                `sub`: a string to substitute the `original` string. `append`: a string to append to `original`.
                        #'                `prepend`: a string to prepend to `original`. The field are evaluated in this order.
                        #'                To remove a warning or error, simple set `new=NULL`
                        
                        translate=function(msgobj) {
                          
                          if (!exists("TRANS_WARNS")) return(msg)
                          
                          msg<-msgobj$message
                          where<-unlist(lapply(TRANS_WARNS,function(x) length(grep(x$original,msg))>0))
                          where<-which(where)
                          if (is.something(where)) {
                            
                            if (length(where)>1) where<-where[1]
                            if ("new" %in% names(TRANS_WARNS[[where]]))
                              msgobj$message<-TRANS_WARNS[[where]]$new
                            if ("sub" %in% names(TRANS_WARNS[[where]]))
                              msgobj$message<-gsub(TRANS_WARNS[[where]]$original,TRANS_WARNS[[where]]$sub,msg,fixed=T)
                            if ("append" %in% names(TRANS_WARNS[[where]]))
                              msgobj$message<-paste(msg,TRANS_WARNS[[where]]$append)
                            if ("prepend" %in% names(TRANS_WARNS[[where]]))
                              msgobj$message<-paste(TRANS_WARNS[[where]]$prepend,msg)
                          }
                          return(msgobj)
                        }
            
                        ),
            active=list(
                        #' @field warnings
                        #' Set a warning message to a jamovi table
                        #' It accepts a list with fields:
                        #' *  `topic` the nickname of a table in the format:
                        #' 
                        #'    *  `tablename` a name of the table as set in the `r.yaml` jamovi file
                        #'    *  `groupname_tablename`  the group the table belongs to and the table name
                        #'    *  `arrayname` the array of the tables target of the message
                        #' 
                        #' *   `message` the message to broadcast
                        #' *   `init` (optional) `init=TRUE` makes the message transient (disappears after init)
                        #' *   `head` (optional)  it can be `info`, `warning`, and `error`. In jamovi, if `tablename`
                        #'             is a table, does not have an effect. If `tablename` is a `Htlm` obj, it gives
                        #'             the message a different format and color. In R, `warning` is issued as an R
                        #'             warning, the others are added as notes to the tables. 
                        #' @md   
                        warnings=function(obj) {

                                ## some code
                               
                        },
                        #' @field errors
                        #' set an error message to a jamovi table
                        #' It accepts a list with fields:
                        #' *   `topic` the nickname of a table in the format:
                        #' 
                        #'     *  `tablename` a name of the table as set in the `r.yaml` jamovi file
                        #'     *  `groupname_tablename`  the group the table belongs to and the table name
                        #'     *  `arrayname` the array of the tables target of the message
                        #' 
                        #' *   `message` the message to broadcast
                        #' *    `final` (optional) `final=TRUE` makes module stop computation immediately
                        #' @md
                        
                        errors=function(obj) {
          
                               if (missing(obj))
                                     return(private$.errors)

                               if (!is.list(obj))
                                     stop("SCAFFOLD: Error requires a named list with `topic` and `message`")
          
                               if (!hasName(obj,"topic") | !hasName(obj,"message"))
                                    stop("SCAFFOLD:: Error requires a named list with `topic` and `message`")
  

                               if (is.null(obj$message) || obj$message==FALSE)
                                    return()
          
                               obj$message<-private$.translate(obj$message)
                          
                               if (hasName(obj,"final") && (obj$final))
                                   stop(obj$message)
                          
                               path<-stringr::str_split(obj$topic,"_")[[1]]
                               table<-private$.find_table(path)
                               table$setError(obj$message)

                       }
#                       ,
#                       warnings_topics=function() {return(names(private$.warnings))},
#                       errors_topics=function() {return(names(private$.errors))}
        
        
            ),
            private = list(
                      .warnings=list(),
                      .errors=list(),
                      .find_table=function(path) {
                        
                        tableobj<-self$tables
                        found<-FALSE
                        for (aname in path)
                          if (hasName(tableobj,aname)) {
                            found<-TRUE
                            tableobj<-tableobj[[aname]]
                          }
                        if (found)
                             return(tableobj)
                        else
                             return(NULL)
                        
                      },
                      .translate=function(msg) {
      
                            for (w in TRANS_WARNS) {
                                 if ("sub" %in% names(w))
                                     msg<-gsub(w$original,w$new,msg,fixed=T)
                                 if ("new" %in% names(w)) {
                                   test<-grep(w$original,msg,fixed=T)
                                   if (length(test)>0)
                                      msg<-w$new
                                 }
                            }
                           return(msg)

                       }
                       
            ) #end of private
) #end of class



