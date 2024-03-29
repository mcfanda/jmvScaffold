SmartTable <- R6::R6Class("SmartTable",
                 inherit = Dispatch,
                 cloneable=FALSE,
                 class=FALSE,
                 public=list(
                        name=NULL,
                        table=NULL,
                        nickname=NULL,
                        expandable=FALSE,
                        expandSuperTitle=NULL,
                        expandFromBegining=TRUE,
                        activated=NULL,
                        key=NULL,
                        spaceBy=NULL,
                        spaceAt=NULL,
                        combineBelow=NULL,
                        initialize=function(table,estimator=NULL) {
                            self$name       <-  table$name
                            self$table      <-  table
                            self$nickname   <-  gsub("/","_",table$path,fixed = TRUE)
                            private$.estimator  <-   estimator
                            private$.init_function<-paste0("init_",self$nickname)
                            private$.fill_function<-paste0("fill_",self$nickname)
                            self$activated<-self$table$visible

                            if ("key" %in% names(table) )
                                  if (is.something(table$key))
                                           self$key<-table$key

                            ginfo("Table",self$nickname,"initialized..")
                         

                        },
                        initTable=function() {
                          
                          if (private$.noinit())
                              return()

                          ginfo("Table",self$nickname,"inited..")
                          
                          self$table$setVisible(TRUE)
                          
                          if (inherits(private$.init_function,"character") ) {
                            
                                 output<-private$.estimator[[private$.init_function]]()
                                 
                                 results<-output$obj
                                 error<-output$error
                                 warning<-output$warning
                                 
                                 if (error!=FALSE) {
                                   mark("Error in ",private$.init_function,error)
                                   self$table$setError(error)
                                   return()
                                 }
                                 
                                 if (warning!=FALSE) {
                                   len<-length(self$table$notes)
                                   self$table$setNote(len+1,warning)
                                 }

                          }
                          else 
                                 results<-private$.init_function
                          
                          if (self$expandable) private$.expand(results)
                          private$.fill(self$table,results)
                          
                          ### handle titles ###
                          self$title
                          
                        },

                        fillTable=function() {
                          

                          if (private$.nofill())
                                return()
                          
                          ginfo("Table",self$nickname,"filled..")
                          
                          if (inherits(private$.fill_function,"character") ) {
                            
                            output<-private$.estimator[[private$.fill_function]]()
                            results<-output$obj
                            error<-output$error
                            warning<-output$warning
                            
                            if (error!=FALSE) {
                              mark("Error in ",private$.fill_function,error)
                              self$table$setError(error)
                              return()
                            }
                            
                            if (warning!=FALSE) {
                              len<-length(self$table$notes)
                              self$table$setNote(len+1,warning)
                            }                          
                            
                          }
                          else 
                            results<-private$.fill_function
                          
#                          mark("with data",results)
                          
                          len<-length(self$table$notes)
                          
                          for (i in seq_along(private$.estimator$warnings[[self$nickname]])) 
                               self$table$setNote(i+len,private$.estimator$warnings[[self$nickname]][[i]])
                          
                          private$.fill(self$table,results)
                          
                          ## handle spacing 
                          private$.spaceBy()
                          
                        },
                        
                        initFunction=function(aObject) {
                             private$.init_function<-aObject
                          },
                        fillFunction=function(aObject) {
                          private$.fill_function<-aObject
                        },
                        
                        ci=function(alist,ciwidth=95,ciformat="{}% Confidence Intervals") {
                          if (is.null(names(alist))) {
                               .names<-alist
                               .labels<-rep("",length(alist))
                          } else {
                            .names<-names(alist)
                            .labels<-alist
                            
                          }
                    
                          for (i in seq_along(alist)) {
                            label <-paste(.labels[[i]],ciformat)
                            name  <-.names[[i]]
                            l<-paste0(name,".ci.lower")
                            u<-paste0(name,".ci.upper")
                            if (l %in% names(self$table$columns))
                                    self$table$getColumn(l)$setSuperTitle(jmvcore::format(label, ciwidth))
                            if (u %in% names(self$table$columns))
                                    self$table$getColumn(u)$setSuperTitle(jmvcore::format(label, ciwidth))
                          }
                          
                          
                        }

                 ), ## end of public
                    active=list(

                      title=function(aname) {
                        mark("setting title")
                        if (missing(aname)) {
                          
                                  test<-grep("___key___",self$table$title,fixed = TRUE)
                                  if (length(test)>0 & is.something(self$key)) {
                                      key<-jmvcore::stringifyTerm(self$key)
                                      aname<-gsub("___key___",key,self$table$title)
                                      self$table$setTitle(aname)
                                      
                                  }
                                  return()
                        }
                        self$table$setTitle(aname)
                        
                        
                      },
                        superTitle=function(alist) {
                          
                          if (missing(alist))
                              return(private$.superTitle)
                          
                          if (!is.list(alist))
                             stop("SuperTitle must be a list")
                          .names<-names(self$table$columns)
                          for (stn in names(alist)) {
                            where<-grep(stn,.names,fixed=T)
                            if (length(where)>0)
                               for (i in where) {
                                  self$table$getColumn(.names[[i]])$setSuperTitle(alist[[stn]])
                               }
                          }
                        }
                      
                    ), #end of active
                    private=list(
                              .estimator=NULL,
                              .init_function=NULL,
                              .fill_function=NULL,
                              .superTitle=NULL,
                              
                              .nofill=function() {
                                
                                if (!self$activated)
                                  return(TRUE)
                                
                                if (is.null(private$.fill_function)) 
                                  return(TRUE)
                                if (is.character(private$.fill_function) & !(private$.fill_function %in% names(private$.estimator))  )
                                  return(TRUE)
                                
                                return(!self$table$isNotFilled())
                                
                                
                              },
                              .noinit=function() {
                                
                                if (!self$activated)
                                   return(TRUE)
                                if (is.null(private$.init_function)) 
                                   return(TRUE)
                                if (is.character(private$.init_function) & !(private$.init_function %in% names(private$.estimator))  )
                                   return(TRUE)
                                
                                return(FALSE)

                              },
                              .getData=function(when) {
                                
                                if (when=="init")
                                    fun<-private$.init_function
                                else
                                    fun<-private$.fill_function
                                
                                
                                
                              },
                              .fill=function(jtable,rtable) {
                              
                                maxrow<-jtable$rowCount
                                
                                .insert<-function(i,w) {
                                  if (i>maxrow)
                                      jtable$addRow(rowKey=i,w)
                                  else
                                      jtable$setRow(rowNo=i,w)
                                }
                                
                                rlist <-listify(rtable)
        
                                for (i in seq_along(rlist)) {
                                  t<-rlist[[i]]
                                  t[which(is.na(t))]<-""
                                  t[which(t==".")]<-NA
                                  .insert(i,t)
                                }

                              },
                              .expand=function(rtable) {
                                
                                
                                   rtable<-framefy(rtable)
                                
                                  .titles   <-  names(rtable)
                                  .names    <- make.names(.titles,unique = T)
                                  .types<-unlist(lapply(rtable,class))
                                  .types<-gsub("numeric","number",.types)
                                  .types<-gsub("integer","number",.types)
                                  .types<-gsub("factor","text",.types)
                                
                                  .present  <-  names(self$table$columns)
                                  .names   <-  setdiff(.names,.present)

                               if (self$expandFromBegining) {
                                  for (i in seq_along(.names)) {
                                    cb<-ifelse(i %in% self$combineBelow,TRUE,FALSE)
                                    self$table$addColumn(index=i,
                                                         name = .names[[i]], 
                                                         title = .titles[[i]], 
                                                         superTitle = self$expandSuperTitle, 
                                                         type=.types[i],
                                                         combineBelow=cb)
                                  }
                               } else {
                                 for (i in seq_along(.names)) {
                                   self$table$addColumn(name = .names[[i]], title = .titles[[i]], superTitle = self$expandSuperTitle, type=.types[i])
                                 }
                               }
                              },
                              
                          .spaceBy=function() {
                                
                            
                                for (sb in self$spaceBy) {
                                  col<-self$table$asDF[[sb]]
                                  rows<-unlist(lapply(unlist(unique(col)),function(x) min(which(col==x))))
                                  for (j in rows)
                                    self$table$addFormat(rowNo=j,col=1,jmvcore::Cell.BEGIN_GROUP)
                                }
                                
                            for (j in self$spaceAt) {
                                self$table$addFormat(rowNo=j,col=1,jmvcore::Cell.BEGIN_GROUP)
                            }
                            
                              }
                              

                              
                              
                          ) #end of private
                              

) ## end of class
 
SmartArray <- R6::R6Class("SmartArray",
                          inherit = SmartTable,
                          cloneable=FALSE,
                          class=TRUE,
                        
                          public=list(
                            itemNames=NULL,
                            itemKeys=NULL,
                            
                            initialize=function(tables,estimator=NULL) {
                              
                              super$initialize(tables,estimator)
                              ginfo("Array",self$nickname,"initiazialased")
                              self$itemNames<-try_hard(tables$itemNames)$obj
                              self$itemKeys<-try_hard(tables$itemKeys)$obj
                              ginfo("Array with keys",self$itemKeys)

                            },
                            initTable=function() {
                              ginfo("Looking for init function",private$.init_function)
                              
                              if (private$.noinit())
                                 return()
                              
                              ginfo("Array",self$nickname,"inited...")
                              
                              self$table$setVisible(TRUE)
                              output<-private$.estimator[[private$.init_function]]()
                              rtables<-output$obj
                              error<-output$error
                              warning<-output$warning

                              if (!isFALSE(error)) {
                                mark("Error in ",private$.init_function,error)
                                self$table$setError(error)
                              }
                              for (i in seq_along(self$itemKeys)) {
                                
                                key     <-  self$itemKeys[[i]]
                                jtable  <-  self$table$get(key = key)
                                
                                if (inherits(jtable,"Group")) {
                                  self$title
                                  aSmartArray<-SmartArray$new(jtable,self)
                                  aSmartArray$activated<-self$activated
                                  aSmartArray$expandable<-self$expandable
                                  aSmartArray$expandSuperTitle<-self$expandSuperTitle
                                  aSmartArray$initTable()
                                  return()
                                } 
                                
                                ### if we are here, children are tablesildren tables
                                   rtable <- rtables[[i]]
                                   aSmartTable<-SmartTable$new(jtable,self)
                                   aSmartTable$expandable<-self$expandable
                                   aSmartTable$expandFromBegining<-self$expandFromBegining
                                   aSmartTable$expandSuperTitle<-self$expandSuperTitle
                                  
                                    if (is.something(self$combineBelow))
                                           if (length(self$combineBelow)==1)
                                                 aSmartTable$combineBelow<-self$combineBelow
                                           else
                                                  aSmartTable$combineBelow<-self$combineBelow[[i]]
                                
                                   aSmartTable$activated<-self$activated
                                   if (is.something(private$.ci)) {
                                       aSmartTable$ci(private$.ci,private$.ciwidth,private$.ciformat)
                                    }
                                   aSmartTable$initFunction(rtable)
                                   aSmartTable$initTable()
                                  }
                              
                            },
                            
                            fillTable=function() {
                              
                              if (private$.nofill())
                                return()
                              
                              ginfo("Array",self$nickname,"filled")
                              
                              output<-private$.estimator[[private$.fill_function]]()
                              results<-output$obj
                              error<-output$error
                              warning<-output$warning
                              if (error!=FALSE) {
                                mark("Error in ",private$.fill_function,error)
                                self$table$setError(error)
                                return()
                              }
                              
                              if (warning!=FALSE) {
                                len<-length(atable$notes)
                                self$table$setNote(len+1,warning)
                              }           
                              
                              for (i in seq_along(self$itemKeys)) {
                                
                                key     <-  self$itemKeys[[i]]
                                rtable <-  results[[i]]
                                jtable  <-  self$table$get(key = key)
                                private$.fill(jtable,rtable)
                                
                              }
                            },
                            ci=function(alist,ciwidth=95,ciformat="{}% Confidence Intervals"){
                              
                              private$.ci<-alist
                              private$.ciwidth<-ciwidth
                              private$.ciformat<-ciformat
                              
                            }
                            
                            
      ), ## end of public
      private=list(
        .ci=NULL,
        .ciwidth=NULL,
        .ciformat=NULL
        
        
        
      ) #end of private
) # end of class