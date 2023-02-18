### for warnings and error, tables responds to:
###  tablename
###  groupname 
###  groupname_tablename
###  arrayname
###  arrayname_key
###  arrayname_key_tablename
###  arrayname_!_tablename

#' SmartTable R6 Class
#' @description A programmer interface to init and polutate \code{\link[jmvcore]{jmvcore::Table}}
#' 
#' @export

SmartTable <- R6::R6Class("SmartTable",
                          cloneable=FALSE,
                          class=TRUE,
                          public=list(
                            #' @field name the name of the \code{\link[jmvcore]{jmvcore::Table}}
                            name=NULL,
                            #' @field table \code{\link[jmvcore]{jmvcore::Table}}
                            table=NULL,
                            #' @field topics the different topics
                            topics=NULL,
                            #' @field nickname the unique identifier of the table used by other classes
                            #' to dispatch messages to the table or to interact with it
                            nickname=NULL,
                            #' @field expandOnInit if TRUE, the jamovi table is expanded columnwise
                            #' if the data.frame passed in the init are more than the ones defined
                            #' in .r.yaml file. If FALSE (default) additional columns passed on init
                            #' are ignored
                            expandOnInit=FALSE,
                            #' @field expandOnRun if TRUE, the jamovi table is expanded columnwise
                            #' if the data.frame passed in to the run function are more than the ones defined
                            #' in .r.yaml file.  If FALSE (default) additional columns passed on run
                            #' are ignored.
                            expandOnRun=FALSE,
                            #' @field expandSuperTitle a character string. If not NULL, the expanded columns are
                            #'  supertitled with the string
                            expandSuperTitle=NULL,
                            #' @field expandFrom from which column should the table be expanded
                            expandFrom=1,
                            #' @field activated SmartTable are populated (run) if their source
                            #' is not NULL and they are visible. jamovi tables with `visible: false`
                            #' in the .r.yaml file can be make visible and activated with this option TRUE 
                            activated=NULL,
                            #' @field key This should be explained in a vignette
                            key=NULL,
                            #' @field keys_sep This should be explained in a vignette
                            keys_sep=NULL,
                            #' @field keys_raise This should be explained in a vignette
                            keys_raise=TRUE,
                            #' @field spaceBy a vector of columns names, \code{\link[jmvcore]{jmvcore::Table}} columns can add extra space
                            #' between rows to enhance readability.  Passing one or more column names put an extra padding
                            #' when a new value is fond in the column.
                            spaceBy=NULL,
                            #' @field spaceAt a vector of rows indices, \code{\link[jmvcore]{jmvcore::Table}} columns can add extra space
                            #' between rows to enhance readability.  An extra space will be added after the rows with the provided
                            #' indices.
                            spaceAt=NULL,
                            #' @field spaceMethod I do not remeber what this does, but it was cool
                            spaceMethod="cb",
                            #' @field indent a vector of row indices. Indent (shift a bit on the right) the value of 
                            #' the first column of each row index provided
                            indent=NULL,
                            #' @field combineBelow a vector of column names or column indices. \code{\link[jmvcore]{jmvcore::Table}} columns can hide repeated consecutive 
                            #' values in a column by setting `combineBelow:` column property in r.yaml file. This option allows
                            #' to set this properties programmatically on execution. The special key `new!` means that all new
                            #' columns created at init or run time should be combineBelow.
                            combineBelow=0,
                            #' @field ci_info this requires a vignette
                            ci_info=list(),
                            #' @field columnTitles a named list of the form list(name=label). Change the title of
                            #' a jamovi table column named `name` to `label`. 
                            columnTitles=list(),
                            
                            #' @description Initialize the SmartTable object
                            #' @param  table A jamovi table of class \code{\link[jmvcore]{jmvcore::Table}}
                            #' @param  estimator optional [R6][R6::R6Class]. If set, SmartTable inquires
                            #' the R6 class for init and run functions to initialize and populate the jamovi Table.
                            #' the R6 class should provide functions using the convention `init_[nickname]` where
                            #' the `nickname` is the unique identifier given to the jamovi table. The unique identifiers can be
                            #' * __tablename__:            the name of the \code{\link[jmvcore]{jmvcore::Table}} in jamovi .r.yaml file
                            #' * __groupname_tablename__:  the name of the \code{\link[jmvcore]{jmvcore::Group}} parenting the table and the table name
                            #' * __arrayname_tablename__:  the name of the \code{\link[jmvcore]{jmvcore::Array}}  parenting the table and the table name
                            #' 
                            #' @md
                            
                            initialize=function(table,estimator=NULL) {
                              
                              OK<-FALSE

                              if(!inherits(table,"ResultsElement"))
                                  stop("Table does not exits")

                              self$name       <-  table$name
                              self$table      <-  table
                              self$nickname   <-  gsub('"','',gsub("/","_",table$path,fixed = TRUE),fixed=TRUE)
                              self$nickname   <-  stringr::str_replace_all(self$nickname,'[\\]"\\[]',"")
                              self$nickname   <-  make.names(self$nickname)
                              private$.init_source<-paste0("init_",self$nickname)
                              private$.run_source<-paste0("run_",self$nickname)
                              
                              self$activated<-self$table$visible
                              
                              if ("key" %in% names(table) ) 
                                self$key<-table$key
                              
                              private$.estimator  <-   estimator

                              if (inherits(private$.estimator,"SmartArray")) {
                                ## here the estimator is not group or array of results
                                self$activated              <- private$.estimator$activated
                                self$expandOnInit           <- private$.estimator$expandOnInit
                                self$expandOnRun            <- private$.estimator$expandOnRun
                                self$expandFrom             <- private$.estimator$expandFrom
                                self$expandSuperTitle       <- private$.estimator$expandSuperTitle
                                self$ci_info                <- private$.estimator$ci_info
                                self$keys_sep               <- private$.estimator$keys_sep
                                self$keys_raise             <- private$.estimator$keys_raise
                                self$combineBelow           <- private$.estimator$combineBelow
                                self$spaceMethod            <- private$.estimator$spaceMethod
                                self$spaceAt                <- private$.estimator$spaceAt
                                self$spaceBy                <- private$.estimator$spaceBy
                                self$columnTitles           <- private$.estimator$columnTitles
                                self$indent                 <- private$.estimator$indent                            
                                self$activateOnData         <- private$.estimator$activateOnData
                                self$hideOn                 <- private$.estimator$hideOn
                                
                              }
                              

                            },
                            #' @description Initialize the table. Check if data are passed, check if
                            #' the table needs to be expanded columnwise, if rows should be added and if
                            #' messages are broadcast to go into errors or warnings. Show the table.
                            initTable=function() {
                              
                              private$.phase<-"init"
                              # prepare some fast stuff anyway, because the table may be already saved 
                              # and should be inited properly
                              private$.setColumnTitle()
                              private$.ci()
                              self$title
                              self$setTopics()

                              ### check if we need to fill it (it may be empty)
                              if (private$.stop()) {
                                return()
                              }
                              
                              ### fill with initial values###

                              rtable<-private$.getData()
                              
                              if (is.null(rtable))
                                  return()
                              ### this should go after the data
                              self$retrieveNotes()
                              
                              ### expand it if needed
                              if (self$expandOnInit) private$.expand(rtable)
                              private$.fill(self$table,rtable)
                              private$.indent()
                              private$.spaceBy()
                              ## in case is a go, the table may be invisible (if activatedOnData). turn visibility on
                              self$table$setVisible(TRUE)
                              
                              tinfo("TABLES: table",self$nickname,"inited")
                            },
                            #' @description Populates the table. Check if data are passed, check if
                            #' the table need to be expanded columnwise, if rows should be added and if
                            #' messages are broadcast to go into errors or warnings. Show the table.
                            runTable=function() {
                            
                              tinfo("TABLES: table",self$nickname,"checked for run")
                              
                              private$.phase<-"run"
                              if (private$.stop()) {
                                self$retrieveNotes()
                                return()
                              }
                              
                              rtable<-private$.getData()
                              self$retrieveNotes()
                              
                              if (is.null(rtable))
                                return()
                              
                              ### check if new column titles are passed
                              .attr <- private$.getAttributes(rtable)
                              if (utils::hasName(.attr,"titles"))
                                  for (.name in names(.attr$titles)) {
                                         self$setColumnTitle(.name,.attr$titles[[.name]])
                                  }

                              if (self$expandOnRun) private$.expand(rtable)
                              
                              private$.fill(self$table,rtable)
                              private$.finalize()
                              
                              tinfo("TABLES: table",self$nickname,"run")
                              
                            },
                            #' @description insert a superTitle over the confidence interval columns
                            #' @param aroot the root of the name of the confidence bounds columns. If one names the
                            #' confidence bounds columns as `some.ci.lower` and `some.ci.upper`, and sets 
                            #' `aroot` as `some`, the two columns will be superTitled with `width % Confidence Intervals`.
                            #' @param width the width of the CI to appear in the superTitle
                            #' @param label add labels to the superTitle
                            #' @param format default is `"{}% Confidence Intervals"`, can be changed to pass different
                            #' superTitle formats
                            
                            ci=function(aroot,width=95,label=NULL,format="{}% Confidence Intervals"){
                              
                              if (is.null(label)) label="" 
                              alist<-list(root=aroot,label=label,width=width,format=format)
                              ladd(self$ci_info)<-alist

                            },
                            
                            #' @description 
                            #' this should probabily fo to private
                            setTopics=function() {

                              .names<-stringr::str_split(self$nickname,"_")[[1]]
                              alist<-list(.names[1])
                              
                              for (aname in .names[-1])
                                   alist[[length(alist)+1]]<-paste(alist[[length(alist)]],aname,sep ="_")
                               
                              self$topics<-unlist(alist)
                              if (is.something(self$key)) {
                                 key<-private$.nice_name(self$key)
                                 others<-stringr::str_replace_all(self$topics,paste0("_",key,"_"),"_*_")
                                 self$topics<-unique(c(others,self$topics))
                              }
                              
                            },
                            #' @description this should probabily fo to private
                            #' @param the Dispatcher class object to dispatch messages
                            retrieveNotes=function(dispatcher=NULL) {

                                  notes<-self$table$state$notes
                                  ### remove init only message
                                  presentnotes<-notes
                                  if (private$.phase=="run") 
                                      for (i in seq_along(notes)) {
                                          anote<-notes[[i]]
                                          if (anote$topic %in% self$topics) {
                                                if (isTRUE(as.logical(anote$init))) {
                                                     self$table$setNote(anote$id,NULL)
                                                     presentnotes[[i]]<-NULL
                                                }
                                          }
                                      }

                                   for (anote in presentnotes) {
                                     if (anote$topic %in% self$topics)
                                         self$table$setNote(anote$id,anote$message)
                                    }
                              
                            },
                            #' @description set a note (footnote) to be displayed under  the table
                            setNotes=function(dispatcher=NULL) {
                              
                                if (is.something(dispatcher)) {
                                    topics<-intersect(dispatcher$warnings_topics,self$topics)
                                    for (t in topics) {
                                      for (i in seq_along(dispatcher$warnings[[t]])) {
                                        m<-dispatcher$warnings[[t]][i]
                                        private$.setNote(m)

                                      } 
                                    }

                                }
                            },
                            #' @description Change the title of the jamovi table programmatically
                            setColumnTitle=function(name,title) {
                              
                                self$columnTitles[[name]]<-title
                                
                            }
                            
                          ), ## end of public
                          active=list(
                            #' @field  initSource
                            #' If `estimator` is not passed to initialize()
                            #' set the source of the init() data used to initialize the table.
                            #' if can be:
                            #' 
                            #' * a data.frame
                            #' * a list of named lists
                            #' * a function returning a data.frame or a list of named lists
                            #' @md
                            initSource= function(value) {
                              
                              if (missing(value)) 
                                private$.init_source 
                              else 
                                private$.init_source<-value 
                            },
                            #' @field  runSource
                            #' If `estimator` is not passed to initialize()
                            #' set the source of the run() data used to initialize the table.
                            #' if can be:
                            #' 
                            #' * a data.frame
                            #' * a list of named lists
                            #' * a function returning a data.frame or a list of named lists
                            #' @md
                            
                            runSource= function(value) {
                              
                              if (missing(value)) 
                                private$.run_source 
                              else 
                                private$.run_source<-value 
                              
                            },
                            #' @field  activateOnData
                            #' If TRUE, check if the table got some data. For tables with 
                            #' `visible: false` is the r.yaml file, makes them visible
                            #' if data are passed to it                             #' 

                            activateOnData=function(value) {
                              
                              if (missing(value)) {
                               return(private$.activateOnData) 
                              }
                              private$.activateOnData<-value
                              
                            },
                            #' @field  title
                            #' a string. Set the title of the jamovi table
                            title=function(aname) {
                              
                              if (missing(aname)) {
                                
                                test<-grep("___key___",self$table$title,fixed = TRUE)
                                if (length(test)>0 & is.something(self$key)) {
                                  if (is.something(self$keys_sep))
                                    key<-jmvcore::stringifyTerm(self$key,sep=self$keys_sep,raise=self$keys_raise)
                                  else
                                    key<-jmvcore::stringifyTerm(self$key,raise=self$keys_raise)
                                  
                                  
                                  aname<-gsub("___key___",key,self$table$title)
                                  self$table$setTitle(aname)
                                  
                                }
                                return()
                              }
                              self$table$setTitle(aname)
                              
                              
                            },
                            #' @field  superTitle
                            #' a named list list(name=value). Set the superTitle
                            #' of `name` column to `value`
                            #' 
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
                            },
                            #' @field  setColumnVisible
                            #' a list of column names to set visible
                            setColumnVisible=function(varnames) {
                              
                              if (missing(varnames))
                                  return()
                              
                              for (name in varnames)
                                self$table$getColumn(name)$setVisible(TRUE)
                            },
                            #' @field  hideOn
                            #' a named list list(name=value)
                            #' for columns in names(list(name=value))
                            #' hide the column if all its cells are equal to 
                            #' value. Value it is typically `Inf`, `NA` or `NaN`
                            #' 
                            hideOn=function(alist) {
                              
                              if (missing(alist))
                                 private$.hideOn
                              else
                                private$.hideOn<-alist
                            } 
                            
                          ), #end of active
                          private=list(
                            .estimator=NULL,
                            .init_source=NULL,
                            .run_source=NULL,
                            .superTitle=NULL,
                            .phase="init",
                            .new_columns=NULL,
                            .activateOnData=FALSE,
                            .column_title=list(),
                            .hideOn=NULL,
                            .stop=function() {
                              
                              if (private$.phase=="init") {
                                fun<-private$.init_source
                                filled<-FALSE
                              } else {
                                
                                fun<-private$.run_source
                                filled<-!self$table$isNotFilled()
                                
                                if (filled & ("rowCount" %in% names(self$table))) 
                                  filled<-self$table$rowCount>0
                              }
                            

                              if (private$.activateOnData)
                                return(FALSE)
                              
                              if (!self$activated)
                                return(TRUE)

                              if (is.null(fun)) 
                                return(TRUE)

                              if (is.character(fun))
                                   if (!(fun %in% names(private$.estimator))) 
                                        return(TRUE)
                              
                             
                              return(filled)
                              
                            },
                            
                            .getData=function(when) {
                              
                              ## check in which phase we are
                              if (private$.phase=="init")
                                fun<-private$.init_source
                              else
                                fun<-private$.run_source

                              ### check how to retrieve the data
                              if (inherits(fun,"character") ) {
                                
                                output<-try_hard(private$.estimator[[fun]]())
                                rtable<-output$obj
                                error<-output$error
                                warning<-output$warning
                                
                                if (error!=FALSE) {
                                  ginfo("TABLES: Error in ",fun,error)
                                  self$table$setError(error)
                                  return()
                                }
                                
                                if (warning!=FALSE) {
                                  if ("notes" %in% names(self$table)) {
                                      self$table$setNote(jmvcore::toB64(warning),warning,init=FALSE)
                                  } 
                                }
                                return(rtable) 
                              }
                              if (inherits(fun,"function") ) {
                                tinfo("TABLES: ",self$nickname," function is ",class(fun))
                                return(fun())
                              }
                              ## if here, fun is a table (data.frame or list)
                              tinfo("TABLES: ",self$nickname," function is ",class(fun))
                              return(fun)

                              
                              
                            },
                            .fill=function(jtable,rtable) {
                              
                              maxrow<-jtable$rowCount
                              
                              .insert<-function(i,w) {
                                if (i>maxrow)
                                  jtable$addRow(rowKey=i,w)
                                else
                                  jtable$setRow(rowNo=i,w)
                              }
                              
                              rlist <-private$.listify(rtable)
                              for (i in seq_along(rlist)) {
                                t<-rlist[[i]]
                                t[which(is.na(t))]<-""
                                t[which(t==".")]<-NA
                                .insert(i,t)
                              }
                            },
                            .finalize=function() {
                              
                              private$.setColumnTitle()
                              private$.setHideOn()
                              self$table$setVisible(TRUE)

                            },
                            .setHideOn=function() {
                              
                              if (is.something(private$.hideOn)) {
                                rtable<-self$table$asDF
                                what<-names(rtable)
                                for (col in names(private$.hideOn))
                                  if (col %in% what) {
                                    test<-all(rtable[[col]] %in% private$.hideOn[[col]])
                                    if (test) 
                                      self$table$getColumn(col)$setVisible(FALSE)
                                    else
                                      self$table$getColumn(col)$setVisible(TRUE)
                                  }
                              }
                            },
                            .expand=function(rtable) {
                              
                              rtable<-private$.framefy(rtable)
                              .names    <-  names(rtable)
                         
                              .types<-unlist(lapply(rtable,class))
                              .types<-gsub("numeric","number",.types)
                              .types<-gsub("integer","number",.types)
                              .types<-gsub("factor","text",.types)
                              .present<-names(self$table$columns)
                              .ncols<-length(.present)
                              .names<-setdiff(.names,.present)
                              if (is.something(attr(rtable,"titles")))
                                .titles<-attr(rtable,"titles")
                              else 
                                 if (exists("fromb64"))
                                           .titles<-fromb64(.names)
                                 else 
                                            .titles<-.names
                              
                                k<-self$expandFrom-1

                                for (i in seq_along(.names)) {
                                  j<-i+k
                                  cb<-FALSE
                                  if (inherits(self$combineBelow,"integer")) {
                                         if(i %in% self$combineBelow) cb<-TRUE
                                  } else {
                                         if (.names[i] %in% c("new!",self$combineBelow)) cb<-TRUE
                                  }

                                  if (j>.ncols) j<-NA
                                  self$table$addColumn(index=j,
                                                       name = .names[[i]], 
                                                       title = .titles[[i]], 
                                                       superTitle = self$expandSuperTitle, 
                                                       type=.types[i],
                                                       combineBelow=cb)
                                  .ncols<-length(self$table$columns)
                                }
                                
                                private$.new_columns<-.names
                                
                            },
                            .setColumnTitle=function() {
                              
                              what<-names(self$table$columns)
                              
                              for (col in names(self$columnTitles))
                                   if (col %in% what)
                                      self$table$getColumn(col)$setTitle(self$columnTitles[[col]])
                              
                            },
                            .spaceBy=function() {
                              
                              k<-names(self$table$asDF)[1]
                              try_hard({
                              for (j in self$spaceAt) {
                                if (j<0) j<-self$table$rowCount+j
                                self$table$addFormat(rowNo=j,col=k,jmvcore::Cell.END_GROUP)
                                self$table$addFormat(rowNo=j+1,col=k,jmvcore::Cell.BEGIN_GROUP)
                              }})
                              
                              if (is.null(self$spaceBy))
                                   return()
                              
                              if (self$spaceBy==0)
                                   return()
                              
                              try_hard({
                                
                                if (self$spaceBy=="new!")
                                     .spaceBy=private$.new_columns
                                else 
                                     .spaceBy=self$spaceBy
                                
                                jnames<-names(self$table$columns)

                                for (sb in .spaceBy) {
                                  col<-self$table$asDF[[sb]]
                                  rows<-unlist(lapply(unlist(unique(col)),function(x) min(which(col==x))))
                                  rows<-which(!col==c(col[-1],col[[length(col)]]))+1
                                  
                                  if (self$spaceMethod=="cb" & length(rows)==length(col)-1)
                                    rows<-NULL
                                
                                  for (j in rows) {
                                    self$table$addFormat(rowNo=j-1,col=k,jmvcore::Cell.END_GROUP)
                                    self$table$addFormat(rowNo=j,col=k,jmvcore::Cell.BEGIN_GROUP)
                                  }

                                }
                                  
                              })
                              
                            },
                            .indent=function() {

                                 if (!is.null(self$indent)) {
                                    trows<-1:length(self$table$getRows())

                                    if (length(trows)==0)
                                         return()
                                    
                                    rows<-trows[self$indent]
                                    for (j in rows)
                                       self$table$addFormat(rowKey=j,col=1,jmvcore::Cell.INDENTED)
                                 }  

                            },
                            .ci=function() {
                              
                              if (!is.something(self$ci_info))
                                return()
                        
                              for (info in self$ci_info) {

                                .label <-paste(info$label,info$format)
                                .name  <-info$root
                                tail1<-".ci.lower"
                                tail2<-".ci.upper"
                                if (length(.name)==0) {
                                             tail1<-"ci.lower"
                                             tail2<-"ci.upper"
                                }
                                l<-paste0(.name,tail1)
                                u<-paste0(.name,tail2)
                                if (l %in% names(self$table$columns))
                                  self$table$getColumn(l)$setSuperTitle(jmvcore::format(.label, info$width))
                                if (u %in% names(self$table$columns))
                                  self$table$getColumn(u)$setSuperTitle(jmvcore::format(.label, info$width))
                                
                              }
                            }, 

                            .listify = function(adata) {
                             
                              if (is.null(adata)) {
                                return()
                              }
                           
                              .attr<-private$.getAttributes(adata)
                              .res<-NULL
                              if (inherits(adata,"data.frame")) {
                                .res <- lapply(1:dim(adata)[1], function(a) {
                                  .al<-as.list(adata[a, ])
                                  names(.al)<-names(adata)
                                  .al
                                })
                                names(.res) <- rownames(adata)
                                res<-private$.setAttributes(.res,.attr)
                                return(res)
                              }
                              if (inherits(adata,"list")) {
                                .res<-adata
                                if (!is.listOfList(.res))
                                  .res<-list(.res)
                                res<-private$.setAttributes(.res,.attr)
                                return(res)
                              }
                              self$table$setError(paste("SmartTabs input table should be list of named lists or data.frame, found ", paste(class(adata),collapse = ",")))
                            },
                            
                            .framefy = function(alist) {
                              
                              .attr<-private$.getAttributes(alist)
                              
                              if (inherits(alist,"list"))
                                alist<-do.call(rbind,alist)
                              
                              aframe<-as.data.frame(alist,stringsAsFactors = F,optional = T)
                              for (n in names(aframe))
                                aframe[[n]]<-unlist(aframe[[n]])
                              
                              aframe<-private$.setAttributes(aframe,.attr)
                              
                              aframe
                            },
                            
                            .getAttributes=function(obj) {
                              
                              list(keys=attr(obj,"keys"),
                                   titles=attr(obj,"titles"))
                              
                            },
                            .setAttributes=function(obj,attrs) {
                              
                              attr(obj,"keys")<-attrs[["keys"]]
                              attr(obj,"titles")<-attrs[["titles"]]
                              obj
                            },
                            .nice_name=function(aname) {
                              a<-stringr::str_replace_all(aname,'[\\]"\\[]',"")
                              a<-strsplit(a,".",fixed = T)
                              a<-make.names(a)
                              paste(a,collapse = ".")
                            }

                            
                          ) #end of private
                          
                          
) ## end of class

#' SmartArray R6 Class
#' @description A programmer interface to init and populutate \code{\link[jmvcore]{jmvcore::Array}}.
#' 
#' It works exactly like \link[jmvScaffold]{SmartTable} class, but it passes
#' all settings to the children tables
#' 
#' @export


SmartArray <- R6::R6Class("SmartArray",
                          inherit = SmartTable,
                          cloneable=FALSE,
                          class=TRUE,
                          public=list(
                            children=NULL,
                            childrenObjs=list(),
                            initialize=function(tables,estimator=NULL) {
                              
                              super$initialize(tables,estimator)
                              if (hasName(tables,"itemKeys"))
                                    self$children<-tables$itemKeys 
                              else 
                                if (hasName(tables,"itemNames"))
                                  self$children<-tables$itemNames 
                                
                            },
                            initTable=function() {
                              
                              if (private$.stop())
                                return()
                              
                              tinfo("TABLES: array",self$nickname,"initiating")
                              self$table$setVisible(TRUE)
                              self$title
                              rtables<-private$.getData()
                              .keys<-names(rtables)
                              if (is.something(attr(rtables,"keys")))
                                    .keys<-attr(rtables,"keys")

                              if (!is.something(self$table$items)) {
                                
                                for (i in seq_along(rtables)) {
                                  
                                  
                                  if (is.something(.keys))
                                    .key<-.keys[[i]]
                                  else 
                                    .key<-i
                                  self$table$addItem(key = .key)
                                  
                                }
                                
                                self$children<-seq_along(rtables)
                                
                              }
                              
                              for (i in seq_along(self$table$items)) {
                                
                                
                                jtable  <-  self$table$items[[i]]
                               
                                if (inherits(jtable,"Group")) {
                                  aSmartArray<-SmartArray$new(jtable,self)
                                  aSmartArray$initSource<-rtables[[i]]
                                  aSmartArray$key<-.keys[[i]]
                                  ladd(self$childrenObjs)<-aSmartArray
                                  
                                } else { 
                                  
                                  ### if we are here, children are tables
                                  aSmartTable<-SmartTable$new(jtable,self)
                                  aSmartTable$initSource<-rtables[[i]]
                                  ladd(self$childrenObjs)<-aSmartTable
                                  
                                }
                              }
                              for (obj in self$childrenObjs)
                                obj$initTable()
                              
                              tinfo("TABLES: array",self$nickname,"inited")
                              
                            },
                            
                            runTable=function() {
                              
                              private$.phase<-"run"
                              tinfo("TABLES: array",self$nickname,"checked for run")
                              self$retrieveNotes()
                              
                              if (private$.stop()) 
                                return()
                              
                              
                              tinfo("TABLES: array",self$nickname,"run")
                              
                              rtables<-private$.getData()
                              
                              for (i in seq_along(self$childrenObjs)) {
                                obj<-self$childrenObjs[[i]]
                                obj$runSource<-rtables[[i]]
                                obj$runTable()
                                
                              }

                            },
                            retrieveNotes=function() {
                              

                              for (child in self$childrenObjs) {
                                    child$table$setState(self$table$state)
                                    child$retrieveNotes()
                              }
                              
                            },
                            #' @description set a note (footnote) to be displayed under the table
                            setNotes=function(dispatcher=NULL) {
                               for (child in self$childrenObjs)
                                    child$setNotes(dispatcher)
                            }
                            
                            
                            
                            
                          ), ## end of public
                          private=list(
                            .ci=NULL,
                            .ciwidth=NULL,
                            .ciformat=NULL
                            
                            
                            
                          ) #end of private
) # end of class