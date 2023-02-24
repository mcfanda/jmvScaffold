#' @title  SmartTable R6 Class
#' @description 
#' A programmer interface to init and polutate \code{\link[jmvcore]{jmvcore::Table}}
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
                            #' @field nickname the unique identifier of the table used by other classes
                            #' to dispatch messages to the table or to interact with it. It is usually
                            #' used internally, but it is left public for ease to access and debug.
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
                            
                            #' @description 
                            #' Initialize the SmartTable object
                            #' @param  table A jamovi table of class \code{\link[jmvcore]{jmvcore::Table}}
                            #' @param  estimator optional [R6][R6::R6Class]. If set, SmartTable inquires
                            #' the R6 class for init and run functions to initialize and populate the jamovi Table.
                            #' the R6 class should provide functions using the convention `init_[nickname]` and `run_[nickname] where
                            #' the `nickname` is the unique identifier given to the jamovi table. The unique identifiers can be
                            #' * __tablename__:            the name of the \code{\link[jmvcore]{jmvcore::Table}} in jamovi .r.yaml file
                            #' * __groupname_tablename__:  the name of the \code{\link[jmvcore]{jmvcore::Group}} parenting the table and the table name
                            #' * __arrayname__:            the name of the \code{\link[jmvcore]{jmvcore::Array}}  parenting the table and the table name
                            #' 
                            #' @md
                            
                            
                            initialize=function(table,estimator=NULL) {
                              
                              stop("Do not use the class exported by the package. Run jmvScaffold::copy_file() and
                                   use the class copied in the module R/ folder")

                            },
                            #' @description 
                            #' initialize the table from `initSource` or `estimator::run_[tablename]`, and 
                            #' set all the aestetical and behavioral options passed to the table.
                            
                            initTable=function() {
                              
                            },
                            #' @description 
                            #' fills the table from `runSource` or `estimator::run_[tablename]`, and 
                            #' takes care of all aesthetic adjustment required

                            runTable=function() {

                            },
                            #' @description 
                            #' a wrapper function to set a superTitle over upper and lower limits columns
                            #' of a confidence interval. 
                            #' @param aroot a string with the root name of the two columns. For example for `es_upper` and `es_lower`
                            #' the root is `es`. 
                            #' @param width the width of the confidence interval.
                            #' @param label default NULL, prefix to put to the title of the Super Title.
                            #' @param format a `format` string of the type `{}% something as a title`. Default is `"{}% Confidence Intervals"`.
                            ci=function(aroot,width=95,label=NULL,format="{}% Confidence Intervals"){
                              
                              if (is.null(label)) label="" 
                              alist<-list(root=aroot,label=label,width=width,format=format)
                              ladd(self$ci_info)<-alist

                            },
                            #' @description 
                            #' only used internally, however it should be public 
                            retrieveNotes=function() {
                              
                            },
                            #' @description
                            #' set a note to the child table. Not necessary if a `Dispatch` object is used instead
                            #' @param note  a string: the message to set
                            #' 
                            setNotes=function(note) {
                                        private$.setNote(jmvcore::toB64(anote),anote)
                            },
                            #' @description
                            #' change the title of a column 
                            #' @param name  a string: the name of the column
                            #' @param title a string: the new title of the column
                            
                            setColumnTitle=function(name,title) {
                            }
                            
                          ), ## end of public
                          active=list(
                            
                            #' @field  initSource
                            #' If `estimator` is not passed to initialize()
                            #' set the source of the init() data used to initialize the table.
                            #' It can be:
                            #' 
                            #' * a data.frame
                            #' * a list of named lists
                            #' * a function returning a data.frame or a list of named lists
                            #' @md
                            
                            initSource= function(value) {
                              
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
                              
                            },
                            
                            #' @field  activateOnData
                            #' If TRUE, check if the table got some data. For tables with 
                            #' `visible: false` is the r.yaml file, makes them visible
                            #' if data are passed to it                             #' 
                            
                            activateOnData=function(value) {

                            },
                            #' @field  title
                            #' a string. Set the title of the jamovi table
                            
                            title=function(aname) {
                            },
                            #' @field  superTitle
                            #' a named list list(columnname=value). Set the superTitle
                            #' of `columnname` column to a `value`
                            #' 
                            
                            superTitle=function(alist) {
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
                            } ,
                            
                            #' @field  debug
                            #' a logical.
                            #' Print debugging information in console. Default is FALSE of the value
                            #' of the constant `t_INFO` if exists.
                            #'
                            debug=function() {
                            }
                            
                          ) #end of active

                          
) ## end of class

#' @title  SmartArray R6 Class
#' @description 
#' A programmer interface to init and polutate \code{\link[jmvcore]{jmvcore::Array}}
#' 
#' @export

SmartArray <- R6::R6Class("SmartArray",
                          inherit = SmartTable,
                          cloneable=FALSE,
                          class=TRUE,
                          public=list(
                            #' @field childrenObjs
                            #' a list of children `jmvScaffold::SmartTable`
                            childrenObjs=list(),
                            #' @description 
                            #' Initialize the SmartArray object, creating the children tables
                            #' @param  array A jamovi array of class \code{\link[jmvcore]{jmvcore::Array}}
                            #' @param  estimator optional [R6][R6::R6Class]. If set, SmartArray inquires
                            #' the R6 class for init and run functions to initialize and populate the jamovi tables.
                            #' the R6 class should provide functions using the convention `init_[nickname]` and `run_[nickname] where
                            #' the `nickname` is the unique identifier given to the jamovi table. The unique identifiers can be
                            #' * __arrayname__:            the name of the \code{\link[jmvcore]{jmvcore::Array}}  
                            #' * __groupname_arrayname__:   the name of the \code{\link[jmvcore]{jmvcore::Group}} containing the \code{\link[jmvcore]{jmvcore::Array}}  and the Array name
                            #' 
                            #' @md
                            
                            
                            
                            initialize=function(array,estimator=NULL) {
                            
                              stop("Do not use the class exported by the package. Run jmvScaffold::copy_file() and
                                   use the class copied in the module R/ folder")
                            },
                            #' @description 
                            #' initializes all children tables from `runSource` or `estimator::run_[tablename]`, and 
                            #' takes care of all aesthetic adjustments required
                            
                            initTable=function() {
                            },
                            #' @description 
                            #' fills all children tables from `runSource` or `estimator::run_[tablename]`, and 
                            #' takes care of all aesthetic adjustments required
                            
                            runTable=function() {
                            },
                            #' @description
                            #' used internally, but must be kept public

                            retrieveNotes=function() {
                            },
                            #' @description
                            #' set a note to all children tables. Not necessary if a `Dispatch` object is used instead
                            #' @param note  a string: the message to set
                            
                            setNotes=function(note) {
                            }
                           
                          ) #end public
) # end of class