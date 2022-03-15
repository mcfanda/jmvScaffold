

ginfo <- function(...) {
    if (j_INFO) {
        cat(paste(list(...)))
        cat("\n")
    }
}

mark <- function(...) {
    if (!j_DEBUG) 
        return()
    
  if (missing(...))
            cat("Mark here\n")
  items<-list(...)
  
  if (length(items)>1)  cat("______begin________\n\n")
  for (a in items)
            if (is.character(a))
                 cat(a,"\n")
            else
                 print(a)
  if (length(items)>1)  cat("_____end_______\n\n")
  
}

is.something <- function(x, ...) UseMethod(".is.something")

.is.something.default <- function(obj) (!is.null(obj))

.is.something.list <- function(obj) (length(obj) > 0)

.is.something.numeric <- function(obj) (length(obj) > 0)

.is.something.character <- function(obj) (length(obj) > 0)

.is.something.logical <- function(obj) !is.na(obj)


#### This function run an expression and returns any warnings or errors without stopping the execution.
#### It does not reterun the results, so the expr should assign a valut to the results
#### something like try_hard({a<-3^2}) and not a<-try_hard(3^2)

try_hard<-function(exp) {
    results<-list(error=FALSE,warning=FALSE,message=FALSE,obj=FALSE)
    
    results$obj <- withCallingHandlers(
        tryCatch(exp, error=function(e) {
            results$error<<-conditionMessage(e)
            NULL
        }), warning=function(w) {
            results$warning<<-conditionMessage(w)
            invokeRestart("muffleWarning")
        }, message = function(m) {
            results$message<<-conditionMessage(m)
            invokeRestart("muffleMessage")
        })
    
    return(results)
}


sourcifyList<-function(option,def) {
    alist<-option$value
    test<-all(sapply(alist,function(a) a$type)==def)
    if (test)
        return("")
    paste0(option$name,"=c(",paste(sapply(alist,function(a) paste0(a$var,' = \"',a$type,'\"')),collapse=", "),")")
}



append_list <- function(alist, aelement, name = NULL) {
    alist[[length(alist) + 1]] <- aelement
    if (!is.null(name)) 
        names(alist)[length(alist)] <- name
    alist
}
prepend_list <- function(alist, aelement, name = NULL) {
    alist <- c(0, alist)
    alist[[1]] <- aelement
    if (!is.null(name)) 
        names(alist)[1] <- name
    alist
}


framefy <- function(alist) {

         if (inherits(alist,"list"))
               alist<-do.call(rbind,alist)
       
       aframe<-as.data.frame(alist,stringsAsFactors = F,optional = T)
    
       for (n in names(aframe))
           aframe[[n]]<-unlist(aframe[[n]])
  aframe
}


########### these are for make it compatible with use of b64 encoding system
########### which is not used here, so they do nothing

tob64<- function(x,...) x

fromb64<- function(x,...) x


