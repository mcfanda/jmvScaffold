library(R6)

Person <- R6Class("Person",
                  public = list(
                    name = NULL,
                    hair = NULL,
                    initialize = function(name = NA, hair = NA) {
                      self$name <- name
                      self$hair <- hair
                      self$greet()
                    },
                    set_hair = function(val) {
                      self$hair <- val
                    },
                    greet = function() {
                      cat(paste0("Hello, my name is ", self$name, ".\n"))
                    }
                  )
)

ann <- Person$new("Ann", "black")
y<-rnorm(10)
x<-rnorm(10)
model<-lm(y~x)
length(model$fitted.values)
matrix(NA,3,3)
a<-list(NA,NA,NA)
all(is.na(a))

a<-list(
  list(s=1,q="a"),
  list(s=2,q="b"),
  list(s=3,q="d ff")
  
)
a


aa<-as.data.frame(do.call(rbind,a),stringsAsFactors = F,optional = T)

aa<-as.data.frame(do.call(rbind,a),stringsAsFactors = F,optional = T)

framefy<-function(alist) {
  aframe<-as.data.frame(do.call(rbind,alist),stringsAsFactors = F,optional = T)
  for (n in names(aframe))
    aframe[[n]]<-unlist(aframe[[n]])
  aframe
}

a<-list(
  list(s=1),
  list(s=2),
  list(s=3)
  
)
b<-list(
  list(s=1,q=1),
  list(s=2,q=2),
  list(s=3,q=3)
  
)
d<-list(s=1,q=1)
listify(a)
listify(b)
listify(d)

listify(1)

names(a)
names(b)
names(list(d))
