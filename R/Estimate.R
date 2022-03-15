Estimate <- R6::R6Class(
  "Estimate",
  class=TRUE, ## this and the next 
  cloneable=FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
  inherit = Dispatch,
  public=list(
    data=NULL,
    storage = NULL,
    random= NULL,
    initialize=function(options) {
      super$initialize(options=options,vars=c(options$dep,options$somevars))
      self$warnings<-list(table="info",message="this is a warn for info")
      self$warnings<-list(table="info",message="this is another warn for info")
    }, # here initialize ends
    
    setStorage = function(aproperty) {
      self$storage<-aproperty
    },
    
    update=function(data) {
      
        ginfo("Module Phase: update")
        formula<-jmvcore::constructFormula(self$options$dep,self$options$somevars)
        self$data<-data

    },
    init_info=function() {
      try_hard(list(
        list(info="Means"),
        list(info="Regression"),
        list(info="Correlations"),
        list(info="N")
      ))
      },
    
    run_info=function() {
      try_hard(list(
        list(specs="Default"),
        list(specs=ifelse(self$options$regression,"Requested","Not requested")),
        list(specs=ifelse(self$options$correlations,"Requested","Not requested")),
        list(specs=dim(self$data)[1]
)
      ))
    },
    
    init_main_means=function() {
      try_hard({
        lapply(self$vars,function(v) list(source=v))
      })
    },
    
    run_main_means=function() {
      try_hard({
        mm<-apply(self$data,2,mean)
        ss<-apply(self$data,2,sd)
        data.frame(mean=mm,sd=ss)
      })
    },
    init_main_correlations=function() {
          try_hard({
                df<-data.frame(matrix(".",ncol=length(self$vars),nrow=length(self$vars)))
                names(df)<-self$vars
                df$source<-self$vars
                df
          })
    },

    run_main_correlations =function() {
          try_hard({
            data.frame(cor(self$data))
          })
    },

    
    init_mediansarray=function() {
     
      try_hard({
        lapply(self$options$focused,function(var) list(source=var))
      })
    },
    run_mediansarray=function() {
      try_hard({
        lapply(self$options$focused,function(var) data.frame(median=median(self$data[[var]])))
        })
    },

    init_regressionsarray=function() {
  
          try_hard(
                    lapply(self$options$focused,function(var) {
                          a<-list(list(source=var),
                                  list(source="Residuals")
                                  )
                          b<-list(list(source="Intercept"),
                                  list(source=var))
                          list(a,b)
                  })
    
            )
    },
    run_regressionsarray=function() {
  
        try_hard(
                lapply(self$options$focused,function(var) {
                   form<-jmvcore::constructFormula(self$options$dep,var)
                   model<- lm(form,self$data)
                   sumr<-summary(model)
                   regression<-as.data.frame(summary(model)$coefficients)
                   names(regression)<-c("estimate","se","t","p")
                   
                   anova<-as.data.frame(car::Anova(model))
                   names(anova)<-c("ss","df","f","p")
                   
                   list(anova,regression)
        })
    
      )
    },
    init_randomarray=function() {
      try_hard({
        self$random<-rbinom(1,5,.5)+1
        results<-lapply(1:self$random,function(a) list(source=letters[a]))
        k<-lapply(letters[1:self$random],function(a) list("y",a))

        attr(results,"keys")<-k
        results
      })
    },
    run_randomarray=function() {
      try_hard({
           results<-lapply(1:self$random,function(a) list(value=a^2))
           results
       })
       }

    
  )   # End public
)