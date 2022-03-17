Estimate <- R6::R6Class(
  "Estimate",
  class=TRUE, ## this and the next 
  cloneable=FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
  inherit = Scaffold,
  public=list(
    data=NULL,
    random= NULL,
    vars=NULL,
    dispatcher=NULL,
    initialize=function(options,dispatcher,datamatic=NULL) {
      
        super$initialize(options,dispatcher)
        self$vars<-unlist(c(options$somevars,options$dep))
        self$dispatcher<-dispatcher
      
    }, # here initialize ends
    

    update=function(data) {
      
        ginfo("Module Phase: update")
        formula<-jmvcore::constructFormula(self$options$dep,self$options$somevars)
        self$data<-data

    },
    init_info=function() {
      
      if (self$option("msg1")) {
        self$dispatcher$warnings<-list(topic="main_means",message="This message from init will stay")
        self$dispatcher$warnings<-list(topic="main_means",message="This message from init will go",init=TRUE)
        self$dispatcher$warnings<-list(topic="randomarray",message="This is a message from init")
        self$dispatcher$warnings<-list(topic="regressionsarray_*_anova",message="This is a message from init to anova")
#        self$dispatcher$warnings<-list(topic="wrongname",message="This is a message will not go")
      }
       list(
        list(info="Means"),
        list(info="Regression"),
        list(info="Correlations"),
        list(info="N")
      )
      },
    
    run_info=function() {

      list(
        list(specs="Default"),
        list(specs=ifelse(self$options$regression,"Requested","Not requested")),
        list(specs=ifelse(self$options$correlations,"Requested","Not requested")),
        list(specs=dim(self$data)[1])
      )
    },
    
    init_main_means=function() {
      
        self$dispatcher$warnings<-list(topic="randomarray_y.c",message="to y.c only")
        lapply(self$vars,function(v) list(source=v))
    
          },
    
    run_main_means=function() {
  
        warning("There is a message internally from main means")
        mm<-apply(self$data,2,mean)
        ss<-apply(self$data,2,sd)
        data.frame(mean=mm,sd=ss)
      
    },
    init_main_correlations=function() {
          
                df<-data.frame(matrix(".",ncol=length(self$vars),nrow=length(self$vars)))
                names(df)<-self$vars
                df$source<-self$vars
                df
          
    },

    run_main_correlations =function() {
            self$dispatcher$warnings<-list(topic="randomarray",message="from correlations run")
            data.frame(cor(self$data))
          
    },

    
    init_mediansarray=function() {
     
      lapply(self$options$focused,function(var) list(source=var))
      
    },
    run_mediansarray=function() {
      
        lapply(self$options$focused,function(var) data.frame(median=median(self$data[[var]])))
      
    },

    init_regressionsarray=function() {

        results<-lapply(self$options$somevars,function(var) {
                          a<-list(list(source=var),
                                  list(source="Residuals")
                                  )
                          b<-list(list(source="Intercept"),
                                  list(source=var))
                          list(a,b)
        })
        attr(results,"keys")<-self$options$somevars
        results
        
    
            
    },
    run_regressionsarray=function() {
  
    lapply(self$options$somevars,function(var) {
                   form<-jmvcore::constructFormula(self$options$dep,var)
                   model<- lm(form,self$data)
                   sumr<-summary(model)
                   regression<-as.data.frame(summary(model)$coefficients)
                   names(regression)<-c("estimate","se","t","p")
                   
                   anova<-as.data.frame(car::Anova(model))
                   names(anova)<-c("ss","df","f","p")
                   
                   list(anova,regression)
        })
    },
    init_randomarray=function() {
      
        self$random<-rbinom(1,5,.5)+1
        results<-lapply(1:self$random,function(a) list(source=letters[a]))
        k<-lapply(letters[1:self$random],function(a) list("y",a))

        attr(results,"keys")<-k
        results
      
    },
    run_randomarray=function() {

           results<-lapply(1:self$random,function(a) list(value=a^2))
           results
       }

    
  )   # End public
)