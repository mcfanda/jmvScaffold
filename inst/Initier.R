

Initer <- R6::R6Class(
  "Initer",
  class=TRUE, 
  cloneable=FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
  inherit = Scaffold,
  public=list(
    datamatic=NULL,

    initialize=function(options,dispatcher,datamatic) {
      
      super$initialize(options,dispatcher)
      self$datamatic<-datamatic


    }, # here initialize ends
    #### init functions #####
    
    init_info=function() {
     mark("init info") 
    }

  ),   # End public
  
  private=list(

  ) # end of private
) # End Rclass


