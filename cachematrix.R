######################################################################
## Programming Assignment 2: Pesistance in superassigned environments
## Author: Chris Shattock, Dec 23 2018
## Licence: I don't care.
##
## We have three functions:
## 1. doTry - a 'general purpose' expression evaluator wrapped
##            in a try/catch block to trap errors and warning.
##            Two arguments: test - a logical expression to evaluate
##                                  as TRUE/FALSE or an arbitrary
##                                  expression to evaluate under error
##                                  control. Mandatory 
##            Two arguments: msg  - used if a logical test should yield
##                                  a FALSE value as a user-assigned
##                                  error message.
##                                  Optional with a default of null.
## 2. makeCacheMatrix - essentially a class with two read/write
##                      properties to encapsulate a square, invertible
##                      matrix and its inverse. Backing fields are
##                      persisted in a superassigned environment
##                      outside of the function scope.
## 3. cacheSolve - a function that attempts to yield the inverse of
##                 a matrix encapsulated via a makeCacheMatrix class 
##                 instance. If 'not yet' persisted (is NULL), then
##                 the inverse is evaluated and persisted to faciltate
##                 later access directly from the superassigned
##                 environment without the need for re-evaluation.

######################################################################
# A function to test state encapuslated in a try/catch expression
# The argument test can be a logical expression or a value - generally
# a function is a value so the argument may be a function invocation.
doTry <- function(test,msg = NULL) {
  # List to maintain error status condition.
  # If State is the output of a function evaluation then test
  # the Message as being not-null for 'successful' evaluation.
  status <- list("State" = NULL, "Message" = NULL)
  # Evaluate the test and assign to output State.
  # For a "logical" expression output will be logical
  # For a function evaluation output is the un-typed function result
  status$State <-
    tryCatch( 
      # 'try' the test
      switch(class(test),
             "logical" = {
               # The result of the logical test
               b <- test
               # If the logical test failed we have an
               # undified FALSE state so use the parsed msg
               # to qualify the message or, if null, provide
               # a default message
               if (!b) {
                 status$Message <- 
                   if(is.null(msg)){
                     "Your logical test returned false."
                   } else {
                     msg
                   }
               } 
               # Output the result of the logical test
               b },
             # For anything that is not a logical test, then
             # just execute the argument expression.
             test
             )
      # If try results in an error...
      ,error=function(exc) {
        # Persist the error message
        status$Message <- sprintf("Error raised of %s",exc)
        # Output a logical State of FALSE
        FALSE }
      # If try results in a warning...
      ,warning=function(exc) {
        # Persist the warning message
        status$Message <- sprintf("Warning raised of %s",exc)
        # Output a logical State of FALSE - we consider a warning
        # to be as terminal as an error.
        FALSE }
      # Omit the finally for the try/catch
    )
  # Output the computed status list.
  status
}

######################################################################
# A function to essentially encapsulate a square, invertible matrix 
# in a class using a superassigned environment rather than the function
# environment. 
# The matrix is always persisted and the inverse is initialised on null
# and only persisted explicitly via the subsequent cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {
  # instantiation validation of function input...
  # 1. Have we parsed a square matrix?
  s <- doTry( is.matrix(x) & nrow(x) == ncol(x),
              "Inavlid matrix argument, or not a square matrix" )
  # If State is FALSE, stop execution
  if(!s$State) stop(s$Message)
  
  # 2. Does matrix have a non-zero determinant?
  s <- doTry( det(x) != 0,
              "Non-invertible matrix")
  # If State is FALSE, stop execution
  if(!s$State) stop(s$Message)
  
  # If we get 'here' the funtion input has been successfully 
  # validated and the function is not 'stopped', so we continue
  # to set-up an 'instance' of the makeCacheMatrix class...

  # We firslty explicitly declare backing fields for the parsed
  # matrix and its inverse which we want to persist using an 
  # initial assignment of the function argument for the matrix 
  # and a null assignment for the inverse.
  # These fields will be superassigned to an environment outside
  # the scope of function evaluation using the <<- operator.
  mtx <<- x
  inv <<- NULL
  
  # Property definitions:
  #
  # 1. The read/write (get/set) properties for the parsed matrix.
  #    Note that the 'backing field' for the parsed matrix, mtx is
  #    implicit - as opposed to the inv field which persists the
  #    matrix inverse.
  # a. Set the value of the encapsulated matrix and re-initialise
  #    its associated inverse.
  #    We assign the set argument value to a matrix for which the inverse
  #    will be evaluated. The class backing field inv is re-initialised
  #    to a null.
  #    The set function returns no output but simply persists the
  #    input matrix, mtx and the null-initialised fields in the 
  #    superassigned environemnt (using the <<- operator) as 
  #    opposed to the function environment.
  set <- function(value) {
    mtx <<- value
    inv <<- NULL
  }
  # b. Gets the value of the parsed matrix
  get <- function() mtx

  # 2. The read/write property for the parsed matrix's inverse
  #    which is persisted as the superassigned backing field inv
  #    which field is explicitly defined above since we will need
  #    to access it in order to change its value when we
  #    evaluate the matix inverse vis this set property.
  # a. Set the inverse of the persisted parsed matrix, inv.
  #    Here we again use superassignment to target the environment 
  #    for the inverse inv which is 'outside' the 
  #    function environment.
  setInverse <- function(value) inv <<- value
  # Gets the value of the field value - the parsed matrix
  getInverse <- function() inv

  # For this 'class', we enumerate the methods to be exposed
  # via a list of the method names and associated functions...
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

######################################################################
# A function to access and output the inverse of a makeCacheMatrix
# class instance based upon its instantiated squre, invertible matrix.
# If the inverse has 'not yet' been evaluated, then do so and persist
# it into the superassigned environment of the class instance's fields.
cacheSolve <- function(cm, ...) {
  # Validate function input as an instance of makeCacheMatrix -
  # We check if it's a list and the list names corresspond.
  s <- doTry ( class(cm) == "list" &
               all(names(cm) == c("set", "get",
                                  "setInverse", "getInverse")),
              "Invalid argument - expecting type of makeCacheMatrix")
  # If validation failed, stop execution
  if(!s$State) stop(s$Message)
  
  # Fetch the inverse from the makeCacheMatrix instance
  inv <- cm$getInverse()
  # Check if previously evaluated and persited...
  if(!is.null(inv)) {
    # A previous evaluation of the inverse has been
    # persisted - show a trace message and return it...
    message("getting cached data...")
    return(inv)
  } else {
    # We must evaluate the inverse: Get the persisted matrix...
    mtx <- cm$get()
    # Attempt to evaluate inverse of the matrix
    i <- doTry( solve(mtx), "Inversion failed" )
    # Here the doTry State will be the inverse if successful
    # but, if not, then the Message will be non-null
    # and, if non-null, we terminate execution...
    if(!is.null(i$Message)) stop(i$Message)
    # Otherwise, we set the persisted inverse matrix value...
    cm$setInverse(i$State)
    # Then output the inverse matrix...
    return(i$State) 
  }
}
