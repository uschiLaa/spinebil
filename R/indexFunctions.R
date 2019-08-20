#' Matching index functions to the required format.
#'
#' These are convenicence functions that format
#' scagnostics, splines2d, dcor2d and mine index functions for direct
#' use with the guided tour or other functionalities in this package.
#'
#' @describeIn scagIndex Scagnostics index from binostics package
#' @param indexName Index name to select from group of indexes.
#' @return function taking 2-d data matrix and returning the index value
#' @export
scagIndex <- function(indexName){
  function(mat){
    sR <- binostics::scagnostics(mat)[1,][indexName]
    return(sR)
  }
}

#' @describeIn scagIndex splines2d index from mbgraphic package
#' @export
splineIndex <- function(){
  function(mat){
    return(mbgraphic::splines2d(mat[,1], mat[,2]))
  }
}

#' @describeIn scagIndex dcor2d index from mbgraphic package
#' @export
dcorIndex <- function(){
  function(mat){
    return(mbgraphic::dcor2d(mat[,1], mat[,2]))
  }
}

#' @describeIn scagIndex MINE index from minerva package
#' @export
mineIndex <- function(indexName){
  function(mat){
    return(minerva::mine(mat[,1], mat[,2])[[indexName]])
  }
}

#' @describeIn scagIndex MINE index from minerva package (updated estimator)
#' @export
mineIndexE <- function(indexName){
  function(mat){
    return(minerva::mine(mat[,1], mat[,2], est = "mic_e")[[indexName]])
  }
}


#' @describeIn scagIndex rescaling the tourr holes index
#' @export
holesR <- function(){
  function(mat){
    ret <- tourr::holes()(mat)
    if(ret<0.8) ret <- 0.
    else ret <- (ret-0.8) * 5
    return(ret)
  }
}

#' @describeIn scagIndex rescaling the tourr cmass index
#' @export
cmassR <- function(){
  function(mat){
    ret <- tourr::cmass()(mat)
    if(ret<0.2) ret <- 0.
    else ret <- (ret-0.2) * 1.25
    return(ret)
  }
}


