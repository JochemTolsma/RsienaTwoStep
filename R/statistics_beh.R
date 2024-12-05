#' @title Behavior Statistics
#'
#' @description These functions calculate the respective behavior statistic for
#'   ego. When multiplied with the importance of each statistic (the
#'   'parameters') this constitutes the network evaluation of ego. See:
#'   [`ts_eval()`].
#'
#' @details For examples on how to use these statistics see:
#'   `vignette("Introduction_RsienaTwoStep", package="RsienaTwoStep")`.
#'
#'   For the mathematical definition of these network statistics see chapter 12
#'   of the RSiena manual \insertCite{ripley2022manual}{RsienaTwoStep}.
#' @family networkstatistics
#' @param beh behavioral dependent variable
#' @param net matrix, the adjacency matrix representing the relations between
#'   actors. Valid values are 0 and 1.
#' @param cov numeric, covariate scores
#' @param ego numeric, the ego for which we want to calculate the network
#'   statistic.
#'
#' @references \insertRef{ripley2022manual}{RsienaTwoStep}
#' @return numeric value
#' @seealso [`ts_eval()`]
#' @examples
#' ts_linear(df_ccovar1$cov2, ego=3)
#'
#' @importFrom Rdpack reprompt
#' @export
ts_linear <- function(beh, ego) {
  statistic <- beh[ego]
  return(statistic)
}
attr(ts_linear, "name") <- "linear"


#' @rdname ts_linear
#' @export
ts_quad <- function(beh, ego) {
  statistic <- (beh[ego])^2
  return(statistic)
}
attr(ts_quad, "name") <- "quad"


#' @rdname ts_linear
#' @export
ts_avAlt <- function(beh, net, ego, cov=NULL) {
  alters <- which(net[ego,]==1)
  if (length(alters) == 0) {
    statistic <- 0
    } else {
  statistic <- beh[ego]* mean(beh[alters])
    }
  return(statistic)
}
attr(ts_avAlt, "name") <- "avAlt"

#' @rdname ts_linear
#' @export
ts_effFrom <- function(beh, net=NULL, ego, cov) {
  statistic <- beh[ego] * cov[ego]
  return(statistic)
}
attr(ts_effFrom, "name") <- "effFrom"


#' @rdname ts_linear
#' @export
#this one calculates the mean opinion push of all connected alters
#opinion = beh, note that this variable does not automatically falls within [-1,1], thus need to correct for this
#group = cov
#H = parameter
ts_avNiAlt <- function(beh, net, ego, cov=NULL, parameter=0.5,...) {
  alters <- which(net[ego,]==1)
  if (length(alters) == 0) {
    statistic <- 0
  } else {

    #making sure opinion variable is treated as if within a range of [-1,1]
    if (!is.null(attributes(beh)$range)) {
      hrange <- 0.5 * attributes(beh)$range
    } else {
      hrange = 1
    }

    weight_alters <- 1 -  ((abs(beh[ego] - beh[alters])/hrange)*parameter + (1-parameter)*2*abs(cov[ego] != cov[alters]))
    opinion_delta <- 0.5*((beh[alters] - beh[ego])/hrange)*weight_alters
    statistic <- 1 - abs(mean(opinion_delta)) #maximize the behavior where the opinion push is minimal
  }
  return(statistic)
}
attr(ts_avNiAlt, "name") <- "avNiAlt"

#' @rdname ts_linear
#' @export
#this one calculates the mean opinion push of one random connected alter
ts_1NiAlt <- function(beh, net, ego, cov, parameter=0.5, seed = NULL) {
  alters <- which(net[ego,]==1)
  if (length(alters) == 0) {
    statistic <- 0
  } else {

    #only dyadic influence from a random connected alter, could tweak this weight distance/homophily function
    if (!is.null(seed)) set.seed(seed)
    alters <- sample(alters, 1)

    #making sure opinion variable is treated as if within a range of [-1,1]
    if (!is.null(attributes(beh)$range)) {
      hrange <- 0.5 * attributes(beh)$range
    } else {
      hrange = 1
    }

    weight_alters <- 1 -  ((abs(beh[ego] - beh[alters])/hrange)*parameter + (1-parameter)*2*abs(cov[ego] != cov[alters]))
    opinion_delta <- 0.5*((beh[alters] - beh[ego])/hrange)*weight_alters
    statistic <- 1 - abs(mean(opinion_delta)) #maximize the behavior where the opinion push is minimal
  }
  return(statistic)
}
attr(ts_1NiAlt, "name") <- "1NiAlt"



