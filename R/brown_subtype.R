#' Determine the subtype of brown coal in accordance with GOST 25543
#'
#' In accordance with
#' \href{http://docs.cntd.ru/document/1200107843}{GOST 25543} (\emph{Table 8})
#' determine the subtype of brown coal by \emph{yield of tar}, [\emph{\%}].
#'
#' @param tscdaf
#'   \emph{yield of tar}, [\emph{\%}], measured in accordance with
#'   \strong{ISO 647} and recalculated on dry ash-free basis.
#'   Type: [\code{double}].
#'
#' @return
#'   Identifier of brown coal subtype according to \emph{Table 8} of
#'   \href{http://docs.cntd.ru/document/1200107843}{GOST 25543}.
#'   Type: [\code{character}].
#'
#' @export
#'
#' @examples
#' brown_subtype(c(0.1, 9.9, 10.04, 10.05, 15.0, 15.09, 20.0, 43.2))
#' # [1] "05" "05" "05" "10" "10" "15" "15" "20"
#'
#' # building test:
#' stopifnot(
#'   all(
#'     brown_subtype(c(0.1, 9.9, 10.04, 10.05, 15.0, 15.09, 20.0, 43.2)) ==
#'       c("05", "05", "05", "10", "10", "15", "15", "20")
#'   )
#' )
#'
brown_subtype <- function(tscdaf){
  checkmate::assert_double(tscdaf, 0, 100, min.len = 1)
  tscdaf <- round(tscdaf, 1)
  sprintf(
    "%02i",
     5*(tscdaf < 10.1) + 10*(tscdaf > 10.0)*(tscdaf < 15.1) +
    15*(tscdaf > 15.0)*(tscdaf < 20.1) + 20*(tscdaf > 20.0)
  )
}


