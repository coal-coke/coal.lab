#' Determine the subtype of anthracite in accordance with GOST 25543
#'
#' In accordance with
#' \href{http://docs.cntd.ru/document/1200107843}{GOST 25543} (\emph{Table 10})
#' determine the subtype of anthracite by anisotropy of vitrinite reflectance,
#' [\emph{\%}].
#'
#' @param ar
#'   anisotropy of vitrinite reflectance, [\emph{\%}], calculated in accordance
#'   with formula 4 in \href{http://docs.cntd.ru/document/1200105476}{GOST R 55659}.
#'   Type: [\code{double}].
#'
#' @return
#'   Identifier of anthracite subtype according to \emph{Table 10} of
#'   \href{http://docs.cntd.ru/document/1200107843}{GOST 25543}.
#'   Type: [\code{character}].
#'
#' @export
#'
#' @examples
#' anthracite_subtype(c(0, 29.4, 29.5, 40, 40.509, 80))
#' # [1] "20" "20" "30" "30" "40" "70"
#'
#' # build test:
#' stopifnot(
#'   all(
#'     anthracite_subtype(c(0, 29.4, 29.5, 40, 40.509, 80)) ==
#'     c("20", "20", "30", "30", "40", "70")
#'   )
#' )
#'
anthracite_subtype <- function(ar){
  checkmate::assert_numeric(ar, 0, 99, min.len = 1)
  ar <- round(ar)
  sprintf(
    "%02i",
    20*(ar < 30) + 30*(ar > 29)*(ar < 41) +
    40*(ar > 40)*(ar < 51) + 50*(ar > 50)*(ar < 61) +
    60*(ar > 60)*(ar < 71) + 70*(ar > 70)
  )
}


