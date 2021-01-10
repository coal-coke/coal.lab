#' Determine the type of anthracite in accordance with GOST 25543
#'
#' In accordance with
#' \href{http://docs.cntd.ru/document/1200107843}{GOST 25543} (\emph{Table 7})
#' determine the type of anthracite by \emph{volatile matter volume yield}, [\emph{cm^3/g}].
#'
#' @param vnudaf
#'   \emph{volatile matter volume yield}, [\emph{cm^3/g}], measured in
#'   accordance with \href{http://docs.cntd.ru/document/1200024047}{GOST 7303}.
#'   Type: [\code{double}].
#'
#' @return
#'   Identifier of anthracite type according to \emph{Table 7} of
#'   \href{http://docs.cntd.ru/document/1200107843}{GOST 25543}.
#'   Type: [\code{character}].
#'
#' @export
#'
#' @examples
#' anthracite_type(c(0, 99.949, 99.950, 150, 150.049, 150.050, 200, 200.050))
#' # [1] "05" "05" "10" "10" "10" "15" "15" "20"
#'
#' # build test:
#' stopifnot(
#'   all(
#'     anthracite_type(c(0, 99.949, 99.950, 150, 150.049, 150.050, 200, 200.050)) ==
#'     c("05", "05", "10", "10", "10", "15", "15", "20")
#'   )
#' )
#'
anthracite_type <- function(vnudaf){
  checkmate::assert_double(vnudaf, 0, 500, min.len = 1)
  vnudaf <- round(vnudaf, 1)
  sprintf(
    "%02i",
     5*(vnudaf < 100) + 10*(vnudaf > 99.9)*(vnudaf < 150.1) +
    15*(vnudaf > 150)*(vnudaf < 200.1) + 20*(vnudaf > 200)
  )
}

