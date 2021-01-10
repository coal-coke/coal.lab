#' Determine the subtype of hard coal in accordance with GOST 25543
#'
#' In accordance with
#' \href{http://docs.cntd.ru/document/1200107843}{GOST 25543} (\emph{Table 9})
#' determine the subtype of hard coal by plastometric index \emph{y}, [\emph{mm}],
#' and \emph{crucible swelling number}, [].
#'
#' @param y
#'   plastometric index \emph{y}, [\emph{mm}], measured in accordance with
#'   \href{http://docs.cntd.ru/document/1200121074}{GOST 1186}.
#'   Type: [\code{double}].
#'
#' @param sl
#'   \emph{crucible swelling number}, [], measured in accordance with
#'   \strong{ISO 501}. Type: [\code{double}].
#'
#' @return
#'   Identifier of hard coal subtype according to \emph{Table 9} of
#'   \href{http://docs.cntd.ru/document/1200107843}{GOST 25543}.
#'   Type: [\code{character}].
#'
#' @export
#'
#' @examples
#' hard_subtype(0:9, 0:9)
#' # [1] "00" "01" "01" "01" "01" "01" "06" "07" "08" "09"
#'
#' # build test:
#' stopifnot(
#'   all(
#'     hard_subtype(0:9, 0:9) ==
#'     c("00", "01", "01", "01", "01", "01", "06", "07", "08", "09")
#'   )
#' )
#'
hard_subtype <- function(y, sl = rep_len(0, length(y))){
  checkmate::assert_numeric(y, 0, 99, min.len = 1)
  n <- length(y)
  checkmate::assert_numeric(sl, 0, 9, len = n)
  y <- round(y)
  id <- 0 + y*(y > 5)
  id[y < 6 & sl >= 1] <- 1
  sprintf("%02i", id)
}


