#' Determine the type of hard coal in accordance with GOST 25543
#'
#' In accordance with
#' \href{http://docs.cntd.ru/document/1200107843}{GOST 25543} (\emph{Table 6})
#' determine the type of hard coal by \emph{volatile matter yield}, [\emph{\%}].
#'
#' @param vdaf
#'   \emph{volatile matter yield}, [\emph{\%}], measured in accordance with
#'   \strong{ISO 562} or \strong{ISO 5071-1} and recalculated
#'   on dry ash-free basis. Type: [\code{double}].
#'
#' @return
#'   Identifier of hard coal type according to \emph{Table 6} of
#'   \href{http://docs.cntd.ru/document/1200107843}{GOST 25543}.
#'   Type: [\code{character}].
#'
#' @export
#'
#' @examples
#' hard_type(c(8.00, 9.23, 19.5, 20.1, 47.94, 47.95, 48, 50))
#' # [1] "08" "08" "18" "20" "46" "48" "48" "48"
#'
#' # building test:
#' stopifnot(
#'   all(
#'     hard_type(c(8.00, 9.23, 19.5, 20.1, 47.94, 47.95, 48, 50)) ==
#'     c("08", "08", "18", "20", "46", "48", "48", "48")
#'   )
#' )
#'
hard_type <- function(vdaf){
  checkmate::assert_double(vdaf, 8, 100, min.len = 1)
  vdaf <- round(vdaf, 1)
  sprintf(
    "%02i",
    2*floor(vdaf/2)*(vdaf < 48) + 48*(vdaf >= 48)
  )
}

