#' Determine the type of brown coal in accordance with GOST 25543
#'
#' In accordance with
#' \href{http://docs.cntd.ru/document/1200107843}{GOST 25543} (\emph{Table 5})
#' determine the type of brown coal by maximum \emph{moisture-holding capacity}.
#'
#' @param wmaxaf
#'   maximum \emph{moisture-holding capacity}, [\emph{\%}], measured in
#'   accordance with \strong{ISO 1018}. Type: [\code{double}].
#'
#' @return
#'   Identifier of brown coal type according to \emph{Table 5} of
#'   \href{http://docs.cntd.ru/document/1200107843}{GOST 25543}.
#'   Type: [\code{character}].
#'
#' @export
#'
#' @examples
#'   brown_type(c(5.678, 19.94, 19.95, 20, 30, 69.9444))
#'   # [1] "10" "10" "20" "20" "30" "60"
#'
#'   # building test
#'   stopifnot(
#'     all(
#'       brown_type(c(5.678, 19.94, 19.95, 20, 30, 69.9444)) ==
#'       c("10", "10", "20", "20", "30", "60")
#'     )
#'   )
#'
#'
brown_type <- function(wmaxaf){
  checkmate::assert_numeric(wmaxaf, 0, 69.9444, any.missing = FALSE, min.len = 1)
  wmaxaf <- round(wmaxaf, 1)
  sprintf(
    "%i",
    10*((wmaxaf < 20) + floor(wmaxaf*.1)*(wmaxaf > 19.9))
  )
}

