#' Slightly modified version of purrr::when. Match/validate a set of conditions
#' for an object and continue with the action associated with the first valid match.
#'
#' purrr_when() serves as a replacement for the when() function, which was
#' deprecated in purrr 1.0.0.
#'
#' Copied from purrr::when description:
#' purrr_when is a flavour of pattern matching (or an if-else abstraction) in which a value
#' is matched against a sequence of condition-action sets. When a valid
#' match/condition is found the action is executed and the result of the
#' action is returned.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> A sequence of two-sided
#' formulas. The left hand side (LHS) determines which values match this
#' case. The right hand side (RHS) provides the replacement value.
#' The LHS must evaluate to a logical vector. The RHS does not need to be logical,
#' and all RHSs are not required to evaluate to the same type of vector.
#'
#' @author Daniel D Sjoberg
#' @name purrr_when
#' @examples
#' y <- 2
#' purrr_when(
#'   y < 8 ~ y + 2,
#'   y > 8 ~ "y > 8"
#' )
#' y <- 10
#' purrr_when(
#'   y < 8 ~ y + 2,
#'   y > 8 ~ "y > 8"
#' )
#' y <- 8
#' purrr_when(
#'   y < 8 ~ y + 2,
#'   y > 8 ~ "y > 8"
#' )
#' @return The result of the action associated with the first valid match/condition found


purrr_when <- function(...) {
  # takes all arguments and puts them in a named list
  lst_formulas <- rlang::dots_list(...)

  for (i in seq_len(length(lst_formulas))) {

    # throw error message if arg is NOT a formula
    if (!inherits(lst_formulas[[i]], "formula")) {
      stop("Each argument must be a formula", call. = FALSE)
    }

    # throw error message if LHS is NOT logical
    if (!is.logical(rlang::eval_tidy(.f_lhs_as_quo(lst_formulas[[1]])))) {
      stop("Left-hand side of each argument must evaluate to a logical", call. = FALSE)
    }

    if (inherits(lst_formulas[[i]], "formula") &
      is.logical(rlang::eval_tidy(.f_lhs_as_quo(lst_formulas[[i]])))) {
      if (isTRUE(rlang::eval_tidy(.f_lhs_as_quo(lst_formulas[[i]])))) {
        if (length(lst_formulas) > i) {
          message(paste0(
            "Note: Condition of the left-hand side of argument ", i,
            " met. Any argument(s) beyond argument ", i,
            " were not checked."
          ))
        }
        tryCatch(rlang::eval_tidy(.f_rhs_as_quo(lst_formulas[[i]])),
                 warning = function(w) message(paste0("Warning for argument ", i)))
        return(rlang::eval_tidy(.f_rhs_as_quo(lst_formulas[[i]])))
      }
    }
  }

  # if not matches, return NULL
  NULL
}

# extract the left-hand side of a formula and convert it to a quosure
.f_lhs_as_quo <- function(x) {
  rlang::new_quosure(
    expr = rlang::f_lhs(x),
    env = attr(x, ".Environment")
  )
}

# extract the right-hand side of a formula and convert it to a quosure
.f_rhs_as_quo <- function(x) {
  rlang::new_quosure(
    expr = rlang::f_rhs(x),
    env = attr(x, ".Environment")
  )
}
