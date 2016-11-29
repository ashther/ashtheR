#' @title caculate scale safely
#'
#' @description scale data, almost equal with \code{scale}, but safely
#'
#' @param vec numeric vector
#' @param center,scale same as \code{scale} parameters
#'
#' @details
#' when vec is all equal, \code{scale} will give all NAN results, which is not
#' acceptable for the fallowing caculation, \code{scaleSafe} will give all 0 vector
#' in this case.
#'
#' @seealso scale
#'
#' @export
scaleSafe <- function(vec, center = TRUE, scale = TRUE) {
  stopifnot(length(vec) > 0, is.atomic(vec), mode(vec) == 'numeric')
  if (diff(range(vec, na.rm = TRUE)) == 0) {
    return(rep(0, length(vec)))
  } else {
    return(as.numeric(scale(vec, center, scale)))
  }
}

coValueRatingSim <- function(u, df) {
  temp <- df[df$user == u, ]
  co_oc_item <- temp$item
  result <- NULL

  if (length(co_oc_item) > 1) {
    idx <- t(combn(co_oc_item, 2))

    result <- data.frame(
      idx,
      apply(idx, 1, function(cb){
        rating_pair <- temp$rating[temp$item %in% as.integer(cb)]
        if (any(rating_pair == 0)) {
          return(0)
        } else if (all(rating_pair > 0)) {
          return(min(rating_pair) / max(rating_pair))
        } else if (all(rating_pair < 0)) {
          return(max(rating_pair) / min(rating_pair))
        } else {
          return(-1 * min(abs(rating_pair)) / max(abs(rating_pair)))
        }
      }),
      stringsAsFactors = FALSE
    )
  }
  return(result)
}

coValueCoTimes <- function(u, df) {
  temp <- df[df$user == u, ]
  co_oc_item <- temp$item
  result <- NULL
  if (length(co_oc_item) > 1) {
    idx <- t(combn(co_oc_item, 2))

    result <- data.frame(idx, 1, stringsAsFactors = FALSE)
  }
  return(result)
}

#' @title make co-occurrence matrix
#'
#' @description make co-occurrence matrix based on users or items
#'
#' @param df dataframe about users , items and rating, if \code{method} is
#' \code{co_times} or \code{jaccard}, users and items columns are need only.
#' @param base \code{user} or \code{item}, difference collective filtering algorithm.
#' @param method \code{co_times}, \code{jaccard} or \code{rating_sim}
#'
#' @details
#' \itemize{
#'  \item \code{co_times} method will simpley get the co-occurrence times as result matrix
#' cell value,
#'  \item \code{jaccard} go one more step based on \code{co_times} method, it will
#' caculate two items(or users) jaccard coefficient as matrix cell value,
#'  \item \code{rating_sim} use algorithm made by myself, it uses rating rating ratio
#' between two items(or users) rated by one user(or item) as matrix cell value.
#' }
#'
#' @export
#' @importFrom dplyr %>% group_by summarise
#' @importFrom magrittr set_colnames
#' @importFrom utils combn
#'
coocCreate <- function(df, base = 'item', method = 'co_times') {
  if (!base %in% c('user', 'item')) {
    stop(sprintf("%s as base parameter, must be user or item", base))
  }

  if (!method %in% c('co_times', 'rating_sim', 'jaccard')) {
    stop(sprintf("%s as method parameter, must be 'co_times', 'rating_sim' or 'jaccard'", method))
  }

  if (ncol(df) == 2) {
    colnames(df) <- c('user', 'item')
  } else {
    colnames(df)[1:3] <- c('user', 'item', 'rating')
  }

  if (base == 'user') {
    colnames(df)[1:2] <- c('item', 'user')
  }
  df$user <- as.character(df$user)
  df$item <- as.character(df$item)

  items <- sort(unique(df$item))
  users <- sort(unique(df$user))

  co <- matrix(0, length(items), length(items),
               dimnames = list(items, items))

  if (method == 'rating_sim') {
    co_value_list <- lapply(users, coValueRatingSim, df = df)
  } else {
    co_value_list <- lapply(users, coValueCoTimes, df = df)
  }

  co_value_df <- co_value_list %>%
    do.call(rbind, .)

  if (is.null(co_value_df)) {
    return(NULL)
  }

  co_value_df <- co_value_df %>%
    set_colnames(c('i1', 'i2', 'sim')) %>%
    group_by(i1, i2) %>%
    summarise(sim = sum(sim))
  co[as.matrix(co_value_df[, 1:2])] <- co_value_df$sim

  co <- co + t(co)
  if (method == 'jaccard') {
    temp <- sapply(items, function(i) {
      length(df$user[df$item == i])
    }) %>%
      rep(each = length(items)) %>%
      matrix(nrow = length(items))
    temp <- temp + t(temp)

    co <- co / (temp - co)
    co[is.infinite(co) | is.nan(co)] <- 0
  }

  return(co)
}

#' @title jaccard coefficient
#'
#' @description caculate jaccard coefficient between two sets
#'
#' @param a,b same class vectors
#'
#' @details
#' jaccard coefficient was caculated by the formula:
#' \deqn{\frac{a\bigcap b}{a\bigcup b}}
#'
#' @export
jaccardCaculate <- function(a, b) {
  la <- length(a)
  lb <- length(b)
  lc <- length(intersect(a, b))
  result <- lc / (la + lb - lc)
  if (is.infinite(result) | is.nan(result)) {
    return(0)
  }
  return(result)
}
