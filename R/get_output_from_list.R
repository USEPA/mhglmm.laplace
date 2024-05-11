#' Title
#'
#' @param object x
#'
#' @return x
#' @export
get_verify_output_from_list <- function(object, only_time = FALSE) {


  family <- object[[1]]$result$family
  nugget <- object[[1]]$result$nugget
  front_path <- here("inst", "output", "verify", nugget)

  object_error <- map_lgl(object, ~ !is.null(.x$error))
  if (any(object_error)) {
    object_bad <- tibble(
      trial = which(object_error)
    )
    object_bad$message <- map_chr(object[object_error], ~ .x$error[[1]])
    if (!only_time) {
      write_csv(object_bad, paste0(front_path, "/", family, "_", nugget, "_error.csv"))
    }
  }
  object <- object[!object_error]
  object <- map(object, ~ .x$result)

  output_fixed <- bind_rows(map(object, ~ .x$fixed))
  # write_csv(output_fixed, here("inst", "output", "verify", paste0(family, "_fixed_full.csv")))


  fixed_summary <- output_fixed %>%
    group_by(beta) %>%
    summarize(
      bias = mean(error),
      mse = mean(error^2),
      ratio = bias^2 / mse,
      cover_fit = mean(cover_fit),
      cover_fit_adj = mean(cover_fit_adj)
    )
  if (!only_time) {
    write_csv(fixed_summary, paste0(front_path, "/", family, "_", nugget, "_fixed_summary.csv"))
  }

  output_pred <- bind_rows(map(object, ~ .x$preds))
  # write_csv(output_pred, here("inst", "output", "verify", paste0(family, "_pred_full.csv")))

  pred_summary <- output_pred %>%
    summarize(
      bias = mean(error),
      mse = mean(error^2),
      ratio = bias^2 / mse,
      cover_fit = mean(cover_fit),
      cover_fit_adj = mean(cover_fit_adj)
    )
  if (!only_time) {
    write_csv(pred_summary, paste0(front_path, "/", family, "_", nugget, "_pred_summary.csv"))
  }



  output_time_fixed <- map_dbl(object, ~ .x$fixed_time)
  output_time_pred <- map_dbl(object, ~ .x$pred_time)
  output_time <- tibble(
    fixed = output_time_fixed,
    pred = output_time_pred
  )
  # write_csv(output_time, here("inst", "output", "verify", paste0(family, "_time_full.csv")))
  time_summary <- tibble(
    fixed = mean(output_time_fixed),
    pred = mean(output_time_pred)
  )
  write_csv(time_summary, paste0(front_path, "/", family, "_", nugget, "_time_summary.csv"))

}


#' Title
#'
#' @param object x
#'
#' @return x
#' @export
get_geostat_output_from_list <- function(object, only_time = FALSE) {



  family <- object[[1]]$result$family
  n <- object[[1]]$result$n
  front_path <- here("inst", "output", "compare")

  object_error <- map_lgl(object, ~ !is.null(.x$error))
  if (any(object_error)) {
    object_bad <- tibble(
      trial = which(object_error)
    )
    object_bad$message <- map_chr(object[object_error], ~ .x$error[[1]])
    if (!only_time) {
      write_csv(object_bad, paste0(front_path, "/", "geostat_", family, "_", n, "_error.csv"))
    }
  }
  object <- object[!object_error]
  object <- map(object, ~ .x$result)

  output_fixed <- bind_rows(map(object, ~ .x$output_fixed))
  # write_csv(output_fixed, here("inst", "output", "verify", paste0(family, "_fixed_full.csv")))


  fixed_summary <- output_fixed %>%
    group_by(beta, software) %>%
    summarize(
      bias = mean(error),
      mse = mean(error^2),
      ratio = bias^2 / mse,
      is_na = sum(is.na(cover_fit)),
      cover_fit = mean(cover_fit, na.rm = TRUE)
    )
  if (!only_time) {
    write_csv(fixed_summary, paste0(front_path, "/", "geostat_", family, "_", n, "_fixed_summary.csv"))
  }

  output_pred <- bind_rows(map(object, ~ .x$output_preds))
  # write_csv(output_pred, here("inst", "output", "verify", paste0(family, "_pred_full.csv")))

  pred_summary <- output_pred %>%
    group_by(software) %>%
    summarize(
      bias = mean(error),
      mse = mean(error^2),
      ratio = bias^2 / mse,
      is_na = sum(is.na(cover_fit)) / 100,
      cover_fit = mean(cover_fit, na.rm = TRUE)
    )
  if (!only_time) {
    write_csv(pred_summary, paste0(front_path, "/", "geostat_", family, "_", n, "_pred_summary.csv"))
  }



  output_time_fixed <- bind_rows(map(object, ~ .x$output_fixed_time)) %>%
    mutate(type = "fixed")
  output_time_preds <- bind_rows(map(object, ~ .x$output_preds_time)) %>%
    mutate(type = "preds")
  output_time <- bind_rows(output_time_fixed, output_time_preds)
  # write_csv(output_time, here("inst", "output", "verify", paste0(family, "_time_full.csv")))
  time_summary <- output_time %>%
    group_by(software, type) %>%
    summarize(time = mean(time))
  write_csv(time_summary, paste0(front_path, "/", "geostat_", family, "_", n, "_time_summary.csv"))

}


#' Title
#'
#' @param object x
#'
#' @return x
#' @export
get_autor_output_from_list <- function(object, only_time = FALSE) {



  family <- object[[1]]$result$family
  n <- object[[1]]$result$sqrt_n
  front_path <- here("inst", "output", "compare")

  object_error <- map_lgl(object, ~ !is.null(.x$error))
  if (any(object_error)) {
    object_bad <- tibble(
      trial = which(object_error)
    )
    object_bad$message <- map_chr(object[object_error], ~ .x$error[[1]])
    if (!only_time) {
      write_csv(object_bad, paste0(front_path, "/", "autor_", family, "_", n, "_error.csv"))
    }
  }
  object <- object[!object_error]
  object <- map(object, ~ .x$result)

  output_fixed <- bind_rows(map(object, ~ .x$output_fixed))
  # write_csv(output_fixed, here("inst", "output", "verify", paste0(family, "_fixed_full.csv")))


  fixed_summary <- output_fixed %>%
    group_by(beta, software) %>%
    summarize(
      bias = mean(error),
      mse = mean(error^2),
      ratio = bias^2 / mse,
      cover_fit = mean(cover_fit)
    )
  if (!only_time) {
    write_csv(fixed_summary, paste0(front_path, "/", "autor_", family, "_", n, "_fixed_summary.csv"))
  }

  output_pred <- bind_rows(map(object, ~ .x$output_preds))
  # write_csv(output_pred, here("inst", "output", "verify", paste0(family, "_pred_full.csv")))

  pred_summary <- output_pred %>%
    group_by(software) %>%
    summarize(
      bias = mean(error),
      mse = mean(error^2),
      ratio = bias^2 / mse,
      cover_fit = mean(cover_fit)
    )
  if (!only_time) {
    write_csv(pred_summary, paste0(front_path, "/", "autor_", family, "_", n, "_pred_summary.csv"))
  }



  output_time_fixed <- bind_rows(map(object, ~ .x$output_fixed_time)) %>%
    mutate(type = "fixed")
  output_time_preds <- bind_rows(map(object, ~ .x$output_preds_time)) %>%
    mutate(type = "preds")
  output_time <- bind_rows(output_time_fixed, output_time_preds)
  # write_csv(output_time, here("inst", "output", "verify", paste0(family, "_time_full.csv")))
  time_summary <- output_time %>%
    group_by(software, type) %>%
    summarize(time = mean(time))
  write_csv(time_summary, paste0(front_path, "/", "autor_", family, "_", n, "_time_summary.csv"))

}


