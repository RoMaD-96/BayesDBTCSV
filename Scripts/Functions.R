# Build Stan data
prepare_stan_data <- function(data,
                              newdata = NULL,
                              mean_logStrength = 0,
                              ind_home = 1,
                              mean_home = 0,
                              sd_home = 10,
                              p_spike = 0.05,
                              mu_slab = 0,
                              mu_spike = 100,
                              sd_slab = 5,
                              sd_spike = 0.1,
                              s_prior_shape = 0.1,
                              s_prior_rate = 0.1) {

  team_ids <- sort(unique(c(data$hometeamId, data$awayteamId)))
  team_lookup <- setNames(seq_along(team_ids), team_ids)

  data2 <- data %>%
    mutate(
      team1 = team_lookup[hometeamId],
      team2 = team_lookup[awayteamId],
      winner_team = team_lookup[as.factor(winner)],
      y = as.integer(homeScore > awayScore)
    )

  stan_list <- list(
    N = nrow(data2),
    nteams = length(team_ids),
    ntimes_rank = length(unique(data2$instants_rank)),
    instants_rank = data2$instants_rank,
    team1 = as.integer(data2$team1),
    team2 = as.integer(data2$team2),
    winner_team = as.integer(data2$winner_team),
    mean_logStrength = mean_logStrength,
    y = data2$y,
    ind_home = as.integer(ind_home),
    mean_home = mean_home,
    sd_home = sd_home,
    s_prior_shape = s_prior_shape,
    s_prior_rate = s_prior_rate,
    #spike and slab commensurate
    p_spike = p_spike,
    mu_slab = mu_slab,
    mu_spike = mu_spike,
    sd_slab = sd_slab,
    sd_spike = sd_spike
  )

  # ---- if playoff data provided, append ----
  if (!is.null(newdata)) {
    newdata2 <- newdata %>%
      mutate(
        team1_prev = team_lookup[hometeamId],
        team2_prev = team_lookup[awayteamId],
        instants_rank = as.integer(instants_rank)
      )
    stan_list$N_prev <- nrow(newdata2)
    stan_list$instants_rank_prev <- as.integer(newdata2$instants_rank)
    stan_list$team1_prev <- as.integer(newdata2$team1_prev)
    stan_list$team2_prev <- as.integer(newdata2$team2_prev)
  } else {
    stan_list$N_prev <- 0
    stan_list$instants_rank_prev <- integer(0)
    stan_list$team1_prev <- integer(0)
    stan_list$team2_prev <- integer(0)
  }

  stan_list
}


# Function to download and resize logos
download_and_resize_logo <- function(team, url, size = 50) {
  # Clean up team names for safe filenames
  file_name <- gsub(" ", "_", team)
  original_path <- file.path("Logos", paste0(file_name, ".png"))
  small_path <- file.path("Logos_small", paste0(file_name, "_small.png"))

  # Check if small version already exists
  if (!file.exists(small_path)) {
    # Download if original doesn't exist
    if (!file.exists(original_path)) {
      download.file(url, original_path, mode = "wb", quiet = TRUE)
    }

    # Resize the image
    img <- image_read(original_path)
    img_small <- image_resize(img, paste0(size, "x", size))
    image_write(img_small, small_path)

    message("Processed: ", team)
  }

  return(small_path)
}
