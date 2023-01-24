# dong awards and analysis

library(tidyverse)

# get helper functions
source("dong-bot/dong_bot_functions.R")

# reverse %in%
`%not_in%` <- purrr::negate(`%in%`)

# data loading

# get outfield dimensions
fences <- readRDS(
  url("https://github.com/danmorse314/dinger-machine/raw/main/data/fences.rds")
  ) %>%
  dplyr::filter(stadium != "Sahlen Field")

# acceleration due to gravity in ft/s^2
g <- -32.174

# team abbreviations, hashtags, and stadium details
team_logos <- readr::read_csv(
  "https://github.com/danmorse314/dinger-machine/raw/main/dong-bot/data/team_logos_hashtags.csv",
  col_types = readr::cols()
  ) #%>%
# use Sahlen Field for Blue Jays at the moment
#mutate(stadium = ifelse(stadium == "Rogers Centre", "Sahlen Field", stadium))

# get plays that have already been tweeted
done_plays <- readRDS(
  url("https://github.com/danmorse314/dinger-machine/raw/main/data/done_plays.rds")
  ) %>%
  #done_plays <- readRDS(url("https://github.com/danmorse314/dinger-machine/raw/main/dong-bot/data/done_plays.rds")) %>%
  dplyr::mutate(game_date = as.character(game_date))

# load manually updated stadium paths
#   -fixed astros/marlins center fields
stadium_paths <- readRDS(
  url("https://github.com/danmorse314/dinger-machine/raw/main/data/stadium_paths.rds")
  ) %>%
  dplyr::filter(stadium != "Sahlen Field")

glimpse(done_plays)

transparent <- function(img) {
  magick::image_fx(img, expression = "0.5*a", channel = "alpha")
}

szn <- 2022

chart <- done_plays |>
  mutate(
    season = as.numeric(substr(game_date, 1, 4))
  ) |>
  filter(season == szn) |>
  group_by(stadium = stadium_observed) |>
  summarize(
    hits = n(),
    hr = sum(events == "Home Run"),
    xhr = sum(total_dongs)/30,
    hrox = hr - xhr,
    mean_stadiums = mean(total_dongs[events=="Home Run"]),
    unicorns = sum(total_dongs == 1 & events == "Home Run"),
    nd = sum(total_dongs == 30),
    nd_rate = nd / hr,
    unicorn_rate = unicorns / hr,
    .groups = "drop"
  ) |>
  arrange(-mean_stadiums) |>
  left_join(
    team_logos |>
      mutate(
        stadium = case_when(
          stadium == "LoanDepot Park" ~ "loanDepot park",
          stadium == "RingCentral Coliseum" ~ "Oakland Coliseum",
          stadium == "Great American Ballpark" ~ "Great American Ball Park",
          TRUE ~ stadium
        )
      ),
    by = "stadium"
  )

make_unicorn_plot <- function(tm, szn){
  stad <- filter(team_logos, team_abbr == tm) |> pull(stadium)
  
  tm_name <- filter(team_logos, team_abbr == tm) |> pull(full_team_name)
  
  team_unis <- filter(done_plays, stadium_observed == stad) |>
    mutate(
      season = as.numeric(substr(game_date, 1, 4))
    ) |>
    filter(season == szn) |>
    filter(total_dongs == 1 & events == "Home Run")
  
  if(nrow(team_unis) ==0){
    stop(glue::glue("No Unicorns were hit at {stad} in {szn}"))
  }
  
  team_unis <- team_unis |>
    mutate(
      Team = ifelse(
        player_team == tm_name,
        glue::glue("{tm} ({sum(nrow(filter(team_unis,player_team==tm_name)))})"),
        glue::glue("Visitors ({sum(nrow(filter(team_unis,player_team!=tm_name)))})")
      )
    )
  
  stadium_logo <- filter(team_logos, team_abbr == tm) |> pull(team_logo)
  
  stadium_path <- dplyr::filter(stadium_paths, stadium == stad)
  min_x <- min(stadium_path$x)
  min_y <- min(stadium_path$y)
  max_x <- max(stadium_path$x)
  
  park_fences <- fences %>%
    dplyr::filter(stadium == stad) %>%
    dplyr::select(stadium, x, y, fence_height)
  
  curv <- ifelse(mean(team_unis$hc_x_) > 0, -.2, .2)
  
  team_unis |>
    ggplot2::ggplot() +
    ggimage::geom_image(
      aes(x = 0, y = 250, image = stadium_logo),
      size = 0.25, image_fun = transparent
    ) +
    geom_text(
      aes(x = 0, y = 160, label = glue::glue("{stad}")),
      alpha = .3, vjust = 0
    ) +
    ggplot2::geom_path(
      data = stadium_path,
      ggplot2::aes(x, y, group = segment)
    ) +
    # add colored fence heights
    ggplot2::geom_path(
      data = park_fences,
      ggplot2::aes(x, y, color = fence_height),
      size = 2
    ) +
    labs(color = "Wall Height (ft)") +
    ggnewscale::new_scale_color() +
    # curve for original hit path
    ggplot2::geom_curve(
      aes(x = 0, y = 0, xend = hc_x_, yend = hc_y_,
          group = play_id, color = Team),
      curvature = curv,
      angle = 135, size = 1.5, alpha = .9
    ) +
    scale_color_manual(values = c("black","gray")) +
    # ball landing spot
    ggimage::geom_emoji(
      data = team_unis,
      aes(hc_x_, hc_y_,image ='1f4a5'),
      size = .07
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = min_x, y = min_y+35,
        label = glue::glue("Total Unicorns\nin {szn}:")
      ), hjust = 0, vjust = 0
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = min_x, y = min_y,
        label = glue::glue("{nrow(team_unis)}")
      ), hjust = 0, vjust = 0, fontface = "bold", size = 5
    ) +
    geom_text(
      aes(x = max_x, y = min_y, label = "@would_it_dong"),
      hjust = 1, vjust = 0, alpha = .7
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = element_rect(fill = "white", color = "white"),
      plot.background = element_rect(fill = "white", color = "white"),
      plot.margin = grid::unit(c(0.5,0.5,0.5,0.5), "mm")
    ) +
    ggplot2::coord_fixed()
  ggsave(glue::glue("figures/{tm}_{szn}_unicorn_chart.png"), width = 6, height = 4, dpi = 500)
  
}

make_unicorn_plot("BOS", 2022)

chart |>
  filter(team_abbr != "COL") |>
  ggplot(aes(xhr, hr)) +
  geom_abline(slope = 1, intercept = 0, color = "lightgray") +
  geom_vline(
    xintercept = weighted.mean(chart$xhr, chart$hits),
    linetype = "dashed", color = "lightgray"
  ) +
  geom_hline(
    yintercept = weighted.mean(chart$hr, chart$hits),
    linetype = "dashed", color = "lightgray"
  ) +
  ggimage::geom_image(
    aes(image = team_logo),
    asp = 8/5, size = 0.05
  ) +
  theme_bw() +
  labs(
    x = "Expected HR", y = "Observed HR",
    title = "Home Runs vs Expectation in 2022 by stadium",
    caption = "data: @would_it_dong"
  )
