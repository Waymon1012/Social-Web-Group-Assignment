library(jsonlite)

# ---- Settings ---------------------------------------------------------------
api_key        <- "c54d3e97e78134b0ad42b125348bfd3d"
years          <- 2005:2025   # release years to sample
pages_per_year <- 5           # 20 films per page -> up to 100 films per year
min_votes      <- 50          # ignore films with fewer TMDb votes than this
top_cast       <- 5           # top-billed actors kept per film

# ---- Helper: request a TMDb URL and return the parsed JSON -------------------
get_json <- function(path, query) {
  url <- paste0("https://api.themoviedb.org/3", path,
                "?api_key=", api_key, "&", query)
  for (attempt in 1:3) {                       # try up to 3 times
    result <- tryCatch(fromJSON(url), error = function(e) NULL)
    if (!is.null(result)) return(result)
    Sys.sleep(attempt)                         # wait, then retry
  }
  NULL
}

dir.create("data", showWarnings = FALSE)

# ---- 1. Genre list ----------------------------------------------------------
genres <- get_json("/genre/movie/list", "language=en-US")$genres
if (NROW(genres) == 0) stop("Could not download data. Check the API key and your internet connection.")
write.csv(genres, "data/tmdb_genres.csv", row.names = FALSE, fileEncoding = "UTF-8")

# ---- 2. Films: the most-voted English-language films of each year -------------
movies <- list()
for (year in years) {
  for (page in 1:pages_per_year) {
    query <- paste0("language=en-US&sort_by=vote_count.desc",
                    "&with_original_language=en&include_adult=false",
                    "&primary_release_year=", year,
                    "&vote_count.gte=", min_votes, "&page=", page)
    res <- get_json("/discover/movie", query)
    if (is.null(res) || NROW(res$results) == 0) break      # no more films this year
    r <- res$results

    # each film has several genre ids; store them as text such as "28|12|878"
    g <- r$genre_ids
    genre_text <- if (is.matrix(g)) apply(g, 1, paste, collapse = "|")
                  else vapply(g, paste, character(1), collapse = "|")

    movies[[length(movies) + 1]] <- data.frame(
      id = r$id, title = r$title, release_date = r$release_date,
      overview = r$overview, genre_ids = genre_text,
      vote_average = r$vote_average, vote_count = r$vote_count,
      popularity = r$popularity, stringsAsFactors = FALSE)
    Sys.sleep(0.05)                                        # be gentle with the API
  }
  cat("Films downloaded for", year, "\n")
}
movies <- do.call(rbind, movies)
if (NROW(movies) == 0) stop("No films were downloaded. Check the API key.")
movies <- movies[!duplicated(movies$id), ]
movies$retrieved <- as.character(Sys.Date())
write.csv(movies, "data/tmdb_movies_raw.csv", row.names = FALSE, fileEncoding = "UTF-8")

# ---- 3. Cast: top-billed actors for every film --------------------------------
cast <- list()
for (i in seq_len(nrow(movies))) {
  res <- get_json(paste0("/movie/", movies$id[i], "/credits"), "language=en-US")
  if (!is.null(res) && NROW(res$cast) > 0) {
    actors <- head(res$cast[order(res$cast$order), ], top_cast)
    cast[[length(cast) + 1]] <- data.frame(
      movie_id = movies$id[i], actor_id = actors$id,
      actor_name = actors$name, billing = actors$order,
      stringsAsFactors = FALSE)
  }
  if (i %% 100 == 0) cat("Cast downloaded for", i, "of", nrow(movies), "films\n")
  Sys.sleep(0.05)
}
cast <- do.call(rbind, cast)
if (NROW(cast) == 0) stop("No cast data was downloaded.")
write.csv(cast, "data/tmdb_cast_raw.csv", row.names = FALSE, fileEncoding = "UTF-8")

# ---- Done -------------------------------------------------------------------
cat("\nFinished.\n",
    nrow(movies), "films,", nrow(cast), "cast rows saved in the data folder.\n")
