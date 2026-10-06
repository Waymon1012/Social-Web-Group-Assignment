DATA_DIR <- "data"    # folder holding the three CSV files

# ---- PACKAGES: install any that are missing ---------------------------------
needed  <- c("tm", "SnowballC", "wordcloud", "igraph")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) install.packages(missing)

# ---- setup -----------------------------------------------------------------
library(tm)          # corpus, stop words, term-document matrices (Modules 4-6)
library(SnowballC)   # stemming (Module 4)
library(wordcloud)   # word clouds (Modules 5-6)
library(igraph)      # network construction and analysis (Modules 7-8)

set.seed(3020)

# Prints a table to the console (stands in for knitr::kable in the report)
show_table <- function(x, col.names = NULL, row.names = TRUE, ...) {
  if (!is.null(col.names)) names(x) <- col.names
  if (is.data.frame(x) && isFALSE(row.names)) print(x, row.names = FALSE)
  else print(x)
  invisible(x)
}


##############################################################################
# OVERVIEW AND RESEARCH QUESTIONS
##############################################################################

# ---- params ----------------------------------------------------------------
# ---- Cleaning and text ----
MIN_WORDS         <- 10       # shortest description kept (words)
MIN_GENRE_N       <- 40       # genres with fewer films are pooled as "Other"
MIN_DOC_FREQ      <- 10       # a term must occur in at least this many descriptions
MIN_TERMS_PER_DOC <- 3        # retained terms needed for a description to stay
CUSTOM_STOP       <- c("film", "movie", "story", "will", "can", "one", "two",
                       "must", "also", "get", "gets")

# ---- Clustering ----
K_FINAL <- 6                  # chosen after inspecting the elbow/silhouette plots

# ---- Small helpers used throughout ----
cramers_v <- function(tab, ct) {
  sqrt(unname(ct$statistic) / (sum(tab) * (min(dim(tab)) - 1)))
}


##############################################################################
# DATA COLLECTION
##############################################################################


# ---- How the data were obtained ----

# ---- load the saved data ----------------------------------------------------
# No download happens here: the files made by get_tmdb_data.R are read from the
# data folder.
genres_file <- file.path(DATA_DIR, "tmdb_genres.csv")
movies_file <- file.path(DATA_DIR, "tmdb_movies_raw.csv")
cast_file   <- file.path(DATA_DIR, "tmdb_cast_raw.csv")

wanted  <- c(genres_file, movies_file, cast_file)
absent  <- wanted[!file.exists(wanted)]
if (length(absent) > 0) {
  stop("Cannot find: ", paste(absent, collapse = ", "), "\n",
       "Put the 'data' folder (made by get_tmdb_data.R) inside your working ",
       "directory, which is currently:\n", getwd())
}

genre_list <- read.csv(genres_file, as.is = TRUE, fileEncoding = "UTF-8")
movies_raw <- read.csv(movies_file, as.is = TRUE, fileEncoding = "UTF-8")
cast_raw   <- read.csv(cast_file,   as.is = TRUE, fileEncoding = "UTF-8")


# ---- Variables and data volume ----

# ---- volume ----------------------------------------------------------------
yrs <- substr(movies_raw$release_date, 1, 4)
yrs <- yrs[!is.na(yrs) & yrs != ""]
volume <- data.frame(
  Item  = c("Films retrieved", "Cast rows retrieved", "Distinct actors",
            "Release years covered", "Retrieval date"),
  Value = c(nrow(movies_raw), nrow(cast_raw), length(unique(cast_raw$actor_id)),
            paste(min(yrs), "-", max(yrs)),
            if ("retrieved" %in% names(movies_raw)) max(movies_raw$retrieved)
            else "see data file timestamp"))
show_table(volume, col.names = c("Item", "Value"))


# ---- Limitations of the data source and collection process ----


# ---- Data cleaning of the structured fields ----

# ---- cleaning --------------------------------------------------------------
m <- movies_raw
m$overview[is.na(m$overview)] <- ""
m$overview <- trimws(m$overview)

n_words        <- lengths(strsplit(m$overview, "\\s+"))     # "" gives 0 words
ascii_share    <- nchar(iconv(m$overview, to = "ASCII", sub = "")) /
  pmax(nchar(m$overview), 1)
primary_id     <- suppressWarnings(as.integer(sub("\\|.*", "",
                                                  as.character(m$genre_ids))))
genre_map      <- setNames(genre_list$name, genre_list$id)  # id -> genre name
primary_genre0 <- unname(genre_map[as.character(primary_id)])

problems <- data.frame(
  `Missing or very short description` = n_words < MIN_WORDS,
  `Description mostly non-English`    = ascii_share < 0.9,
  `No usable rating (missing or 0)`   = is.na(m$vote_average) | m$vote_average == 0,
  `Missing release date`              = is.na(m$release_date) |
    m$release_date == "",
  `No genre listed`                   = is.na(primary_genre0),
  `Duplicate film or description`     = duplicated(m$id) | duplicated(m$overview),
  check.names = FALSE)

cleaning_log <- data.frame(Check = names(problems),
                           Films_flagged = colSums(problems), row.names = NULL)
show_table(cleaning_log)

keep   <- rowSums(problems) == 0
movies <- m[keep, ]
movies$primary_genre <- primary_genre0[keep]
movies$year          <- as.integer(substr(movies$release_date, 1, 4))

# Pool rare genres so that the chi-squared test has adequate expected counts.
n_by_genre <- as.vector(table(movies$primary_genre)[movies$primary_genre])
movies$genre_group <- ifelse(n_by_genre >= MIN_GENRE_N,
                             movies$primary_genre, "Other")

# Rating group: High = at or above the median rating of the cleaned sample.
movies$rating_group <- factor(ifelse(movies$vote_average >=
                                       median(movies$vote_average),
                                     "High", "Low"), levels = c("Low", "High"))

# ---- clean-summary ---------------------------------------------------------
show_table(as.data.frame(table(Genre = movies$genre_group)),
           col.names = c("Genre group (primary genre)", "Films"))
summary(movies[, c("vote_average", "vote_count", "popularity", "year")])