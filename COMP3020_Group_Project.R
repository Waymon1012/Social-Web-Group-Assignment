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


##############################################################################
# HYPOTHESIS TESTING
##############################################################################

# ---- hypothesis ------------------------------------------------------------
tab_rating <- table(movies$genre_group, movies$rating_group)
tab_rating

ct <- chisq.test(tab_rating)
ct
min(ct$expected)                         # should be >= 5 for the usual test

# Monte Carlo p-value as a robustness check (as in the Module 3 lab)
set.seed(3020)
ct_mc <- chisq.test(tab_rating, simulate.p.value = TRUE, B = 10000)
ct_mc$p.value

cv_genre <- cramers_v(tab_rating, ct)    # effect size: 0 = none, 1 = perfect
cv_genre

# ---- hypothesis-posthoc ----------------------------------------------------
genre_summary <- data.frame(
  Genre = rownames(tab_rating),
  Films = as.vector(rowSums(tab_rating)),
  Mean_rating = round(as.vector(tapply(movies$vote_average, movies$genre_group,
                                       mean)[rownames(tab_rating)]), 2),
  Pct_high = round(100 * prop.table(tab_rating, 1)[, "High"], 1),
  Std_residual_high = round(ct$stdres[, "High"], 2))
genre_summary <- genre_summary[order(-genre_summary$Mean_rating), ]
show_table(genre_summary, row.names = FALSE)

# Visual 1: rating distribution by genre (ordered by median)
gf <- factor(movies$genre_group,
             levels = names(sort(tapply(movies$vote_average,
                                        movies$genre_group, median))))
par(mar = c(4, 9, 3, 1))
boxplot(movies$vote_average ~ gf, horizontal = TRUE, las = 1, col = "lightblue",
        main = "TMDb rating by primary genre", xlab = "Average rating (0-10)",
        ylab = "")

# Visual 2: share of "High" films per genre vs the overall share
prop_high <- sort(prop.table(tab_rating, 1)[, "High"])
barplot(prop_high, horiz = TRUE, las = 1, col = "steelblue",
        main = "Share of films rated High, by genre", xlab = "Proportion High")
abline(v = mean(movies$rating_group == "High"), lty = 2, col = "red")
par(mar = c(5, 4, 4, 2) + 0.1)

# ---- hypothesis-sensitivity ------------------------------------------------
movies$rating_band <- cut(movies$vote_average,
                          breaks = quantile(movies$vote_average, c(0, 1/3, 2/3, 1)),
                          labels = c("Low", "Medium", "High"),
                          include.lowest = TRUE)
tab_band <- table(movies$genre_group, movies$rating_band)
ct_band  <- chisq.test(tab_band)
ct_band


##############################################################################
# TEXT/CONTENT ANALYSIS AND VISUALISATION
##############################################################################


# ---- Cleaning and representing the text ----

# ---- text-clean ------------------------------------------------------------
corpus <- Corpus(VectorSource(movies$overview))

to_ascii <- content_transformer(function(x) {
  x <- gsub("[\u2018\u2019]", "'", x)      # curly -> straight apostrophes
  iconv(x, to = "ASCII", sub = " ")        # replace other non-ASCII symbols
})
letters_only <- content_transformer(function(x) gsub("[^a-z]+", " ", x))

corpus <- tm_map(corpus, to_ascii)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, c(stopwords("english"), CUSTOM_STOP))
corpus <- tm_map(corpus, letters_only)
corpus <- tm_map(corpus, stripWhitespace)
cleaned_text <- trimws(as.character(content(corpus)))

# Stems such as "famili" are hard to read, so keep a lookup from each stem to its
# most common original word. It is used only to label tables and plots.
tokens <- unlist(strsplit(cleaned_text, " "))
tokens <- tokens[nchar(tokens) >= 3]
stem_lookup <- tapply(tokens, wordStem(tokens, language = "english"),
                      function(w) names(which.max(table(w))))
readable <- function(stems) {
  out <- as.character(stem_lookup[stems])
  ifelse(is.na(out), stems, out)
}

corpus_stem <- tm_map(corpus, stemDocument)

# ---- text-matrix -----------------------------------------------------------
tdm_all   <- TermDocumentMatrix(corpus_stem,
                                control = list(wordLengths = c(3, Inf)))
doc_freq  <- rowSums(as.matrix(tdm_all) > 0)

tdm       <- tdm_all[which(doc_freq >= MIN_DOC_FREQ), ]
counts    <- as.matrix(tdm)                       # terms x films (raw counts)
keep_docs <- which(colSums(counts > 0) >= MIN_TERMS_PER_DOC)
tdm       <- tdm[, keep_docs]
counts    <- counts[, keep_docs]
films     <- movies[keep_docs, ]                  # final film set used from here on
rm(tdm_all)

# TF-IDF weighting down-weights terms found in many descriptions (Modules 5-6).
tfidf <- weightTfIdf(tdm)
W     <- t(as.matrix(tfidf))                      # films x terms (rows = films)

# Unit-length rows: needed for cosine distance in the clustering section.
norms <- sqrt(rowSums(W^2)); norms[norms == 0] <- 1
Wn    <- W / norms

text_summary <- data.frame(
  Item = c("Films analysed", "Distinct terms before filtering",
           "Terms kept (in >= MIN_DOC_FREQ descriptions)",
           "Films removed at this stage"),
  Value = c(nrow(films), length(doc_freq), ncol(W), nrow(movies) - nrow(films)))
show_table(text_summary)


# ---- Summaries and visualisations ----

# ---- text-summary ----------------------------------------------------------
raw_words <- lengths(strsplit(films$overview, "\\s+"))
par(mfrow = c(1, 2))
hist(raw_words, breaks = 30, col = "lightblue", main = "Words per description",
     xlab = "Number of words")
hist(films$vote_average, breaks = 30, col = "lightgreen", main = "TMDb rating",
     xlab = "Average rating (0-10)")
par(mfrow = c(1, 1))
summary(raw_words)

# ---- text-topterms ---------------------------------------------------------
term_freq <- sort(rowSums(counts), decreasing = TRUE)
top20 <- head(term_freq, 20)
par(mar = c(4, 7, 3, 1))
barplot(rev(top20), names.arg = rev(readable(names(top20))), horiz = TRUE,
        las = 1, col = "steelblue", xlab = "Total count",
        main = "20 most frequent terms in movie descriptions")
par(mar = c(5, 4, 4, 2) + 0.1)

# ---- text-wordcloud --------------------------------------------------------
tfidf_weight <- sort(colSums(W), decreasing = TRUE)
par(mfrow = c(1, 2), mar = c(0, 0, 2, 0))
wordcloud(readable(names(term_freq)), term_freq, min.freq = 0, max.words = 80,
          random.order = FALSE, colors = brewer.pal(8, "Dark2"),
          scale = c(2.5, 0.4))
title("Raw frequency")
wordcloud(readable(names(tfidf_weight)), tfidf_weight, min.freq = 0,
          max.words = 80, random.order = FALSE, colors = brewer.pal(8, "Dark2"),
          scale = c(2.5, 0.4))
title("TF-IDF weighted")
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)


# ---- Vocabulary by genre and by rating group ----

# ---- text-bygroup ----------------------------------------------------------
top_terms_for <- function(rows, n = 8) {
  w <- colMeans(W[rows, , drop = FALSE])
  readable(names(sort(w, decreasing = TRUE))[seq_len(n)])
}

genre_names <- sort(unique(films$genre_group))
genre_terms <- data.frame(
  Genre = genre_names,
  Films = as.vector(table(films$genre_group)[genre_names]),
  Top_terms = sapply(genre_names, function(g)
    paste(top_terms_for(films$genre_group == g), collapse = ", ")))
show_table(genre_terms, row.names = FALSE)

rating_terms <- data.frame(
  Rating_group = c("High", "Low"),
  Films = as.vector(table(films$rating_group)[c("High", "Low")]),
  Top_terms = sapply(c("High", "Low"), function(r)
    paste(top_terms_for(films$rating_group == r, n = 12), collapse = ", ")))
show_table(rating_terms, row.names = FALSE)


# ---- Descriptions in two dimensions ----

# ---- text-mds --------------------------------------------------------------
d_euc <- dist(Wn)                 # Euclidean distance between unit vectors
d_cos <- d_euc^2 / 2              # cosine distance (as in the Module 6 lab)
mds2  <- cmdscale(d_euc, k = 2)   # MDS of these distances keeps the cosine geometry

# Six most common genres; the pooled "Other" group is shown separately in grey
genre_counts <- sort(table(films$genre_group[films$genre_group != "Other"]),
                     decreasing = TRUE)
top_genres <- head(names(genre_counts), 6)
n_top      <- length(top_genres)
genre_pal  <- c(brewer.pal(max(3, n_top), "Dark2")[seq_len(n_top)], "grey80")
genre_idx  <- match(films$genre_group, top_genres)
genre_idx[is.na(genre_idx)] <- n_top + 1
plot(mds2, col = genre_pal[genre_idx], pch = 16, cex = 0.6,
     xlab = "MDS dimension 1", ylab = "MDS dimension 2",
     main = "MDS of movie descriptions (cosine distance, TF-IDF)")
legend("topright", legend = c(top_genres, "Other"), col = genre_pal, pch = 16,
       cex = 0.7, bty = "n")


##############################################################################
# CLUSTERING
##############################################################################


# ---- Design decisions ----


# ---- Choosing the number of clusters ----

# ---- cluster-k -------------------------------------------------------------
ks <- 1:12
km_list <- lapply(ks, function(k) kmeans(Wn, centers = k, nstart = 10,
                                         iter.max = 50))
ssw <- sapply(km_list, function(x) x$tot.withinss)

# Average silhouette width from a distance matrix.
d_mat <- as.matrix(d_cos)
mean_silhouette <- function(cl) {
  k   <- max(cl)
  Z   <- sapply(seq_len(k), function(j) as.numeric(cl == j))  # film x cluster 0/1
  S   <- d_mat %*% Z                       # total distance: film to each cluster
  n_c <- colSums(Z)
  own <- cbind(seq_along(cl), cl)
  a   <- S[own] / pmax(n_c[cl] - 1, 1)     # mean distance to own cluster
  S_other <- sweep(S, 2, n_c, "/")         # mean distance to every cluster
  S_other[own] <- Inf                      # ignore own cluster
  S_other[, n_c == 0] <- Inf
  b   <- apply(S_other, 1, min)            # nearest other cluster
  s   <- (b - a) / pmax(a, b)
  s[n_c[cl] == 1] <- 0
  mean(s)
}
sil <- sapply(km_list[-1], function(x) mean_silhouette(x$cluster))

par(mfrow = c(1, 2))
plot(ks, ssw, type = "b", xlab = "Number of clusters k",
     ylab = "Within-cluster sum of squares", main = "Elbow plot")
abline(v = K_FINAL, lty = 2, col = "red")
plot(ks[-1], sil, type = "b", xlab = "Number of clusters k",
     ylab = "Average silhouette width", main = "Silhouette")
abline(v = K_FINAL, lty = 2, col = "red")
par(mfrow = c(1, 1))
ks[-1][which.max(sil)]                     # k with the highest silhouette

# ---- cluster-final ---------------------------------------------------------
set.seed(3020)
km <- kmeans(Wn, centers = K_FINAL, nstart = 25, iter.max = 50)
films$cluster <- km$cluster
table(films$cluster)
round(km$betweenss / km$totss, 3)   # share of total variation explained by clusters


# ---- Visualising and describing the clusters ----

# ---- cluster-mds -----------------------------------------------------------
cl_cols <- brewer.pal(8, "Dark2")[(films$cluster - 1) %% 8 + 1]
plot(mds2, col = cl_cols, pch = 16, cex = 0.6, xlab = "MDS dimension 1",
     ylab = "MDS dimension 2", main = "k-means clusters on the MDS map")
legend("topright", legend = paste("Cluster", 1:K_FINAL),
       col = brewer.pal(8, "Dark2")[(1:K_FINAL - 1) %% 8 + 1], pch = 16,
       cex = 0.7, bty = "n")

# ---- cluster-profile -------------------------------------------------------
cluster_profile <- do.call(rbind, lapply(1:K_FINAL, function(k) {
  idx <- which(films$cluster == k)
  g   <- sort(table(films$genre_group[idx]), decreasing = TRUE)
  dist_to_centre <- sqrt(rowSums(sweep(Wn[idx, , drop = FALSE], 2,
                                       km$centers[k, ])^2))
  data.frame(Cluster = k, Films = length(idx),
             Mean_rating = round(mean(films$vote_average[idx]), 2),
             Top_genre = paste0(names(g)[1], " (",
                                round(100 * g[1] / length(idx)), "%)"),
             Top_terms = paste(top_terms_for(idx, 8), collapse = ", "),
             Typical_films = gsub("|", "/", fixed = TRUE,
                                  paste(head(films$title[idx][order(dist_to_centre)],
                                             3), collapse = "; ")))
}))
show_table(cluster_profile, row.names = FALSE)

# ---- cluster-clouds --------------------------------------------------------
par(mfrow = c(ceiling(K_FINAL / 3), 3), mar = c(0, 0, 2, 0))
for (k in 1:K_FINAL) {
  w <- colMeans(W[films$cluster == k, , drop = FALSE])
  w <- sort(w, decreasing = TRUE)[1:40]
  wordcloud(readable(names(w)), w, min.freq = 0, max.words = 40,
            random.order = FALSE, colors = brewer.pal(8, "Dark2"),
            scale = c(2, 0.4))
  title(paste("Cluster", k, "(n =", sum(films$cluster == k), ")"))
}
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)


# ---- Do the clusters agree with other evidence? ----

# ---- cluster-hclust --------------------------------------------------------
hc    <- hclust(d_euc, method = "ward.D2")
hc_cl <- cutree(hc, K_FINAL)
cross <- table(kmeans = films$cluster, ward = hc_cl)
cross
sum(apply(cross, 1, max)) / sum(cross)     # agreement ("purity")

plot(hc, labels = FALSE, hang = -1, main = "Ward dendrogram of movie descriptions",
     xlab = "", sub = "")
rect.hclust(hc, k = K_FINAL, border = brewer.pal(8, "Dark2")[1:K_FINAL])

# ---- cluster-genre ---------------------------------------------------------
genre_top <- factor(ifelse(films$genre_group %in% top_genres, films$genre_group,
                           "Other"), levels = c(top_genres, "Other"))
tab_cg <- table(genre_top, films$cluster)
barplot(prop.table(tab_cg, 2), col = genre_pal, legend.text = TRUE,
        xlab = "Cluster", ylab = "Share of films in cluster",
        main = "Genre composition of each cluster",
        xlim = c(0, ncol(tab_cg) * 1.2 + 3),
        args.legend = list(x = ncol(tab_cg) * 1.2 + 3.2, y = 1, cex = 0.7,
                           bty = "n"))

tab_cg_all <- table(films$genre_group, films$cluster)
set.seed(3020)
ct_cg <- chisq.test(tab_cg_all, simulate.p.value = TRUE, B = 5000)
ct_cg
cramers_v(tab_cg_all, ct_cg)

# ---- cluster-rating --------------------------------------------------------
boxplot(films$vote_average ~ films$cluster, col = brewer.pal(8, "Dark2")[1:K_FINAL],
        xlab = "Cluster", ylab = "Average rating (0-10)",
        main = "TMDb rating by thematic cluster")
tab_cr <- table(cluster = films$cluster, rating = films$rating_group)
tab_cr
ct_cr <- chisq.test(tab_cr)
ct_cr
round(ct_cr$stdres, 2)
cramers_v(tab_cr, ct_cr)


##############################################################################
# NETWORK ANALYSIS
##############################################################################


# ---- Network definition ----

# ---- network-build ---------------------------------------------------------
to_label <- function(x) {                       # plain-ASCII names for plots/tables
  y <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  ifelse(is.na(y) | y == "", x, y)
}

cast <- cast_raw[cast_raw$movie_id %in% films$id, ]
cast <- cast[!duplicated(cast[, c("movie_id", "actor_id")]), ]

# All pairs of co-billed actors within each film
pairs <- do.call(rbind, lapply(split(cast$actor_id, cast$movie_id), function(a) {
  if (length(a) < 2) return(NULL)
  t(combn(sort(a), 2))
}))
edges <- data.frame(from = as.character(as.integer(pairs[, 1])),
                    to   = as.character(as.integer(pairs[, 2])), weight = 1)
edge_df <- aggregate(weight ~ from + to, data = edges, FUN = sum)

# Actor attributes
cast_films <- merge(cast, films[, c("id", "vote_average", "genre_group")],
                    by.x = "movie_id", by.y = "id")
n_films     <- tapply(cast_films$movie_id, cast_films$actor_id, length)
mean_rating <- tapply(cast_films$vote_average, cast_films$actor_id, mean)
main_genre  <- tapply(cast_films$genre_group, cast_films$actor_id,
                      function(x) names(which.max(table(x))))
actor_stats <- data.frame(actor_id = as.character(as.integer(names(n_films))),
                          n_films = as.integer(n_films),
                          mean_rating = as.numeric(mean_rating[names(n_films)]),
                          main_genre = as.character(main_genre[names(n_films)]),
                          stringsAsFactors = FALSE)
actors <- cast[!duplicated(cast$actor_id), c("actor_id", "actor_name")]
actors$actor_id   <- as.character(as.integer(actors$actor_id))
actors$actor_name <- to_label(actors$actor_name)
vertices <- merge(actors, actor_stats, by = "actor_id")
vertices <- vertices[vertices$actor_id %in% c(edge_df$from, edge_df$to), ]

g <- graph_from_data_frame(edge_df, directed = FALSE, vertices = vertices)

# Colour nodes by main genre (same palette as before; other genres in grey)
node_genre <- ifelse(V(g)$main_genre %in% top_genres, V(g)$main_genre, "Other")
V(g)$color <- genre_pal[match(node_genre, c(top_genres, "Other"))]


# ---- Network properties ----

# ---- network-props ---------------------------------------------------------
comp    <- components(g)
g_gc    <- induced_subgraph(g, which(comp$membership == which.max(comp$csize)))
g_gc_u  <- delete_edge_attr(g_gc, "weight")   # unweighted copy (path measures)

net_props <- data.frame(
  Property = c("Nodes (actors)", "Edges (co-appearing pairs)", "Density",
               "Connected components", "Nodes in largest component",
               "Share of nodes in largest component", "Mean degree",
               "Maximum degree", "Diameter (largest component)",
               "Mean path length (largest component)",
               "Edges with weight >= 2 (repeat collaborations)"),
  Value = c(vcount(g), ecount(g), round(edge_density(g), 5), comp$no,
            max(comp$csize), round(max(comp$csize) / vcount(g), 3),
            round(mean(degree(g)), 2), max(degree(g)),
            diameter(g_gc_u), round(mean_distance(g_gc_u), 2),
            sum(E(g)$weight >= 2)))
show_table(net_props)

# ---- network-degree --------------------------------------------------------
g_er <- sample_gnm(vcount(g), ecount(g))
par(mfrow = c(1, 2))
hist(degree(g), breaks = 30, col = "lightblue", main = "Actor network",
     xlab = "Degree")
hist(degree(g_er), breaks = 30, col = "grey80", main = "Random graph, same n and m",
     xlab = "Degree", xlim = range(degree(g)))
par(mfrow = c(1, 1))
summary(degree(g))


# ---- Visualising the network ----

# ---- network-full ----------------------------------------------------------
set.seed(3020)
lay_full <- layout_with_drl(g)
par(mar = c(0, 0, 2, 0))
plot(g, layout = lay_full, vertex.size = 1.5, vertex.label = NA,
     vertex.frame.color = NA, edge.width = 0.3, edge.color = "grey85",
     main = "Actor co-appearance network (all actors)")
legend("bottomleft", legend = c(top_genres, "Other"), col = genre_pal, pch = 16,
       cex = 0.7, bty = "n")
par(mar = c(5, 4, 4, 2) + 0.1)

# ---- network-sub -----------------------------------------------------------
# Normally keep pairs sharing 2+ films; with a very small sample fall back to all
min_w <- if (sum(E(g)$weight >= 2) >= 10) 2 else 1
g_rep <- delete_edges(g, E(g)[E(g)$weight < min_w])
g_rep <- delete_vertices(g_rep, V(g_rep)[degree(g_rep) == 0])
comp_r <- components(g_rep)
g_sub  <- induced_subgraph(g_rep, which(comp_r$membership ==
                                          which.max(comp_r$csize)))

set.seed(3020)
lay_sub <- layout_with_fr(g_sub)
deg_sub <- degree(g_sub)
lab_sub <- ifelse(rank(-deg_sub, ties.method = "first") <= 25,
                  V(g_sub)$actor_name, "")
par(mar = c(0, 0, 2, 0))
plot(g_sub, layout = lay_sub, vertex.size = 2 + 1.2 * sqrt(deg_sub),
     vertex.label = lab_sub, vertex.label.cex = 0.6, vertex.label.color = "black",
     vertex.frame.color = "white", edge.width = 0.4 * E(g_sub)$weight,
     edge.color = "grey70",
     main = "Repeat collaborations (pairs sharing 2+ films), largest component")
legend("bottomleft", legend = c(top_genres, "Other"), col = genre_pal, pch = 16,
       cex = 0.7, bty = "n")
par(mar = c(5, 4, 4, 2) + 0.1)
c(nodes = vcount(g_sub), edges = ecount(g_sub))


# ---- Centrality ----

# ---- network-centrality ----------------------------------------------------
cent <- data.frame(
  actor       = V(g_gc)$actor_name,
  n_films     = V(g_gc)$n_films,
  mean_rating = round(V(g_gc)$mean_rating, 2),
  main_genre  = V(g_gc)$main_genre,
  degree      = degree(g_gc),
  strength    = strength(g_gc, weights = E(g_gc)$weight),
  closeness   = closeness(g_gc_u, normalized = TRUE),
  betweenness = betweenness(g_gc_u),
  stringsAsFactors = FALSE)
rownames(cent) <- NULL

top_table <- function(measure, n = 10) {
  o <- order(cent[[measure]], decreasing = TRUE)[seq_len(n)]
  data.frame(Actor = cent$actor[o], Score = round(cent[[measure]][o], 4),
             Films = cent$n_films[o], Mean_film_rating = cent$mean_rating[o],
             Main_genre = cent$main_genre[o])
}

# ---- network-top-degree ----------------------------------------------------
show_table(top_table("degree"))

# ---- network-top-closeness -------------------------------------------------
show_table(top_table("closeness"))

# ---- network-top-betweenness -----------------------------------------------
show_table(top_table("betweenness"))

# ---- network-overlap -------------------------------------------------------
top20_actors <- function(m) cent$actor[order(cent[[m]], decreasing = TRUE)[1:20]]
shared <- function(a, b) length(intersect(top20_actors(a), top20_actors(b)))
c(degree_and_betweenness    = shared("degree", "betweenness"),
  degree_and_closeness      = shared("degree", "closeness"),
  closeness_and_betweenness = shared("closeness", "betweenness"))


# ---- Connecting the network back to genre and rating ----

# ---- network-link ----------------------------------------------------------
top_n       <- ceiling(0.10 * nrow(cent))
central_idx <- order(cent$betweenness, decreasing = TRUE)[seq_len(top_n)]

central_cmp <- data.frame(
  Group = c("Top 10% by betweenness", "All other actors"),
  Actors = c(top_n, nrow(cent) - top_n),
  Mean_films = round(c(mean(cent$n_films[central_idx]),
                       mean(cent$n_films[-central_idx])), 2),
  Mean_film_rating = round(c(mean(cent$mean_rating[central_idx]),
                             mean(cent$mean_rating[-central_idx])), 2))
show_table(central_cmp)

gen_all <- prop.table(table(cent$main_genre))
gen_cen <- prop.table(table(factor(cent$main_genre[central_idx],
                                   levels = names(gen_all))))
show_table(data.frame(Genre = names(gen_all),
                      All_actors_pct = round(100 * as.vector(gen_all), 1),
                      Top10pct_btw_pct = round(100 * as.vector(gen_cen), 1)))


# ---- How data collection and construction choices affect these results ----


##############################################################################
# OVERALL FINDINGS
##############################################################################

# ---- overview-table --------------------------------------------------------
key_results <- data.frame(
  Question = c("Data", "RQ1: rating vs genre", "RQ2: text",
               "RQ3: clusters vs genre", "RQ3: clusters vs rating",
               "RQ4: network"),
  Result = c(
    paste(nrow(films), "films,", length(unique(cast$actor_id)), "actors,",
          min(films$year), "-", max(films$year)),
    paste0("chi-sq = ", round(ct$statistic, 1), ", df = ", ct$parameter,
           ", p = ", signif(ct$p.value, 3), ", V = ", round(cv_genre, 2)),
    paste(ncol(W), "terms; top terms:",
          paste(readable(names(top20))[1:5], collapse = ", ")),
    paste0("k = ", K_FINAL, ", p = ", signif(ct_cg$p.value, 3), ", V = ",
           round(cramers_v(tab_cg_all, ct_cg), 2)),
    paste0("chi-sq = ", round(ct_cr$statistic, 1), ", df = ", ct_cr$parameter,
           ", p = ", signif(ct_cr$p.value, 3), ", V = ",
           round(cramers_v(tab_cr, ct_cr), 2)),
    paste(vcount(g), "nodes,", ecount(g), "edges,",
          round(100 * max(comp$csize) / vcount(g)), "% in largest component")))
show_table(key_results, col.names = c("", "Key result"))