# Made up by Claude

library(igraph)
set.seed(42)

# 1. Letters as polylines of corner points, in a unit box
# fmt: skip
letter_strokes <- list(
  C = list(
    matrix(c(0.8,0.8,  0.5,1,  0.2,0.8,  0,0.5,  0.2,0.2,  0.5,0,  0.8,0.2),
           ncol = 2, byrow = TRUE)
  ),
  E = list(
    matrix(c(0.8,1,  0,1,  0,0,  0.8,0), ncol = 2, byrow = TRUE),  # outline
    matrix(c(0,0.5,  0.6,0.5),           ncol = 2, byrow = TRUE)   # middle bar
  ),
  S = list(
    matrix(c(0.8,0.85,  0.5,1,  0.15,0.9,  0,0.7,  0.2,0.5,
             0.6,0.5,  0.8,0.3,  0.6,0,  0.25,0,  0,0.15),
           ncol = 2, byrow = TRUE)
  ),
  A = list(
    matrix(c(0,0,  0.4,1,  0.8,0), ncol = 2, byrow = TRUE),        # two legs
    matrix(c(0.15,0.4,  0.65,0.4), ncol = 2, byrow = TRUE)         # crossbar
  ),
  B = list(
    matrix(c(0,0,  0,1,  0.6,1,  0.8,0.75,  0.6,0.5,  0,0.5),
           ncol = 2, byrow = TRUE),                                 # upper bowl
    matrix(c(0.6,0.5,  0.8,0.25,  0.6,0,  0,0),
           ncol = 2, byrow = TRUE)                                  # lower bowl
  )
)

# 2. Sample evenly spaced points along a polyline
d_spa <- 0.07
sample_polyline <- function(P, spacing = d_spa) {
  out <- list()
  for (i in 1:(nrow(P) - 1)) {
    a <- P[i, ]
    b <- P[i + 1, ]
    n <- max(1, ceiling(sqrt(sum((b - a)^2)) / spacing))
    t <- seq(0, 1, length.out = n + 1)[-(n + 1)] # drop end point to avoid duplicates
    out[[i]] <- cbind(a[1] + t * (b[1] - a[1]), a[2] + t * (b[2] - a[2]))
  }
  rbind(do.call(rbind, out), P[nrow(P), ])
}

# 3. Place each letter horizontally and sample its strokes
letter_gap <- 1.1
strokes <- list()
for (k in seq_along(letter_strokes)) {
  offset <- (k - 1) * letter_gap
  for (P in letter_strokes[[k]]) {
    P[, 1] <- P[, 1] + offset
    strokes[[length(strokes) + 1]] <- sample_polyline(P, spacing = d_spa)
  }
}

# 4. Merge identical points into core nodes
all_pts <- do.call(rbind, strokes)
key <- sprintf("%.4f,%.4f", all_pts[, 1], all_pts[, 2])
is_new <- !duplicated(key)
core <- all_pts[is_new, , drop = FALSE]
id_of <- match(key, key[is_new])

# 5. Stroke edges join consecutive points along each stroke
stroke_edges <- NULL
row_idx <- 0
for (P in strokes) {
  n <- nrow(P)
  ids <- id_of[row_idx + seq_len(n)]
  row_idx <- row_idx + n
  if (n > 1) stroke_edges <- rbind(stroke_edges, cbind(ids[-n], ids[-1]))
}

# 6. Scattered satellite nodes around the letters
n_extra <- 0
extra <- core[sample(nrow(core), n_extra, replace = TRUE), , drop = FALSE] +
  matrix(rnorm(n_extra * 2, sd = 0.05), ncol = 2)
coords <- rbind(core, extra)

# 7. Jitter all nodes so strokes look hand-drawn rather than rigid
sd_jit <- 0.05
coords <- coords + matrix(rnorm(nrow(coords) * 2, sd = sd_jit), ncol = 2)

# 8. Fuzzy edges: connect each node to its 3 nearest neighbours if close enough
D <- as.matrix(dist(coords))
diag(D) <- Inf
K_nei <- 15
max_d <- 0.30
knn_edges <- do.call(
  rbind,
  lapply(seq_len(nrow(D)), function(i) {
    nb <- order(D[i, ])[1:K_nei]
    nb <- nb[D[i, nb] < max_d]
    if (length(nb)) cbind(i, nb) else NULL
  })
)

# 9. Build graph (duplicates and self-loops removed)
g <- simplify(graph_from_edgelist(
  rbind(stroke_edges, knn_edges),
  directed = FALSE
))

# 10. Colours: gradient from C (#6e508d) to B (#bccf00) by horizontal position
X <- coords[, 1]
Y <- coords[, 2]
pal <- colorRampPalette(c("#6e508d", "#bccf00"))(101)
t <- (X - min(X)) / (max(X) - min(X))
node_cols <- pal[1 + round(100 * t)]

# 11. Node sizes: letter nodes bigger, satellites smaller
node_size <- c(rep(3.5, nrow(core)), rep(2.5, nrow(coords) - nrow(core)))

# 12. Plot

png(
  "slides/img/CESAB_network_2.png",
  width = 2400,
  height = 1000,
  res = 200,
  bg = "transparent"
)

par(mar = c(0, 0, 0, 0))
plot(
  g,
  layout = cbind(X, Y),
  rescale = FALSE,
  xlim = range(X) + c(-0.15, 0.15),
  ylim = range(Y) + c(-0.15, 0.15),
  asp = 1,
  vertex.size = 7,
  vertex.label = NA,
  vertex.color = node_cols,
  vertex.frame.color = NA,
  edge.color = adjustcolor("grey30", alpha.f = 0.4),
  edge.width = 1,
)

dev.off()
