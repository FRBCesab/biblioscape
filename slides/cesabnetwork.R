# Made up by Claude

# 1. Letters as polylines of corner points, in a unit box
#fmt: skip
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

# 2. Place each letter horizontally and collect all strokes
letter_gap <- 1.1
strokes <- list()
for (k in seq_along(letter_strokes)) {
  offset <- (k - 1) * letter_gap
  for (P in letter_strokes[[k]]) {
    P[, 1] <- P[, 1] + offset
    strokes[[length(strokes) + 1]] <- P
  }
}

# 3. Merge identical corner points into single nodes
all_pts <- do.call(rbind, strokes)
key <- sprintf("%.4f,%.4f", all_pts[, 1], all_pts[, 2])
is_new <- !duplicated(key)
coords <- all_pts[is_new, , drop = FALSE]
id_of <- match(key, key[is_new]) # node id for every row of all_pts

# 4. Edges join consecutive corners along each stroke
edges <- NULL
row_idx <- 0
for (P in strokes) {
  n <- nrow(P)
  ids <- id_of[row_idx + seq_len(n)]
  row_idx <- row_idx + n
  if (n > 1) edges <- rbind(edges, cbind(ids[-n], ids[-1]))
}

g <- simplify(graph_from_edgelist(edges, directed = FALSE))

# 5. Colour nodes along a gradient from C (#6e508d) to B (#bccf00)
X <- coords[, 1]
Y <- coords[, 2]
pal <- colorRampPalette(c("#6e508d", "#bccf00"))(101)
t <- (X - min(X)) / (max(X) - min(X))
node_cols <- pal[1 + round(100 * t)]

# 6. Plot
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
  xlim = range(X) + c(-0.1, 0.1),
  ylim = range(Y) + c(-0.1, 0.1),
  asp = 1,
  vertex.size = 10,
  vertex.label = NA,
  vertex.color = node_cols,
  vertex.frame.color = NA,
  edge.color = "grey40",
  edge.width = 2
)

dev.off()
