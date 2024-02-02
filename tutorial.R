library(ergm)

library(foreign)

# Reading the data
dat <- foreign::read.dta("data/03-sns.dta")

# Taking a look at the data's first 5 columns and 5 rows
dat[1:5, 1:10]

library(tidyverse)
(dat <- dat |>
  mutate(id = school*10000 + photoid)) |>
  head() |>
  select(school, photoid, id)

# Maybe too much piping... but its cool!
net <- dat %>%
  select(id, school, starts_with("sch_friend")) %>%
  pivot_longer(starts_with('sch_'), names_to = "varname", values_to = "content") %>%
  filter(!is.na(content)) %>%
  mutate(
    friendid = school*10000 + content,
    year     = as.integer(str_extract(varname, "(?<=[a-z])[0-9]")),
    nnom     = as.integer(str_extract(varname, "(?<=[a-z][0-9])[0-9]+"))
  )
vertex_attrs <- dat %>%
  select(id, school, hispanic, female1, starts_with("eversmk"))

library(igraph)

ig_year1 <- net %>%
  filter(year == "1") %>%
  select(id, friendid, nnom) %>%
  graph_from_data_frame(
    vertices = vertex_attrs
  )

ig_year1 <- net %>%
  filter(year == "1") %>%

  # Extra line, all nominations must be in ego too.
  filter(friendid %in% id) %>%

  select(id, friendid, nnom) %>%
  graph_from_data_frame(
    vertices = vertex_attrs
  )

ig_year1

list.vertex.attributes(ig_year1)
list.edge.attributes(ig_year1)

# Which ids are from school 111?
school111ids <- which(V(ig_year1)$school == 111)

# Creating a subgraph
ig_year1_111 <- induced_subgraph(
  graph = ig_year1,
  vids  = school111ids
)
# Computing centrality measures for each vertex
V(ig_year1_111)$indegree   <- degree(ig_year1_111, mode = "in")
V(ig_year1_111)$outdegree  <- degree(ig_year1_111, mode = "out")
V(ig_year1_111)$closeness  <- closeness(ig_year1_111, mode = "total")
V(ig_year1_111)$betweeness <- betweenness(ig_year1_111, normalized = TRUE)

# Extracting each vectex features as a data.frame
stats <- as_data_frame(ig_year1_111, what = "vertices")

# Computing quantiles for each variable
stats_degree <- with(stats, {
  cbind(
    indegree   = quantile(indegree, c(.025, .5, .975), na.rm = TRUE),
    outdegree  = quantile(outdegree, c(.025, .5, .975), na.rm = TRUE),
    closeness  = quantile(closeness, c(.025, .5, .975), na.rm = TRUE),
    betweeness = quantile(betweeness, c(.025, .5, .975), na.rm = TRUE)
  )
})

stats_degree
cbind(
  size    = vcount(ig_year1_111),
  nedges  = ecount(ig_year1_111),
  density = edge_density(ig_year1_111),
  recip   = reciprocity(ig_year1_111),
  centr   = centr_betw(ig_year1_111)$centralization,
  pathLen = mean_distance(ig_year1_111)
)
triadic <- triad_census(ig_year1_111)
triadic
knitr::kable(cbind(
  Pcent = triadic/sum(triadic)*100,
  read.csv("triadic_census.csv")
), digits = 2)
