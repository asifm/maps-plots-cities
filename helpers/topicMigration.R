library(tidyverse)
library(cowplot)
library(data.world)
library(igraph)



sql_migrationflow <- qry_sql("SELECT * FROM msa_msa_migrationflow_acs12_16")

migflow <- query(sql_migrationflow, "asifmeh/citee-geodata")

migflow <- migflow %>% mutate_at(c(1, 2), as.character)

migflow <- migflow %>%
  mutate(
    flow_perc_sourcepop = flow_prev_to_current / prevmsa_pop
  )

migflow %>% 
  top_n(20, flow_perc_sourcepop)

msa_5mplus <- filter(migflow, currentmsa_pop > 5000000) %>% 
  select(currentmsa_code13) %>% 
  distinct()

migflow_bigmsas <- migflow %>%
  filter((currentmsa_code13 %in% msa_5mplus[[1]]) &
           (prevmsa_code13 %in% msa_5mplus[[1]]))



q1 <- ggplot(
  migflow_bigmsas,
  aes(y = currentmsa_code13, x = prevmsa_code13, fill = flow_prev_to_current)
) +
  geom_tile()


q2 <- ggplot(
  migflow_bigmsas,
  aes(y = currentmsa_code13,
      x = prevmsa_code13,
      fill = flow_perc_sourcepop)
) +
  geom_tile()

plot_grid(q1, q2)

# =============================================
# Adjacency Matrix
# =============================================

G <- graph.data.frame(migflow, directed = TRUE)
# Columns represent source and rows represent target of flow
A <- as_adjacency_matrix(G, names=TRUE, sparse=FALSE,attr = 'flow_prev_to_current')
A_moe <- as_adjacency_matrix(G, names=TRUE, sparse=FALSE,attr = 'moe')

df_A <- as.data.frame(A)
df_A_moe <- as.data.frame(A_moe)


# charlottesville = 16820
# new york = 35620

# People moving out of Charlottesville: 13771
sum(df_A[, "16820"])

# People moving into Charlottesville: 14892
sum(df_A["16820", ])


df_A["16820", ] %>% 
  top_n(10)


df_A["16820", "35620"]
df_A["35620", "16820"]

