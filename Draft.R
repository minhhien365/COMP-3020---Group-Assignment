options(timeout = 300)
install.packages("vroom")


people <- read_csv("data/people.csv")
glimpse(people)
count(people, gender)
count(people, era)

install.packages(c("tidyverse", "tidytext"))   
library(tidyverse)   # data wrangling and ggplot2 charts
library(tidytext)    # splitting text into words, stop words, tf-idf
library(cluster)     # silhouette widths (comes with R)

dir.create("figures", showWarnings = FALSE) 
install.packages(c("rmarkdown", "bslib", "data.table", "systemfonts", "textshaping"))
#1. Load the data 
#The dataset was collected from Wikidata and Wikipedia 
#Columns are renamed so the code is easier to read 
#Only people identified as female or male are kept 

people <- read_csv("data/people.csv") |>
  rename(wikidata_id = qid, name = title, intro = intro_text) |>
  filter(gender %in% c("female", "male")) |>
  mutate(era    = factor(era, levels = c("pre-1950", "1950-69", "1970-89", "1990+")),
         gender = factor(gender, levels = c("female", "male")))

glimpse (people)
nrow(people)

#Group sizes: number of women and men in each birth ere 
people |>
  count(era, gender) |>
  pivot_wider(names_from = gender, values_from = n, values_fill = 0)

sum(people$birth_year == 2000, na.rm = TRUE)
#affect the 1990+ era


#2.CLEAN THE TEXT 
#a: remove uniusable text (people with no intro)
n_missing <- sum(is.na(people$intro) | str_trim(people$intro) == "")
n_missing                   
# create new datafram containing only the rows of people where the intro has text 
people_text <- people |>
  filter(!is.na(intro), str_trim(intro) != "")
#b: build a lookup table of the words that make up each person's name 
# Step 5 helper: the words of each person's own name
name_tokens <- people_text |>
  select(wikidata_id, name) |>
  unnest_tokens(word, name) |>
  distinct()
# create a custom stop-word list in R 
custom_stop <- tibble(word = c("born", "also", "known"))
#c: tokenise & clean 
tokens <- people_text |>
  select(wikidata_id, gender, era, intro) |>
  unnest_tokens(word, intro) |>                          #lower-case, no punctuation
  filter(!str_detect(word, "[0-9]"), str_length(word) > 2) |>
  anti_join(stop_words, by = "word") |>
  anti_join(custom_stop, by = "word") |>
  anti_join(name_tokens, by = c("wikidata_id", "word"))
head(tokens)


#3: Represent the text 
#Each person's intro is one document, represented as a bag of words: how often each word appears, ignoring word order 
#Counts converted to tf-idf weights:
# tf (term frequency) = how often the word appears in this intro 
# idf: down-weights words found in many intros and up-weights words that distinguish one intro from others 

word_counts <- tokens |> count(wikidata_id, word)

tfidf_long <- word_counts |> bind_tf_idf(word, wikidata_id, n)

keep_words <- word_counts |> count(word) |> filter(n >= 5) |> pull(word)

tfidf_mat <- tfidf_long |>
  filter(word %in% keep_words) |>
  cast_sparse(wikidata_id, word, tf_idf) |>
  as.matrix()

dim(tfidf_mat)     # rows = people, columns = words kept

#SUMMARY 
tibble(
  people_with_text = n_distinct(tokens$wikidata_id),
  total_words      = nrow(tokens),
  distinct_words   = n_distinct(tokens$word),
  words_kept_tfidf = length(keep_words)
)

# Length of cleaned intros, women vs men
tokens |>
  count(wikidata_id, gender) |>
  group_by(gender) |>
  summarise(people = n(),
            median_words = median(n),
            mean_words = round(mean(n), 1),
            .groups = "drop")

#===============================================
#4: RQ3: HOW ARE WOMEN AND MEN DESCRIBED?
#RQ3: Is the way Wikipedia describes women in AI different from the way it describes men?

#Plot 1: most frequent words overall 
plot_freq <- tokens |>
  count(word, sort = TRUE) |>
  slice_head(n = 20) |>
  ggplot(aes(n, reorder(word, n))) +
  geom_col() +
  labs(title = "Most frequent words in AI biographies",
       x = "Number of occurrences", y = NULL)
plot_freq
ggsave("figures/freq_words_overall.png", plot_freq, width = 6, height = 5, dpi = 300)

#Plot 2: top words for women vs men
#Far more men than women in AI, the chart uses each word's share of all words within a gender, not raw counts. 
plot_gender <- tokens |>
  count(gender, word) |>
  group_by(gender) |>
  mutate(share = n / sum(n)) |>
  slice_max(share, n = 12, with_ties = FALSE) |>
  ungroup() |>
  ggplot(aes(share, reorder_within(word, share, gender), fill = gender)) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  facet_wrap(~gender, scales = "free_y") +
  labs(title = "Most frequent words: women vs men",
       x = "Share of all words", y = NULL)
plot_gender
ggsave("figures/freq_words_gender.png", plot_gender, width = 8, height = 5, dpi = 300)

#Plot 3: distinctive words by era and gender 
# Each era x gender group is treated as one document, tf-idf is calculated across the groups. Highest tf-idf words for a group are the words that most distinguish it from the others 
plot_era <- tokens |>
  filter(!is.na(era)) |>
  mutate(group = paste(era, gender, sep = " | ")) |>
  count(group, word) |>
  bind_tf_idf(word, group, n) |>
  group_by(group) |>
  slice_max(tf_idf, n = 6, with_ties = FALSE) |>
  ungroup() |>
  ggplot(aes(tf_idf, reorder_within(word, tf_idf, group))) +
  geom_col() +
  scale_y_reordered() +
  facet_wrap(~group, scales = "free_y", ncol = 2) +
  labs(title = "Most distinctive words by era and gender (tf-idf)",
       x = "tf-idf", y = NULL)
plot_era
ggsave("figures/distinctive_words_era_gender.png", plot_era,
       width = 8, height = 10, dpi = 300)

#RQ4: Clustering 
# Which AI subfields do women and men appear in, how does the distribution differ? 
# Biographies are groyped by the similarity of their vocabulary, clusters are named from their top words, women's share of each cluster is then examined 

#5.1. Data representation
#each person is a row if tf-idf weights over the kept words. Intros differ in length, longer intro have larger values and look different just because of length. To avoid this, each row is scaled to length 1
#5.2. Distance measure
# Cosine (write in report)
row_norm <- sqrt(rowSums(tfidf_mat^2))
m_norm  <- tfidf_mat[row_norm > 0, , drop = FALSE] / row_norm[row_norm > 0]
dim(m_norm)
#5.3. Choosing the number of clusters (k)
# Two standard checks, for k from 2 to 10:
# Elbow plot: total within-cluster variation always falls as k grows. The "elbow" is where adding more clusters stop helping much 
# Average silhouette width: how well each person fits their own cluster compared with the nearest other cluster. Closer to 1 is better. 
# Neither rule is decisive on its own, so the final choice also considers whether the clusters are interpretable.

set.seed(1)  #make the result repoducible 
ks <- 2:10
fits <- map(ks, \(k) kmeans(m_norm, centers = k, nstart = 10, iter.max = 50))
d    <- dist(m_norm)

k_check <- tibble(
  k          = ks,
  elbow      = map_dbl(fits, "tot.withinss"),
  silhouette = map_dbl(fits, \(f) mean(silhouette(f$cluster, d)[, 3]))
)
k_check

p_k <- k_check |>
  pivot_longer(c(elbow, silhouette), names_to = "measure") |>
  ggplot(aes(k, value)) +
  geom_line() + geom_point() +
  facet_wrap(~measure, scales = "free_y") +
  scale_x_continuous(breaks = ks) +
  labs(title = "Choosing the number of clusters", y = NULL)
p_k
ggsave("figures/choose_k.png", p_k, width = 8, height = 4, dpi = 300)

#5.4. Run the clustering 
#nstart = 25 runs k-means from 25 random starting points and keeps the best, because k-means can get stuck in poor solutions 

K<-5 
#set this to the k justified above 
set.seed(1)
km <- kmeans(m_norm, centers = K, nstart = 25, iter.max = 100)
table(km$cluster)    # number of people in each cluster

#5.5. What is each cluster about?
# Each cluster is described by the words with the highest weight at its centre and the 3 ppl closest to its centre 
cluster_words <- map_dfr(seq_len(K), \(i) {
  ctr <- km$centers[i, ]
  tibble(cluster = i,
         size = sum(km$cluster == i),
         top_words = paste(names(sort(ctr, decreasing = TRUE))[1:8], collapse = ", "))
})
cluster_words


ctr_norm <- km$centers / sqrt(rowSums(km$centers^2))
cos_sims   <- m_norm %*% t(ctr_norm)           # cosine similarity: people x clusters

examples <- map_dfr(seq_len(K), \(i) {
  members <- which(km$cluster == i)
  top <- members[order(cos_sims[members, i], decreasing = TRUE)][seq_len(min(3, length(members)))]
  tibble(cluster = i, wikidata_id = rownames(m_norm)[top])
}) |>
  left_join(select(people, wikidata_id, name), by = "wikidata_id") |>
  group_by(cluster) |>
  summarise(closest_people = paste(name, collapse = "; "))
examples

# EDIT: one name per cluster, in order 1..K, after reading the two tables above
cluster_names <- paste("Cluster", seq_len(K))
# Example: cluster_names <- c("Machine learning", "Robotics", "Vision", ...)
# WRITE IN REPORT: describe each cluster in 1-2 sentences (top words, example
# people, the theme you named it after).

#5.6. VISUALISE THE CLUSTER
# PCA squashes the many word dimensions into 2 so the clusters can be drawn 
# It shows only part of the structure, so clusters that overlap here may still differ in the full word space 
# pc <- prcomp (m_norm, rank = 2)

clusters <- tibble(wikidata_id = rownames(m_norm),
                   cluster = km$cluster) |>
  left_join(select(people, wikidata_id, name, gender, era), by = "wikidata_id") |>
  mutate(cluster_label = factor(cluster, levels = seq_len(K), labels = cluster_names))

pc <- prcomp(m_norm, rank. = 2)
p_clusters <- clusters |>
  mutate(PC1 = pc$x[, 1], PC2 = pc$x[, 2]) |>
  ggplot(aes(PC1, PC2, colour = cluster_label)) +
  geom_point(alpha = 0.6) +
  labs(title = "AI biographies clustered by intro text (PCA view)",
       colour = "Cluster")
p_clusters
ggsave("figures/clusters_pca.png", p_clusters, width = 7, height = 5, dpi = 300)

#5.7 
#First the overall split of women and men across clusters, with a chi-square test of independence (the same test family as RQ1). The test needs reasonably large expected counts, so check them first.
tab <- table(clusters$cluster_label, clusters$gender)
tab
round(chisq.test(tab)$expected, 1) #ideally most value are 5 or more 
chisq.test(tab)

# Then women's share in each cluster by era. The number of people (n) is printed on the bars because small groups give unreliable shares.
share_cluster_era <- clusters |>
  filter(!is.na(era)) |>
  group_by(cluster_label, era) |>
  summarise(n = n(), share_women = mean(gender == "female"), .groups = "drop")
share_cluster_era

p_share <- ggplot(share_cluster_era, aes(era, share_women)) +
  geom_col() +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.3, size = 3) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  facet_wrap(~cluster_label) +
  labs(title = "Share of women in each cluster, by birth era",
       x = "Birth era", y = "Share of women") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p_share
ggsave("figures/share_women_cluster_era.png", p_share,
       width = 8, height = 6, dpi = 300)

# WRITE IN REPORT: are women concentrated in some clusters and under-represented in others? Does the share change across eras? Report the
# chi-square result in plain words (statistic, degrees of freedom, p-value)
# and say whether the expected-count condition was met.

saveRDS(clusters, "data/clusters.rds")

#6: Limitations (report )
