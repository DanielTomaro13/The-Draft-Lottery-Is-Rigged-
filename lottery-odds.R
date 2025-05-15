library(dplyr)
library(tibble)

# Define 2025 NBA Draft Lottery odds
lottery_odds <- tibble(
  team = c("Utah", "Washington", "Charlotte", "New Orleans", "Philadelphia", "Brooklyn",
           "Toronto", "San Antonio", "Phoenix", "Portland", "Dallas", "Chicago",
           "Sacramento", "Atlanta"),
  odds = c(14.0, 14.0, 14.0, 12.5, 10.5, 9.0,
           7.5, 6.0, 4.5, 3.0, 2.0, 1.5,
           1.0, 0.5)
) %>%
  mutate(prob = odds / sum(odds),
         seed = row_number())

######################################
Calculate probability of GIVEN lottery result
######################################

# Fixed top 4
top_4 <- c("Dallas", "San Antonio", "Philadelphia", "Charlotte")

# Conditional probability steps
step1 <- lottery_odds %>% filter(team == top_4[1]) %>% pull(prob)
remaining1 <- lottery_odds %>% filter(team != top_4[1]) %>% mutate(prob = prob / sum(prob))
step2 <- remaining1 %>% filter(team == top_4[2]) %>% pull(prob)
remaining2 <- remaining1 %>% filter(team != top_4[2]) %>% mutate(prob = prob / sum(prob))
step3 <- remaining2 %>% filter(team == top_4[3]) %>% pull(prob)
remaining3 <- remaining2 %>% filter(team != top_4[3]) %>% mutate(prob = prob / sum(prob))
step4 <- remaining3 %>% filter(team == top_4[4]) %>% pull(prob)

top4_prob <- step1 * step2 * step3 * step4

# Remaining picks by seed
remaining_teams <- setdiff(lottery_odds$team, top_4)
expected_rest_order <- lottery_odds %>%
  filter(team %in% remaining_teams) %>%
  arrange(seed) %>%
  pull(team)

# Provided full draft result (pick 5–14)
observed_order <- c("Utah", "Washington", "New Orleans", "Brooklyn", "Toronto",
                    "Phoenix", "Portland", "Chicago", "Sacramento", "Atlanta")

is_deterministic_match <- identical(expected_rest_order, observed_order)
final_prob <- if (is_deterministic_match) top4_prob else 0

cat("\n--- Full Lottery Probability Calculation (Given Result) ---\n")
cat("Top 4 Exact Order Probability:", signif(top4_prob, 10), "\n")
if (is_deterministic_match) {
  cat("Remaining picks match deterministic order ✅\n")
  cat("Final Full Lottery Probability:", signif(final_prob, 10), "\n")
} else {
  cat("Remaining picks do NOT match expected deterministic order ❌\n")
  cat("Final Full Lottery Probability: 0\n")
}

######################################
# Simulate a lottery + calculate probability
######################################

# Simulate top 4
set.seed(42)
sim_top4 <- sample(lottery_odds$team, size = 4, prob = lottery_odds$prob, replace = FALSE)

# Remaining teams filled by seed order
sim_remaining <- setdiff(lottery_odds$team, sim_top4)
sim_rest <- lottery_odds %>%
  filter(team %in% sim_remaining) %>%
  arrange(seed) %>%
  pull(team)

# Full simulated draft order
sim_order <- c(sim_top4, sim_rest)

# Calculate probability of simulated order
s1 <- lottery_odds %>% filter(team == sim_top4[1]) %>% pull(prob)
r1 <- lottery_odds %>% filter(team != sim_top4[1]) %>% mutate(prob = prob / sum(prob))
s2 <- r1 %>% filter(team == sim_top4[2]) %>% pull(prob)
r2 <- r1 %>% filter(team != sim_top4[2]) %>% mutate(prob = prob / sum(prob))
s3 <- r2 %>% filter(team == sim_top4[3]) %>% pull(prob)
r3 <- r2 %>% filter(team != sim_top4[3]) %>% mutate(prob = prob / sum(prob))
s4 <- r3 %>% filter(team == sim_top4[4]) %>% pull(prob)

sim_top4_prob <- s1 * s2 * s3 * s4

cat("\n--- Simulated Lottery Outcome ---\n")
for (i in seq_along(sim_order)) {
  cat(paste0(i, ". ", sim_order[i], "\n"))
}

cat("\n--- Probability of Simulated Lottery Occurring ---\n")
cat("P(Exact Outcome):", signif(sim_top4_prob, 10), "\n")
cat("This outcome is expected ~1 in", format(round(1 / sim_top4_prob), big.mark = ","), "lotteries\n")

