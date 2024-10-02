#########################################################################
# R Script for Thirst for Victory
# Ryan Juricic & Will Best
# Last Edit: 4/2024
#########################################################################
# Coke Data
coke <- tibble(
  "Sponsor" = c(rep("Coke", 72)),
  "Round" = c(rep("R64", 34), rep("R32", 14), rep("N16", 10), rep('N8', 8), rep("N4", 4), rep("N2", 2))
)

# Pepsi Data
pepsi <- tibble(
  "Sponsor" = c(rep("Pepsi", 54)),
  "Round" = c(rep("R64", 30), rep("R32", 18), rep("N16", 6))
)

# Combine
total <- bind_rows(coke, pepsi)

# Perform Fisher's exact test
fisher_result <- fisher.test(table(total$Sponsor, total$Round))

# Print the result
print(fisher_result)









