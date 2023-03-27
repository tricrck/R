y = runif(n=1000, min=0, max=20)
x = rnorm(1000, mean = 6, sd = 8)
z = sample(c(0, 1), 1000, replace = TRUE)# 1 to represent + and 0  for o

library("partykit") 

plot(x, y)

# Give the chart file a name.
png(file = "decision_tree.png")

# Create the tree.
output <- ctree(
  z ~ x + y)

# Plot the tree.
plot(output)

# Save the file.
dev.off()
