# Function to compute Pn (mod 10^9 + 7)
#Used Dynamic Programming to optimise the solution

#Functon to compute the probability
compute = function(n) 
{
  mod_val = 10^9 + 7
  
  # Initialize the DP table
  dp = matrix(data = 0, nrow = n + 1, ncol = n + 1)
  
  # Base case: Only one way to reach (0, 0)
  dp[1, 1] = 1
  
  # Fill the DP table
  for (i in 1:(n + 1)) 
  {
    for (j in 1:(n + 1))
    {
      # Skip if the current point is above the diagonal
      if (j > i)
        next
      
      # Update the DP table using the previous values
      if (i > 1) 
        dp[i, j] = (dp[i, j] + dp[i - 1, j]) %% mod_val
      if (j > 1) 
        dp[i, j] = (dp[i, j] + dp[i, j - 1]) %% mod_val
    }
  }
  
  return(dp[n + 1, n + 1])
}

# Compute and print Pn (mod 10^9 + 7) for n = 1 to 100
for (n in 1:100) 
{
  Probability = compute(n)
  ans = Probability%%(10^9+7)
  print(paste("P at", n,"=", ans))
}

