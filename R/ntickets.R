#' @title ntickets
#'
#' @param N Number of seats
#' @param gamma target overbooking probability
#' @param p probability a passenger shows up
#'
#' @return Two plots demonstrating likelihood of passengers showing up based on variables
#' @export
#'
#' @examples
#' \dontrun{ntickets(N=400, gamma=0.02, p=0.95)}
ntickets <- function(N, gamma, p) {
  # N: number of seats
  # gamma: target overbooking probability
  # p: probability a passenger shows up

  # Discrete Calculation
  nd = N
  while (pbinom(N, size = nd, prob = p) > 1 - gamma) {
    nd = nd + 1
  }
  if (abs(pbinom(N, size = nd, prob = p) > abs(pbinom(N, size = nd - 1, prob = p)))) {
    nd = nd - 1
  }

  # Normal Approximation Calculation
  objective_function = function(n) {
    1 - gamma - pnorm(N + 0.5, mean = n * p, sd = sqrt(n * p * (1 - p)))
  }

  root_result = uniroot(objective_function, lower = N, upper = N + 50)
  nc = root_result$root

  n_vals = seq(N, nc + 40, by = 1)

  # Objective function for discrete case
  obj_discrete = 1 - gamma - pbinom(N, size = n_vals, prob = p)

  # Objective function for normal approximation
  obj_normal = 1 - gamma - pnorm(N + 0.5, mean = n_vals * p, sd = sqrt(n_vals * p * (1 - p)))

  # Plot both objective functions with dots
  plot(n_vals, obj_discrete, type = "p", col = "blue", ylim = c(0, 1),
       ylab = "Objective", xlab = "n",
       main = paste0("Objective Vs n to find optimal tickets sold\n(",nd,") gamma = ",gamma," N = ",N," discrete"))

  points(n_vals, obj_discrete, col = "blue", pch = 16)  # Add blue dots for discrete case
  lines(n_vals, obj_discrete, col = "blue")  # Blue line for discrete case
  abline(h = 0, col = "black")
  abline(v = nd, col = "blue")

  plot(n_vals, obj_normal, type = "l", col = "black", ylim = c(0, 1),
       ylab = "Objective Function", xlab = "n (number of tickets sold)",
       main = paste0("Objective Vs n to find optimal tickets sold\n(",nc,") gamma = ",gamma," N = ",N," continuous"))
  lines(n_vals, obj_normal, col = "black")

  # Add horizontal line at y=0
  abline(h = 0, col = "black")
  abline(v = nc, col = "black")

  data = list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)
  # 5. Return a named list of results
  cat("nd: \n")
  print(data$nd)
  cat("nc: \n")
  print(data$nc)
  cat("N: \n")
  print(data$N)
  cat("p: \n")
  print(data$p)
  cat("gamma: \n")
  print(data$gamma)
}
