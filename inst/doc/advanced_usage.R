## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
library(latexSymb)
data(common)
attach(common)

## -----------------------------------------------------------------------------
# my_symbols.R
library(latexSymb)
data(common)
attach(common)

# Define document-specific symbols
pt <- lsymb("x")
pt_prime <- lsymb("x'")
metric_space <- lsymb("\\mathcal{X}")
measure <- lsymb("\\mu")

# Create specialized functions
dist <- function(x, y) lsymb("d") * pths(x * comma * y)

## -----------------------------------------------------------------------------
# Expectation operator
exp_val <- function(x) lsymb("\\mathbb{E}") * sqbr(x)

sq <- function(x) pths(x)^2

# Absolute value
abs <- function(x) lsymb("\\abs{", x, "}")

# Indicator function
indic <- function(condition) {
  lsymb("\\mathbbm{1}") |> under(br(condition))
}

# Composed example
X <- lsymb("X")
Y <- lsymb("Y")

## -----------------------------------------------------------------------------
# Build a proof step by step
proof_steps <- list(
  ruler * dist(x, y) * leq * dist(x, z) + dist(z, y) * endl,
  ruler * thus * dist(x, y) - dist(x, z) * leq * dist(z, y)
) |>
  lenv("align*", rows = _)

## -----------------------------------------------------------------------------
# Setup
sample_size <- lsymb("n")
observation <- lsymb("X") |> under(i)
sample_mean <- lsymb("\\bar{X}")
conv_distr <- lsymb("\\overset{d}{\\rightarrow}")
sqrt <- function(x) lsymb("\\sqrt{", x, "}")

# Build a Central Limit Theorem statement
clt_statement <- list(
  ruler * sqrt(sample_size) * (pths(sample_mean - mu) / si) *
    conv_distr * lsymb("N(0,1)") * endl,
  ruler * lsymb("\\text{where }") *
    sample_mean * eq * (1 / sample_size) * Sum(observation, from = i * eq * 1, to = sample_size)
) |>
  lenv("align*", rows = _)

