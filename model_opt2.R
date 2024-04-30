# could be further optimized to remove run as random effect!
update_formula <- function(formula, term_to_remove) {
  # Convert formula to a character string for manipulation
  formula_str <- deparse(formula)
  
  # Define a pattern to match the term to remove, ensuring it's a standalone term
  # This pattern matches the term with optional whitespace and leading/trailing operators
  pattern <- paste0("\\b", term_to_remove, "\\b\\s*[\\|]")
  
  # Split the formula into individual terms
  terms <- unlist(strsplit(formula_str, split = " \\+ "))
  
  # Step 1: Handle random effects
  # Replace term_to_remove with 1 in random effects if it is the term before the '|'
  # Replace the term with (1|group) 
  if (grepl("\\|", paste(terms,collapse="|"))) { # Check if it's a random effect term
    formula_str <- gsub(pattern, "1 |", formula_str)
  }
  
  # Step 2: Handle fixed effects and interactions
  # Split the formula into individual terms
  terms <- unlist(strsplit(formula_str, split = " \\+ "))
  
  # Filter out terms that contain term_to_remove as a fixed effect or interaction
  terms <- terms[!grepl(paste0("(^|:)", term_to_remove, "($|:)"), terms)]
  
  # Reconstruct the formula by collapsing the terms with ' + '
  formula_str <- paste(terms, collapse = " + ")
  
  # Remove any extra operators (resulting from removing terms) and trim whitespace
  formula_str <- gsub("\\+\\s*\\+", "+", formula_str) # Fix multiple '+' signs
  formula_str <- gsub("^\\s*\\+\\s*|\\s*\\+\\s*$", "", formula_str) # Trim leading/trailing '+' signs
  
  # Convert the updated string back to a formula
  updated_formula <- as.formula(formula_str)
  
  # Set the environment of the new formula to the environment from the original formula
  environment(updated_formula) <- environment(formula)
  
  return(updated_formula)
}

formi <- as.formula("y ~ one + two + three:two + (two|three) + (1|four)")
new_formi <- update_formula(formi, "two")

# Print the updated formula
print(new_formi)  # should show 'y ~ one + (1|three) + (1|four)'

df <- data.frame(y=rnorm(40), time=rep(seq(0,12, 4), each=10), unit=rep(sample(100, 8), 5), anal=sample(50, 40), run=rep(1:4, 10))
df$run <- as.factor(df$run)

f1 <- y ~ time + anal + (1 | unit)
f2 <- y ~ time + run + (1 | unit) + run:anal
f3 <- y ~ time + (anal | run) + (1 | unit)
lmer(update_formula(f2, "anal"), df)
