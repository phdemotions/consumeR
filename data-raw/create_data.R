# Script to create example consumer_survey dataset
# Cloud 9 / Dunder Mifflin themed customer data
# Inspired by Superstore and The Office TV shows

set.seed(42)  # For reproducibility

# Create 100 customers, 50 in each group
n_per_group <- 50

# Fun customer names inspired by The Office and Superstore
# Mix of common names and a few Easter eggs for fans
customer_names <- c(
  # Got Flyer group (50 names)
  "Amy Sosa", "Jonah Simms", "Garrett McNeill", "Dina Fox", "Cheyenne Lee",
  "Mateo Liwanag", "Glenn Sturgis", "Sandra Kaluiokalani", "Marcus White", "Bo Thompson",
  "Jim Halpert", "Pam Beesly", "Dwight Schrute", "Michael Scott", "Angela Martin",
  "Kevin Malone", "Oscar Martinez", "Stanley Hudson", "Phyllis Vance", "Ryan Howard",
  "Kelly Kapoor", "Andy Bernard", "Erin Hannon", "Toby Flenderson", "Creed Bratton",
  "Meredith Palmer", "Darryl Philbin", "Jan Levinson", "David Wallace", "Holly Flax",
  "Karen Chen", "Marcus Lee", "Sarah Williams", "Tom Anderson", "Lisa Martinez",
  "James Brown", "Emily Davis", "Chris Wilson", "Jessica Moore", "Daniel Taylor",
  "Rachel Thomas", "David Jackson", "Laura White", "Mike Harris", "Amanda Clark",
  "Josh Lewis", "Michelle Robinson", "Kevin Walker", "Nicole Hall", "Brandon Allen",

  # No Flyer group (50 names)
  "Carol Stills", "Jeff Sutin", "Tate", "Janet", "Justine Simms",
  "Brett", "Sal", "Cody", "Sayid", "Earl",
  "Roy Anderson", "Todd Packer", "Pete Miller", "Clark Green", "Gabe Lewis",
  "Robert California", "Nellie Bertram", "Charles Miner", "Jo Bennett", "Danny Cordray",
  "Val Johnson", "Nick", "Brian", "Cathy Simms", "Jordan Garfield",
  "Kathy Ireland", "Rolf Ahl", "Tony Gardner", "Deangelo Vickers", "Will Ferrell",
  "Jason Martinez", "Rebecca Johnson", "Tyler Brown", "Ashley Davis", "Justin Wilson",
  "Stephanie Moore", "Matthew Taylor", "Samantha White", "Andrew Harris", "Jennifer Clark",
  "Christopher Lewis", "Melissa Robinson", "Ryan Walker", "Laura Hall", "Brandon Allen",
  "Nicole Young", "Kevin King", "Amanda Wright", "David Lopez", "Sarah Hill"
)

# Generate data for "Got Flyer" group (slightly higher spending/satisfaction)
# These customers saw the promotional flyer with deals
flyer_data <- data.frame(
  customer_id = 1:n_per_group,
  customer_name = customer_names[1:n_per_group],
  flyer_group = "Got Flyer",
  spending = round(rnorm(n_per_group, mean = 65, sd = 25), 2),
  satisfaction = round(rnorm(n_per_group, mean = 7.8, sd = 1.5)),
  loyalty_score = round(rnorm(n_per_group, mean = 68, sd = 18), 1)
)

# Generate data for "No Flyer" group (regular shopping behavior)
# These customers shopped without seeing the promotional flyer
no_flyer_data <- data.frame(
  customer_id = (n_per_group + 1):(2 * n_per_group),
  customer_name = customer_names[(n_per_group + 1):(2 * n_per_group)],
  flyer_group = "No Flyer",
  spending = round(rnorm(n_per_group, mean = 52, sd = 25), 2),
  satisfaction = round(rnorm(n_per_group, mean = 7.2, sd = 1.5)),
  loyalty_score = round(rnorm(n_per_group, mean = 62, sd = 18), 1)
)

# Combine into one dataset
consumer_survey <- rbind(flyer_data, no_flyer_data)

# Ensure satisfaction is within 1-10 range
consumer_survey$satisfaction <- pmax(1, pmin(10, consumer_survey$satisfaction))

# Ensure loyalty_score is within 0-100 range
consumer_survey$loyalty_score <- pmax(0, pmin(100, consumer_survey$loyalty_score))

# Ensure spending is non-negative
consumer_survey$spending <- pmax(0, consumer_survey$spending)

# Add a few missing values for realism (about 3%)
# Sometimes customers don't complete all survey questions
set.seed(123)
missing_indices <- sample(1:nrow(consumer_survey), size = 3)
consumer_survey$spending[missing_indices[1]] <- NA
consumer_survey$satisfaction[missing_indices[2]] <- NA
consumer_survey$loyalty_score[missing_indices[3]] <- NA

# Save to data/ directory
usethis::use_data(consumer_survey, overwrite = TRUE)

# Display summary
cat("Cloud 9 Customer Dataset Created Successfully!\n")
cat("==============================================\n\n")
cat("Dataset summary:\n")
print(str(consumer_survey))
cat("\nGroup sizes:\n")
print(table(consumer_survey$flyer_group))
cat("\nMissing values:\n")
print(colSums(is.na(consumer_survey)))
cat("\nSample customers:\n")
print(head(consumer_survey, 10))
