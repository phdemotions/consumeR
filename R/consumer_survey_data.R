# Generated dataset used for examples and package documentation.
# This mirrors data-raw/create_data.R without requiring build-time R data files.

set.seed(42)

n_per_group <- 50

customer_names <- c(
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

flyer_data <- data.frame(
  customer_id = 1:n_per_group,
  customer_name = customer_names[1:n_per_group],
  flyer_group = "Got Flyer",
  spending = round(stats::rnorm(n_per_group, mean = 65, sd = 25), 2),
  satisfaction = round(stats::rnorm(n_per_group, mean = 7.8, sd = 1.5)),
  loyalty_score = round(stats::rnorm(n_per_group, mean = 68, sd = 18), 1),
  stringsAsFactors = FALSE
)

no_flyer_data <- data.frame(
  customer_id = (n_per_group + 1):(2 * n_per_group),
  customer_name = customer_names[(n_per_group + 1):(2 * n_per_group)],
  flyer_group = "No Flyer",
  spending = round(stats::rnorm(n_per_group, mean = 52, sd = 25), 2),
  satisfaction = round(stats::rnorm(n_per_group, mean = 7.2, sd = 1.5)),
  loyalty_score = round(stats::rnorm(n_per_group, mean = 62, sd = 18), 1),
  stringsAsFactors = FALSE
)

consumer_survey <- rbind(flyer_data, no_flyer_data)

consumer_survey$satisfaction <- pmax(1, pmin(10, consumer_survey$satisfaction))
consumer_survey$loyalty_score <- pmax(0, pmin(100, consumer_survey$loyalty_score))
consumer_survey$spending <- pmax(0, consumer_survey$spending)

set.seed(123)
missing_indices <- sample(1:nrow(consumer_survey), size = 3)
consumer_survey$spending[missing_indices[1]] <- NA
consumer_survey$satisfaction[missing_indices[2]] <- NA
consumer_survey$loyalty_score[missing_indices[3]] <- NA
