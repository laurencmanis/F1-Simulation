# Formula 1 Race Simulation and Betting Markets Creation

## Project Overview
This project simulates a Formula 1 race to provide probabilities, betting markets, and true odds for various selections and events. Below is an overview of the steps and goals of the project:

## Data Collection
- Source: DraftKings
- Event: 2024 Austrian Grand Prix
- Data: Outright win probabilities for each driver
  
## Methodology
- Scraping: Collected outright win probabilities from DraftKings for the upcoming race.
- True Probabilities: Adjusted for the vig to determine the true probabilities of each driver winning, under the assumption that the vig is equally distributed across outcomes.
- Simulation: Simulated the race 5000 times using the true probabilities to model potential outcomes.
- Betting Markets: Created and priced various betting markets, including some not typically offered by major US sportsbooks.

## Key Features
- Reproducibility: The analysis is easily replicable for any other race as long as outright odds are available.
- Comprehensive Markets: Developed several unique betting markets not commonly offered, such as:
  - Driver Props (e.g., Head-to-Head, Teammate vs. Teammate, Total Points Scored)
  - Team Props (e.g., Total Points Scored, Drivers Scoring Points, Team to Score the Most Points)
  - Podium Props (e.g., Podium Finish in No Particular Order, Exact Podium Order)

## Applications
1. For Sportsbooks: Enhance the range and depth of Formula 1 race markets offered.
2. For Bettors: Gain a better understanding of race probabilities to make more informed betting decisions on books where similar markets are offered.

## Conclusion
This project demonstrates the potential to enrich the betting landscape for Formula 1 races, providing both sportsbooks and bettors with valuable insights and expanded betting options.

