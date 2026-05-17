# J-1 Work & Travel Net Income Model
A decision-support tool built to compare US job placements when choosing where to spend a J-1 work & travel season. Instead of eyeballing offer letters, I modeled net income across locations, hours scenarios, and trip durations — all-in.

## What it does
- Compares 12 placements across 7 US states (lifeguarding, hospitality, retail, customer service)
- Models 6 working-hours scenarios from 32h to 60h/week, with overtime at 1.5x
- Simulates tip income stochastically where applicable (uniform distribution over realistic tip multipliers)
- Accounts for all costs: accommodation, food, J-1 visa fee, program registration fee, location-specific costs, and federal tax withholding (~10%)
- Runs duration sensitivity across 10, 11, and 12-week placements
- Outputs faceted ggplot2 visualizations for scenario comparison and location ranking
- 
## Output examples
- **Scenario comparison** — grouped bar chart of net income by location across all hours scenarios
- **Realistic scenario** — position-specific hours assumptions ranked by net income
- **Duration sensitivity** — faceted chart across 10/11/12-week placements
- 
## Stack
- R
- tidyverse
- ggplot2
- dplyr

## Why I built this
J-1 offers vary wildly on paper — a higher hourly wage in one state can easily be offset by expensive accommodation or a costlier visa program. This model made the decision quantitative.
