Two-Step Sankey 
by Dr Michal Marek Hoppe, 2024
version 1.0.0

mmlhoppe@gmail.com

A handy tool for visualizing categorical groupings of items across two states (e.g., changes over time).

Input Data:

data.csv:

- The first column contains item identifiers.
- The second and third columns represent the initial and follow-up states of the items, respectively.
- States are denoted by category labels, which must match the categories listed in the categories file.

categories.csv:

- Lists all possible category states.
- If the categories follow an ordinal scale (e.g., good, better, best), they should be ordered accordingly in this file.