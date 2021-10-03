# GraphQueries

Implementing GraphQueries in Haskell using Reachability Queries. The Graph Queries are implemeted on top of [Daison (DAta lISt comprehensiON)](https://github.com/krangelov/daison/). In this project we evaluate the following three algorithms,

1. Implementing Reachability Queries for Acyclic Statig graphs using AILabel (Augmented Interval Labelling) Approach  (https://doi.org/10.1007/978-3-319-25255-1_46)
2. Implementing Reachability Queries for Acyclic Dynamic graphs using D-AILabel (Dynamic Augmented Interval Labelling) approach.
3. Implementing Reachability Queries for Acyclic Dynamic graphs using D-AILabel with smart relabelling technique

##Project structure
- [`app/`](DAILabel/app) - Haskell source code.
  -[`MainProject.hs/`](DAILabel/app/MainProject.hs)
  -[`AILabel.hs/`](DAILabel/app/AILabel.hs)
  -[`DAILabel.hs/`](DAILabel/app/DAILabel.hs)
  -[`DAILabelModified.hs/`](DAILabel/app/DAILabelModified.hs)

##How to run the application

### Go to /DAILabel folder
`stack build --exec main-exe`
  
  
