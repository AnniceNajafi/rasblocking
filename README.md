# rasblocking

Scoring, verification and structural diagnostics for railroad blocking plans,
as posed by the INFORMS RAS 2026 Problem Solving Competition.

The package depends on **no mathematical programming solver and no Python**. It
reads instances in the [General Modeling Network Specification](https://github.com/zephyr-data-specs/GMNS)
node/link format, so the diagnostics apply to networks beyond the competition
instances.

## Install

```r
# install.packages("remotes")
remotes::install_local("rasblocking")
```

Dependencies: `data.table`, `jsonlite`, `igraph`.

## What it does

### 1. Scores a plan exactly

`ras_cost()` and `ras_stress()` reproduce the competition objective — block
fixed cost, transport, handling and interchange, plus `M` times unserved
car-miles. On the `l1_05` scenario this agrees with the competition backend to
the cent on every component:

```r
inst <- ras_read_instance("datasets/l1")
od   <- ras_read_od("scoring/od_distance_matrix.csv")
sol  <- ras_read_solution("solution_result_l1_05.json")

ras_stress(sol, inst, od, multiplier = 0.5)$stress
#> 1956721815.48
```

`ras_score()` turns nine scenario stresses into the final laddered score,
including the group scaling and the interval a partially-valid submission falls
into.

```r
ras_score(stresses)$score
#> 0.2165173894
```

### 2. Checks feasibility

`ras_validate()` implements the eleven official checks and reports violations
per constraint.

```r
ras_validate(sol, inst, od, 0.5)
#> <ras_validation>  ALL CHECKS PASSED
#>   1_flow_conservation                      pass
#>   ...
#>   9b_demand_volume_consistency             pass
```

### 3. Verifies that a plan reports its own cost honestly

Passing every feasibility check does not mean a payload's implied cost is the
cost the scorer will compute. Several constructions satisfy every official rule
while pricing themselves wrongly, and all of them err in the flattering
direction — typically because a defaulted lookup, `.get(key, 0)`, silently turns
a missing key into a free operation.

Two gates catch these, and are deliberately independent:

* `ras_gate_structure()` knows nothing about the cost model. It checks block
  identifiers, orphan and unused blocks, and that no declared physical route is
  shorter than the shortest path between the block's endpoints.
* `ras_gate_arithmetic()` recomputes all four cost components from the primary
  data files and compares them against what the payload claims.

Neither suffices alone. The arithmetic gate accepts a cost that is absent from
*both* the payload and the recomputation — a blank physical path, say, where the
two implementations read the same missing record and agree with each other.
Agreement between two implementations is evidence of consistency, not of
correctness, whenever both draw on the same source. Run the structural gate
first: it is cheap, model-free, and catches the case whose downstream arithmetic
would otherwise look perfect.

```r
ras_verify(sol, inst)
#> <ras_gate: structure>   PASS
#> <ras_gate: arithmetic>  PASS
#> OVERALL: PROMOTABLE
```

### 4. Diagnoses the instance before you solve it

`ras_diagnose()` reports two facts that govern how hard an instance is,
independently of any solver.

**The sub-tier share.** Minimum block volume is expressed in absolute cars while
demand scales with the multiplier. When demand is scaled down, most demands fall
below the tier set by their own origin-destination distance and cannot open a
block alone — so every block must be a *coalition*, and the problem stops being
"which arcs carry flow" and becomes "which demands agree to travel together".

**The direct-only floor.** Commodity types confined to a single direct block,
whose entire available pool is below tier, cannot be served by *any* feasible
design. The penalty they carry is irreducible and worth no further search.

```r
ras_diagnose(inst, od, multiplier = 0.5)
#> <ras_diagnosis>  demand multiplier 0.5
#>   demands                2044
#>   sub-tier               1728  (84.5%)  cannot open a block alone
#>   direct-only locked      213  (90,990 cars)  unservable by ANY design
#>   addressable sub-tier demand by type:
#>     Merchandise      1417 demands     388,156 cars
#>     Grain              98 demands      32,638 cars
#>   NOTE: the locked count is a floor over direct-only types only.
#>         The rows above are NOT covered by it.
```

That closing note is deliberate, and it is the most useful thing in the package.
The floor is a statement about the set it was proved over. Restated without its
scope it becomes a claim about the whole unserved term, and that inference does
not follow — the addressable rows are in the same penalty term and are *not*
covered. Treating the floor as though it closed everything is an expensive
mistake; the printed output refuses to let the qualifier fall off.

### 5. Enumerates the construction that serves sub-tier demand

Minimum volume is evaluated on **aggregate** block volume, not per demand. A
demand stranded below tier can therefore reach an otherwise unreachable
destination by riding a block that some other demand's volume already justifies.

`ras_pool_candidates()` enumerates and prices these: take an existing block
`X -> Y`, pick sub-tier demands sharing a destination `D`, build `X -> D` and
`D -> Y`, and move the donor's riders onto the new pair. The old block empties
and closes, refunding its fixed cost and freeing a classification track at `X`.

Pooling by shared *destination* rather than origin halves the donor requirement
while serving the whole group from one funded block. Candidates must be
re-enumerated after each applied move, because every accepted move creates
blocks that become staging yards for the next.

The function prices candidates; it does not apply them. Anything it returns must
still pass `ras_validate()` and `ras_verify()` before use.

## Scope and limits

Implemented: the objective, the eleven feasibility checks, both verification
gates, the structural diagnostics, and the pooling candidate generator.

Not implemented: the set-partitioning master that produces a plan from scratch.
That needs a MIP solver; this package deliberately has no solver dependency, so
it analyses, verifies and improves plans rather than generating them. A master
using the CRAN `highs` package would be the natural addition.

The `ras_pool_candidates()` gain estimate is a *screening* figure computed on
average origin-destination distances for the pooled group. It ranks candidates;
it does not certify them. Re-score any applied move with `ras_stress()`.

## Licence

MIT.
