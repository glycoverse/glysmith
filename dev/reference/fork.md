# Fan out from the current blueprint output

Each argument is a sibling route that receives the same input. Pass a
step for a one-step route, a `%=>%` sequence or a reusable blueprint.

## Usage

``` r
fork(...)
```

## Arguments

- ...:

  One or more routes. Argument names can label routes.

## Value

A fork specification to use in a blueprint.
