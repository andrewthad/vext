# Notes

## Pairs

Vectors of pairs (implemented with SoA) are tricky to deal with. I am not
totally satisfied with the solution in this library. In order to make the
same machinery (i.e. `indef`) usable for both primitive types and pairs,
we don't track vector length in `A#` an `M#` defined in
`src-pair-indef/Element.hs`. We will need to write additional code
elsewhere that projects the components from a vector pair (or constructs
a vector pair from vectors of equal length). What has been done is a bit
of a compromise.

## Equality and Ordering

Contrasting strongly with Haskell's usual take on equality and ordering,
this library assumes equality and ordering based on runtime representation.
This means that we do not offer functions like `unique` and `sort` for any
boxed types, but we do offer them for integral types (`Int#` and friends).
In practice, this does not matter. This is a mild violation of
parametricity since it exposes something about the representation that
a user may, in theory, want to hide.

Right now, `unique` is implemented, but `sort` is missing.

## Bit Vectors

No bit-per-boolean implementation of boolean vectors is provided at the
moment, but this not difficult to add.

## Grouping Aggregation

Aggregation (e.g. sum, count) is not difficult. Combining grouping with
aggregation is more useful, but it's also more difficult. Consider a
pair of vectors that tracks the contribution of some entity towards
a goal:

    id,contribution
    1,1000
    1,1100
    1,1700
    2,1200
    3,1600
    3,1500

We want to end up with this:

    id,contribution
    1,3800
    2,1200
    3,3100

How can we accomplish this in a simple way that provides reasonable
performance? The first step is to sort on the first column. (The example
data was presorted.) Then, we could build index ranges:

    id,range
    1,slice(0,3)
    2,slice(3,1)
    3,slice(4,2)

A slice can be represented by two integers, `base` and `length`, and we
can even enforce with the type system that `base + length <= n`. In a world
with dependent types, this would look like:

    data Slice : Nat -> Type where
      Slice :
           (base : Nat) -> (length : Nat) -> (base + length <= n)
        -> Slice n

However, we need the slice to be represented compactly, not with a boxed
type, so we would end up needing to use an unlifted newtype and accessors
to shield the user from the representation. We may even pack the integers
into the two halves of a Word64 if can commit to not supporting vectors
with more than 4B elements.

An alternative would be to only include the lengths in the range vector
since the offsets are redundant, but doing that makes it impossible to
provide any guarantees around whether or not indices are in bounds.

Either way, once we have the ranges, we can perform a aggregation on
each slice, and then we have our result. If we wanted to perform multiple
aggregations with the same partition key, we could reuse the range vector.
