# vext

Proof of concept that backpack can be used to share code for working
with both boxed and unboxed vectors. This requires using `unsafeCoerce#`
in a few places, but these uses are sound.

To handle arrays of pointers and arrays of non-pointers with the same
machinery, it is important to make these type constructors have the
same kind. In this library, this is accomplished by leaning into GHC's
rich representation kind system. From the exported modules, we end up
with:

* `Vector.Word8.Vector :: TYPE 'Word8Rep -> Type`
* `Vector.Lifted.Vector :: TYPE 'LiftedRep -> Type`

The idea is that the `Word8` vector can actually handle any type that
is a newtype (using `UnliftedNewtypes`) over `Word8#`.
