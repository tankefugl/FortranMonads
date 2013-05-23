# Monads in Fortran?
*Petter Rønningen, 23. May 2013, Researcher at SINTEF Materials & Chemistry, Environmental Technology*

Is it possible? Can it be useful? Could it make a Fortran life easier? I don't know, but I'd like to try!

The full source code is available in the repository, compiled with ifort but should be compatible
with other compilers. You might have to specify a F03 flag.

Feedback, ideas and constructive critisism is very much welcome!

## Maybe = Nothing | Just Fortran

Let's play around a bit. Imagine that we create a tuple in Fortran, representing
an optional value:

    type tMaybeInt
      logical :: Nothing
      integer :: Just
    end type

`Nothing` is `.false.` if the value is contained in `Just`, `.true.` if we only have ... well, nothing.

We can then lift an integer into the Maybe by

    function lift(a) result(JustA)
      implicit none
      integer, intent(in) :: a
      type(tMaybeInt) :: JustA
      
      JustA % Just = a
      JustA % Nothing = .false.
    end function

and perform the bind (named `mbind` as Fortran uses `bind` as a keyword) with

    function mbind(a, f) result(b)
      implicit none
      type(tMaybeInt), intent(in) :: a
      procedure(f_Int_MaybeInt), pointer :: f
      type(tMaybeInt) :: b

      if (a % Nothing) then
         b % Nothing = .true.
      else
         b = f(a % Just)
      end if
    end function

However, here comes the ugly part. To make `mbind` apply a function `f` to `a`, we need to create
an abstract interface representing the exact signature of `f`. Obviously, making abstract
interfaces for ALL possible signatures is a tad much work for one evening, so let's just
create it for `Int -> Maybe Int`:

    abstract interface
      function f_Int_MaybeInt(a) result(b)
         use MaybeIntType
         integer, intent(in) :: a
         type(tMaybeInt) :: b
      end function
    end interface

Now, let's create some horribly silly functions with the right signatures, just to illustrate a
computation or data retrival that is successful and returns an integer and a failed computation 
or data retrival that returns `Nothing`.

    function successCalculation(a) result(b)
      implicit none
      integer, intent(in) :: a
      type(tMaybeInt) :: b

      b = lift(a + 2)
    end function

    function failedCalculation(a) result(b)
      implicit none
      integer, intent(in) :: a
      type(tMaybeInt) :: b

      b % Nothing = .true.
    end function

And for the grand finale, we'll sew this together in a small test!

    subroutine test_MaybeInt
      implicit none

      type(tMaybeInt) :: MaybeA, MaybeB, MaybeC, MaybeD
      procedure(f_Int_MaybeInt), pointer :: success, failure

      success => successCalculation
      failure => failedCalculation

      MaybeA = mbind(lift(4), success)
      MaybeB = mbind(MaybeA, success)
      MaybeC = mbind(MaybeB, success)
      MaybeD = mbind(MaybeC, success)

      if (MaybeD % Nothing) then
         print *, "Nothing"
      else
         print *, "Just ", MaybeD % Just
      end if
    end subroutine

Well, it's not **that** bad! I've seen worse syntax. We can play around with replacing
the `mbind`s to `success` to failure to see that `Nothing` properly propagates.

Note that we need to assign procedure pointers to `success` and `failure`. Ifort wouldn't
allow calling the functions `successCalculation` and `failedCalculation` directly.

## Abusing null pointers

Let's try something else, Maybe by (ab)using integer pointers! Simply put, if a pointer
points to `NULL()`, which can be checked by the `ASSOCIATED()` intrinsic, we'll pretend
it's `Nothing`. If not, we'll use it's value as an integer. Sounds good? It kinda works, 
but beware of crashes. They're easy to create and without a single compile time
warning.

The tune goes something like this ...

    abstract interface
      function f_Int_MaybeInt(a) result(b)
         use MaybeIntType
         integer, intent(in) :: a
         type(tMaybeInt) :: b
      end function
    end interface

... with the chorus singing ...

    function lift(a) result(JustA)
      implicit none
      integer, target, intent(in) :: a
      integer, pointer :: JustA
      
      JustA => a
    end function

    function mbind(a, f) result(b)
      implicit none
      integer, pointer, intent(in) :: a
      procedure(f_Int_MaybeInt), pointer :: f
      integer, pointer :: b

      if (.not. ASSOCIATED(a)) then
         b => NULL()
      else
         b => f(a)
      end if
    end function

We can test this too, by doing something very similar to the above test:

    subroutine test_MaybeInt_Ptr
      implicit none

      integer, pointer :: MaybeA, MaybeB, MaybeC, MaybeD
      procedure(f_Int_MaybeInt), pointer :: success, failure

      integer :: returned

      success => successCalculation
      failure => failedCalculation

      MaybeA => mbind(lift(4), success)
      MaybeB => mbind(MaybeA, failure)
      MaybeC => mbind(MaybeB, success)
      MaybeD => mbind(MaybeC, success)

      if (.not. ASSOCIATED(MaybeD)) then
         print *, "Nothing"
      else
         returned = MaybeD
         print *, "Just ", returned
      end if
    end subroutine

Very similar, apart from the fact that `print` didn't want to print a pointer variable 
directly. I don't know why, but the solution was to assign it to an integer first.

## Conclusion

It kinda works! Not quite convinced I want to use it in a real scenario yet, though. A
couple of issues:

* How can we avoid having to create a abstract interfaces for funtions, or can we
  generate them?

* How is performance impacted by these two approaches?

* Can the overal syntax be tweaked to something more streamlined or elegant?

Error monad should be easy to implement in a similar manner, I think that'll be the next
step.



