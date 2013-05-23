module MaybeIntType
   implicit none
   public

   type tMaybeInt
      logical :: Nothing
      integer :: Just
   end type
end module

module MaybeIntInterface
   abstract interface
      function f_Int_MaybeInt(a) result(b)
         use MaybeIntType
         integer, intent(in) :: a
         type(tMaybeInt) :: b
      end function
   end interface
end module

module MaybeInt
   use MaybeIntType
   use MaybeIntInterface
   implicit none
   public

contains

   function lift(a) result(JustA)
      implicit none
      integer, intent(in) :: a
      type(tMaybeInt) :: JustA
      
      JustA % Just = a
      JustA % Nothing = .false.
   end function

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
   
end module