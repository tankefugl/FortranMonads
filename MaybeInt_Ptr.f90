module MaybeIntInterface_Ptr
   abstract interface
      function f_Int_MaybeInt(a) result(b)
         integer, pointer, intent(in) :: a
         integer, pointer :: b
      end function
   end interface
end module

module MaybeInt_Ptr
   use MaybeIntInterface_Ptr
   implicit none
   public

contains

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
   
end module