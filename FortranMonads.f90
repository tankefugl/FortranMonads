module TestMaybeInt
   use MaybeInt
contains

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

   subroutine test_MaybeInt
      implicit none

      type(tMaybeInt) :: MaybeA, MaybeB, MaybeC, MaybeD
      procedure(f_Int_MaybeInt), pointer :: success, failure

      print *, "Testing MaybeInt ..."

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

end module

module TestMaybeInt_Ptr
   use MaybeInt_Ptr
contains

   function successCalculation(a) result(b)
      implicit none
      integer, pointer, intent(in) :: a
      integer, pointer :: b

      b => lift(a + 2)
   end function

   function failedCalculation(a) result(b)
      implicit none
      integer, pointer, intent(in) :: a
      integer, pointer :: b

      b => NULL()
   end function

   function mreturn(a) result(b)
      implicit none
      integer, pointer, intent(in) :: a
      integer :: b

      b = a
   end function

   subroutine test_MaybeInt_Ptr
      implicit none

      integer, pointer :: MaybeA, MaybeB, MaybeC, MaybeD
      procedure(f_Int_MaybeInt), pointer :: success, failure

      integer :: returned

      print *, "Testing MaybeInt_Ptr ..."

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

end module

program FortranMonads
   use TestMaybeInt
   use TestMaybeInt_Ptr
   implicit none

   call test_MaybeInt
   call test_MaybeInt_Ptr
end program FortranMonads
