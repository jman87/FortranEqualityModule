!------------------------------------------------------------------------------!
! MIT License
! 
! Copyright (c) 2023 Robert S. Browning IV
! 
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
! 
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
! 
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!------------------------------------------------------------------------------!
module FortranEqualityModule

    use, intrinsic :: iso_fortran_env, only : rk=>real64

    implicit none

    ! Module constants
    real(rk), parameter :: zero=0.0_rk
    real(rk), parameter :: real_min=tiny(zero)

    private
    public nearlyEqual

contains

pure function nearlyEqual(a, b, eps) result(flag)
  !---------------------------------------------!
  !  This function is adapted from the paper:   !
  !        "What Every Programmer Should Know   !
  !         About Floating Point Arithmetic"    !
  !---------------------------------------------!
    ! Input Variables
    real(rk), intent(in) :: a, b, eps
    ! Output Variable
    logical              :: flag
    ! Internal Variables
    real(rk)             :: diff
    ! Calculations
    diff = abs(a - b)
    ! Test if a and b are nearly equal
    if (a == b) then
        ! Handles infinity values
        flag = .TRUE.
    else if ( a==zero  .OR.  b==zero  .OR.  diff<real_min ) then
        ! Either a or b is zero or both are extremely close to it.
        ! Thus, relative error is less meaningful here.
        flag = (diff < (eps * real_min))
    else
        ! Use relative error to test equality
        flag = ( (diff / (abs(a)+abs(b))) < eps )
    end if
end function

end module FortranEqualityModule

