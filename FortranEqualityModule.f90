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

