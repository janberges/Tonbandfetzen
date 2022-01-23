module samples
   use constants, only: dp, pi, stderr
   use intervals, only: interval
   use lcg, only: minstd
   implicit none
   private

   public :: sample

contains

   subroutine sample(x, what, how)
      real(dp), intent(out) :: x(:)
      character(*), intent(in) :: what, how

      integer :: i

      select case (what)
      case ('wave')
         select case (how)
         case default ! harmonic
            call warn
            call interval(x, 0.0_dp, 2.0_dp * pi, 1)
            x = sin(x)

         case ('power')
            call interval(x, 0.0_dp, 2.0_dp * pi, 1)
            x = sin(x) ** 3

         case ('major')
            call interval(x, 0.0_dp, 2.0_dp * pi, 1)
            x = sin(x) ** 5

         case ('constant')
            call interval(x, 0.0_dp, 2.0_dp * pi, 1)
            x = sign(1.0_dp, sin(x))

         case ('linear')
            call interval(x, 0.0_dp, 2.0_dp * pi, 1)
            x = 2.0_dp / pi * asin(sin(x))

         case ('quadratic')
            call interval(x, -2.0_dp, 2.0_dp, 1)
            x = sign(2.0_dp * abs(x) - x ** 2, x)

         case ('circular')
            call interval(x, -2.0_dp, 2.0_dp, 1)
            x = sign(sqrt(2.0_dp * abs(x) - x ** 2), x)

         case ('cubic')
            call interval(x, -1.0_dp, 1.0_dp, 1)
            x = 1.5_dp * sqrt(3.0_dp) * (x ** 3 - x)

         case ('random')
            do i = 1, size(x)
               call minstd(x(i))
            end do
            x = 2.0_dp * x - 1.0_dp
         end select

      case ('fade')
         select case (how)
         case default ! harmonic
            call warn
            call interval(x, 0.0_dp, 0.5_dp * pi, 0)
            x = sin(x)

         case ('smooth')
            call interval(x, 0.0_dp, 0.5_dp * pi, 0)
            x = sin(x) ** 2

         case ('power')
            call interval(x, 0.0_dp, 0.5_dp * pi, 0)
            x = sin(x) ** 3

         case ('major')
            call interval(x, 0.0_dp, 0.5_dp * pi, 0)
            x = sin(x) ** 5

         case ('linear')
            call interval(x, 0.0_dp, 1.0_dp, 0)

         case ('quadratic')
            call interval(x, -1.0_dp, 0.0_dp, 0)
            x = 1.0_dp - x ** 2

         case ('circular')
            call interval(x, -1.0_dp, 0.0_dp, 0)
            x = sqrt(1.0_dp - x ** 2)

         case ('cubic')
            call interval(x, 0.0_dp, 1.0_dp, 0)
            x = 3.0_dp * x ** 2 - 2.0_dp * x ** 2
         end select
      end select

   contains

      subroutine warn
         if (how .ne. 'harmonic') then
            write (stderr, "('Warning: Unknown sample ''', A, '''.')") how
            write (stderr, "('The sample ''harmonic'' is used instead.')")
            write (stderr, "('See ''man mel'' for list of samples.')")
         end if
      end subroutine warn
   end subroutine sample
end module samples
