module samples
   use constants
   use intervals
   implicit none
   private

   public :: sample

contains

   subroutine sample(x, what, how)
      real(dp), intent(out), target :: x(:)
      character(*), intent(in) :: what, how

      select case (what)
      case ('wave')
         select case (how)
         case ('harmonic')
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
            call random_number(x)
            x = 2.0_dp * x - 1.0_dp

         case default
            write (stderr, "('Error: unknown sample ''', A, '''.')") how
            write (stderr, "('See ''man mel'' for list of samples.')")
            stop
         end select

      case ('fade')
         select case (how)
         case ('harmonic')
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

         case default
            write (stderr, "('Error: unknown sample ''', A, '''.')") how
            write (stderr, "('See ''man mel'' for list of samples.')")
            stop
         end select
      end select
   end subroutine sample
end module samples
