subroutine repeatz
   use constants, only: audio, dp
   use io, only: command_argument
   use rationals, only: rational
   use riff, only: read_riff, write_riff
   implicit none

   integer :: i
   real(dp) :: factor
   type(audio) :: s1, s

   factor = rational(command_argument(1, '2'))

   call read_riff(command_argument(2, '/dev/stdin'), s1)

   s%channels = s1%channels
   s%points = nint(abs(factor) * s1%points)
   s%rate = s1%rate
   s%amplitude = s1%amplitude

   allocate(s%sound(s%channels, s%points))

   if (factor .gt. 0.0_dp) then
      do i = 1, s%points
         s%sound(:, i) = s1%sound(:, 1 + modulo(i - 1, s1%points))
      end do
   else
      do i = 1, s%points
         s%sound(:, s%points + 1 - i) &
            = s1%sound(:, 1 + modulo(s1%points - i, s1%points))
      end do
   end if

   call write_riff(command_argument(3, '/dev/stdout'), s)
end subroutine repeatz
