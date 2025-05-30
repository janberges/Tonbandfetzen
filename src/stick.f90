subroutine stick
   use constants, only: audio, dp, i2
   use io, only: command_argument
   use riff, only: read_riff, write_riff
   implicit none

   integer :: i, n, offset
   integer(i2) :: c
   type(audio), allocatable :: p(:)
   type(audio) :: s

   s%channels = 1_i2
   s%points = 0
   s%rate = 1.0_dp
   s%amplitude = 0.0_dp

   n = command_argument_count() - 2

   allocate(p(n))

   do i = 1, n
      call read_riff(command_argument(i, '/dev/stdin'), p(i))

      s%points = s%points + p(i)%points

      s%channels = max(s%channels, p(i)%channels)
      s%amplitude = max(s%amplitude, p(i)%amplitude)
      s%rate = max(s%rate, p(i)%rate)
   end do

   allocate(s%sound(s%channels, s%points))

   offset = 0

   do i = 1, n
      if (p(i)%amplitude .ne. s%amplitude) then
         p(i)%sound = int(p(i)%sound * p(i)%amplitude / s%amplitude, i2)
      end if

      do c = 1, s%channels
         s%sound(c, offset + 1:offset + p(i)%points) &
            = p(i)%sound(1_i2 + modulo(c - 1_i2, p(i)%channels), :)
      end do

      offset = offset + p(i)%points
   end do

   call write_riff(command_argument(-1, '/dev/stdout'), s)
end subroutine stick
