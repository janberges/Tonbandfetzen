program mel
   use constants
   use interpreter
   use io
   use paths
   use riff
   implicit none

   integer :: i
   character(:), allocatable :: path
   type(audio) :: music

   do i = 1, command_argument_count()
      path = command_argument(i)

      call play(slurp(path), music)

      call make(stem(path) // '.wav', music)
   end do
end program mel
