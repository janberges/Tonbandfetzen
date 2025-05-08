subroutine guitar
   use constants, only: stderr
   use io, only: command_argument, slurp
   use tab, only: preprocess
   implicit none

   integer :: fun, error

   character(:), allocatable :: path, notes

   notes = preprocess(slurp(command_argument(1, '/dev/stdin')))

   path = command_argument(2, '/dev/stdout')

   if (path .eq. 'stdout') then
      write (*, '(A)', advance='no') notes
   else
      open (newunit=fun, file=path, iostat=error, &
         action='write', status='replace', access='stream')

      if (error .ne. 0) then
         write (stderr, "('Error: Cannot write file ''', A, '''.')") path
         stop
      end if

      write (fun) notes
      close (fun)
   end if
end subroutine guitar
