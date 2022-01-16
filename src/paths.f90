module paths
   implicit none
   private

   public :: extension

contains

   function extension(path)
      character(:), allocatable :: extension

      character(*), intent(in) :: path

      integer :: dot, slash

      dot = index(path, '.', back=.true.)
      slash = scan(path, '/\:', back=.true.)

      if (dot .gt. slash) then
         extension = path(dot + 1:)
      else
         extension = ''
      end if
   end function extension
end module paths
