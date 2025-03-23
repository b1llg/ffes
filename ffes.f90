program main
   use m_precision, only : wp, ip
   use m_mesh, only: generate2dgrid
   use m_io, only : cliparse

   implicit none
   real(wp), allocatable :: nodes(:,:)
   integer(ip), allocatable :: elems(:,:)
   !  integer :: argc, i
   !  character(len=100) :: arg, meshfile

   print*,"*****************************************"
   print*,"*   ███████╗███████╗███████╗███████╗    *"
   print*,"*   ██╔════╝██╔════╝██╔════╝██╔════╝    *"
   print*,"*   █████╗  █████╗  █████╗  ███████╗    *"
   print*,"*   ██╔══╝  ██╔══╝  ██╔══╝  ╚════██║    *"
   print*,"*   ██║     ██║     ███████╗███████║    *"
   print*,"*   ╚═╝     ╚═╝     ╚══════╝╚══════╝    *"
   print*,"*    Fortran Finite Element Solver      *"
   print*,"*****************************************"
   print*, " "

   call cliparse()

   call generate2dgrid(1.0_wp, 1.0_wp, 3_ip, 3_ip, 'E2LN8', nodes, elems)

   deallocate(nodes)
   deallocate(elems)

end program main
