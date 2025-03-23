program main
   use m_precision, only : wp, ip
   use m_mesh, only: gen2dmesh
   use m_io, only : cliparse, m2parav

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

   call gen2dmesh(0.01_wp, 0.01_wp, 100_ip, 100_ip, 'E2LN4', nodes, elems)

   call m2parav(nodes, elems, "genmesh.vtk")

   deallocate(nodes)
   deallocate(elems)
   
end program main
