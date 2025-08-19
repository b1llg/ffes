program main
   use m_precision, only : wp, ip
   use m_mesh, only: gen2dmesh, t_meshfile
   use m_io, only : cliparse, m2parav, read_netcdf

   implicit none
   real(wp), allocatable      :: nodes(:,:)
   integer(ip), allocatable   :: elems(:,:)
   type(t_meshfile)           :: mesh

   !  integer :: argc, i
   !  character(len=100) :: arg, meshfile

   print*,"*********************************************************************"
   print*,"*   ______  ______ _____ ______   _____  _______ _______ _     _   *"
   print*,"*  |  ____ |_____/   |   |     \ |_____] |_____| |       |____/    *"
   print*,"*  |_____| |    \_ __|__ |_____/ |       |     | |_____  |    \_   *"
   print*,"*                                                                  *"
   print*,"*                 Grid based PDE solving package                   *"
   print*,"********************************************************************"

   call cliparse()

   ! call gen2dmesh(1.0_wp, 1.0_wp, 10_ip, 10_ip, 'E2LN4', nodes, elems)

   ! call m2parav(nodes, elems, "../genmesh.vtk")

   call mesh%set_filename("/home/bill/projects/flinalg/ressources/simple_netcdf.e")

   call read_netcdf(mesh)

   if (allocated(nodes)) deallocate(nodes)
   
   if (allocated(elems)) deallocate(elems)

end program main
