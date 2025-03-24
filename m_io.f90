module m_io
   use m_precision, only: ip, wp
   implicit none

   private

   public cliparse, m2parav

contains

   subroutine cliparse()
      integer :: argc, i
      character(len=100) :: arg, meshfile
      argc = command_argument_count()

      if (argc > 0) then
         print*, "FFES called with ", argc, " argument(s):"
         if (mod(argc,2) /= 0) then ! Arguments count should be even
            print*, "ERROR: argument count should be even"
            stop
         end if

         do i=1, argc, 2
            call get_command_argument(i, arg)

            arg = trim(arg)

            if (arg == "-i") then
               call get_command_argument(i+1, arg)

               meshfile = trim(arg)
               print*, "-i ", meshfile
               print*,""

            end if
         end do
      else
         print*, "FFES called with no arguments"
      end if

   end subroutine cliparse

   subroutine m2parav(nodes, elements, filename)
      !>  Write mesh data to a VTK file for Paraview
      !>  The VTK file format is a simple text-based format.
      !>  This subroutine writes a VTK file suitable for visualizing
      !>  2D quadrilateral elements.
      !>
      !> Args:
      !>  nodes    :: (input) real(wp) array, shape (nnodes, 3), node coordinates
      !>  elements :: (input) integer(ip) array, shape (nels, 4), element connectivity
      !>  filename :: (input) character string, the name of the VTK file to write

      real(wp), intent(in) :: nodes(:,:)
      integer(ip), intent(in) :: elements(:,:)
      character(*), intent(in) :: filename

      integer(ip) :: nnodes, nels, i
      integer :: ios

      nnodes = size(nodes, 1)
      nels = size(elements, 1)

      open(unit=10, file=trim(filename), action='write', iostat=ios)
      if (ios /= 0) then
          write(*, '(a,i0)') 'Error opening file: ', ios
          return
      end if

      !> Write VTK header
      write(10, '(a)') '# vtk DataFile Version 2.0'
      write(10, '(a)') '2D Mesh Data'  ! Description
      write(10, '(a)') 'ASCII'
      write(10, '(a)') 'DATASET POLYDATA'

      !> Write node coordinates
      write(10, '(a,i0,a)') 'POINTS ', nnodes, ' float'
      do i = 1, nnodes
          write(10, '(3e20.12)') nodes(i,1), nodes(i,2), nodes(i,3)
      end do

      !> Write element connectivity
      write(10, '(a,i0,a,i0,a)') 'POLYGONS ', nels, ' ', nels * 5, ' ' ! 5 = 4 nodes + 1 count
      do i = 1, nels
          write(10, '(5i10)') 4, elements(i,1)-1, elements(i,2)-1, elements(i,3)-1, elements(i,4)-1  ! Counter-clockwise
      end do

      close(10)

      print*, "Wrote Paraview VTK file: ", trim(filename)

  end subroutine m2parav

end module m_io
