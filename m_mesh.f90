module m_mesh
   use m_precision, only : wp, ip
   implicit none
   private

   public :: generate2dgrid
contains
   subroutine generate2dgrid(dx, dy, nx, ny, eltype, nodes, elements)
      real(wp), intent(in) :: dx, dy
      integer(ip), intent(in) :: nx, ny
      character(len=5), intent(in) :: eltype
      integer(ip), allocatable :: elements(:,:)
      real(wp), allocatable :: nodes(:, :)

      integer(ip) ::  nnodes, nels, npel, i, j, k, l, m, n

      !> checks
      if (nx <= 0_ip .OR. ny <= 0_ip) then
         print *, "Error while generating mesh in 'generate2dgrid':"
         print *, "nx= ", nx, " and ny= ", ny
         stop
      end if

      !> Setting dimensions
      nels = nx * ny
      nnodes = (nx+1)*(ny+1)

      print*, "Element type: ", eltype
      print*, "Number of elements: ", nels
      print*, "Number of nodes: ", nnodes

      !> Allocate
      npel = 4 ! Node per element
      allocate(elements(nels, npel))
      allocate(nodes(nnodes, 3))

      print*, "generating mesh..."


      !> Generate nodes coordinate COOR table
      k=1 ! node number counter

      do j=1, ny+1
         do i=1, nx+1
            nodes(k,1) = (i-1) * dx    ! x
            nodes(k,2) = (j-1) * dy    ! y
            nodes(k,3) = 0_wp          ! z = 0 (2D)

            k = k + 1
         end do
      end do

      !> Generate Element/nodes list CONNEC
      k=1 ! "First" row index (Node)
      l=1 ! "Second" row index (Node)
      m=1 ! Element counter
      ! n=1 ! "First" Row counter

      ! **********************
      !
      !     Element loops stop working at 3x3 element
      !
      ! **********************


      do while (m <= nels) 
         elements(m,1) = k    ! Node 1
         elements(m,2) = k+1  ! Node 2

         l = k + 1 + (nx + 1) ! Node 3, counter clock wise

         elements(m,3) = l
         elements(m,4) = l-1 ! Node 4

         if (k == 3) then
            print*,""
         end if
         !> Prepare k for next row or next element
         if (k/2+1 == nx + 1) then! Next row

            ! n = n + 2             
            k = k + 1
         else ! next element
            k = k + 1
         end if

         m = m + 1
      end do

      print*," "
      print*, "Node list (COOR)"
      do i=1, nnodes
         print*,i, nodes(i,:)
      end do

      print*, " "
      print*, "Element list (CONNEC)"
      do i=1, nels
         print*,i, elements(i,:)
      end do

      print*, "mesh_generated!"
   end subroutine generate2dgrid
end module m_mesh
