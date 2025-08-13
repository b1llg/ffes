module m_mesh
   use m_precision, only : wp, ip
   implicit none
   private

   public :: gen2dmesh, GeneratedMesh

   type, abstract :: Mesh
      integer :: nnodes, nels
   end type

   type, extends(Mesh) :: GeneratedMesh

   end type

   type, extends(Mesh) :: MeshFile
      character(:), allocatable :: Filename
   end type

contains
   subroutine gen2dmesh(dx, dy, nx, ny, eltype, nodes, elements)
      real(wp), intent(in) :: dx, dy
      integer(ip), intent(in) :: nx, ny
      character(len=5), intent(in) :: eltype
      integer(ip), allocatable :: elements(:,:)
      real(wp), allocatable :: nodes(:, :)

      integer(ip) ::  nnodes, nels, npel, i, j, k

      !> checks
      if (nx <= 0_ip .OR. ny <= 0_ip) then
         print *, "Error while generating mesh in 'gen2dmesh':"
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
      k = 1 ! Start node index for the current element row
      do j = 1, ny
          do i = 1, nx
              ! Corrected node numbering for a quad element
              elements( (j-1)*nx + i, 1 ) = k
              elements( (j-1)*nx + i, 2 ) = k + 1
              elements( (j-1)*nx + i, 3 ) = k + nx + 2
              elements( (j-1)*nx + i, 4 ) = k + nx + 1
              k = k + 1 ! Move to the next node in the row
          end do
          k = k + 1 ! Move to the first node of the next row
      end do

      print*, "mesh_generated!"
   end subroutine gen2dmesh
end module m_mesh
