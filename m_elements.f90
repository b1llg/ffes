module m_elements
   use m_precision, only: ip, wp
   implicit none
   
   private 

   public E1QD, E2Q8

   type, abstract :: Element
      integer(ip) :: id
      integer :: ndim
      integer :: nnodes   
   end type Element

   type, extends(Element), abstract :: E1D
      integer :: ndim = 1
   end type E1D

   type, extends(Element), abstract :: E2D
      integer :: ndim = 2

   end type E2D

   type, extends(E1D) :: E1QD
      integer :: nnodes = 3

   end type E1QD

   type, extends(E2D) :: E2Q8
      intege :: nnodes = 8

   end type E2Q8
   
contains
   

end module m_elements