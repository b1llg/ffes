program main
  use m_precision, only : wp, ip
  use m_mesh, only: generate2dgrid

  implicit none
  real(wp), allocatable :: nodes(:,:)
  integer(ip), allocatable :: elems(:,:)
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

  call generate2dgrid(1.0_wp, 1.0_wp, 3_ip, 3_ip, 'E2LN8', nodes, elems)

  deallocate(nodes)
  deallocate(elems)

end program main
