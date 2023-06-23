subroutine read_basis(nNuc,rNuc,nBas,nO,nV,nShell,TotAngMomShell,CenterShell,KShell,DShell,ExpShell)

! Read basis set information

  implicit none
  include 'parameters.h'

! Input variables

  integer,intent(in)            :: nNuc,nO(nspin)
  double precision,intent(in)   :: rNuc(nNuc,ncart)

! Local variables

  integer                       :: nShAt,iNuc,iShell
  integer                       :: i,j,k,kk
  character                     :: shelltype

! Output variables

  integer,intent(out)           :: nShell,nBas
  double precision,intent(out)  :: CenterShell(maxShell,ncart)
  integer,intent(out)           :: TotAngMomShell(maxShell),KShell(maxShell)
  double precision,intent(out)  :: DShell(maxShell,maxK),ExpShell(maxShell,maxK)
  integer,intent(out)           :: nV(nspin)

!------------------------------------------------------------------------
! Primary basis set information
!------------------------------------------------------------------------

! Open file with basis set specification

  open(unit=2,file='input/basis')

! Read basis information

  write(*,'(A28)') 'Gaussian basis set'
  write(*,'(A28)') '------------------'

! Initailization

  nShell = 0

!------------------------------------------------------------------------
! Loop over atoms
!------------------------------------------------------------------------
  do i=1,nNuc

    read(2,*) iNuc,nShAt
    write(*,'(A28,1X,I16)') 'Atom n. ',iNuc
    write(*,'(A28,1X,I16)') 'number of shells ',nShAt
    write(*,'(A28)') '------------------'

!------------------------------------------------------------------------
! Loop over shells
!------------------------------------------------------------------------
    do j=1,nShAt

      nShell = nShell + 1

     ! Basis function centers

      do k=1,ncart
        CenterShell(nShell,k) = rNuc(iNuc,k)
      enddo

      ! Shell type and contraction degree

      read(2,*) shelltype,KShell(nShell)

      select case (shelltype)
        case ("S") 

          TotAngMomShell(nShell) = 0
          write(*,'(A28,1X,I16)') 's-type shell with K = ',KShell(nShell)

        case ("P")

          TotAngMomShell(nShell) = 1
           write(*,'(A28,1X,I16)') 'p-type shell with K = ',KShell(nShell)

        case ("D")

          TotAngMomShell(nShell) = 2
          write(*,'(A28,1X,I16)') 'd-type shell with K = ',KShell(nShell)

        case ("F")

          TotAngMomShell(nShell) = 3
          write(*,'(A28,1X,I16)') 'f-type shell with K = ',KShell(nShell)

        case ("G")

          TotAngMomShell(nShell) = 4
          write(*,'(A28,1X,I16)') 'g-type shell with K = ',KShell(nShell)

        case ("H")

          TotAngMomShell(nShell) = 5
          write(*,'(A28,1X,I16)') 'h-type shell with K = ',KShell(nShell)

        case ("I")

          TotAngMomShell(nShell) = 6
          write(*,'(A28,1X,I16)') 'i-type shell with K = ',KShell(nShell)

        case ("J")

          TotAngMomShell(nShell) = 7
          write(*,'(A28,1X,I16)') 'j-type shell with K = ',KShell(nShell)

        case default 

         call print_warning('!!! Angular momentum too high !!!')
         stop

      end select

! Read exponents and contraction coefficients

      write(*,'(A28,1X,A16,A16)') '','Exponents','Contraction'
      do k=1,Kshell(nShell)
        read(2,*) kk,ExpShell(nShell,k),DShell(nShell,k)
        write(*,'(A28,1X,F16.10,F16.10)') '',ExpShell(nShell,k),DShell(nShell,k)
      enddo

    enddo
!------------------------------------------------------------------------
!   End loop over shells
!------------------------------------------------------------------------

    write(*,'(A28)') '------------------'

  enddo
!------------------------------------------------------------------------
! End loop over atoms
!------------------------------------------------------------------------

! Total number of shells

  write(*,'(A28,1X,I16)') 'Number of shells',nShell
  write(*,'(A28)') '------------------'
  write(*,*)

! Close file with basis set specification

  close(unit=2)

! Calculate number of basis functions

  nBas = 0
  do iShell=1,nShell
    nBas = nBas + (TotAngMomShell(iShell)*TotAngMomShell(iShell) + 3*TotAngMomShell(iShell) + 2)/2
  enddo

  write(*,'(A28)') '------------------'
  write(*,'(A28,1X,I16)') 'Number of basis functions',NBas
  write(*,'(A28)') '------------------'
  write(*,*)

! Number of virtual orbitals

  nV(:) = nBas - nO(:)

end subroutine read_basis
