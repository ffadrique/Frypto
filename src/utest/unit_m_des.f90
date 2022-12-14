program unit_m_des

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests driver for m_des
!
! License   : This file is part of Frypto.
!
!             Frypto is free software: you can redistribute it and/or modify
!             it under the terms of the GNU Lesser General Public License as
!             published by the Free Software Foundation, either version 3 of
!             the License, or (at your option) any later version.
!
!             Frypto is distributed in the hope that it will be useful,
!             but WITHOUT ANY WARRANTY; without even the implied warranty of
!             MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
!             See the GNU Lesser General Public License for more details.
!
!             You should have received a copy of the GNU Lesser General Public
!             License along with Frypto.  
!             If not, see <http://www.gnu.org/licenses/>.
!-------------------------------------------------------------------------------

!---USE statements--------------------------------------------------------------

  use m_xfunit

  use unit_m_des_tests

!---End of use statements-------------------------------------------------------

  implicit none

  character(len=*), parameter :: package = 'crypto'
  character(len=*), parameter :: module = 'm_des'
  
!---Declaration of local variables----------------------------------------------


!---End of declaration of local variables---------------------------------------

!---Executable Code-------------------------------------------------------------

! Local variables
  character(len=256) :: xfunit_root_dir
  logical :: junit_strict
  type(t_xfunit_suite), allocatable :: suite
  type(t_xfunit_unit), allocatable :: ut

! Initialise report generation flag
  junit_strict = .false.

! Initialise unit test infrastructure
  allocate( manager )
  allocate( suite )
  allocate( ut )

! Initialise the unit test manager
  call get_environment_variable( 'XFUNIT_ROOT_DIR', xfunit_root_dir )
  manager = xfunit_manager_eclipse( module, xfunit_root_dir, junit_strict )

! Initialise test suite
  suite = xfunit_suite( package=package, &
                        source='m_des.f03', &
                        before=unit_m_des_suite_before, &
                        after=unit_m_des_suite_after, &
                        annotation='DES block encryption' )

! Create test
  ut = xfunit_unit( name='unit_m_des_test_001', &
                    classname="t_des", &
                    executer=unit_m_des_test_001, &
                    before=unit_m_des_before_001, &
                    after=unit_m_des_after_001, &
                    annotation='Permutate function' )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_m_des_test_002', &
                    classname="t_des", &
                    executer=unit_m_des_test_002, &
                    before=unit_m_des_before_002, &
                    after=unit_m_des_after_002, &
                    annotation='Crypto function' )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_m_des_test_003', &
                    classname="t_des", &
                    executer=unit_m_des_test_003, &
                    before=unit_m_des_before_003, &
                    after=unit_m_des_after_003, &
                    annotation='DES block encryption and decryption' )
  call suite%add_unit_test( ut )

! Execute tests
  call manager%execute( suite )
  if( manager%is_error() ) then
    call manager%dump_error( 0 )
  end if

! Generate output
  call manager%write_xml( suite )

! Terminate unit test infrastructure
  deallocate( manager )
  deallocate( suite )
  deallocate( ut )

end program unit_m_des

