program unit_m_crypto

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests driver for m_crypto
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

  use unit_m_crypto_tests

!---End of use statements-------------------------------------------------------

  implicit none

  character(len=*), parameter :: package = 'crypto'
  character(len=*), parameter :: module = 'm_crypto'
  
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
                        source='m_crypto.f03', &
                        annotation='Generic cryptographic interface' )

! Create test
  ut = xfunit_unit( name='unit_m_crypto_test_001', &
                    classname="t_crypto", &
                    executer=unit_m_crypto_test_001, &
                    before=unit_m_crypto_before_001, &
                    after=unit_m_crypto_after_001, &
                    annotation='AES256 from NIST CAVP' )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_m_crypto_test_002', &
                    classname="t_crypto", &
                    executer=unit_m_crypto_test_002, &
                    before=unit_m_crypto_before_002, &
                    after=unit_m_crypto_after_002, &
                    annotation='3-DES from NIST CAVP' )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_m_crypto_test_003', &
                    classname="t_crypto", &
                    executer=unit_m_crypto_test_003, &
                    before=unit_m_crypto_before_003, &
                    after=unit_m_crypto_after_003, &
                    annotation='Bloswfish from CAVP' )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_m_crypto_test_004', &
                    classname="t_crypto", &
                    executer=unit_m_crypto_test_004, &
                    annotation='Padding' )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_m_crypto_test_005', &
                    classname="t_crypto", &
                    executer=unit_m_crypto_test_005, &
                    annotation='Encrypt file' )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_m_crypto_test_006', &
                    classname="t_crypto", &
                    executer=unit_m_crypto_test_006, &
                    annotation='Decrypt file' )
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

end program unit_m_crypto

