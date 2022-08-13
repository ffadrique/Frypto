program unit_m_triple_des

!-------------------------------------------------------------------------------
! Copyright : Fran Martinez Fadrique
! Project   : FORTRAN
! Author    : Fran Martinez Fadrique
! Language  : Fortran 95
! Synopsis  : Unit tests for m_triple_des
!-------------------------------------------------------------------------------

!---USE statements--------------------------------------------------------------

  use m_unit_support

  use m_util_convert
  use m_triple_des

!---End of use statements-------------------------------------------------------

  implicit none

  character(len=*), parameter :: package = 'crypto'
  
  character(len=130), parameter :: sccs_info = &
  '$Id: $'

!---Executable Code-------------------------------------------------------------

! Initialise unit test
  call unit_initialise()

! Select the test case
  select case( unit_case() )
  
    case(1)

!     unit_m_triple_des_test_001
      call unit_header( 'unit_m_triple_des_test_001' )
      call unit_m_triple_des_test_001
      call unit_footer( 'unit_m_triple_des_test_001' )

    case(2)

!     unit_m_triple_des_test_002
      call unit_header( 'unit_m_triple_des_test_002' )
      call unit_m_triple_des_test_002
      call unit_footer( 'unit_m_triple_des_test_002' )

    case(10)

!     unit_m_des_test_010
      call unit_header( 'unit_m_des_test_010' )
      call unit_m_des_test_010
      call unit_footer( 'unit_m_des_test_010' )

    case default
    
  end select

contains

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_triple_des_test_001

! The DES context
  type(t_triple_des) :: ctx

! Local variables
  integer(kind=1), dimension(24)  :: key
  integer(kind=1), dimension(8)  :: text
  integer(kind=1), dimension(8)  :: cipher


! Initialise
  key  = character_to_bytes( '123456789012345678901234' )
  text = character_to_bytes( 'codetext' )
  ctx = triple_des( key )

! Dump key
  write( 6, '(a,24(1x,z2))' ) 'Key     :', key
  call unit_separator

! Encrypt
  call ctx%encrypt( text, cipher )

! Get the results
  write( 6, '(a,8(1x,z2))' ) 'Clear   :', text
  write( 6, '(a,8(1x,z2))' ) 'Cipher  :', cipher
  write( 6, '(a,1x,a)'     ) 'Ref.    :', 'a5 ca dc 4b 41 a9 ad 81'  
  call unit_separator

! Decrypt
  call ctx%decrypt( cipher, text )

! Get the results
  write( 6, '(a,8(1x,z2))' ) 'Cipher  :', cipher
  write( 6, '(a,8(1x,z2))' ) 'Clear   :', text
  write( 6, '(a,1x,a)'     ) 'Ref.    :', '63 6f 64 65 74 65 78 74'

end subroutine unit_m_triple_des_test_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_triple_des_test_002

! The DES context
  type(t_triple_des) :: ctx

! Local variables
  integer(kind=1), dimension(16)  :: key
  integer(kind=1), dimension(8)  :: text
  integer(kind=1), dimension(8)  :: cipher


! Initialise
  key  = character_to_bytes( '1234567890123456' )
  text = character_to_bytes( 'codetext' )
  ctx = triple_des( key )

! Dump key
  write( 6, '(a,24(1x,z2))' ) 'Key     :', key
  call unit_separator

! Encrypt
  call ctx%encrypt( text, cipher )

! Get the results
  write( 6, '(a,8(1x,z2))' ) 'Clear   :', text
  write( 6, '(a,8(1x,z2))' ) 'Cipher  :', cipher
  write( 6, '(a,1x,a)'     ) 'Ref.    :', '5a 98 f8 c3 4e 3a ca c7'  
  call unit_separator

! Decrypt
  call ctx%decrypt( cipher, text )

! Get the results
  write( 6, '(a,8(1x,z2))' ) 'Cipher  :', cipher
  write( 6, '(a,8(1x,z2))' ) 'Clear   :', text
  write( 6, '(a,1x,a)'     ) 'Ref.    :', '63 6f 64 65 74 65 78 74'

end subroutine unit_m_triple_des_test_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_des_test_010

! The encryption direction and mode
  character(len=16) :: direction
  integer(kind=1) :: mode

! The list of test cases
  character(len=*), dimension(8), parameter :: input_case = (/ &
       'ECBVarTxte.txt     ',  'ECBVarTxtd.txt     ',  &
       'ECBVarKeye.txt     ',  'ECBVarKeyd.txt     ',  &
       'ECBPerKeye.txt     ',  'ECBPerKeyd.txt     ',  &
       'ECBSubTabe.txt     ',  'ECBSubTabd.txt     ' /)   

! The input file variables
  integer, parameter :: unit = 77
  character(len=128) :: input

! Auxiliary variables
  integer :: idx
  integer :: ios
  integer :: result
  integer :: iinput


! Loop on the input cases
  do iinput = 1, size(input_case)

!   Set input file name
    input = trim(unit_data_path(package))//'/des/'//input_case(iinput)

!   Open the input file
    open( unit, file=input, status='old', action='read', iostat=ios )
    if( ios == 0 ) then

!     Read the first record
      read( unit, '(a)', iostat=ios ) direction
      if( ios == 0 ) then
      
!       Trace direction
        direction = direction(2:len_trim(direction)-1)
        write( 6, '(a,3x,a)' ) trim(direction), input_case(iinput)

!       Loop processing blocks
        do
        
!         Proccess block
          call process_block( unit, direction, result, ios )
          if( ios <= 0 ) then
            if( result == 0 ) then
              write( 6, '(a)' ) 'PASSED'
            else
              write( 6, '(a)' ) 'FAILED'
            end if
          end if

!         Check end of file or error
          if( ios < 0 ) then
            exit
          end if

        end do

!       Separator
        call unit_separator

      end if
  
    end if

  end do

end subroutine unit_m_des_test_010

! ############################################################################
! ############################################################################
! ############################################################################

! Process one block from the input file
subroutine process_block( unit, direction, result, ios )

! The open fortran unit
  integer,          intent(in)  :: unit

! The processin direction (ENCRYPT/DECRYPT)
  character(len=*), intent(in)  :: direction

! The block processing result
  integer,          intent(out) :: result
  
! The block processing status
  integer,          intent(out) :: ios

! The AES context structure
  type(t_triple_des) :: ctx

! The information for a test case
  integer :: counter
  integer(kind=1), dimension(24) :: key
  integer(kind=1), dimension(16) :: iv
  integer(kind=1), dimension(:), allocatable :: ciphertext
  integer(kind=1), dimension(:), allocatable :: plaintext
  integer(kind=1), dimension(:), allocatable :: output
  
! Local variables
  character(len=128) :: rec
  character(len=16)  :: ckey
  character(len=128) :: cvalue
  integer :: idx
  integer :: n
  character(len=16) :: fmt


! Read records until not empty or end of file
  rec = ' '
  ios = 0
  do while( len_trim(rec) == 0 .and. ios == 0 )
    read( unit, '(a128)', iostat=ios ) rec
  end do

! Now loop until next empty record found
  counter = -1
  do while( len_trim(rec) /= 0 .and. ios == 0 )

!   Split record contents
    idx = index( rec, '=' )
    ckey = trim(adjustl(rec(:idx-1)))
    cvalue = trim(adjustl(rec(idx+1:)))

!   Check the record contents
    select case( ckey )

      case( 'COUNT' )
        read( cvalue, * ) counter
    
      case( 'KEY' )
        read( cvalue, '(32z2)' ) key
    
      case( 'IV' )
        read( cvalue, '(32z2)' ) iv

      case( 'CIPHERTEXT' )
        n = len_trim(adjustl(cvalue)) / 2
        allocate( ciphertext(n) )
        write( fmt, '(a,i0,a)' ) '(', n, 'z2)'
        read( cvalue, fmt ) ciphertext

      case( 'PLAINTEXT' )
        n = len_trim(adjustl(cvalue)) / 2
        allocate( plaintext(n) )
        write( fmt, '(a,i0,a)' ) '(', n, 'z2)'
        read( cvalue, fmt ) plaintext
    
    end select

!   Read next record
    read( unit, '(a128)', iostat=ios ) rec

  end do

! Check status
  if( ios <= 0 .and. counter >= 0 ) then

!   Initiate output
    write( 6, '(a)' )
    write( 6, '(a,i0)' ) 'COUNT      = ', counter
    write( 6, '(a,1000z2.2)' )  'KEY        = ', key
    write( 6, '(a,1000z2.2)' )  'IV         = ', iv

!   Initialise AES
    ctx = triple_des( key )
    allocate( output(size(ciphertext)) )

!   Process the data block
    select case( direction )

      case( 'ENCRYPT' )
        call ctx%encrypt( plaintext, output )
        write( 6, '(a,1000z2.2)' )  'PLAINTEXT  = ', plaintext
        write( 6, '(a,1000z2.2)' )  'CIPHERTEXT = ', ciphertext
        write( 6, '(a,1000z2.2)' )  'OUTPUT     = ', output
        write( 6, '(a,1000z2.2)' )  'DIFFERENCE = ', output - ciphertext
        result = sum( output - ciphertext )

      case( 'DECRYPT' )
        call ctx%decrypt( ciphertext, output )
        write( 6, '(a,1000z2.2)' )  'CIPHERTEXT = ', ciphertext
        write( 6, '(a,1000z2.2)' )  'PLAINTEXT  = ', plaintext
        write( 6, '(a,1000z2.2)' )  'OUTPUT     = ', output
        write( 6, '(a,1000z2.2)' )  'DIFFERENCE = ', output - plaintext
        result = sum( output - plaintext )

    end select


  end if

end subroutine process_block

! ############################################################################
! ############################################################################
! ############################################################################

end program unit_m_triple_des

