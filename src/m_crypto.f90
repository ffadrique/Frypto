module m_crypto

!------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Encryption/decryption front-end
!             Implements the encryption modes ECB/CBC/PCBC/CFB/OFB
!             http://en.wikipedia.org/wiki/Block_cipher_modes_of_operation
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
!------------------------------------------------------------------------------

!- Start of use statements ----------------------------------------------------

  use, intrinsic :: iso_fortran_env
  
  use m_object
  use m_file_handler
  use m_messages
  use m_util_convert

  use m_crypto_base
  use m_crypto_padding
  
!- End of use statements ------------------------------------------------------

  implicit none

!- Start of Public/Private declarations ---------------------------------------

  private
  public t_crypto
  public crypto

  public crypto_aes256
  public crypto_triple_des
  public crypto_blowfish
  
  public crypto_mode_ecb
  public crypto_mode_cbc
  public crypto_mode_pcbc
  public crypto_mode_cfb
  public crypto_mode_ofb

!- End of Public/Private declarations -----------------------------------------

!- Start of module variable declarations --------------------------------------

! Encryption algorithm enumeration
  integer, parameter :: crypto_aes256     = crypto_base_aes256    
  integer, parameter :: crypto_triple_des = crypto_base_triple_des
  integer, parameter :: crypto_blowfish   = crypto_base_blowfish  

! Encryption mode eunmeration
  integer, parameter :: crypto_mode_ecb  = crypto_base_mode_ecb 
  integer, parameter :: crypto_mode_cbc  = crypto_base_mode_cbc 
  integer, parameter :: crypto_mode_pcbc = crypto_base_mode_pcbc
  integer, parameter :: crypto_mode_cfb  = crypto_base_mode_cfb 
  integer, parameter :: crypto_mode_ofb  = crypto_base_mode_ofb 
  
! Number of block to read into buffer at once
  integer, parameter :: crypto_blocks_from_buffer = 10
  
! Size of the reading buffer
  integer, parameter :: crypto_buffer_size = 1024
  
! The encryption type
  type, extends(t_crypto_base) :: t_crypto
    private

!     Read buffer
      integer(kind=1), dimension(crypto_buffer_size) :: buffer = 0_1

!     Counter of bytes in buffer
      integer :: buffer_count = 0

!     Fortran input unit
      integer :: lunin = -1

!     Fortran output unit
      integer :: lunout = -1

!     Flag to detect input EOF
      logical :: lunin_eof = .false.

    contains

!     Encryption interface
      procedure :: encrypt_file => crypto_encrypt_file

!     Decryption interface
      procedure :: decrypt_file => crypto_decrypt_file

!     Get text from buffer
      procedure, private :: get_data_from_buffer => crypto_get_data_from_buffer

!     Read data into the buffer
      procedure, private :: read_data_into_buffer => crypto_read_data_into_buffer

  end type t_crypto

!- End of module variable declarations ----------------------------------------

contains

! Constuctor
pure function crypto( algorithm, key, mode, padding, inivec ) result(res)

! The algorithm
  integer, intent(in) :: algorithm

! The initialisation key
  integer(kind=1), dimension(:), intent(in) :: key

! The encryption mode
  integer, optional, intent(in) :: mode

! The encryption padding scheme
  integer, optional, intent(in) :: padding

! The initial vector (when applicable for mode)
  integer(kind=1), optional, dimension(:), intent(in) :: inivec

! Constructed object
  type(t_crypto) :: res
  
! Initialise
  res%t_crypto_base = crypto_base( algorithm, key, mode, padding, inivec )
  
! Initialise buffer
  res%buffer = 0_1
  
end function crypto


! Encryption
subroutine crypto_encrypt_file( this, text_file_name, cipher_file_name, msg )

! The crypto context
  class(t_crypto), intent(inout) :: this

! The file name with the plain text to encrypt
  character(len=*), intent(in) :: text_file_name

! The file name with the encrypted text
  character(len=*), intent(in) :: cipher_file_name

! Error handling structure
  type(t_messages), intent(inout) :: msg

! Local variables
  type(t_file_handler) :: text_handler, cipher_handler
  integer :: text_size
  integer(kind=1), dimension(:), allocatable :: text, cipher
  character(len=:), allocatable :: errmsg

! Open the input file
  text_handler = file_handler( text_file_name )
  call text_handler%open( write=.false., stream=.true. )
  if( text_handler%is_open() ) then

!   Open the output file
    cipher_handler = file_handler( cipher_file_name )
    call cipher_handler%open( write=.true., stream=.true. )
    if( cipher_handler%is_open() ) then
  
!     Initialise the buffer retrieval parameters
      text_size = crypto_blocks_from_buffer * this%block_size()
      this%buffer_count = 0

!     Assign read/write units
      this%lunin = text_handler%get_unit()
      this%lunout = cipher_handler%get_unit()
      
!     Loop reading the file
      do while( .not. msg%on_error() )

!       Update buffers
        call this%get_data_from_buffer( text_size, text, msg )
        if( .not. msg%on_error() ) then

!         Encrypt this fragment of text
          call this%encrypt( text, cipher )

!         Write output
          write( this%lunout ) cipher

!         Release encrypted text
          deallocate( cipher )

!         Check status
          if( this%buffer_count == 0 .and. this%lunin_eof ) then
            exit
          end if

        else
          errmsg = "Getting data from buffer; iostat=" // character(text_handler%get_iostat())
          call msg%error( 'm_crypto', 'crypto_encrypt_file', 3, errmsg )
        end if

      end do
    
  !   Close files
      call text_handler%close()
      call cipher_handler%close()
      
    else
      errmsg = "Cannot open ouput file '" // trim(cipher_file_name) // "'; iostat=" // character(cipher_handler%get_iostat())
      call msg%error( 'm_crypto', 'crypto_encrypt_file', 2, errmsg )
    end if
    
  else
    errmsg = "Cannot open input file '" // trim(text_file_name) // "'; iostat=" // character(text_handler%get_iostat())
    call msg%error( 'm_crypto', 'crypto_encrypt_file', 1, errmsg )
  end if
  
end subroutine crypto_encrypt_file


! Decryption
subroutine crypto_decrypt_file( this, cipher_file_name, text_file_name, msg )

! The crypto context
  class(t_crypto), intent(inout) :: this

! The file name with the encrypted text to decrypt
  character(len=*), intent(in) :: cipher_file_name

! The file name with the plain text
  character(len=*), intent(in) :: text_file_name

! Error handling structure
  type(t_messages), intent(inout) :: msg

! Local variables
  type(t_file_handler) :: text_handler, cipher_handler
  integer :: cipher_size
  integer(kind=1), dimension(:), allocatable :: text, cipher, trail
  type(t_crypto_padding) :: padding
  character(len=:), allocatable :: errmsg

! Open the input file
  cipher_handler = file_handler( cipher_file_name )
  call cipher_handler%open( write=.false., stream=.true. )
  if( cipher_handler%is_open() ) then

!   Open the output file
    text_handler = file_handler( text_file_name )
    call text_handler%open( write=.true., stream=.true. )
    if( text_handler%is_open() ) then

!     Initialise the buffer retrieval parameters
      cipher_size = crypto_blocks_from_buffer * this%block_size()
      this%buffer_count = 0

!     Assign read/write units
      this%lunin = cipher_handler%get_unit()
      this%lunout = text_handler%get_unit()

!     Loop reading the file
      do while( .not. msg%on_error() )

!       Update buffers
        call this%get_data_from_buffer( cipher_size, cipher, msg )
        if( .not. msg%on_error() ) then

!         Decrypt this fragment of text
          call this%decrypt( cipher, text )

!         If this is the last block try to remove padding
          if( this%buffer_count == 0 ) then
            call padding%remove( text, trail )
            text = trail
          end if

!         Write output
          write( this%lunout ) text

!         Release decrypted text
          deallocate( text )

!         Check status
          if( this%buffer_count == 0 ) then
            exit
          end if

        else
          errmsg = "Getting data from buffer; iostat=" // character(cipher_handler%get_iostat())
          call msg%error( 'm_crypto', 'crypto_decrypt_file', 3, errmsg )
        end if

      end do
  
    else
      errmsg = "Cannot open ouput file '" // trim(text_file_name) // "'; iostat=" // character(text_handler%get_iostat())
      call msg%error( 'm_crypto', 'crypto_edecrypt_file', 2, errmsg )
    end if
    
  else
    errmsg = "Cannot open input file '" // trim(cipher_file_name) // "'; iostat=" // character(cipher_handler%get_iostat())
    call msg%error( 'm_crypto', 'crypto_decrypt_file', 1, errmsg )
  end if

end subroutine crypto_decrypt_file


! Get text from buffer
subroutine crypto_get_data_from_buffer( this, text_size, text, msg )

! The crypto context
  class(t_crypto), intent(inout) :: this

! The requested number of bytes to be retrieved from the buffer
! Actual size is adjusted to block size
  integer, intent(in) :: text_size

! The text retrieved from buffer
  integer(kind=1), dimension(:), allocatable, intent(out) :: text

! Error handling structure
  type(t_messages), intent(inout) :: msg

! Local variables
  integer :: the_size
  integer :: block_size

! Initialise retrieval size
  block_size = this%block_size()
  the_size = ( text_size / block_size ) * block_size
  if( the_size > size(this%buffer) ) then
    the_size = size(this%buffer)
  end if

! Check the amount of text loaded in the buffer
  if( this%buffer_count < the_size ) then

!   Read more data into buffer
    call this%read_data_into_buffer( msg )

  end if

! Verify process to this point
  if( .not. msg%on_error() ) then

!   Process the data loaded in the buffer
    if( this%buffer_count < the_size ) then

!     Set the size to the available data
      the_size = this%buffer_count

    end if

!   Get the data from the buffer
    allocate( text(the_size) )
    text = this%buffer(:the_size)

!   Remove data from buffer
    this%buffer = eoshift( this%buffer, the_size, 0_1 )
!    this%buffer = this%buffer(the_size+1:)
    this%buffer_count = this%buffer_count - the_size

  else

      call msg%error( 'm_crypto', 'get_data_from_buffer', 1, 'Reading text into buffer' )

  end if

end subroutine crypto_get_data_from_buffer


! Read data into the buffer
subroutine crypto_read_data_into_buffer( this, msg )

! The crypto file context
  class(t_crypto), intent(inout) :: this

! Error handling structure
  type(t_messages), intent(inout) :: msg

! Local variables
  integer :: ios
  integer :: k

! Check the data in the buffer
  if( .not. this%lunin_eof ) then

!   Read data into buffer
    this%buffer(this%buffer_count+1:) = 0_1
    read( this%lunin, iostat=ios ) (this%buffer(k),k=this%buffer_count+1,size(this%buffer))

!   Process return status
    select case( ios )

!     End of file
      case(iostat_end)
        this%lunin_eof = .true.
        this%buffer_count = k - 1

!     Succesful read
      case(0)
        this%buffer_count = size(this%buffer)

!     Error
      case(1:)
        this%buffer_count = 0
        call msg%error( 'm_crypto', 'crypto_read', &
                    1, 'Reading data from file' )

    end select

  end if

end subroutine crypto_read_data_into_buffer

end module m_crypto
