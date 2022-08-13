module m_blowfish

!------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Blowfish crypto implementation
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

  use m_block_cipher

!- End of use statements ------------------------------------------------------

  implicit none

!- Start of Public/Private declarations ---------------------------------------

  private
  public t_blowfish
  public blowfish

!- End of Public/Private declarations -----------------------------------------

!- Start of module variable declarations --------------------------------------

! Numeric constants
  integer(kind=1), parameter :: z00 = int(z'00',1)
  integer(kind=1), parameter :: z01 = int(z'01',1)
  integer(kind=1), parameter :: zff = int(z'ff',1)
  integer(kind=4), parameter :: z000000ff = int(z'ff')
  integer(kind=4), parameter :: zffffffff = int(z'ffffffff')

! Blowfish type
  type, extends(t_block_cipher) :: t_blowfish
    private

!     Working area (P)
      integer(kind=4), dimension(16+2) :: p = 0

!     Working area (S)
      integer(kind=4), dimension(256,4) :: s = 0

!     Encryption key
      integer(kind=1), dimension(:), allocatable :: key

    contains

!     Block size
      procedure, nopass :: block_size => blowfish_block_size
      
!     Key size
      procedure, nopass :: key_size => blowfish_key_size
      
!     Encryption interface
      procedure :: encrypt => blowfish_encrypt

!     Decryption interface
      procedure :: decrypt => blowfish_decrypt

!     The blowfish encrypt core algorithm
      procedure, private :: encrypt_lr => blowfish_encrypt_lr

!     The blowfish decrypt core algorithm
      procedure, private :: decrypt_lr => blowfish_decrypt_lr

!     Auxiliary function
      procedure, private :: F => blowfish_F

  end type t_blowfish
  
! The Blowfish block size in bytes
  integer, parameter :: blowfish_block_size_in_bytes = 8

! The Blowfish key size in bytes
  integer, parameter :: blowfish_key_size_in_bytes = 8

! The Blowfish lookup tables
  integer(kind=4), dimension(16+2), parameter :: orig_p = &
    [ int(z'243f6a88'), int(z'85a308d3'), int(z'13198a2e'), int(z'03707344'), &
      int(z'a4093822'), int(z'299f31d0'), int(z'082efa98'), int(z'ec4e6c89'), &
      int(z'452821e6'), int(z'38d01377'), int(z'be5466cf'), int(z'34e90c6c'), &
      int(z'c0ac29b7'), int(z'c97c50dd'), int(z'3f84d5b5'), int(z'b5470917'), &
      int(z'9216d5d9'), int(z'8979fb1b') ]

  integer(kind=4), dimension(256,4), parameter :: orig_s = reshape( [ &
      int(z'd1310ba6'), int(z'98dfb5ac'), int(z'2ffd72db'), int(z'd01adfb7'), int(z'b8e1afed'), &
      int(z'6a267e96'), int(z'ba7c9045'), int(z'f12c7f99'), int(z'24a19947'), int(z'b3916cf7'), &
      int(z'0801f2e2'), int(z'858efc16'), int(z'636920d8'), int(z'71574e69'), int(z'a458fea3'), &
      int(z'f4933d7e'), int(z'0d95748f'), int(z'728eb658'), int(z'718bcd58'), int(z'82154aee'), &
      int(z'7b54a41d'), int(z'c25a59b5'), int(z'9c30d539'), int(z'2af26013'), int(z'c5d1b023'), &
      int(z'286085f0'), int(z'ca417918'), int(z'b8db38ef'), int(z'8e79dcb0'), int(z'603a180e'), &
      int(z'6c9e0e8b'), int(z'b01e8a3e'), int(z'd71577c1'), int(z'bd314b27'), int(z'78af2fda'), &
      int(z'55605c60'), int(z'e65525f3'), int(z'aa55ab94'), int(z'57489862'), int(z'63e81440'), &
      int(z'55ca396a'), int(z'2aab10b6'), int(z'b4cc5c34'), int(z'1141e8ce'), int(z'a15486af'), &
      int(z'7c72e993'), int(z'b3ee1411'), int(z'636fbc2a'), int(z'2ba9c55d'), int(z'741831f6'), &
      int(z'ce5c3e16'), int(z'9b87931e'), int(z'afd6ba33'), int(z'6c24cf5c'), int(z'7a325381'), &
      int(z'28958677'), int(z'3b8f4898'), int(z'6b4bb9af'), int(z'c4bfe81b'), int(z'66282193'), &
      int(z'61d809cc'), int(z'fb21a991'), int(z'487cac60'), int(z'5dec8032'), int(z'ef845d5d'), &
      int(z'e98575b1'), int(z'dc262302'), int(z'eb651b88'), int(z'23893e81'), int(z'd396acc5'), &
      int(z'0f6d6ff3'), int(z'83f44239'), int(z'2e0b4482'), int(z'a4842004'), int(z'69c8f04a'), &
      int(z'9e1f9b5e'), int(z'21c66842'), int(z'f6e96c9a'), int(z'670c9c61'), int(z'abd388f0'), &
      int(z'6a51a0d2'), int(z'd8542f68'), int(z'960fa728'), int(z'ab5133a3'), int(z'6eef0b6c'), &
      int(z'137a3be4'), int(z'ba3bf050'), int(z'7efb2a98'), int(z'a1f1651d'), int(z'39af0176'), &
      int(z'66ca593e'), int(z'82430e88'), int(z'8cee8619'), int(z'456f9fb4'), int(z'7d84a5c3'), &
      int(z'3b8b5ebe'), int(z'e06f75d8'), int(z'85c12073'), int(z'401a449f'), int(z'56c16aa6'), &
      int(z'4ed3aa62'), int(z'363f7706'), int(z'1bfedf72'), int(z'429b023d'), int(z'37d0d724'), &
      int(z'd00a1248'), int(z'db0fead3'), int(z'49f1c09b'), int(z'075372c9'), int(z'80991b7b'), &
      int(z'25d479d8'), int(z'f6e8def7'), int(z'e3fe501a'), int(z'b6794c3b'), int(z'976ce0bd'), &
      int(z'04c006ba'), int(z'c1a94fb6'), int(z'409f60c4'), int(z'5e5c9ec2'), int(z'196a2463'), &
      int(z'68fb6faf'), int(z'3e6c53b5'), int(z'1339b2eb'), int(z'3b52ec6f'), int(z'6dfc511f'), &
      int(z'9b30952c'), int(z'cc814544'), int(z'af5ebd09'), int(z'bee3d004'), int(z'de334afd'), &
      int(z'660f2807'), int(z'192e4bb3'), int(z'c0cba857'), int(z'45c8740f'), int(z'd20b5f39'), &
      int(z'b9d3fbdb'), int(z'5579c0bd'), int(z'1a60320a'), int(z'd6a100c6'), int(z'402c7279'), &
      int(z'679f25fe'), int(z'fb1fa3cc'), int(z'8ea5e9f8'), int(z'db3222f8'), int(z'3c7516df'), &
      int(z'fd616b15'), int(z'2f501ec8'), int(z'ad0552ab'), int(z'323db5fa'), int(z'fd238760'), &
      int(z'53317b48'), int(z'3e00df82'), int(z'9e5c57bb'), int(z'ca6f8ca0'), int(z'1a87562e'), &
      int(z'df1769db'), int(z'd542a8f6'), int(z'287effc3'), int(z'ac6732c6'), int(z'8c4f5573'), &
      int(z'695b27b0'), int(z'bbca58c8'), int(z'e1ffa35d'), int(z'b8f011a0'), int(z'10fa3d98'), &
      int(z'fd2183b8'), int(z'4afcb56c'), int(z'2dd1d35b'), int(z'9a53e479'), int(z'b6f84565'), &
      int(z'd28e49bc'), int(z'4bfb9790'), int(z'e1ddf2da'), int(z'a4cb7e33'), int(z'62fb1341'), &
      int(z'cee4c6e8'), int(z'ef20cada'), int(z'36774c01'), int(z'd07e9efe'), int(z'2bf11fb4'), &
      int(z'95dbda4d'), int(z'ae909198'), int(z'eaad8e71'), int(z'6b93d5a0'), int(z'd08ed1d0'), &
      int(z'afc725e0'), int(z'8e3c5b2f'), int(z'8e7594b7'), int(z'8ff6e2fb'), int(z'f2122b64'), &
      int(z'8888b812'), int(z'900df01c'), int(z'4fad5ea0'), int(z'688fc31c'), int(z'd1cff191'), &
      int(z'b3a8c1ad'), int(z'2f2f2218'), int(z'be0e1777'), int(z'ea752dfe'), int(z'8b021fa1'), &
      int(z'e5a0cc0f'), int(z'b56f74e8'), int(z'18acf3d6'), int(z'ce89e299'), int(z'b4a84fe0'), &
      int(z'fd13e0b7'), int(z'7cc43b81'), int(z'd2ada8d9'), int(z'165fa266'), int(z'80957705'), &
      int(z'93cc7314'), int(z'211a1477'), int(z'e6ad2065'), int(z'77b5fa86'), int(z'c75442f5'), &
      int(z'fb9d35cf'), int(z'ebcdaf0c'), int(z'7b3e89a0'), int(z'd6411bd3'), int(z'ae1e7e49'), &
      int(z'00250e2d'), int(z'2071b35e'), int(z'226800bb'), int(z'57b8e0af'), int(z'2464369b'), &
      int(z'f009b91e'), int(z'5563911d'), int(z'59dfa6aa'), int(z'78c14389'), int(z'd95a537f'), &
      int(z'207d5ba2'), int(z'02e5b9c5'), int(z'83260376'), int(z'6295cfa9'), int(z'11c81968'), &
      int(z'4e734a41'), int(z'b3472dca'), int(z'7b14a94a'), int(z'1b510052'), int(z'9a532915'), &
      int(z'd60f573f'), int(z'bc9bc6e4'), int(z'2b60a476'), int(z'81e67400'), int(z'08ba6fb5'), &
      int(z'571be91f'), int(z'f296ec6b'), int(z'2a0dd915'), int(z'b6636521'), int(z'e7b9f9b6'), &
      int(z'ff34052e'), int(z'c5855664'), int(z'53b02d5d'), int(z'a99f8fa1'), int(z'08ba4799'), &
      int(z'6e85076a'), int(z'4b7a70e9'), int(z'b5b32944'), int(z'db75092e'), int(z'c4192623'), &
      int(z'ad6ea6b0'), int(z'49a7df7d'), int(z'9cee60b8'), int(z'8fedb266'), int(z'ecaa8c71'), &
      int(z'699a17ff'), int(z'5664526c'), int(z'c2b19ee1'), int(z'193602a5'), int(z'75094c29'), &
      int(z'a0591340'), int(z'e4183a3e'), int(z'3f54989a'), int(z'5b429d65'), int(z'6b8fe4d6'), &
      int(z'99f73fd6'), int(z'a1d29c07'), int(z'efe830f5'), int(z'4d2d38e6'), int(z'f0255dc1'), &
      int(z'4cdd2086'), int(z'8470eb26'), int(z'6382e9c6'), int(z'021ecc5e'), int(z'09686b3f'), &
      int(z'3ebaefc9'), int(z'3c971814'), int(z'6b6a70a1'), int(z'687f3584'), int(z'52a0e286'), &
      int(z'b79c5305'), int(z'aa500737'), int(z'3e07841c'), int(z'7fdeae5c'), int(z'8e7d44ec'), &
      int(z'5716f2b8'), int(z'b03ada37'), int(z'f0500c0d'), int(z'f01c1f04'), int(z'0200b3ff'), &
      int(z'ae0cf51a'), int(z'3cb574b2'), int(z'25837a58'), int(z'dc0921bd'), int(z'd19113f9'), &
      int(z'7ca92ff6'), int(z'94324773'), int(z'22f54701'), int(z'3ae5e581'), int(z'37c2dadc'), &
      int(z'c8b57634'), int(z'9af3dda7'), int(z'a9446146'), int(z'0fd0030e'), int(z'ecc8c73e'), &
      int(z'a4751e41'), int(z'e238cd99'), int(z'3bea0e2f'), int(z'3280bba1'), int(z'183eb331'), &
      int(z'4e548b38'), int(z'4f6db908'), int(z'6f420d03'), int(z'f60a04bf'), int(z'2cb81290'), &
      int(z'24977c79'), int(z'5679b072'), int(z'bcaf89af'), int(z'de9a771f'), int(z'd9930810'), &
      int(z'b38bae12'), int(z'dccf3f2e'), int(z'5512721f'), int(z'2e6b7124'), int(z'501adde6'), &
      int(z'9f84cd87'), int(z'7a584718'), int(z'7408da17'), int(z'bc9f9abc'), int(z'e94b7d8c'), &
      int(z'ec7aec3a'), int(z'db851dfa'), int(z'63094366'), int(z'c464c3d2'), int(z'ef1c1847'), &
      int(z'3215d908'), int(z'dd433b37'), int(z'24c2ba16'), int(z'12a14d43'), int(z'2a65c451'), &
      int(z'50940002'), int(z'133ae4dd'), int(z'71dff89e'), int(z'10314e55'), int(z'81ac77d6'), &
      int(z'5f11199b'), int(z'043556f1'), int(z'd7a3c76b'), int(z'3c11183b'), int(z'5924a509'), &
      int(z'f28fe6ed'), int(z'97f1fbfa'), int(z'9ebabf2c'), int(z'1e153c6e'), int(z'86e34570'), &
      int(z'eae96fb1'), int(z'860e5e0a'), int(z'5a3e2ab3'), int(z'771fe71c'), int(z'4e3d06fa'), &
      int(z'2965dcb9'), int(z'99e71d0f'), int(z'803e89d6'), int(z'5266c825'), int(z'2e4cc978'), &
      int(z'9c10b36a'), int(z'c6150eba'), int(z'94e2ea78'), int(z'a5fc3c53'), int(z'1e0a2df4'), &
      int(z'f2f74ea7'), int(z'361d2b3d'), int(z'1939260f'), int(z'19c27960'), int(z'5223a708'), &
      int(z'f71312b6'), int(z'ebadfe6e'), int(z'eac31f66'), int(z'e3bc4595'), int(z'a67bc883'), &
      int(z'b17f37d1'), int(z'018cff28'), int(z'c332ddef'), int(z'be6c5aa5'), int(z'65582185'), &
      int(z'68ab9802'), int(z'eecea50f'), int(z'db2f953b'), int(z'2aef7dad'), int(z'5b6e2f84'), &
      int(z'1521b628'), int(z'29076170'), int(z'ecdd4775'), int(z'619f1510'), int(z'13cca830'), &
      int(z'eb61bd96'), int(z'0334fe1e'), int(z'aa0363cf'), int(z'b5735c90'), int(z'4c70a239'), &
      int(z'd59e9e0b'), int(z'cbaade14'), int(z'eecc86bc'), int(z'60622ca7'), int(z'9cab5cab'), &
      int(z'b2f3846e'), int(z'648b1eaf'), int(z'19bdf0ca'), int(z'a02369b9'), int(z'655abb50'), &
      int(z'40685a32'), int(z'3c2ab4b3'), int(z'319ee9d5'), int(z'c021b8f7'), int(z'9b540b19'), &
      int(z'875fa099'), int(z'95f7997e'), int(z'623d7da8'), int(z'f837889a'), int(z'97e32d77'), &
      int(z'11ed935f'), int(z'16681281'), int(z'0e358829'), int(z'c7e61fd6'), int(z'96dedfa1'), &
      int(z'7858ba99'), int(z'57f584a5'), int(z'1b227263'), int(z'9b83c3ff'), int(z'1ac24696'), &
      int(z'cdb30aeb'), int(z'532e3054'), int(z'8fd948e4'), int(z'6dbc3128'), int(z'58ebf2ef'), &
      int(z'34c6ffea'), int(z'fe28ed61'), int(z'ee7c3c73'), int(z'5d4a14d9'), int(z'e864b7e3'), &
      int(z'42105d14'), int(z'203e13e0'), int(z'45eee2b6'), int(z'a3aaabea'), int(z'db6c4f15'), &
      int(z'facb4fd0'), int(z'c742f442'), int(z'ef6abbb5'), int(z'654f3b1d'), int(z'41cd2105'), &
      int(z'd81e799e'), int(z'86854dc7'), int(z'e44b476a'), int(z'3d816250'), int(z'cf62a1f2'), &
      int(z'5b8d2646'), int(z'fc8883a0'), int(z'c1c7b6a3'), int(z'7f1524c3'), int(z'69cb7492'), &
      int(z'47848a0b'), int(z'5692b285'), int(z'095bbf00'), int(z'ad19489d'), int(z'1462b174'), &
      int(z'23820e00'), int(z'58428d2a'), int(z'0c55f5ea'), int(z'1dadf43e'), int(z'233f7061'), &
      int(z'3372f092'), int(z'8d937e41'), int(z'd65fecf1'), int(z'6c223bdb'), int(z'7cde3759'), &
      int(z'cbee7460'), int(z'4085f2a7'), int(z'ce77326e'), int(z'a6078084'), int(z'19f8509e'), &
      int(z'e8efd855'), int(z'61d99735'), int(z'a969a7aa'), int(z'c50c06c2'), int(z'5a04abfc'), &
      int(z'800bcadc'), int(z'9e447a2e'), int(z'c3453484'), int(z'fdd56705'), int(z'0e1e9ec9'), &
      int(z'db73dbd3'), int(z'105588cd'), int(z'675fda79'), int(z'e3674340'), int(z'c5c43465'), &
      int(z'713e38d8'), int(z'3d28f89e'), int(z'f16dff20'), int(z'153e21e7'), int(z'8fb03d4a'), &
      int(z'e6e39f2b'), int(z'db83adf7'), int(z'e93d5a68'), int(z'948140f7'), int(z'f64c261c'), &
      int(z'94692934'), int(z'411520f7'), int(z'7602d4f7'), int(z'bcf46b2e'), int(z'd4a20068'), &
      int(z'd4082471'), int(z'3320f46a'), int(z'43b7d4b7'), int(z'500061af'), int(z'1e39f62e'), &
      int(z'97244546'), int(z'14214f74'), int(z'bf8b8840'), int(z'4d95fc1d'), int(z'96b591af'), &
      int(z'70f4ddd3'), int(z'66a02f45'), int(z'bfbc09ec'), int(z'03bd9785'), int(z'7fac6dd0'), &
      int(z'31cb8504'), int(z'96eb27b3'), int(z'55fd3941'), int(z'da2547e6'), int(z'abca0a9a'), &
      int(z'28507825'), int(z'530429f4'), int(z'0a2c86da'), int(z'e9b66dfb'), int(z'68dc1462'), &
      int(z'd7486900'), int(z'680ec0a4'), int(z'27a18dee'), int(z'4f3ffea2'), int(z'e887ad8c'), &
      int(z'b58ce006'), int(z'7af4d6b6'), int(z'aace1e7c'), int(z'd3375fec'), int(z'ce78a399'), &
      int(z'406b2a42'), int(z'20fe9e35'), int(z'd9f385b9'), int(z'ee39d7ab'), int(z'3b124e8b'), &
      int(z'1dc9faf7'), int(z'4b6d1856'), int(z'26a36631'), int(z'eae397b2'), int(z'3a6efa74'), &
      int(z'dd5b4332'), int(z'6841e7f7'), int(z'ca7820fb'), int(z'fb0af54e'), int(z'd8feb397'), &
      int(z'454056ac'), int(z'ba489527'), int(z'55533a3a'), int(z'20838d87'), int(z'fe6ba9b7'), &
      int(z'd096954b'), int(z'55a867bc'), int(z'a1159a58'), int(z'cca92963'), int(z'99e1db33'), &
      int(z'a62a4a56'), int(z'3f3125f9'), int(z'5ef47e1c'), int(z'9029317c'), int(z'fdf8e802'), &
      int(z'04272f70'), int(z'80bb155c'), int(z'05282ce3'), int(z'95c11548'), int(z'e4c66d22'), &
      int(z'48c1133f'), int(z'c70f86dc'), int(z'07f9c9ee'), int(z'41041f0f'), int(z'404779a4'), &
      int(z'5d886e17'), int(z'325f51eb'), int(z'd59bc0d1'), int(z'f2bcc18f'), int(z'41113564'), &
      int(z'257b7834'), int(z'602a9c60'), int(z'dff8e8a3'), int(z'1f636c1b'), int(z'0e12b4c2'), &
      int(z'02e1329e'), int(z'af664fd1'), int(z'cad18115'), int(z'6b2395e0'), int(z'333e92e1'), &
      int(z'3b240b62'), int(z'eebeb922'), int(z'85b2a20e'), int(z'e6ba0d99'), int(z'de720c8c'), &
      int(z'2da2f728'), int(z'd0127845'), int(z'95b794fd'), int(z'647d0862'), int(z'e7ccf5f0'), &
      int(z'5449a36f'), int(z'877d48fa'), int(z'c39dfd27'), int(z'f33e8d1e'), int(z'0a476341'), &
      int(z'992eff74'), int(z'3a6f6eab'), int(z'f4f8fd37'), int(z'a812dc60'), int(z'a1ebddf8'), &
      int(z'991be14c'), int(z'db6e6b0d'), int(z'c67b5510'), int(z'6d672c37'), int(z'2765d43b'), &
      int(z'dcd0e804'), int(z'f1290dc7'), int(z'cc00ffa3'), int(z'b5390f92'), int(z'690fed0b'), &
      int(z'667b9ffb'), int(z'cedb7d9c'), int(z'a091cf0b'), int(z'd9155ea3'), int(z'bb132f88'), &
      int(z'515bad24'), int(z'7b9479bf'), int(z'763bd6eb'), int(z'37392eb3'), int(z'cc115979'), &
      int(z'8026e297'), int(z'f42e312d'), int(z'6842ada7'), int(z'c66a2b3b'), int(z'12754ccc'), &
      int(z'782ef11c'), int(z'6a124237'), int(z'b79251e7'), int(z'06a1bbe6'), int(z'4bfb6350'), &
      int(z'1a6b1018'), int(z'11caedfa'), int(z'3d25bdd8'), int(z'e2e1c3c9'), int(z'44421659'), &
      int(z'0a121386'), int(z'd90cec6e'), int(z'd5abea2a'), int(z'64af674e'), int(z'da86a85f'), &
      int(z'bebfe988'), int(z'64e4c3fe'), int(z'9dbc8057'), int(z'f0f7c086'), int(z'60787bf8'), &
      int(z'6003604d'), int(z'd1fd8346'), int(z'f6381fb0'), int(z'7745ae04'), int(z'd736fccc'), &
      int(z'83426b33'), int(z'f01eab71'), int(z'b0804187'), int(z'3c005e5f'), int(z'77a057be'), &
      int(z'bde8ae24'), int(z'55464299'), int(z'bf582e61'), int(z'4e58f48f'), int(z'f2ddfda2'), &
      int(z'f474ef38'), int(z'8789bdc2'), int(z'5366f9c3'), int(z'c8b38e74'), int(z'b475f255'), &
      int(z'46fcd9b9'), int(z'7aeb2661'), int(z'8b1ddf84'), int(z'846a0e79'), int(z'915f95e2'), &
      int(z'466e598e'), int(z'20b45770'), int(z'8cd55591'), int(z'c902de4c'), int(z'b90bace1'), &
      int(z'bb8205d0'), int(z'11a86248'), int(z'7574a99e'), int(z'b77f19b6'), int(z'e0a9dc09'), &
      int(z'662d09a1'), int(z'c4324633'), int(z'e85a1f02'), int(z'09f0be8c'), int(z'4a99a025'), &
      int(z'1d6efe10'), int(z'1ab93d1d'), int(z'0ba5a4df'), int(z'a186f20f'), int(z'2868f169'), &
      int(z'dcb7da83'), int(z'573906fe'), int(z'a1e2ce9b'), int(z'4fcd7f52'), int(z'50115e01'), &
      int(z'a70683fa'), int(z'a002b5c4'), int(z'0de6d027'), int(z'9af88c27'), int(z'773f8641'), &
      int(z'c3604c06'), int(z'61a806b5'), int(z'f0177a28'), int(z'c0f586e0'), int(z'006058aa'), &
      int(z'30dc7d62'), int(z'11e69ed7'), int(z'2338ea63'), int(z'53c2dd94'), int(z'c2c21634'), &
      int(z'bbcbee56'), int(z'90bcb6de'), int(z'ebfc7da1'), int(z'ce591d76'), int(z'6f05e409'), &
      int(z'4b7c0188'), int(z'39720a3d'), int(z'7c927c24'), int(z'86e3725f'), int(z'724d9db9'), &
      int(z'1ac15bb4'), int(z'd39eb8fc'), int(z'ed545578'), int(z'08fca5b5'), int(z'd83d7cd3'), &
      int(z'4dad0fc4'), int(z'1e50ef5e'), int(z'b161e6f8'), int(z'a28514d9'), int(z'6c51133c'), &
      int(z'6fd5c7e7'), int(z'56e14ec4'), int(z'362abfce'), int(z'ddc6c837'), int(z'd79a3234'), &
      int(z'92638212'), int(z'670efa8e'), int(z'406000e0'), int(z'3a39ce37'), int(z'd3faf5cf'), &
      int(z'abc27737'), int(z'5ac52d1b'), int(z'5cb0679e'), int(z'4fa33742'), int(z'd3822740'), &
      int(z'99bc9bbe'), int(z'd5118e9d'), int(z'bf0f7315'), int(z'd62d1c7e'), int(z'c700c47b'), &
      int(z'b78c1b6b'), int(z'21a19045'), int(z'b26eb1be'), int(z'6a366eb4'), int(z'5748ab2f'), &
      int(z'bc946e79'), int(z'c6a376d2'), int(z'6549c2c8'), int(z'530ff8ee'), int(z'468dde7d'), &
      int(z'd5730a1d'), int(z'4cd04dc6'), int(z'2939bbdb'), int(z'a9ba4650'), int(z'ac9526e8'), &
      int(z'be5ee304'), int(z'a1fad5f0'), int(z'6a2d519a'), int(z'63ef8ce2'), int(z'9a86ee22'), &
      int(z'c089c2b8'), int(z'43242ef6'), int(z'a51e03aa'), int(z'9cf2d0a4'), int(z'83c061ba'), &
      int(z'9be96a4d'), int(z'8fe51550'), int(z'ba645bd6'), int(z'2826a2f9'), int(z'a73a3ae1'), &
      int(z'4ba99586'), int(z'ef5562e9'), int(z'c72fefd3'), int(z'f752f7da'), int(z'3f046f69'), &
      int(z'77fa0a59'), int(z'80e4a915'), int(z'87b08601'), int(z'9b09e6ad'), int(z'3b3ee593'), &
      int(z'e990fd5a'), int(z'9e34d797'), int(z'2cf0b7d9'), int(z'022b8b51'), int(z'96d5ac3a'), &
      int(z'017da67d'), int(z'd1cf3ed6'), int(z'7c7d2d28'), int(z'1f9f25cf'), int(z'adf2b89b'), &
      int(z'5ad6b472'), int(z'5a88f54c'), int(z'e029ac71'), int(z'e019a5e6'), int(z'47b0acfd'), &
      int(z'ed93fa9b'), int(z'e8d3c48d'), int(z'283b57cc'), int(z'f8d56629'), int(z'79132e28'), &
      int(z'785f0191'), int(z'ed756055'), int(z'f7960e44'), int(z'e3d35e8c'), int(z'15056dd4'), &
      int(z'88f46dba'), int(z'03a16125'), int(z'0564f0bd'), int(z'c3eb9e15'), int(z'3c9057a2'), &
      int(z'97271aec'), int(z'a93a072a'), int(z'1b3f6d9b'), int(z'1e6321f5'), int(z'f59c66fb'), &
      int(z'26dcf319'), int(z'7533d928'), int(z'b155fdf5'), int(z'03563482'), int(z'8aba3cbb'), &
      int(z'28517711'), int(z'c20ad9f8'), int(z'abcc5167'), int(z'ccad925f'), int(z'4de81751'), &
      int(z'3830dc8e'), int(z'379d5862'), int(z'9320f991'), int(z'ea7a90c2'), int(z'fb3e7bce'), &
      int(z'5121ce64'), int(z'774fbe32'), int(z'a8b6e37e'), int(z'c3293d46'), int(z'48de5369'), &
      int(z'6413e680'), int(z'a2ae0810'), int(z'dd6db224'), int(z'69852dfd'), int(z'09072166'), &
      int(z'b39a460a'), int(z'6445c0dd'), int(z'586cdecf'), int(z'1c20c8ae'), int(z'5bbef7dd'), &
      int(z'1b588d40'), int(z'ccd2017f'), int(z'6bb4e3bb'), int(z'dda26a7e'), int(z'3a59ff45'), &
      int(z'3e350a44'), int(z'bcb4cdd5'), int(z'72eacea8'), int(z'fa6484bb'), int(z'8d6612ae'), &
      int(z'bf3c6f47'), int(z'd29be463'), int(z'542f5d9e'), int(z'aec2771b'), int(z'f64e6370'), &
      int(z'740e0d8d'), int(z'e75b1357'), int(z'f8721671'), int(z'af537d5d'), int(z'4040cb08'), &
      int(z'4eb4e2cc'), int(z'34d2466a'), int(z'0115af84'), int(z'e1b00428'), int(z'95983a1d'), &
      int(z'06b89fb4'), int(z'ce6ea048'), int(z'6f3f3b82'), int(z'3520ab82'), int(z'011a1d4b'), &
      int(z'277227f8'), int(z'611560b1'), int(z'e7933fdc'), int(z'bb3a792b'), int(z'344525bd'), &
      int(z'a08839e1'), int(z'51ce794b'), int(z'2f32c9b7'), int(z'a01fbac9'), int(z'e01cc87e'), &
      int(z'bcc7d1f6'), int(z'cf0111c3'), int(z'a1e8aac7'), int(z'1a908749'), int(z'd44fbd9a'), &
      int(z'd0dadecb'), int(z'd50ada38'), int(z'0339c32a'), int(z'c6913667'), int(z'8df9317c'), &
      int(z'e0b12b4f'), int(z'f79e59b7'), int(z'43f5bb3a'), int(z'f2d519ff'), int(z'27d9459c'), &
      int(z'bf97222c'), int(z'15e6fc2a'), int(z'0f91fc71'), int(z'9b941525'), int(z'fae59361'), &
      int(z'ceb69ceb'), int(z'c2a86459'), int(z'12baa8d1'), int(z'b6c1075e'), int(z'e3056a0c'), &
      int(z'10d25065'), int(z'cb03a442'), int(z'e0ec6e0e'), int(z'1698db3b'), int(z'4c98a0be'), &
      int(z'3278e964'), int(z'9f1f9532'), int(z'e0d392df'), int(z'd3a0342b'), int(z'8971f21e'), &
      int(z'1b0a7441'), int(z'4ba3348c'), int(z'c5be7120'), int(z'c37632d8'), int(z'df359f8d'), &
      int(z'9b992f2e'), int(z'e60b6f47'), int(z'0fe3f11d'), int(z'e54cda54'), int(z'1edad891'), &
      int(z'ce6279cf'), int(z'cd3e7e6f'), int(z'1618b166'), int(z'fd2c1d05'), int(z'848fd2c5'), &
      int(z'f6fb2299'), int(z'f523f357'), int(z'a6327623'), int(z'93a83531'), int(z'56cccd02'), &
      int(z'acf08162'), int(z'5a75ebb5'), int(z'6e163697'), int(z'88d273cc'), int(z'de966292'), &
      int(z'81b949d0'), int(z'4c50901b'), int(z'71c65614'), int(z'e6c6c7bd'), int(z'327a140a'), &
      int(z'45e1d006'), int(z'c3f27b9a'), int(z'c9aa53fd'), int(z'62a80f00'), int(z'bb25bfe2'), &
      int(z'35bdd2f6'), int(z'71126905'), int(z'b2040222'), int(z'b6cbcf7c'), int(z'cd769c2b'), &
      int(z'53113ec0'), int(z'1640e3d3'), int(z'38abbd60'), int(z'2547adf0'), int(z'ba38209c'), &
      int(z'f746ce76'), int(z'77afa1c5'), int(z'20756060'), int(z'85cbfe4e'), int(z'8ae88dd8'), &
      int(z'7aaaf9b0'), int(z'4cf9aa7e'), int(z'1948c25c'), int(z'02fb8a8c'), int(z'01c36ae4'), &
      int(z'd6ebe1f9'), int(z'90d4f869'), int(z'a65cdea0'), int(z'3f09252d'), int(z'c208e69f'), &
      int(z'b74e6132'), int(z'ce77e25b'), int(z'578fdfe3'), int(z'3ac372e6') ], [ 256, 4 ] )

     
! Unit test interface for privte parts
  interface 

    module pure function unit_test_private_blowfish_P( this ) result(res)
      class(t_blowfish), intent(in) :: this
      integer(kind=4), dimension(size(this%p)) :: res
    end function unit_test_private_blowfish_P

    module pure function unit_test_private_blowfish_S( this ) result(res)
      class(t_blowfish), intent(in) :: this
      integer(kind=4), dimension(size(this%s,1),size(this%s,2)) :: res
    end function unit_test_private_blowfish_S

    module pure function unit_test_private_blowfish_F( this, x ) result(res)
      class(t_blowfish), intent(in) :: this
      integer(kind=4),  intent(in) :: x
      integer(kind=4) :: res
    end function unit_test_private_blowfish_F

    module pure subroutine unit_test_private_blowfish_encrypt_lr( this, left, right )
      class(t_blowfish), intent(in) :: this
      integer(kind=4), intent(inout) :: left
      integer(kind=4), intent(inout) :: right
    end subroutine unit_test_private_blowfish_encrypt_lr

    module pure subroutine unit_test_private_blowfish_decrypt_lr( this, left, right )
      class(t_blowfish), intent(in) :: this
      integer(kind=4), intent(inout) :: left
      integer(kind=4), intent(inout) :: right
    end subroutine unit_test_private_blowfish_decrypt_lr

  end interface
  
  public unit_test_private_blowfish_P, unit_test_private_blowfish_S
  public unit_test_private_blowfish_F
  public unit_test_private_blowfish_encrypt_lr, unit_test_private_blowfish_decrypt_lr
  
!- End of module variable declarations ----------------------------------------

contains

! Constuctor from key
pure function blowfish( key ) result(res)

! The initialisation key
  integer(kind=1), dimension(:), intent(in) :: key

! The Blowfish context
  type(t_blowfish) :: res

! Local variables
  integer :: i,j ,k
  integer(kind=4) :: data, datal, datar
  integer(kind=4), dimension(size(key)) :: ikey
  integer(kind=1), dimension(4) :: xdata

! Initialise key
  allocate( res%key(size(key)) )
  res%key = key

! Initialise locals
  ikey = key

! Initialise S structure
  res%s = orig_s

! Initialise P structure
  j = 1
  do i = 1, size(orig_p)
    data = 0_4
    xdata = 0_4
    do k = 1, 4
      xdata(k) = ior( xdata(k), key(j) )
      j = j + 1
      if( j > size(key) ) j = 1
    end do
    data = transfer( xdata(4:1:-1), data )
    res%p(i) = ieor( orig_p(i), data )
  end do

! Finalise P structure
  datal = 0
  datar = 0
  do i = 1, size(orig_p), 2
    call res%encrypt_lr( datal, datar )
    res%p(i)   = datal
    res%p(i+1) = datar
  end do

! Finalise S structure
  do i = 1, size(orig_s,2)
    do j = 1, size(orig_s,1), 2
      call res%encrypt_lr( datal, datar )
      res%s(j  ,i) = datal
      res%s(j+1,i) = datar
    end do
  end do

end function blowfish


! Blowfish block size
pure function blowfish_block_size() result(res)

! Blowfish block size
  integer :: res
   
! Return the Blowfish block size
  res = blowfish_block_size_in_bytes
   
end function blowfish_block_size


! Blowfish key size
pure function blowfish_key_size() result(res)

! Blowfish key size
  integer :: res
   
! Return the Blowfish key size
  res = blowfish_key_size_in_bytes
   
end function blowfish_key_size


! Blowfish encryption (from 8 bytes buffer)
pure subroutine blowfish_encrypt( this, text, cipher )

! The Blowfish context
  class(t_blowfish), intent(in) :: this

! The plain text to encrypt
  integer(kind=1), dimension(:), intent(in)  :: text

! The encrypted text
  integer(kind=1), dimension(size(text)), intent(out) :: cipher

! Local variables
  integer(kind=4) :: left, right


! Split input buffer into left and right
  left  = transfer( text(4:1:-1), left  )
  right = transfer( text(8:5:-1), right )

! Inovke the core encrypter
  call this%encrypt_lr( left, right )

! Build the encrypted buffer
  cipher(4:1:-1) = transfer( left, cipher(1:4)  )
  cipher(8:5:-1) = transfer( right, cipher(5:8) )

end subroutine blowfish_encrypt


! Blowfish decryption (from 8 bytes buffer)
pure subroutine blowfish_decrypt( this, cipher, text )

! The Blowfish context
  class(t_blowfish), intent(in) :: this

! The encrypted text
  integer(kind=1), dimension(:),            intent(in)  :: cipher

! The plain decrypted text
  integer(kind=1), dimension(size(cipher)), intent(out) :: text

! Local variables
  integer(kind=4) :: left, right

! Split input buffer into left and right
  left  = transfer( cipher(4:1:-1), left  )
  right = transfer( cipher(8:5:-1), right )

! Inovke the core encrypter
  call this%decrypt_lr( left, right )

! Build the encrypted buffer
  text(4:1:-1) = transfer( left, text(1:4)  )
  text(8:5:-1) = transfer( right, text(5:8) )

end subroutine blowfish_decrypt


! The blowfish encrypt core algorithm
pure subroutine blowfish_encrypt_lr( this, left, right )

! The Blowfish context
  class(t_blowfish), intent(in) :: this

! The left part of the data to encrypt/decrypt
  integer(kind=4), intent(inout) :: left

! The right part of the data to encrypt/decrypt
  integer(kind=4), intent(inout) :: right

! Local variables
  integer(kind=4) :: xl, xr, tmp
  integer :: i

! Initialise locals
  xl = left
  xr = right

! Loop on P
  do i = 1, size(this%p)-2

!   Update left and right words
    xl = ieor( xl, this%p(i) )
    xr = ieor( this%F( xl ), xr )

!   Exchange left and right words
    tmp = xl
    xl = xr
    xr = tmp

  end do

! Exchange left and right
  tmp = xl
  xl = xr
  xr = tmp

! Finalise left and right
  xr = ieor( xr, this%p(size(this%p)-1) )
  xl = ieor( xl, this%p(size(this%p)) )

! Return values
  left  = xl
  right = xr

end subroutine blowfish_encrypt_lr


! The blowfish decrypt core algorithm
pure subroutine blowfish_decrypt_lr( this, left, right )

! The Blowfish context
  class(t_blowfish), intent(in) :: this

! The left part of the data to encrypt/decrypt
  integer(kind=4),  intent(inout)  :: left

! The right part of the data to encrypt/decrypt
  integer(kind=4),  intent(inout)  :: right

! Local variables
  integer(kind=4) :: xl, xr, tmp
  integer :: i

! Initialise locals
  xl = left
  xr = right

! Loop on P
  do i = size(this%p), 3, -1

!   Update left and right words
    xl = ieor( xl, this%p(i) )
    xr = ieor( this%F( xl ), xr )

!   Exchange left and right words
    tmp = xl
    xl = xr
    xr = tmp

  end do

! Exchange left and right
  tmp = xl
  xl = xr
  xr = tmp

! Finalise left and right
  xr = ieor( xr, this%p(2) )
  xl = ieor( xl, this%p(1) )

! Return values
  left  = xl
  right = xr

end subroutine blowfish_decrypt_lr


! Auxiliary function
pure function blowfish_F( this, x ) result(res)

! The Blowfish context
  class(t_blowfish), intent(in) :: this

! The double word to process
  integer(kind=4),  intent(in) :: x

! The result
  integer(kind=4) :: res

! Local variables
  integer(kind=2) :: a, b, c, d
  integer(kind=4) :: y
  integer(kind=8), dimension(4) :: s

  y = x
  d = int( iand( y, z000000ff ) + 1, kind=2 )
  y = ishft( y, -8 )
  c = int( iand( y, z000000ff ) + 1, kind=2 )
  y = ishft( y, -8 )
  b = int( iand( y, z000000ff ) + 1, kind=2 )
  y = ishft( y, -8 )
  a = int( iand( y, z000000ff ) + 1, kind=2 )

  s(1) = ibits( int(this%s(a,1),kind=8), 0, 32 )
  s(2) = ibits( int(this%s(b,2),kind=8), 0, 32 )
  s(3) = ibits( int(this%s(c,3),kind=8), 0, 32 )
  s(4) = ibits( int(this%s(d,4),kind=8), 0, 32 )

  res = int( ieor( s(1) + s(2), s(3) ) + s(4), kind=4 )

end function blowfish_F

end module m_blowfish


! Unit test of private methods
submodule (m_blowfish) unit_test_private_blowfish

contains

! The blowfish P array
module procedure unit_test_private_blowfish_P
  res = this%P
end procedure unit_test_private_blowfish_P

! The blowfish S array
module procedure unit_test_private_blowfish_S
  res = this%S
end procedure unit_test_private_blowfish_S

! The blowfish auxiliary function
module procedure unit_test_private_blowfish_F
  res = this%F( x )
end procedure unit_test_private_blowfish_F

! The blowfish encrypt core algorithm
module procedure unit_test_private_blowfish_encrypt_lr
  call this%encrypt_lr( left, right )
end procedure unit_test_private_blowfish_encrypt_lr

! The blowfish decrypt core algorithm
module procedure unit_test_private_blowfish_decrypt_lr
  call this%decrypt_lr( left, right )
end procedure unit_test_private_blowfish_decrypt_lr

end submodule unit_test_private_blowfish