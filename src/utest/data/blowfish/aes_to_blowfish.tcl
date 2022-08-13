#!/usr/bin/tclsh

package require blowfish

set files [lsort [glob [file join .. aes *.txt]]]

foreach file $files {

  set name [file tail $file]
  puts $name

  set aes [split [read [open $file r]] \n]
  
  set direction [string range [lindex $aes 0] 1 end-1]
  set aes [lrange $aes 1 end]

  set mode [string tolower [string range $name 0 2]]
  
  foreach rec $aes {
  
    if { [string length [string trim $rec]] > 0 } {
    
      set key [string trim [lindex [split $rec =] 0]]
      set value [string trim [lindex [split $rec =] 1]]

      if { $key == "COUNT" } {
        set count $value
      } elseif { $key == "IV" } {
        set testcase($count,$key) [binary format H* [string range $value 0 15]]
      } elseif { $key == "KEY" } {
        set testcase($count,$key) [binary format H* [string range $value 0 31]]
      } else {
        set testcase($count,$key) [binary format H* $value]
      }

    }  
  
  }

  if { $mode == "cbc" || $mode == "ecb" } {
    foreach test [lsort [array names testcase]] {
      set idx [lindex [split $test ,] 0]
      set key $testcase($idx,KEY)
      if { [info exists testcase($idx,IV)] } {
        set iv "$testcase($idx,IV)"
      } else {
        set iv ""
      }
      set text $testcase($idx,PLAINTEXT)
      set cipher $testcase($idx,CIPHERTEXT)
      if { $direction == "ENCRYPT" } {
        if { $iv == "" } {
          set cipher [::blowfish::blowfish -mode $mode -dir encrypt -key $key -pad \0 -- $text]
        } else {
          set cipher [::blowfish::blowfish -mode $mode -dir encrypt -key $key -iv $iv -pad \0 -- $text]
        }
        set testcase($idx,CIPHERTEXT) $cipher
      } else {
        if { $iv == "" } {
          set text [::blowfish::blowfish -mode $mode -dir decrypt -key $key -pad \0 -- $cipher]
        } else {
          set text [::blowfish::blowfish -mode $mode -dir decrypt -key $key -iv $iv -pad \0 -- $cipher]
        }
        set testcase($idx,PLAINTEXT) $text
      }
    }
  }

  set ohand [open $name w]
  puts $ohand "\[$direction]"

  for { set idx 0 } { $idx < [llength [array names testcase *,KEY]] } { incr idx } {

    puts $ohand ""
    puts $ohand "COUNT = $idx"
    binary scan $testcase($idx,KEY) H* ascii
    puts $ohand "KEY = $ascii"
    if { [info exists testcase($idx,IV)] } {
      binary scan $testcase($idx,IV) H* ascii
      puts $ohand "IV = $ascii"
    }
    if { $direction == "ENCRYPT" } {
      binary scan $testcase($idx,PLAINTEXT) H* ascii
      puts $ohand "PLAINTEXT = $ascii"
      binary scan $testcase($idx,CIPHERTEXT) H* ascii
      puts $ohand "CIPHERTEXT = $ascii"
    } else {
      binary scan $testcase($idx,CIPHERTEXT) H* ascii
      puts $ohand "CIPHERTEXT = $ascii"
      binary scan $testcase($idx,PLAINTEXT) H* ascii
      puts $ohand "PLAINTEXT = $ascii"
    }
  }

  close $ohand

  unset testcase

}
