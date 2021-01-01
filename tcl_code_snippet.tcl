#!/usr/bin/wish

proc font_metric {cvs} {
    if { [winfo class $cvs] ne "Canvas"] } { return "" }
    set str "M"
    set id [$cvs create 0 0 -text $str]
    foreach {x0 y0 x1 y1} [$cvs bbox $id]
    set width [expr {abs($x1-$x0)}]
    set height [expr {abs($y1-$y0)}]
    if { $width < 8 } { set width 8 }
    if { $height < 14 } { set height 14 }
    set result [list $width $height]
    return $result
}

proc puts_ansi {mode str} {
    # escapse
    set esc "\033"
    
    # foreground
    set blackf "${esc}[30m"
    set redf "${esc}[31m"
    set greenf "${esc}[32m"
    set yellowf "${esc}[33m"
    set bluef "${esc}[34m"
    set purple "${esc}[35m"
    set cyanf "${esc}[36m"
    set whitef "${esc}[37m"

    # background
    set blackb "${esc}[40m"
    set redb "${esc}[41m"
    set greenb "${esc}[42m"
    set yellowb "${esc}[43m"
    set blueb "${esc}[44m"
    set purpleb "${esc}[45m"
    set cyanb "${esc}[46m"
    set whiteb "${esc}[47m"
    
    # other style
    set boldon "${esc}[1m"
    set boldoff "${esc}[22m"
    set italicson "${esc}[3m"
    set italicsoff "${esc}[23m"
    set ulon "${esc}[4m"
    set uloff "${esc}[24m"
    set invon "${esc}[7m"
    set invoff "${esc}[27m"

    set reset "${esc}[0m"

    set line ""
    switch $mode {
        red {
            set line "${redf}${str}${reset}"
        }
        blue {
            set line "${bluef}${str}${reset}"
        }
        bold {
            set line "${bold}${str}${reset}"
        }
        italic {
            set line "${italic}${str}${reset}"
        }
        underline {
            set line "${ulon}${str}${reset}"
        }
    }
    puts $line
}
