#!/usr/bin/wish

# @from:
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

# @example:  puts_ansi red "here"
# @from:
proc puts_ansi {mode str} {
    # escapse
    set esc "\033"

    # foreground
    set blackf "${esc}\[30m"
    set redf "${esc}\[31m"
    set greenf "${esc}\[32m"
    set yellowf "${esc}\[33m"
    set bluef "${esc}\[34m"
    set purple "${esc}\[35m"
    set cyanf "${esc}\[36m"
    set whitef "${esc}\[37m"

    # background
    set blackb "${esc}\[40m"
    set redb "${esc}\[41m"
    set greenb "${esc}\[42m"
    set yellowb "${esc}\[43m"
    set blueb "${esc}\[44m"
    set purpleb "${esc}\[45m"
    set cyanb "${esc}\[46m"
    set whiteb "${esc}\[47m"

    # other style
    set boldon "${esc}\[1m"
    set boldoff "${esc}\[22m"
    set italicson "${esc}\[3m"
    set italicsoff "${esc}\[23m"
    set ulon "${esc}\[4m"
    set uloff "${esc}\[24m"
    set invon "${esc}\[7m"
    set invoff "${esc}\[27m"

    set reset "${esc}\[0m"

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
        default {
            set line $str
        }
    }
    puts $line
}

# @example:  set a 1; set b 20; swap_vars a b
# @result:   >>> a = 20; b = 1
# @from:
proc swap_vars {x y} {
    upvar $x a $y b
    lassign [list $a $b] b a
}

# @example:  set lst {11 78 aa}; lremove $lst 78
# @result:   >>> a = {11 aa}
# @from:
proc lremove {lst val} {
    set pos [lsearch -exact $lst $val]
    if {$pos >= 0} {
        set lst [lreplace $lst $pos $pos]
    }
    return $lst
}

# @example:  set coords {1 2 3 4}; line_coords_reverse $coords
# @result:   >>> coords = {3 4}
# @from:
proc line_coords_reverse {coords} {
    set out {}
    foreach {y x} [lreverse $coords] {
        lappend out $x $y
    }
    return $out
}

# @from:
proc mainmenu_nowin_create {} {
    set win ""
    set mb [menu $win.menubar -tearoff 0]

    set filemenu [menu $mb.file -tearoff 0]
    set recentmenu [menu $filemenu.recent -tearoff 0 -postcommand "mainmenu_recentmenu_populate . $filemenu.recent"]
    $filemenu add command -label "New"   -underline 0 -accelerator "Command+N" -command "mainwin_create"
    $filemenu add command -label "Open..." -underline 0 -accelerator "Command+O" -command "mainwin_open ."
    $filemenu add cascade -label "Open Recent" -underline 5 -menu $recentmenu
    $filemenu add separator
    $filemenu add command -label "Close" -underline 0 -accelerator "Command+W" -state disabled
    $filemenu add command -label "Save"  -underline 0 -accelerator "Command+S" -state disabled
    $filemenu add command -label "Save As..." -underline 1 -accelerator "Shift+Command+S" -state disabled

    set editmenu [menu $mb.edit -tearoff 0]
    $editmenu add command -label "Cut"   -underline 1 -accelerator "Command+X" -command "mainmenu_cut \[focus\] 1"
    $editmenu add command -label "Copy"  -underline 0 -accelerator "Command+C" -command "mainmenu_copy \[focus\] 1"
    $editmenu add command -label "Paste" -underline 0 -accelerator "Command+V" -command "mainmenu_paste \[focus\] 1"
    $editmenu add separator
    $editmenu add command -label "Clear" -underline 1 -accelerator "Delete" -command "mainmenu_clear \[focus\] 1"

    $mb add cascade -label File -underline 0 -menu $filemenu
    $mb add cascade -label Edit -underline 0 -menu $editmenu

    mainmenu_recentmenu_populate . $recentmenu
    . configure -menu $mb
    mainmenu_nowin_rebind
}

# @from:
proc mainmenu_recentmenu_populate {win recentmenu} {
    $recentmenu delete 0 end
    set count 0
    foreach filename [/prefs:get recent_files] {
        if {[file exists $filename] && [file isfile $filename] && [file readable $filename]} {
            $recentmenu add command -label [file tail $filename] -command "mainwin_open $win $filename"
        }
        incr count
    }
    set state disabled
    if {$count > 0} {
        $recentmenu add separator
        set state normal
    }
    $recentmenu add command -label "Clear Menu" -state $state -command "/prefs:set recent_files {}"
}

# @from:
proc mainmenu_nowin_rebind {} {
    bind all <Command-Key-n>        "mainmenu_menu_invoke %W {File|New} ; break"
    bind all <Command-Key-o>        "mainmenu_menu_invoke %W {File|Open...} ; break"
    bind all <Command-Key-s>        "mainmenu_menu_invoke %W {File|Save} ; break"
    bind all <Shift-Command-Key-S>  "mainmenu_menu_invoke %W {File|Save As...} ; break"
    bind all <Command-Key-w>        "mainmenu_menu_invoke %W {File|Close} ; break"
    bind all <Command-Key-W>        "mainmenu_menu_invoke %W {File|Close} ; break"

    bind all <Command-Key-x>        "mainmenu_cut %W ; break"
    bind all <Command-Key-c>        "mainmenu_copy %W ; break"
    bind all <Command-Key-v>        "mainmenu_paste %W ; break"
    bind all <Command-Shift-Option-Control-Key-K> "console show; puts stderr \[winfo children .\]; break"
}

# @example:  mainmenu_menu_invoke %W {File|Save As...}
# @from:
proc mainmenu_menu_invoke {w menustr} {
    set toplev [mainwin_current]
    if {$toplev == ""} {
        set toplev "."
    }
    set menw [$toplev cget -menu]
    if {$menw==""} {
        set menw $toplev.menubar
    }
    # set menus [list "File" "Save As..."]
    set menus [split $menustr "|"]
    set lev 0
    set mcount [llength $menus]
    foreach mentxt $menus {
        incr lev
        set idx ""
        catch {set idx [$menw index $mentxt]}
        if {$idx == "" || $idx == "none"} {
            error "Menu item not found: $menustr (1)"
        }
        if {[$menw type $idx] == "cascade"} {
            set menw [$menw entrycget $idx -menu]
        } else {
            if {$lev != $mcount} {
                error "Menu item not found: $menustr (2)"
            }
            set cmd [$menw entrycget $idx -command]
            if {$cmd != "" && [string first "%" $cmd] >= 0} {
                eval [string map [list %W $w] $cmd]
            } else {
                $menw invoke $idx
            }
            return
        }
    }
    error "Menu item not found: $menustr (3)"
}

# @example:  mainmenu_copy %W
# @from:
proc mainmenu_copy {w {frommenu 0}} {
    switch -exact -- [string tolower [winfo class $w]] {
        text -
        entry -
        spinbox {
            if {$frommenu} {
                event generate $w <<Copy>>
            }
            return
        }
        default {
            mainwin_copy [mainwin_current]
        }
    }
}

# @example:  plugin_line_getfield .cvs $objid {12 34 56 78} LENGTH 2
# @from:
proc plugin_line_setfield {canv objid coords field val} {
    constants pi
    foreach {cx0 cy0 cx1 cy1} $coords break
    set dist [expr {hypot($cy1-$cy0,$cx1-$cx0)}]

    switch -exact -- $field {
        ANGLE {
            set cx1 [expr {$dist*cos($val*$pi/180.0)+$cx0}]
            set cy1 [expr {$dist*sin($val*$pi/180.0)+$cy0}]
            set coords [list $cx0 $cy0 $cx1 $cy1]
            cadobjects_object_set_coords $canv $objid $coords
        }
        LENGTH {
            set d 0.0
            if {$dist > 1e-6} {
                set d [expr {$val/$dist}]
            }
            set cx1 [expr {($cx1-$cx0)*$d+$cx0}]
            set cy1 [expr {($cy1-$cy0)*$d+$cy0}]
            set coords [list $cx0 $cy0 $cx1 $cy1]
            cadobjects_object_set_coords $canv $objid $coords
        }
    }
}

# @example:  plugin_line_getfield .cvs $objid {12 34 56 78} LENGTH
# @example:  plugin_line_getfield .cvs $objid {12 34 56 78} ANGLE
# @from:
proc plugin_line_getfield {canv objid coords field} {
    constants pi
    foreach {cx0 cy0 cx1 cy1} $coords break
    switch -exact -- $field {
        LENGTH {
            set d [expr {hypot($cy1-$cy0,$cx1-$cx0)}]
            return $d
        }
        ANGLE {
            set d [expr {atan2($cy1-$cy0,$cx1-$cx0)*180.0/$pi}]
            return $d
        }
    }
}

# @description: rename proc
# @from:
rename proc ::prof::_proc
::prof::_proc proc {procname procargs procbody} {
    set ns [uplevel namespace current]
    # ns = ::
    # ns = ::namespace
    if {$ns == "::"} {
        set ns ""
    }
    # ns = ""
    # ns = ::namespace
    if {[string range $procname 0 1] != "::"} {
        set procname "${ns}::${procname}"
    }
    set res [::prof::_proc $procname $procargs $procbody]
    if {![string match "::tcl::*" $procname]} {
        trace add execution $procname enter ::prof::enter
        trace add execution $procname leave ::prof::leave
    }
    return $res
}

# @from:    layers.tcl
proc layer_init {canv} {
    global layersInfo
    # variableInfo($canvasID-$Property)
    set layersInfo($canv-LAYERNUM) 0
    set layersInfo($canv-LAYERS) {}
    set layersInfo($canv-LAYERCURR) -1
    layer_set_current $canv [layer_create $canv]
}

# @from:    layers.tcl
proc layer_create {canv {name ""}} {
    set res [layer_create_noundo $canv $name]
    cutpaste_remember_layer_creation $canv $res
    # return $layerid
    return $res
}

# @from:    layers.tcl
proc layer_create_noundo {canv {name ""} {layerid -1}} {
    global layersInfo
    if {$layerid == -1} {
        set layerid [incr layersInfo($canv-LAYERNUM)]
    }
    if {$name == ""} {
        set name "Layer $layerid"
    }
    # variableInfo($canvasID-$Property-$layerID)
    # lappend variableInfo($canvasID-layerIDs) $layerID
    set layersInfo($canv-LAYERCHILDREN-$layerid) {}
    set layersInfo($canv-LAYERNAME-$layerid) $name
    set layersInfo($canv-LAYERVISIBLE-$layerid) 1
    set layersInfo($canv-LAYERLOCK-$layerid) 0
    set layersInfo($canv-LAYERCOLOR-$layerid) black
    set layersInfo($canv-LAYERCUTBIT-$layerid) 0
    set layersInfo($canv-LAYERCUTDEPTH-$layerid) 0.0
    lappend layersInfo($canv-LAYERS) $layerid
    return $layerid
}

# @from:    layers.tcl
proc layer_delete {canv layerid} {
    return [layer_delete_noundo $canv $layerid 1]
}

# @from:    layers.tcl
proc layer_delete_noundo {canv layerid {enableundo 0}} {
    global layersInfo
    set layers $layersInfo($canv-LAYERS)

    foreach objid [layer_objects $canv $layerid] {
        cadobjects_object_delete $canv $objid
    }

    # layer_delete_noundo $canv $layerid 1
    if {$enableundo} {
        cutpaste_remember_layer_deletion $canv $layerid
    }

    set pos [lsearch -exact $layers $layerid]
    if {[layer_get_current $canv] == $layerid} {
        set nlpos 0
        if {$pos == 0} {
            set nlpos 1
        }
        layer_set_current $canv [lindex $layers $nlpos]
    }

    # unset related layers property
    unset layersInfo($canv-LAYERCHILDREN-$layerid)
    unset layersInfo($canv-LAYERNAME-$layerid)
    unset layersInfo($canv-LAYERVISIBLE-$layerid)
    unset layersInfo($canv-LAYERLOCK-$layerid)
    unset layersInfo($canv-LAYERCOLOR-$layerid)
    unset layersInfo($canv-LAYERCUTBIT-$layerid)
    unset layersInfo($canv-LAYERCUTDEPTH-$layerid)

    # delete layerID from list of layerIDs
    if {$pos >= 0} {
        set layers [lreplace $layers $pos $pos]
    }
    # restore layerIDs
    set layersInfo($canv-LAYERS) $layers
    # if layerIDs is null, reset related variables
    if {[llength $layers] == 0} {
        set layersInfo($canv-LAYERNUM) 0
        set layersInfo($canv-LAYERCURR) -1
    }
}

# @from:    layers.tcl
proc layer_name_id {canv name} {
    foreach lid [layer_ids $canv] {
        set lname [layer_name $canv $lid]
        if {$name eq $lname} {
            return $lid
        }
    }
    return ""
}

# @from:    layers.tcl
# @return:  boolean
proc layer_exists {canv layerid} {
    global layersInfo
    return [info exists layersInfo($canv-LAYERNAME-$layerid)]
}

# @example: layer_serialize .cvs 12
# @result:  key value key value ...
# @result:  "layerid" 12 "pos" {11 34} "name" "layerid12" ...
# @from:    layers.tcl
proc layer_serialize {canv layerid} {
    # set layers [layer_ids $canv]
    set out {}
    lappend out "layerid"  $layerid
    lappend out "pos"      [layer_pos $canv $layerid]
    lappend out "name"     [layer_name $canv $layerid]
    lappend out "visible"  [layer_visible $canv $layerid]
    lappend out "locked"   [layer_locked $canv $layerid]
    lappend out "color"    [layer_color $canv $layerid]
    lappend out "cutbit"   [layer_cutbit $canv $layerid]
    lappend out "cutdepth" [layer_cutdepth $canv $layerid]
    return $out
}

# @example: layer_deserialize .cvs 12 1 {"layerid" 12 "pos" {11 34} "name" "layerid12" ...}
# @result:  $layerid
# @from:    layers.tcl
proc layer_deserialize {canv layerid forcenew info} {
    global layersInfo

    array set data $info
    # data(layerid) = 12
    # data(pos) = {11 34}
    # data(name) = layerid12
    if {$forcenew} {
        set layerid -1
    }
    foreach tag {layerid name visible locked color cutbit cutdepth pos} {
        if {![info exists data($tag)]} {
            error "Internal error: serialization contains no $tag."
        }
    }
    if {$layerid == -1} {
        set layerid [layer_create_noundo $canv $data(name)]
    } elseif {![layer_exists $canv $layerid]} {
        set layerid [layer_create_noundo $canv $data(name) $layerid]
    } else {
        set layersInfo($canv-LAYERNAME-$layerid) $data(name)
    }
    set layersInfo($canv-LAYERVISIBLE-$layerid) $data(visible)
    set layersInfo($canv-LAYERLOCK-$layerid) $data(locked)
    set layersInfo($canv-LAYERCOLOR-$layerid) $data(color)
    set layersInfo($canv-LAYERCUTBIT-$layerid) $data(cutbit)
    set layersInfo($canv-LAYERCUTDEPTH-$layerid) $data(cutdepth)
    layer_reorder $canv $layerid $data(pos)
    mainwin_redraw [winfo toplevel $canv]
    return $layerid
}

# @example: layer_ids .cvs
# @result:  list of $layerIDs
# @from:    layers.tcl
proc layer_ids {canv} {
    global layersInfo
    if {![info exists layersInfo($canv-LAYERS)]} {
        return {}
    }
    return $layersInfo($canv-LAYERS)
}

# @example: layer_get_current .cvs
# @result:  $layerid
# @from:    layers.tcl
proc layer_get_current {canv} {
    global layersInfo
    # get current layerid from variables
    if {[info exists layersInfo($canv-LAYERCURR)]} {
        set layerid $layersInfo($canv-LAYERCURR)
        if {[llength $layersInfo($canv-LAYERS)] == 0} {
            return -1
        }
    } else {
        return -1
    }
    if {$layerid == -1} {
        set layerid [layer_create $canv]
        set layersInfo($canv-LAYERCURR) $layerid
    }
    return $layerid
}

proc layer_pos {canv layerid} {
    set layers [layer_ids $canv]
    set pos [lsearch -exact $layers $layerid]
    return $pos
}

proc layer_reorder {canv layerid newpos} {
    global layersInfo
    set layers $layersInfo($canv-LAYERS)
    set oldpos [lsearch -exact $layers $layerid]
    set layers [lreplace $layers $oldpos $oldpos]
    set layers [linsert $layers $newpos $layerid]
    set layersInfo($canv-LAYERS) $layers
}

proc layer_set_current {canv layerid} {
    global layersInfo
    set layersInfo($canv-LAYERCURR) $layerid
}


proc layer_name {canv layerid} {
    global layersInfo
    return $layersInfo($canv-LAYERNAME-$layerid)
}

proc layer_set_name {canv layerid val} {
    global layersInfo
    cutpaste_remember_layer_change $canv $layerid
    set layersInfo($canv-LAYERNAME-$layerid) $val
}


proc layer_visible {canv layerid} {
    global layersInfo
    return $layersInfo($canv-LAYERVISIBLE-$layerid)
}


proc layer_set_visible {canv layerid val} {
    global layersInfo
    set layersInfo($canv-LAYERVISIBLE-$layerid) $val
}


proc layer_locked {canv layerid} {
    global layersInfo
    return $layersInfo($canv-LAYERLOCK-$layerid)
}


proc layer_set_locked {canv layerid val} {
    global layersInfo
    set layersInfo($canv-LAYERLOCK-$layerid) $val
}


proc layer_color {canv layerid} {
    global layersInfo
    return $layersInfo($canv-LAYERCOLOR-$layerid)
}


proc layer_set_color {canv layerid val} {
    global layersInfo
    cutpaste_remember_layer_change $canv $layerid
    set layersInfo($canv-LAYERCOLOR-$layerid) $val
}


proc layer_cutbit {canv layerid} {
    global layersInfo
    return $layersInfo($canv-LAYERCUTBIT-$layerid)
}


proc layer_set_cutbit {canv layerid val} {
    global layersInfo
    cutpaste_remember_layer_change $canv $layerid
    set layersInfo($canv-LAYERCUTBIT-$layerid) $val
}


proc layer_cutdepth {canv layerid} {
    global layersInfo
    return $layersInfo($canv-LAYERCUTDEPTH-$layerid)
}


proc layer_set_cutdepth {canv layerid val} {
    global layersInfo
    cutpaste_remember_layer_change $canv $layerid
    set layersInfo($canv-LAYERCUTDEPTH-$layerid) $val
}


proc layer_objects {canv layerid} {
    global layersInfo
    return $layersInfo($canv-LAYERCHILDREN-$layerid)
}


proc layer_object_add {canv layerid objid} {
    global layersInfo
    lappend layersInfo($canv-LAYERCHILDREN-$layerid) $objid
    return
}


proc layer_object_delete {canv layerid objid} {
    global layersInfo
    set objs $layersInfo($canv-LAYERCHILDREN-$layerid)
    set pos [lsearch -exact $objs $objid]
    if {$pos >= 0} {
        set objs [lreplace $objs $pos $pos]
    }
    set layersInfo($canv-LAYERCHILDREN-$layerid) $objs
}



proc layer_object_arrange {canv layerid objid relpos} {
    set objs [layer_objects $canv $layerid]
    set origpos [lsearch -exact $objs $objid]
    set objs [lreplace $objs $origpos $origpos]
    set nupos $origpos
    if {$relpos == "bottom"} {
        set nupos end
    } elseif {$relpos == "top"} {
        set nupos 0
    } else {
        incr nupos $relpos
    }
    set objs [linsert $objs $nupos $objid]
    set layersInfo($canv-LAYERCHILDREN-$layerid) $objs
}

# vim: set ts=4 sw=4 nowrap expandtab: settings
